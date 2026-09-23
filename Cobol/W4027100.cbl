000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4027100.                                                
000300 AUTHOR.         HENRIKSSON ANDERS.                                       
000400 DATE-WRITTEN.   03/02/19.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THE PROGRAM UPDATES VOR ROLES                                    
000900*        HANDLING EACH DISTRICT AND PROCURER                              
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDP4                                       
001200*        STARTS BMP W412S3 VIA PGM W00606                                 
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSACTION: W4T271                                              
001600*        TRANSACTION: W4T271U                                             
001700*        MID:         W4I27101                                            
001800*                                                                         
001900*    OUTDATA.                                                             
002000*        MOD:         W4O27101                                            
002100*        TRANSAKTION: W0T606U                                             
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W4027100'.            
003000                                                                          
003100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003200 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  YES                         PIC X       VALUE 'J'.                   
003500 77  NOO                         PIC X       VALUE 'N'.                   
003600 77  CURRENT-SECTION             PIC X(16)   VALUE 'MAIN'.                
003700 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
003800                                                                          
003900*    --- INDEX FOR SCROLL LINES                                           
004000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004100 77  MAX-INDX                    PIC S9(4)  VALUE +24   COMP SYNC.        
004200                                                                          
004300*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004500     88  INDATA-OK                           VALUE 'J'.                   
004600     88  INDATA-WRONG                        VALUE 'N'.                   
004700                                                                          
004800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004900     88  KEYS-OK                             VALUE 'J'.                   
005000     88  KEYS-WRONG                          VALUE 'N'.                   
005100                                                                          
005200 77  INTERVALL-KOLL-SW           PIC X       VALUE SPACE.                 
005300     88  INTERVALL-KOLL-OK                   VALUE 'J'.                   
005400     88  INTERVALL-KOLL-WRONG                VALUE 'N'.                   
005500                                                                          
005600 77  INTERVALL-SW                PIC X       VALUE SPACE.                 
005700     88  INTERVALL-OK                        VALUE 'J'.                   
005800     88  INTERVALL-WRONG                     VALUE 'N'.                   
005900                                                                          
006000 77  UPDATE-SW                   PIC X       VALUE SPACE.                 
006100     88  SW-INSERT                           VALUE 'N' 'I'.               
006200     88  SW-DELETE                           VALUE 'D'.                   
006300     88  SW-PGM-TO-PGM                       VALUE 'U'.                   
006400                                                                          
006500 77  UPDATE-INTERVALL-SW         PIC X       VALUE SPACE.                 
006600     88  INTERVALL-INSERT                    VALUE 'I'.                   
006700     88  INTERVALL-DELETE                    VALUE 'D'.                   
006800     88  INTERVALL-INVALID                   VALUE 'N'.                   
006900                                                                          
007000 77  UPDATE-ROLL-SW              PIC X       VALUE SPACE.                 
007100     88  ROLL-INSERT                         VALUE 'I'.                   
007200     88  ROLL-DELETE                         VALUE 'D'.                   
007300     88  ROLL-INVALID                        VALUE 'N'.                   
007400                                                                          
007500 77  DUBBLA-ROLLER-SW            PIC X       VALUE 'N'.                   
007600     88  DUBBLA-ROLLER                       VALUE 'J'.                   
007700                                                                          
007800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007900     88  OWN-MID                             VALUE '4271'.                
008000     88  GOOD-MID                            VALUE '4271' '4225'          
008100                                                   '4226' '4227'          
008200                                                   '4228' '4275'          
008300                                                   '4276' '4277'          
008400                                                   '4278'.                
008500     88  HELP-MID                            VALUE '0551'.                
008600     EJECT                                                                
008700                                                                          
008800 01  WS-CURRENT-DATE             PIC 9(6)    VALUE ZERO.                  
008900 01  WS-IDDISTR-FOM              PIC 9(4)    VALUE ZERO.                  
009100 01  WS-IDROLL-OTHER             PIC X(5)    VALUE SPACE.                 
009200                                                                          
009300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009400 01  GENERAL-SUBPROGRAMS.                                                 
009500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009900     EJECT                                                                
010000                                                                          
010100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
010200*01 -COPY WMEDAREA                                                        
010300     SKIP3                                                                
010400                                                                          
010500 01  MESSAGE-CODES.                                                       
010600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011300     03  ROLL-MISSING            PIC X(3)    VALUE '275'.                 
011400     EJECT                                                                
011500                                                                          
011600*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
011700*                                                                         
011800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011900     SKIP3                                                                
012000*01 -COPY WMSGINIT                                                        
012100     EJECT                                                                
012200                                                                          
012300*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
012400*                                                                         
012500 01  SAVE-AREA.                                                           
012600     03  SAVE-IDTRANS             PIC X(4)  VALUE '4271'.                 
012700     03  SAVE-IDROLL-ENTER        PIC X(5).                               
012800     03  SAVE-IDROLL-NEXT         PIC X(5).                               
012900     03  SAVE-IDDISTR-ENTER       PIC S9(5) COMP-3.                       
013000     03  SAVE-IDDISTR-NEXT        PIC S9(5) COMP-3.                       
013300     EJECT                                                                
013400                                                                          
013500*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
013600*                                                                         
013700 01  FILLER                      PIC X(16)  VALUE 'MID-AREA'.             
013800     SKIP3                                                                
013900*01  MID -COPY W4I27101                                                   
014000     EJECT                                                                
014100                                                                          
014200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014300     SKIP3                                                                
014400*01  -COPY WMSGAREA                                                       
014500     EJECT                                                                
014600     03  MOD REDEFINES MSG-AREA.                                          
014700*      05  -COPY W4O27101                                                 
014800     EJECT                                                                
014900                                                                          
015000 01  FILLER                      PIC X(16)  VALUE 'MFS-AREA'.             
015100     SKIP3                                                                
015200*01  -COPY WMFSAREA                                                       
015300     EJECT                                                                
015400                                                                          
015500 01  W-PROG-TO-PROG-SW.                                                   
015600*  03    -COPY WMSGSOP                                                    
015700                                                                          
015800*    --- WORK-AREAS FOR IMS-SECTIONS                                      
015900*                                                                         
016000 01  FILLER                      PIC X(16)  VALUE 'IMS-WS'.               
016100     SKIP3                                                                
016200 01  KEYS-TO-DLI.                                                         
016300*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
016400     03  W-IDROLL-MIN-X.                                                  
016500         05  W-IDROLL-MIN     PIC X(5) VALUE SPACE.                       
016600                                                                          
016700     03  W-IDDISTR-MIN-X.                                                 
016800         05  W-IDDISTR-MIN     PIC S9(5) VALUE ZERO COMP-3.               
016900                                                                          
017000     03  W-IDROLL-X.                                                      
017100         05  W-IDROLL.                                                    
017200             07  FILLER          PIC X(3)   VALUE SPACE.                  
017300             07  W-IDROLL-LOPNR  PIC 9(2)   VALUE ZERO.                   
017400                                                                          
017500     03  W-IDROLL-SEARCH-X.                                               
017600         05  W-IDROLL-SEARCH     PIC X(5)   VALUE SPACE.                  
017700                                                                          
017800     03  W-IDDISTR-X.                                                     
017900         05  W-IDDISTR           PIC S9(5)  VALUE ZERO COMP-3.            
018000                                                                          
018100     03  W-IDDISTR-0001-X.                                                
018200         05  W-IDDISTR-0001      PIC S9(5)  VALUE +1    COMP-3.           
018300                                                                          
018400     03  W-IDDISTR-9999-X.                                                
018500         05  W-IDDISTR-9999      PIC S9(5)  VALUE +9999 COMP-3.           
018600                                                                          
019800     SKIP2                                                                
019900                                                                          
020000*    --- STATUS-KOD FRÅN IMS                                              
020100 01  STATUS-WS                   PIC XX.                                  
020200     88  SEGMENT-FOUND                      VALUE '  '.                   
020300     88  SEGMENT-FOUND-EXISTS               VALUE 'II'.                   
020400     88  SEGMENT-MISSING                    VALUE 'GE'.                   
020500     88  END-OF-DB                          VALUE 'GB'.                   
020600     SKIP2                                                                
020700 01  GOOD-STATUSCODES.                                                    
020800     03  GOOD-STATUS OCCURS 7 INDEXED BY STATUS-IX PIC XX.                
020900     SKIP3                                                                
021000 01  SSA1                        PIC X(64).                               
021100 01  SSA2                        PIC X(64).                               
021200     EJECT                                                                
021300                                                                          
021400*    --- IMS FUNCTION CODES                                               
021500*01  -COPY W0003                                                          
021600     EJECT                                                                
021700                                                                          
021800*    ---  DLI INPUT-OUTPUT AREA                                           
021900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP401'.                      
022000 01  DLI-IO-WDP401.                                                       
022100*    03  -COPY WDP401                                                     
022200     EJECT                                                                
022300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP411'.                      
022400 01  DLI-IO-WDP411.                                                       
022500*    03  -COPY WDP411                                                     
022600     EJECT                                                                
022700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP4A11'.                     
022800 01  DLI-IO-WDP411-A11.                                                   
022900*    03  -COPY WDP411 -PRE A11-                                           
023000     EJECT                                                                
023800                                                                          
023900 LINKAGE SECTION.                                                         
024000*01  -COPY W0009   -PRE MSG-                                              
024100     SKIP2                                                                
024200*01  -COPY W0009   -PRE ALT-                                              
024300     SKIP2                                                                
024400*01  -COPY W0008   -PRE USEA-                                             
024500     05  FILLER                  PIC X.                                   
024600*01  -COPY W0008   -PRE WDP4-                                             
024700     05 FILLER                   PIC X.                                   
024800*01  -COPY W0008   -PRE WDP4-SEARCH-                                      
024900     05 FILLER                   PIC X.                                   
025000*01  -COPY W0008   -PRE WDP4A-                                            
025100     05 FILLER                   PIC X.                                   
025200*01  -COPY W0008   -PRE WDP4A-NEXT-                                       
025300     05 FILLER                   PIC X.                                   
025900                                                                          
026000 PROCEDURE DIVISION  USING MSG-PCB   ALT-PCB   USEA-PCB                   
026100                           WDP4-PCB  WDP4-SEARCH-PCB                      
026200                           WDP4A-PCB WDP4A-NEXT-PCB.                      
026300                                                                          
026400 MAIN SECTION.                                                            
026500     ENTRY 'DLITCBL' USING MSG-PCB   ALT-PCB   USEA-PCB                   
026600                           WDP4-PCB  WDP4-SEARCH-PCB                      
026700                           WDP4A-PCB WDP4A-NEXT-PCB.                      
026800                                                                          
026900                                                                          
027000     PERFORM IMS-01-GET-MSG                                               
027100     IF SEGMENT-FOUND                                                     
027200       PERFORM A-INIT                                                     
027300       PERFORM B-CHECK-KEYS                                               
027400       IF KEYS-OK                                                         
027500         IF MFS-UPDATE                                                    
027600           PERFORM G-CHECK-INPUT                                          
027700           IF INDATA-OK                                                   
027800             PERFORM H-UPDATE                                             
027900           END-IF                                                         
028000         ELSE                                                             
028100           IF MFS-FIRST                                                   
028200             PERFORM C-FIRST-PAGE                                         
028300           ELSE                                                           
028400             IF MFS-NEXT                                                  
028500               PERFORM D-NEXT-PAGE                                        
028600             ELSE                                                         
028700               PERFORM E-SAME-PAGE                                        
028800             END-IF                                                       
028900           END-IF                                                         
029000         END-IF                                                           
029100         PERFORM F-READ-SHOW-INFO                                         
029200       END-IF                                                             
029300       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O27101 + 4                      
029400       PERFORM IMS-02-INSERT-MSG                                          
029500     END-IF                                                               
029600                                                                          
029700     MOVE ZERO TO RETURN-CODE                                             
029800     GOBACK                                                               
029900     .                                                                    
030000     EJECT                                                                
030100                                                                          
030200 A-INIT SECTION.                                                          
030300     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
030400                                                                          
030500     IF MSG-DOUBLE-TRANSACTIONS                                           
030600       MOVE MSG-INDATA-MINUS-2-TRANSACT  TO MID-W4I27101                  
030700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
030800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
030900     ELSE                                                                 
031000       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I27101                  
031100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
031200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
031300     END-IF                                                               
031400                                                                          
031500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
031600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
031700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
031800                                                                          
031900     MOVE LOW-VALUE       TO MSG-AREA                                     
032000     MOVE 'W4O271N1'      TO MFS-IDMOD                                    
032100     MOVE '4271'          TO MOD-IDTRANS                                  
032200     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
032300                                                                          
032400     IF OWN-MID OR HELP-MID                                               
032500       CONTINUE                                                           
032600     ELSE                                                                 
032700       MOVE SPACE TO MFS-KDTRTYP                                          
032800       MOVE '7' TO MFS-IDPFK                                              
032900     END-IF                                                               
033000                                                                          
033100*    -- COMPUTERS DATE FOR INSERT                                         
033200     MOVE FUNCTION CURRENT-DATE (3:6) TO WS-CURRENT-DATE                  
033300     .                                                                    
033400                                                                          
033500                                                                          
033600 B-CHECK-KEYS SECTION.                                                    
033700     MOVE 'B-CHECK-KEYS    ' TO CURRENT-SECTION                           
033800                                                                          
033900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
034000     MOVE '001'             TO MSGI-KDCALL                                
034100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
034200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
034300     MOVE '4271'            TO MSGI-IDTRANS                               
034400                                                                          
034500     IF GOOD-MID                                                          
034600       IF MID-IDROLL NOT = ALL '+'                                        
034700         MOVE MID-IDROLL    TO W-IDROLL                                   
034800                               MOD-IDROLL                                 
034900                               MSGI-IDROLL                                
035000* VID BYTE ELLER UPPREPANDE AV SAMMA ROLL VILL MAN STARTA OM              
035100         MOVE ALL '+'       TO MID-W4I27101                               
035200         MOVE W-IDROLL      TO MID-IDROLL                                 
035300                                                                          
035400       ELSE                                                               
035500         MOVE MID-IDROLL-UT TO W-IDROLL                                   
035600                               MOD-IDROLL                                 
035700                               MSGI-IDROLL                                
035800       END-IF                                                             
035900     END-IF                                                               
036000                                                                          
036100     IF GOOD-MID                                                          
036200       INSPECT MID-IDDISTR-FOM-UPD REPLACING LEADING SPACE BY ZERO        
036300       IF MID-IDDISTR-FOM-UPD = ALL '+'                                   
036400          MOVE ZERO                TO W-IDDISTR                           
036401       ELSE                                                               
036410          MOVE MID-IDDISTR-FOM-UPD TO W-IDDISTR                           
036500       END-IF                                                             
036510     END-IF                                                               
036600                                                                          
036700     CALL W005INIT          USING MSGI-WMSGINIT USEA-PCB                  
036800     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
036900                                                                          
037000*    - LANGUAGE TO BE USED BY MEDKONV                                     
037100     MOVE '2'               TO MED-IDSKYLT                                
037200                                                                          
037300     MOVE YES               TO KEYS-SW                                    
037400                                                                          
037500*    -- CHECK OF IDROLL                                                   
037600     MOVE MFS-ERASE-FIELD   TO MOD-IDROLL-IN                              
037700     MOVE MFS-ERASE-FIELD   TO MOD-KDBEHX                                 
037800     MOVE MFS-ERASE-FIELD   TO MOD-IDDISTR-TOM-UPD                        
037900     MOVE MFS-ERASE-FIELD   TO MOD-IDDISTR-FOM-UPD                        
038200                                                                          
038300     IF W-IDROLL = 'VOR99' OR W-IDROLL-LOPNR NOT NUMERIC                  
038400       MOVE NOO TO KEYS-SW                                                
038500     END-IF                                                               
038600                                                                          
038700     IF W-IDROLL > 'VOR00' AND W-IDROLL < 'VOR99'                         
038800       CONTINUE                                                           
038900     ELSE                                                                 
039000       MOVE NOO TO KEYS-SW                                                
039100     END-IF                                                               
039200                                                                          
039300     IF KEYS-WRONG                                                        
039400       MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                               
039500       CALL WMEDKONV        USING MED-WMEDAREA                            
039600       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
039700       PERFORM MFS-ERASE-FIELD-IN                                         
039800       PERFORM MFS-ERASE-FIELD-OUT                                        
039900     END-IF                                                               
040000     .                                                                    
040100     EJECT                                                                
040200                                                                          
040300 C-FIRST-PAGE SECTION.                                                    
040400     MOVE 'C-FIRST-PAGE    ' TO CURRENT-SECTION                           
040500                                                                          
040600     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
040700     CALL WMEDKONV       USING MED-WMEDAREA                               
040800     MOVE MED-MFSFEL     TO MOD-TEMFSFEL                                  
040900                                                                          
041000     PERFORM MFS-ERASE-FIELD-IN                                           
041100     .                                                                    
041200     EJECT                                                                
041300                                                                          
041400 D-NEXT-PAGE SECTION.                                                     
041500     MOVE 'D-NEXT-PAGE     ' TO CURRENT-SECTION                           
041600                                                                          
041700     IF SAVE-IDTRANS = '4271'                                             
041800       MOVE SAVE-IDDISTR-NEXT TO W-IDDISTR-MIN                            
041900     ELSE                                                                 
042000       PERFORM MFS-ERASE-FIELD-IN                                         
042100     END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400                                                                          
042500 E-SAME-PAGE SECTION.                                                     
042600     MOVE 'E-SAME-PAGE     ' TO CURRENT-SECTION                           
042700                                                                          
042800     IF SAVE-IDTRANS = '4271' OR '0551'                                   
042900       IF MID-KDBEHX = ALL '+'                                            
043000         PERFORM MFS-ERASE-FIELD-IN                                       
043100       ELSE                                                               
043200         MOVE INF-PRESS-PF11   TO MED-IDMFSFEL                            
043300         CALL WMEDKONV         USING MED-WMEDAREA                         
043400         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
043500         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
043600         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
043700         PERFORM MFS-READ-IN-AGAIN                                        
043800       END-IF                                                             
043900     ELSE                                                                 
044000       PERFORM MFS-ERASE-FIELD-IN                                         
044100     END-IF                                                               
044200     .                                                                    
044300     EJECT                                                                
044400                                                                          
044500 F-READ-SHOW-INFO SECTION.                                                
044600     MOVE 'F-READ-SHOW-INFO' TO CURRENT-SECTION                           
044700                                                                          
044800     INSPECT MID-IDDISTR-TOM-UPD REPLACING LEADING SPACE BY ZERO          
044910       IF MID-IDDISTR-TOM-UPD = ALL '+'                                   
044920          MOVE ZERO                TO W-IDDISTR                           
044930       ELSE                                                               
044940          MOVE MID-IDDISTR-TOM-UPD TO W-IDDISTR                           
044950       END-IF                                                             
045000     PERFORM IMS-04-GU-WDP401                                             
045100                                                                          
045200     IF SEGMENT-MISSING                                                   
045300        STRING 'VOR ROLE MISSING:'                                        
045400        DELIMITED BY SIZE INTO MOD-TEMFSFEL                               
045500        PERFORM MFS-ERASE-FIELD-OUT                                       
045600     ELSE                                                                 
045700        PERFORM FA-SHOW-IDDISTR                                           
045800                                                                          
045900        PERFORM IMS-04-GU-WDP401                                          
046100        MOVE '002'      TO MSGI-KDCALL                                    
046200        MOVE '4271'     TO SAVE-IDTRANS                                   
046300        MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                 
046400        CALL W005INIT   USING MSGI-WMSGINIT USEA-PCB                      
046500     END-IF                                                               
046600     .                                                                    
046700     EJECT                                                                
046800                                                                          
046900 FA-SHOW-IDDISTR  SECTION.                                                
047000     MOVE 'FA-SHOW-IDDISTR ' TO CURRENT-SECTION                           
047100                                                                          
047200     IF MFS-NEXT                                                          
047300        PERFORM IMS-08-GHNP-WDP411-NEXT                                   
047400     ELSE                                                                 
047500        PERFORM IMS-11-GNP-WDP411                                         
047600     END-IF                                                               
047700     MOVE +1 TO INDX                                                      
047800     IF SEGMENT-FOUND                                                     
047900        MOVE ROL-IDROLL      TO SAVE-IDROLL-ENTER                         
048000        MOVE DEF-IDDISTR-TOM TO SAVE-IDDISTR-ENTER                        
048100        MOVE DEF-IDDISTR-TOM TO SAVE-IDDISTR-NEXT                         
048200                                                                          
048300        PERFORM UNTIL SEGMENT-MISSING OR INDX > MAX-INDX                  
048400           IF DEF-IDDISTR-TOM = 9999 AND                                  
048500              DEF-IDDISTR-FOM = 0001                                      
048600              CONTINUE                                                    
048700           ELSE                                                           
048800              MOVE DEF-IDDISTR-TOM TO MOD-IDDISTR-TOM(INDX)               
048900              MOVE DEF-IDDISTR-FOM TO MOD-IDDISTR-FOM(INDX)               
049000              ADD 1 TO INDX                                               
049100           END-IF                                                         
049200           PERFORM IMS-11-GNP-WDP411                                      
049300           IF NOT MFS-UPDATE AND MFS-NEXT                                 
049400             STRING 'LAST PAGE'                                           
049500             DELIMITED BY SIZE INTO MOD-TEMFSFEL                          
049600           END-IF                                                         
049700        END-PERFORM                                                       
049800                                                                          
049900     ELSE                                                                 
050000        MOVE W-IDROLL-MIN      TO SAVE-IDROLL-ENTER                       
050100        MOVE W-IDDISTR-MIN     TO SAVE-IDDISTR-ENTER                      
050200     END-IF                                                               
050300                                                                          
050400     PERFORM UNTIL INDX > MAX-INDX                                        
050500        MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-FOM(INDX)                     
050600                                MOD-IDDISTR-TOM(INDX)                     
050700        ADD 1 TO INDX                                                     
050800     END-PERFORM                                                          
050900                                                                          
051000     IF SEGMENT-FOUND                                                     
051100        MOVE ROL-IDROLL           TO SAVE-IDROLL-NEXT                     
051200        MOVE DEF-IDDISTR-TOM      TO SAVE-IDDISTR-NEXT                    
051300        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSFEL                         
051400        CALL WMEDKONV             USING MED-WMEDAREA                      
051500        MOVE MED-TEMFSFEL         TO MOD-TEMFSINF                         
051600     ELSE                                                                 
051700        MOVE ROL-IDROLL           TO SAVE-IDROLL-NEXT                     
051800     END-IF                                                               
051900     .                                                                    
052000                                                                          
057300                                                                          
057400 G-CHECK-INPUT SECTION.                                                   
057500     MOVE 'G-CHECK-INPUT   ' TO CURRENT-SECTION                           
057600                                                                          
057700     MOVE YES  TO INDATA-SW                                               
057800     IF MID-KDBEHX = ALL '+'                                              
057900       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
058000       CALL WMEDKONV USING MED-WMEDAREA                                   
058100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
058200       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
058300       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
058400       PERFORM MFS-READ-IN-AGAIN                                          
058500       MOVE NOO TO INDATA-SW                                              
058600     ELSE                                                                 
058700       IF MID-KDBEHX = 'N' OR MID-KDBEHX = 'D' OR                         
058800          MID-KDBEHX = 'I' OR MID-KDBEHX = 'U'                            
058900          MOVE MFS-ALPHA-FIELD-OK TO MOD-KDBEHX-ATTR                      
059000       ELSE                                                               
059100          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDBEHX-ATTR                   
059200          MOVE NOO TO INDATA-SW                                           
059300       END-IF                                                             
059400     END-IF                                                               
059500                                                                          
059600     IF INDATA-OK                                                         
059700       IF MID-IDDISTR-TOM-UPD = ALL '+' AND                               
059800          MID-IDDISTR-FOM-UPD = ALL '+' AND                               
060100          MID-KDBEHX NOT = 'U'                                            
060200                                                                          
060300          MOVE MFS-NUM-FIELD-WRONG TO MOD-IDDISTR-TOM-ATTR                
060400          MOVE MFS-NUM-FIELD-WRONG TO MOD-IDDISTR-FOM-ATTR                
060700          MOVE NOO TO INDATA-SW                                           
060800       END-IF                                                             
060900     END-IF                                                               
061000                                                                          
061100     IF INDATA-OK                                                         
061200       IF MID-KDBEHX = 'U' AND                                            
061300         (MID-IDDISTR-TOM-UPD NOT = ALL '+' OR                            
061400          MID-IDDISTR-FOM-UPD NOT = ALL '+' )                             
061700                                                                          
061800                                                                          
061900          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDBEHX-ATTR                   
062000          MOVE MFS-NUM-FIELD-WRONG   TO MOD-IDDISTR-TOM-ATTR              
062100          MOVE MFS-NUM-FIELD-WRONG   TO MOD-IDDISTR-FOM-ATTR              
062400          MOVE NOO TO INDATA-SW                                           
062500       END-IF                                                             
062600     END-IF                                                               
062700                                                                          
062800     IF INDATA-OK                                                         
062900        IF MID-IDDISTR-TOM-UPD = ALL '+' AND                              
063000           MID-IDDISTR-FOM-UPD = ALL '+'                                  
063100           CONTINUE                                                       
063200        ELSE                                                              
063300           PERFORM GA-CHECK-DISTR-INTERVALL                               
063400        END-IF                                                            
063500     END-IF                                                               
064500                                                                          
064600     IF INDATA-WRONG                                                      
064700        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
064800        CALL WMEDKONV USING MED-WMEDAREA                                  
064900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
065000        PERFORM MFS-DONT-TOUCH-FIELD-OUT                                  
065100        PERFORM MFS-DONT-TOUCH-FIELD-IN                                   
065200     ELSE                                                                 
065300        PERFORM GC-CHECK-ROLL                                             
065400        IF SEGMENT-FOUND                                                  
065500           IF MID-IDDISTR-TOM-UPD NOT = ALL '+'                           
065600              PERFORM GD-CHECK-IDDISTR                                    
065700           END-IF                                                         
065800                                                                          
066500           IF INDATA-OK                                                   
066600             CONTINUE                                                     
066700           ELSE                                                           
066800             PERFORM MFS-ERASE-FIELD-IN                                   
066900             PERFORM MFS-ERASE-FIELD-OUT                                  
067000             PERFORM MFS-READ-IN-AGAIN                                    
067100             MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                    
067200             CALL WMEDKONV USING MED-WMEDAREA                             
067300             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
067400             MOVE NOO TO INDATA-SW                                        
067500           END-IF                                                         
067600        ELSE                                                              
067700           IF SW-DELETE AND NOT INTERVALL-INVALID                         
067800             IF MID-IDDISTR-TOM-UPD NOT = ALL '+'                         
067900                PERFORM GD-CHECK-IDDISTR                                  
068000             END-IF                                                       
068700                                                                          
068800             IF INDATA-OK                                                 
068900               CONTINUE                                                   
069000             ELSE                                                         
069100               PERFORM MFS-ERASE-FIELD-IN                                 
069200               PERFORM MFS-ERASE-FIELD-OUT                                
069300               PERFORM MFS-READ-IN-AGAIN                                  
069400               MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                  
069500               CALL WMEDKONV USING MED-WMEDAREA                           
069600               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
069700               MOVE NOO TO INDATA-SW                                      
069800             END-IF                                                       
069900           ELSE                                                           
070000             IF SW-INSERT                                                 
070100             IF MID-IDDISTR-TOM-UPD NOT = ALL '+'                         
070200                PERFORM GD-CHECK-IDDISTR                                  
070300             END-IF                                                       
070400                                                                          
071100               IF INTERVALL-OK                                            
071200                 CONTINUE                                                 
071300               ELSE                                                       
071400                 PERFORM MFS-ERASE-FIELD-IN                               
071500                 PERFORM MFS-ERASE-FIELD-OUT                              
071600                 PERFORM MFS-READ-IN-AGAIN                                
071700                 MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                
071800                 CALL WMEDKONV USING MED-WMEDAREA                         
071900                 MOVE MED-MFSFEL TO MOD-TEMFSFEL                          
072000                 MOVE NOO TO INDATA-SW                                    
072100               END-IF                                                     
072200             ELSE                                                         
072300               MOVE NOO TO INDATA-SW                                      
072400             END-IF                                                       
072500           END-IF                                                         
072600        END-IF                                                            
072700     END-IF                                                               
072800     .                                                                    
072900     EJECT                                                                
073000                                                                          
073100 GA-CHECK-DISTR-INTERVALL SECTION.                                        
073200     MOVE 'GA-CHECK-DISTR-I' TO CURRENT-SECTION                           
073300                                                                          
073400     IF MID-IDDISTR-TOM-UPD = ALL '+'                                     
073500        MOVE MFS-NUM-FIELD-WRONG TO MOD-IDDISTR-TOM-ATTR                  
073600        MOVE NOO TO INDATA-SW                                             
073700     ELSE                                                                 
073800        IF MID-IDDISTR-TOM-UPD NOT NUMERIC                                
073900           MOVE MFS-NUM-FIELD-WRONG TO MOD-IDDISTR-TOM-ATTR               
074000           MOVE NOO TO INDATA-SW                                          
074100        ELSE                                                              
074200           INSPECT MID-IDDISTR-TOM-UPD                                    
074300           REPLACING LEADING SPACE BY ZERO                                
074400           MOVE MFS-NUM-FIELD-OK TO MOD-IDDISTR-TOM-ATTR                  
074500        END-IF                                                            
074600     END-IF                                                               
074700                                                                          
074800     IF INDATA-OK                                                         
074900        IF MID-IDDISTR-FOM-UPD = ALL '+'                                  
075000           MOVE MFS-NUM-FIELD-WRONG TO MOD-IDDISTR-FOM-ATTR               
075100           MOVE NOO TO INDATA-SW                                          
075200        ELSE                                                              
075300           IF MID-IDDISTR-FOM-UPD NOT NUMERIC                             
075400              MOVE MFS-NUM-FIELD-WRONG TO MOD-IDDISTR-FOM-ATTR            
075500              MOVE NOO TO INDATA-SW                                       
075600           ELSE                                                           
075700              INSPECT MID-IDDISTR-FOM-UPD                                 
075800              REPLACING LEADING SPACE BY ZERO                             
075900              MOVE MFS-NUM-FIELD-OK TO MOD-IDDISTR-FOM-ATTR               
076000           END-IF                                                         
076100        END-IF                                                            
076200     END-IF                                                               
076300                                                                          
076400     IF INDATA-OK                                                         
076500        IF MID-IDDISTR-FOM-UPD > MID-IDDISTR-TOM-UPD                      
076600           MOVE MFS-NUM-FIELD-WRONG TO MOD-IDDISTR-FOM-ATTR               
076700           MOVE MFS-NUM-FIELD-WRONG TO MOD-IDDISTR-TOM-ATTR               
076800           MOVE NOO TO INDATA-SW                                          
076900        END-IF                                                            
077000     END-IF                                                               
077100     .                                                                    
077200     EJECT                                                                
077300                                                                          
081700 GC-CHECK-ROLL SECTION.                                                   
081800     MOVE 'GC-CHECK-ROLL   ' TO CURRENT-SECTION                           
081900                                                                          
082000     MOVE MID-KDBEHX TO UPDATE-SW                                         
082100     IF SW-INSERT                                                         
082200       PERFORM IMS-04-GU-WDP401                                           
082300       IF SEGMENT-MISSING                                                 
082400         MOVE 'I' TO UPDATE-ROLL-SW                                       
082500       ELSE                                                               
082600         MOVE 'N' TO UPDATE-ROLL-SW                                       
082700       END-IF                                                             
082800     ELSE                                                                 
082900       IF SW-DELETE                                                       
083000         PERFORM IMS-04-GU-WDP401                                         
083100         IF SEGMENT-MISSING                                               
083200           MOVE 'N' TO UPDATE-INTERVALL-SW                                
083300         END-IF                                                           
083400       ELSE                                                               
083500         IF NOT SW-PGM-TO-PGM                                             
083600            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
083700            CALL WMEDKONV USING MED-WMEDAREA                              
083800            MOVE MED-MFSFEL TO MOD-TEMFSFEL                               
083900            PERFORM MFS-DONT-TOUCH-FIELD-OUT                              
084000            PERFORM MFS-DONT-TOUCH-FIELD-IN                               
084100            MOVE NOO TO INDATA-SW                                         
084200         END-IF                                                           
084300       END-IF                                                             
084400     END-IF                                                               
084500     .                                                                    
084600     EJECT                                                                
084700                                                                          
084800 GD-CHECK-IDDISTR SECTION.                                                
084900     MOVE 'GD-CHECK-IDDISTR' TO CURRENT-SECTION                           
085000                                                                          
085100     MOVE YES TO INTERVALL-SW                                             
085200                 INTERVALL-KOLL-SW                                        
085300                                                                          
085400     IF SW-INSERT                                                         
085500        PERFORM IMS-04-GU-WDP401                                          
085600        IF SEGMENT-FOUND                                                  
085700           PERFORM IMS-11-GNP-WDP411                                      
085800           PERFORM UNTIL SEGMENT-MISSING OR                               
085900                         END-OF-DB OR                                     
086000                         INTERVALL-KOLL-WRONG                             
086100                                                                          
086200              IF (MID-IDDISTR-TOM-UPD < DEF-IDDISTR-FOM OR                
086300                  MID-IDDISTR-FOM-UPD > DEF-IDDISTR-TOM)                  
086400              OR (DEF-IDDISTR-FOM     = 0001 AND                          
086500                  DEF-IDDISTR-TOM     = 9999)                             
086600                                                                          
086700                 CONTINUE                                                 
086800              ELSE                                                        
086900                 MOVE NOO TO INTERVALL-KOLL-SW                            
087000              END-IF                                                      
087100                                                                          
087200              PERFORM IMS-11-GNP-WDP411                                   
087300           END-PERFORM                                                    
087400        END-IF                                                            
087500                                                                          
087600        IF INTERVALL-KOLL-OK                                              
087710            PERFORM GEA-CHECK-DIST-ROLLER                                 
087800        END-IF                                                            
087900                                                                          
088000        IF INTERVALL-KOLL-OK                                              
088100           MOVE 'I' TO UPDATE-INTERVALL-SW                                
088200        END-IF                                                            
088300     ELSE                                                                 
088400       IF SW-DELETE                                                       
088500         MOVE MID-IDDISTR-TOM-UPD TO W-IDDISTR                            
088600         PERFORM IMS-09-GHNP-WDP411-DELETE                                
088700         IF SEGMENT-MISSING                                               
088800          STRING 'DISTRICT INTERVALL DONT MATCH AN EXISTING ONE:'         
088900          DELIMITED BY SIZE INTO MOD-TEMFSFEL                             
089000          MOVE 'N' TO UPDATE-INTERVALL-SW                                 
089100         ELSE                                                             
089200          MOVE DEF-IDDISTR-FOM TO WS-IDDISTR-FOM                          
089300          IF WS-IDDISTR-FOM = MID-IDDISTR-FOM-UPD                         
089400           MOVE 'D' TO UPDATE-INTERVALL-SW                                
089500          ELSE                                                            
089600           STRING 'DISTRICT INTERVALL DONT MATCH AN EXISTING ONE'         
089700           DELIMITED BY SIZE INTO MOD-TEMFSFEL                            
089800           MOVE 'N' TO UPDATE-INTERVALL-SW                                
089900          END-IF                                                          
090000         END-IF                                                           
090100       ELSE                                                               
090200         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
090300         CALL WMEDKONV USING MED-WMEDAREA                                 
090400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
090500         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
090600         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
090700         MOVE NOO TO INDATA-SW                                            
090800         MOVE YES TO INTERVALL-SW                                         
090900       END-IF                                                             
091000     END-IF                                                               
091100     .                                                                    
105400 GEA-CHECK-DIST-ROLLER    SECTION.                                        
105500     MOVE 'GEA-CHECK-DIST-R' TO CURRENT-SECTION                           
105600                                                                          
105700*    LÄS IN DEN ROLL VI VILL FÖRÄNDRA                                     
105800*                                                                         
105900     PERFORM IMS-04-GU-WDP401                                             
106000     IF SEGMENT-FOUND                                                     
106100        PERFORM IMS-11-GNP-WDP411                                         
106200        PERFORM UNTIL SEGMENT-MISSING OR                                  
106300                      END-OF-DB       OR                                  
106400                      INTERVALL-KOLL-WRONG                                
106500*                                                                         
106600*    KOLLA ALLA DISTRIKTSINTERVALL FÖR DEN AKTUELLA ROLLEN                
106700*                                                                         
106800                                                                          
106900           PERFORM IMS-20-GN-WDP4A11                                      
107000           PERFORM UNTIL SEGMENT-MISSING OR                               
107100                         END-OF-DB       OR                               
107200                         INTERVALL-KOLL-WRONG                             
107300*                                                                         
107400*    FÖR VARJE DISTRIKTSINTERVALL, KOLLA OM DET KOLLIDERAR MED            
107500*    INTERVALLET PÅ AKTUELL ROLL.                                         
107600*    I SÅ FALL KOLLA VILKEN ROLL DET TILLHÖR OCH KOLLA OM DENNA           
107700*    ROLL HAR ETT ANSKAFFARINTERVALL SOM KOLLIDERAR MED DET               
107800*    INTERVALL VI FÖRSÖKER LÄGGA TILL.                                    
107900*    (UNDANTAG GÖRS FÖR DEN ROLL VI JUST HÅLLER PÅ ATT FÖRÄNDRA)          
108000*                                                                         
108100              IF A11-DEF-IDDISTR-TOM < DEF-IDDISTR-FOM OR                 
108200                 A11-DEF-IDDISTR-FOM > DEF-IDDISTR-TOM                    
108300                 CONTINUE                                                 
108400              ELSE                                                        
108500                 PERFORM IMS-21-GNP-WDP401A                               
110600              END-IF                                                      
110700              PERFORM IMS-20-GN-WDP4A11                                   
110800           END-PERFORM                                                    
110900           PERFORM IMS-11-GNP-WDP411                                      
111000        END-PERFORM                                                       
111100     END-IF                                                               
111200                                                                          
111300     IF INTERVALL-KOLL-WRONG                                              
111400        MOVE ROL-IDROLL TO WS-IDROLL-OTHER                                
111500        STRING 'DISTRICT AND PROCURER EXISTS IN: ' WS-IDROLL-OTHER        
111600        DELIMITED BY SIZE INTO MOD-TEMFSFEL                               
111700        PERFORM MFS-DONT-TOUCH-FIELD-OUT                                  
111800        PERFORM MFS-DONT-TOUCH-FIELD-IN                                   
111900        MOVE NOO TO UPDATE-ROLL-SW                                        
112000        MOVE NOO TO UPDATE-INTERVALL-SW                                   
112100     END-IF                                                               
112200     .                                                                    
112300                                                                          
112400                                                                          
112500 H-UPDATE SECTION.                                                        
112600     MOVE 'H-UPDATE        ' TO CURRENT-SECTION                           
112700                                                                          
112710       IF MID-IDDISTR-TOM-UPD = ALL '+'                                   
112720          MOVE ZERO                TO W-IDDISTR                           
112730       ELSE                                                               
112741          MOVE MID-IDDISTR-TOM-UPD TO W-IDDISTR                           
112750       END-IF                                                             
112900     IF SW-INSERT                                                         
113000       MOVE MID-IDROLL-UT TO ROL-IDROLL                                   
113100       PERFORM IMS-07-ISRT-WDP401                                         
113200       IF ROLL-INSERT                                                     
113300*        MOVE MID-IDROLL-UT TO ROL-IDROLL                                 
113400*        PERFORM IMS-07-ISRT-WDP401                                       
113500* OBS!! ROLLEN FÅR ALDRIG FINNAS UTAN SEGMENT                             
113600* DÄRFÖR INSERTAS 11- RESPEKTIVE 12- SEGMENT OM                           
113700* DISTRIKT- ELLER ANSKAFFARINTERVALL EJ ANGIVITS                          
113800          IF MID-IDDISTR-TOM-UPD = ALL '+'                                
113900             MOVE 9999                TO DEF-IDDISTR-TOM                  
114000             MOVE 1                   TO DEF-IDDISTR-FOM                  
114100             MOVE MSGI-IDUSER         TO DEF-IDUSER                       
114200             MOVE WS-CURRENT-DATE     TO DEF-TIREGDAT                     
114300             PERFORM IMS-12-ISRT-WDP411                                   
114400          END-IF                                                          
115200       END-IF                                                             
115300       IF INTERVALL-INSERT                                                
115400          IF MID-IDDISTR-TOM-UPD NOT = ALL '+'                            
115500             PERFORM HA-JUST-FORSTA-DIST-INTERVALL                        
115600             MOVE MID-IDDISTR-TOM-UPD TO DEF-IDDISTR-TOM                  
115700             MOVE MID-IDDISTR-FOM-UPD TO DEF-IDDISTR-FOM                  
115800             MOVE MSGI-IDUSER         TO DEF-IDUSER                       
115900             MOVE WS-CURRENT-DATE     TO DEF-TIREGDAT                     
116000             PERFORM IMS-12-ISRT-WDP411                                   
116100             STRING 'OK, UPDATE DONE'                                     
116200             DELIMITED BY SIZE INTO MOD-TEMFSFEL                          
116300             PERFORM MFS-ERASE-FIELD-IN                                   
116400          END-IF                                                          
117600       END-IF                                                             
117700     END-IF                                                               
117800                                                                          
117900     IF SW-DELETE                                                         
118000        IF INTERVALL-DELETE                                               
118100           PERFORM IMS-05-GHU-WDP401                                      
118200           IF MID-IDDISTR-TOM-UPD NOT = ALL '+'                           
118300              PERFORM IMS-04-GU-WDP401                                    
118400              PERFORM IMS-10-GHNP-WDP411                                  
118500              IF SEGMENT-FOUND                                            
118600                 PERFORM IMS-13-DLET-WDP411                               
118700              END-IF                                                      
118800              STRING 'OK, DELETE DONE'                                    
118900              DELIMITED BY SIZE INTO MOD-TEMFSFEL                         
119000              PERFORM MFS-ERASE-FIELD-IN                                  
119100              PERFORM HC-JUST-SISTA-DIST-INTERVALL                        
119200           END-IF                                                         
120400        END-IF                                                            
120500     END-IF                                                               
120600                                                                          
120700     IF SW-PGM-TO-PGM                                                     
120800        PERFORM HE-STARTA-BMP                                             
120900        MOVE 'BMP STARTED' TO MOD-TEMFSINF                                
121000     END-IF                                                               
121100     .                                                                    
121200                                                                          
121300                                                                          
121400 HA-JUST-FORSTA-DIST-INTERVALL SECTION.                                   
121500     MOVE 'HA-JUST-FORSTA-D' TO CURRENT-SECTION                           
121600                                                                          
121700*    OM FIXSEGMENT MED INTERVALL 1-9999 FINNS,                            
121800*    SKALL DET TAS BORT NÄR FÖRSTA 'RIKTIGA' INTERVALL SKAPAS.            
121900*                                                                         
122000     PERFORM IMS-26-GHU-WDP411-FIX                                        
122100     IF SEGMENT-FOUND                                                     
122200        PERFORM IMS-13-DLET-WDP411                                        
122300     END-IF                                                               
122400     .                                                                    
123900                                                                          
124000 HC-JUST-SISTA-DIST-INTERVALL SECTION.                                    
124100     MOVE 'HC-JUST-SISTA-DI' TO CURRENT-SECTION                           
124200                                                                          
124300*    OM INGA INTERVALL FINNS KVAR SKALL ETT 'FIXSEGMENT' SKAPAS           
124400*    MED INTERVALL 1-9999.                                                
124500*                                                                         
124600     MOVE ROL-IDROLL TO W-IDROLL-SEARCH                                   
124700     PERFORM IMS-06-GU-WDP401-SEARCH                                      
124800     PERFORM IMS-24-GNP-WDP411-SEARCH                                     
124900                                                                          
125000     IF SEGMENT-MISSING                                                   
125100        MOVE 9999                TO DEF-IDDISTR-TOM                       
125200        MOVE 1                   TO DEF-IDDISTR-FOM                       
125300        MOVE MSGI-IDUSER         TO DEF-IDUSER                            
125400        MOVE WS-CURRENT-DATE     TO DEF-TIREGDAT                          
125500        PERFORM IMS-12-ISRT-WDP411                                        
125600     END-IF                                                               
125700     .                                                                    
125800                                                                          
125900                                                                          
128000 HE-STARTA-BMP SECTION.                                                   
128100     MOVE 'HE-STARTA-BMP   ' TO CURRENT-SECTION                           
128200                                                                          
128300*                                                                         
128400*  STARTA BMP W412S3                                                      
128500*                                                                         
128600     MOVE '4271'          TO MSGSOP-IDTRANS                               
128700     MOVE MFS-KDMFSFOR    TO MSGSOP-KDMFSFOR                              
128800     MOVE 'W412S3'        TO MSGSOP-IDPROCESS                             
128900     MOVE 'O'             TO MSGSOP-KDSOPFUNK                             
129000                                                                          
129100     STRING 'IDROLL(' MOD-IDROLL ')'                                      
129200          DELIMITED BY SIZE INTO MSGSOP-TESYMBV                           
129300     PERFORM IMS-03-INSERT-ALTMSG                                         
129400     .                                                                    
129500 MFS-ERASE-FIELD-OUT SECTION.                                             
129600*    --- ALLA UTDATA-FÄLT                                                 
129700*    --- INCL. SCROLL KEYS                                                
129800     MOVE MFS-ERASE-FIELD TO MOD-IDROLL-IN                                
129900                             MOD-IDDISTR-TOM-UPD                          
130000                             MOD-IDDISTR-FOM-UPD                          
130300                             MOD-KDBEHX                                   
130400     .                                                                    
130500     SKIP3                                                                
130600                                                                          
130700 MFS-ERASE-FIELD-IN SECTION.                                              
130800*    --- ALLA INDATA-FÄLT                                                 
130900     MOVE MFS-ERASE-FIELD TO MOD-IDROLL-IN                                
131000                             MOD-IDDISTR-TOM-UPD                          
131100                             MOD-IDDISTR-FOM-UPD                          
131400                             MOD-KDBEHX                                   
131500     .                                                                    
131600     EJECT                                                                
131700                                                                          
131800 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
131900*    --- ALLA UTDATA-FÄLT                                                 
132000*    --- INCL SCROLL KEYS AND LINEDATA                                    
132100     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR-FOM-UPD                   
132200                                    MOD-IDDISTR-TOM-UPD                   
132500                                    MOD-KDBEHX                            
132600     MOVE +1 TO INDX                                                      
132700     PERFORM UNTIL INDX > MAX-INDX                                        
132800       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
132900       ADD +1 TO INDX                                                     
133000     END-PERFORM                                                          
133100     SKIP2                                                                
133200     .                                                                    
133300                                                                          
133400 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
133500*    --- OUTDATA FIELD ON SCROLL KEYS                                     
133600     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR-FOM (INDX)                
133700                                    MOD-IDDISTR-TOM (INDX)                
134000     .                                                                    
134100     SKIP3                                                                
134200                                                                          
134300 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
134400*    --- ALLA INDATA-FÄLT                                                 
134500     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDROLL                            
134600     .                                                                    
134700     EJECT                                                                
134800                                                                          
134900 MFS-READ-IN-AGAIN SECTION.                                               
135000*    --- ALL INDATA-FIELDS                                                
135100     MOVE MFS-ADD-READ-FIELD TO MOD-KDBEHX-ATTR                           
135200                                MOD-IDDISTR-FOM-ATTR                      
135300                                MOD-IDDISTR-TOM-ATTR                      
135600     .                                                                    
135700     EJECT                                                                
135800                                                                          
135900* --- IMS SECTIONS ---                                                    
136000     SKIP3                                                                
136100                                                                          
136200 IMS-01-GET-MSG SECTION.                                                  
136300     MOVE 'IMS-01' TO CURRENT-IMS-SECTION                                 
136400                                                                          
136500     MOVE '  QC' TO GOOD-STATUSCODES                                      
136600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
136700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
136800     PERFORM IMS-STATUSCHECK                                              
136900     .                                                                    
137000                                                                          
137100                                                                          
137200 IMS-02-INSERT-MSG SECTION.                                               
137300     MOVE 'IMS-02' TO CURRENT-IMS-SECTION                                 
137400                                                                          
137500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
137600     MOVE SPACE TO GOOD-STATUSCODES                                       
137700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
137800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
137900     PERFORM IMS-STATUSCHECK                                              
138000     .                                                                    
138100                                                                          
138200                                                                          
138300 IMS-03-INSERT-ALTMSG SECTION.                                            
138400     MOVE 'IMS-03' TO CURRENT-IMS-SECTION                                 
138500                                                                          
138600     MOVE '  ' TO GOOD-STATUSCODES                                        
138700     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
138800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
138900     PERFORM IMS-STATUSCHECK                                              
139000     .                                                                    
139100                                                                          
139200                                                                          
139300 IMS-04-GU-WDP401 SECTION.                                                
139400     MOVE 'IMS-04' TO CURRENT-IMS-SECTION                                 
139500                                                                          
139600     STRING 'WDP401  (IDROLL   =' W-IDROLL-X ')'                          
139700          DELIMITED BY SIZE INTO SSA1                                     
139800     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
139900     CALL CBLTDLI USING GU WDP4-PCB DLI-IO-WDP401 SSA1                    
140000     MOVE WDP4-STATUS-CODE    TO STATUS-WS                                
140100     PERFORM IMS-STATUSCHECK                                              
140200     .                                                                    
140300                                                                          
140400                                                                          
140500 IMS-05-GHU-WDP401 SECTION.                                               
140600     MOVE 'IMS-05' TO CURRENT-IMS-SECTION                                 
140700                                                                          
140800     STRING 'WDP401  (IDROLL   =' W-IDROLL-X ')'                          
140900          DELIMITED BY SIZE INTO SSA1                                     
141000     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
141100     CALL CBLTDLI USING GHU WDP4-PCB DLI-IO-WDP401 SSA1                   
141200     MOVE WDP4-STATUS-CODE    TO STATUS-WS                                
141300     PERFORM IMS-STATUSCHECK                                              
141400     .                                                                    
141500                                                                          
141600                                                                          
141700 IMS-06-GU-WDP401-SEARCH SECTION.                                         
141800     MOVE 'IMS-O5' TO CURRENT-IMS-SECTION                                 
141900                                                                          
142000     STRING 'WDP401  (IDROLL   =' W-IDROLL-SEARCH-X ')'                   
142100          DELIMITED BY SIZE INTO SSA1                                     
142200     MOVE '  ' TO GOOD-STATUSCODES                                        
142300     CALL CBLTDLI USING GU WDP4-SEARCH-PCB DLI-IO-WDP401 SSA1             
142400     MOVE WDP4-SEARCH-STATUS-CODE    TO STATUS-WS                         
142500     PERFORM IMS-STATUSCHECK                                              
142600     .                                                                    
142700                                                                          
142800                                                                          
142900 IMS-07-ISRT-WDP401 SECTION.                                              
143000     MOVE 'IMS-07' TO CURRENT-IMS-SECTION                                 
143100                                                                          
143200     MOVE 'WDP401 ' TO SSA1                                               
143300     MOVE '  II' TO GOOD-STATUSCODES                                      
143400     CALL CBLTDLI USING ISRT WDP4-PCB DLI-IO-WDP401 SSA1                  
143500     MOVE WDP4-STATUS-CODE TO STATUS-WS                                   
143600     PERFORM IMS-STATUSCHECK                                              
143700     .                                                                    
143800                                                                          
143900                                                                          
144000 IMS-08-GHNP-WDP411-NEXT SECTION.                                         
144100     MOVE 'IMS-08' TO CURRENT-IMS-SECTION                                 
144200                                                                          
144300     STRING 'WDP411  (IDDISTRT>=' W-IDDISTR-MIN-X ')'                     
144400          DELIMITED BY SIZE INTO SSA1                                     
144500     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
144600     CALL CBLTDLI USING GHNP WDP4-PCB DLI-IO-WDP411 SSA1                  
144700     MOVE WDP4-STATUS-CODE TO STATUS-WS                                   
144800     PERFORM IMS-STATUSCHECK                                              
144900     .                                                                    
145000                                                                          
145100                                                                          
145200 IMS-09-GHNP-WDP411-DELETE SECTION.                                       
145300     MOVE 'IMS-09' TO CURRENT-IMS-SECTION                                 
145400                                                                          
145500     STRING 'WDP411  (IDDISTRT =' W-IDDISTR-X ')'                         
145600          DELIMITED BY SIZE INTO SSA1                                     
145700     MOVE '  GE' TO GOOD-STATUSCODES                                      
145800     CALL CBLTDLI USING GHNP WDP4-PCB DLI-IO-WDP411 SSA1                  
145900     MOVE WDP4-STATUS-CODE TO STATUS-WS                                   
146000     PERFORM IMS-STATUSCHECK                                              
146100     .                                                                    
146200                                                                          
146300                                                                          
146400 IMS-10-GHNP-WDP411 SECTION.                                              
146500     MOVE 'IMS-10' TO CURRENT-IMS-SECTION                                 
146600                                                                          
146700     STRING 'WDP411  (IDDISTRT =' W-IDDISTR-X ')'                         
146800          DELIMITED BY SIZE INTO SSA1                                     
146900     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
147000     CALL CBLTDLI USING GHNP WDP4-PCB DLI-IO-WDP411 SSA1                  
147100     MOVE WDP4-STATUS-CODE TO STATUS-WS                                   
147200     PERFORM IMS-STATUSCHECK                                              
147300     .                                                                    
147400                                                                          
147500                                                                          
147600 IMS-11-GNP-WDP411 SECTION.                                               
147700     MOVE 'IMS-11' TO CURRENT-IMS-SECTION                                 
147800                                                                          
147900     MOVE   'WDP411   ' TO SSA1                                           
148000     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
148100     CALL CBLTDLI USING GNP WDP4-PCB DLI-IO-WDP411 SSA1                   
148200     MOVE WDP4-STATUS-CODE TO STATUS-WS                                   
148300     PERFORM IMS-STATUSCHECK                                              
148400     .                                                                    
148500                                                                          
148600                                                                          
148700 IMS-12-ISRT-WDP411 SECTION.                                              
148800     MOVE 'IMS-12' TO CURRENT-IMS-SECTION                                 
148900                                                                          
149000     STRING 'WDP401  (IDROLL   =' W-IDROLL-X ')'                          
149100          DELIMITED BY SIZE INTO SSA1                                     
149200     MOVE 'WDP411 ' TO SSA2                                               
149300     MOVE '  II' TO GOOD-STATUSCODES                                      
149400     CALL CBLTDLI USING ISRT WDP4-PCB DLI-IO-WDP411 SSA1 SSA2             
149500     MOVE WDP4-STATUS-CODE TO STATUS-WS                                   
149600     PERFORM IMS-STATUSCHECK                                              
149700     .                                                                    
149800                                                                          
149900                                                                          
150000 IMS-13-DLET-WDP411 SECTION.                                              
150100     MOVE 'IMS-13' TO CURRENT-IMS-SECTION                                 
150200                                                                          
150300     MOVE '  ' TO GOOD-STATUSCODES                                        
150400     CALL CBLTDLI USING DLET WDP4-PCB DLI-IO-WDP411                       
150500     MOVE WDP4-STATUS-CODE TO STATUS-WS                                   
150600     PERFORM IMS-STATUSCHECK                                              
150700     .                                                                    
157900                                                                          
158000 IMS-20-GN-WDP4A11 SECTION.                                               
158100     MOVE 'IMS-20' TO CURRENT-IMS-SECTION                                 
158200                                                                          
158300     STRING 'WDP411  ' DELIMITED BY SIZE INTO SSA1                        
158400     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
158500     CALL CBLTDLI USING GN WDP4A-NEXT-PCB DLI-IO-WDP411-A11 SSA1          
158600     MOVE WDP4A-NEXT-STATUS-CODE TO STATUS-WS                             
158700     PERFORM IMS-STATUSCHECK                                              
158800     .                                                                    
159000                                                                          
159100 IMS-21-GNP-WDP401A SECTION.                                              
159200     MOVE 'IMS-21' TO CURRENT-IMS-SECTION                                 
159300                                                                          
159400     MOVE   'WDP401   ' TO SSA1                                           
159500     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
159600     CALL CBLTDLI USING GNP WDP4A-NEXT-PCB DLI-IO-WDP401 SSA1             
159700     MOVE WDP4A-NEXT-STATUS-CODE TO STATUS-WS                             
159800     PERFORM IMS-STATUSCHECK                                              
159900     .                                                                    
160000                                                                          
162400 IMS-24-GNP-WDP411-SEARCH SECTION.                                        
162500     MOVE 'IMS-24' TO CURRENT-IMS-SECTION                                 
162600                                                                          
162700     MOVE 'WDP411  ' TO SSA1                                              
162800     MOVE '  GEGB'   TO GOOD-STATUSCODES                                  
162900     CALL CBLTDLI USING GNP WDP4-SEARCH-PCB DLI-IO-WDP411 SSA1            
163000     MOVE WDP4-SEARCH-STATUS-CODE TO STATUS-WS                            
163100     PERFORM IMS-STATUSCHECK                                              
163200     .                                                                    
163300                                                                          
163400                                                                          
164600 IMS-26-GHU-WDP411-FIX SECTION.                                           
164700     MOVE 'IMS-26' TO CURRENT-IMS-SECTION                                 
164800                                                                          
164900     STRING 'WDP401  (IDROLL   =' W-IDROLL-X ')'                          
165000          DELIMITED BY SIZE INTO SSA1                                     
165100     STRING 'WDP411  (IDDISTRF =' W-IDDISTR-0001-X                        
165200                    '&IDDISTRT =' W-IDDISTR-9999-X ')'                    
165300          DELIMITED BY SIZE INTO SSA2                                     
165400     MOVE '  GE' TO GOOD-STATUSCODES                                      
165500     CALL CBLTDLI USING GHU WDP4-PCB DLI-IO-WDP411                        
165600                           SSA1 SSA2                                      
165700     MOVE WDP4-STATUS-CODE    TO STATUS-WS                                
165800     PERFORM IMS-STATUSCHECK                                              
165900     .                                                                    
166000                                                                          
166100                                                                          
167700                                                                          
167800 IMS-STATUSCHECK SECTION.                                                 
167900     SET STATUS-IX TO 1                                                   
168000     SEARCH GOOD-STATUS                                                   
168100       AT END                                                             
168200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
168300         DELIMITED BY SIZE INTO ERROR-TEXT                                
168400         CALL FELLOG                                                      
168500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
168600         CONTINUE                                                         
168700     END-SEARCH                                                           
168800     .                                                                    
