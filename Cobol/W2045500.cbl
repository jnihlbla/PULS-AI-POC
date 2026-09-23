000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2045500.                                                
000300 AUTHOR.         SURESH GUDIVADA.                                         
000400 DATE-WRITTEN.   23/11/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FEATURE:                                                             
000800*    DC SEGMENT TABLE STEERING                                            
000900*                                                                         
001000*    SEGMENTATION IS USEFUL TO CONTROL STOCK LEVEL BASED ON               
001100*    CHARACTERISTICS OF THE PART. STOCK CONTROL LEVELS CAN NOW BE         
001200*    CONTROLLED BASED ON SOME MAJOR SEGMENTS, STORAGE AND LIFE            
001300*    CYCLE AND SPECIAL.                                                   
001400*                                                                         
001500*    THE ABILITY TO STEER SAFETY STOCK OF PARTS EFFECTIVELY,              
001600*    TO MAINTAIN TABLES PER DC AND PROCSEGMCODE COMBINATION.              
001700*                                                                         
001800*    BASED ON THE SEGMENT AND PICK FREQUENCY, WHICH CAN BE                
001900*    CONTROLLED ON THE SCREEN, STEER EACH SEGMENT TO A SPECIFIC           
002000*    TABLE.                                                               
002100*                                                                         
002200*    PROGRAM READS  : WDB6                                                
002300*    PROGRAM UPDATES: WDB6                                                
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSACTION: W2T455                                              
002700*                     W2T455U                                             
002800*        MID:         W2I45501                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W2O45501                                            
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500                                                                          
003600 DATA DIVISION.                                                           
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W2045500'.            
004000                                                                          
004100*    --- WORK FIELD FOR ERROR MESSAGES AT CALL ABEND/ERROR LOG            
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300*                                                                         
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600 77  FL-UPD                      PIC X       VALUE 'N'.                   
004700 77  FL-ARRAY                    PIC X       VALUE 'N'.                   
004800 77  INDX                        PIC S9(4)   VALUE ZERO.                  
004900 77  MAX-INDX                    PIC S9(4)   VALUE 270.                   
005000 77  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005100 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005200 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005300*                                                                         
005400                                                                          
005500**PROCSEGMCODE DESCRIPTION                                                
005600*    -COPY WBEPSEGM                                                       
005700**                                                                        
005800*    --- WORK FIELD FOR CURRENT KEY VALUES FROM THE SCREEN                
005900 01  WORK.                                                                
006000     03 IX-RAD                   PIC S9(3)  COMP-3  VALUE ZERO.           
006100     03 RAD-MAX                  PIC S9(3)  COMP-3  VALUE 13.             
006200                                                                          
006300 01  SWITCHAR.                                                            
006400     03  SW-INPUT-RAETT          PIC X       VALUE 'J'.                   
006500*                                                                         
006600 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006700     88  KEYS-OK                             VALUE 'J'.                   
006800     88  KEYS-WRONG                          VALUE 'N'.                   
006900                                                                          
007000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007100     88  OWN-MID                             VALUE '2455'.                
007200     88  GODK-MID                            VALUE '2455'.                
007300     88  HELP-MID                            VALUE '0551'.                
007400     EJECT                                                                
007500*    --- SUBROUTINES AND PARAMETER AREAS                                  
007600 01  GENERAL-SUBPROGRAM.                                                  
007700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008100     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008400*01 -COPY WMEDAREA                                                        
008500     SKIP3                                                                
008600*    --- COPYTEXT TILL SUBPROGRAM WDECEDIT                                
008700*01  -COPY WDECAREA                                                       
008800     EJECT                                                                
008900 01  MESSAGE-CODES.                                                       
009000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009100     03  ERR-WRONG-DC            PIC X(3)    VALUE '440'.                 
009200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009300     03  CONFLICT                PIC X(3)    VALUE '002'.                 
009400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009500     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
009600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009900     03  UPDATING-NOT-ALLOWED    PIC X(3)    VALUE '777'.                 
010000     03  ERR-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
010100     03  INF-NO-MORE-F6          PIC X(3)    VALUE '368'.                 
010200     EJECT                                                                
010300 01  FELTEXTER.                                                           
010400     03  MED-1                  PIC X(40)                                 
010500         VALUE 'UPDATING NOT ALLOWED          '.                          
010600     03  MED-2                  PIC X(40)                                 
010700         VALUE 'INCORRECT FIELDS VALUES       '.                          
010800                                                                          
010900     EJECT                                                                
011000*    --- PARAMETERS FOR SUBROUTINE W005INITT                              
011100*                                                                         
011200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011300     SKIP3                                                                
011400*01 -COPY WMSGINIT                                                        
011500     EJECT                                                                
011600*FIELDS FOR USER DATABASE                                                 
011700 01  SPAR-AREA.                                                           
011800     03  SPAR-IDDC                  PIC X(2)    VALUE SPACE.              
011900     03  SPAR-SEGM-KDANSKSEG        PIC S9(5)   VALUE +0 COMP-3.          
012000     03  SPAR-SEGM-KDANSKSEG-FIRST  PIC S9(5)   VALUE +0 COMP-3.          
012100*                                                                         
012200 01  W-COPY-PSEGM-TAB.                                                    
012300     03  W-COPY-PSEGM-RAD OCCURS 270 INDEXED BY IX-RAD1.                  
012400       05  W-COPY-KDANSKSEG      PIC S9(5)   VALUE +0 COMP-3.             
012500       05  W-COPY-IDREFTAB-LFL   PIC X(01)   VALUE SPACE.                 
012600       05  W-COPY-IDREFTAB-LFM   PIC X(01)   VALUE SPACE.                 
012700       05  W-COPY-IDREFTAB-LFH   PIC X(01)   VALUE SPACE.                 
012800       05  W-COPY-IDREFTAB-LFXH  PIC X(01)   VALUE SPACE.                 
012900       05  W-COPY-IDREFTAB-HFL   PIC X(01)   VALUE SPACE.                 
013000       05  W-COPY-IDREFTAB-HFM   PIC X(01)   VALUE SPACE.                 
013100       05  W-COPY-IDREFTAB-HFH   PIC X(01)   VALUE SPACE.                 
013200       05  W-COPY-IDREFTAB-HFXH  PIC X(01)   VALUE SPACE.                 
013300*                                                                         
013400     EJECT                                                                
013500 01  FILLER                      PIC X(16)   VALUE 'DAT-AREA'.            
013600     SKIP3                                                                
013700*01 -COPY WDATAREA                                                        
013800     EJECT                                                                
013900*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
014000*                                                                         
014100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014200     SKIP3                                                                
014300*01  MID -COPY W2I45501                                                   
014400     EJECT                                                                
014500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014600     SKIP3                                                                
014700*01  -COPY WMSGAREA                                                       
014800     EJECT                                                                
014900     03  MOD REDEFINES MSG-AREA.                                          
015000*      05  -COPY W2O45501                                                 
015100     EJECT                                                                
015200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015300     SKIP3                                                                
015400*01  -COPY WMFSAREA                                                       
015500     EJECT                                                                
015600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015700*                                                                         
015800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015900     SKIP3                                                                
016000 01  KEYS-TILL-DLI.                                                       
016100     03  W-KDANSKSEG-X.                                                   
016200         05  W-KDANSKSEG         PIC S9(5) VALUE +0 COMP-3.               
016300     03  W-IDDC-X.                                                        
016400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
016500     03  W-IDDC-COPY-X.                                                   
016600         05  W-IDDC-COPY         PIC X(2)    VALUE SPACE.                 
016700     03  W-IDTRANS-B6-X.                                                  
016800         05  W-IDTRANS-B6        PIC X(4)    VALUE '2455'.                
016900     SKIP2                                                                
017000*    --- STATUS CODE FROM IMS                                             
017100 01  STATUS-WS                   PIC XX.                                  
017200     88  SEGMENT-FOUND                       VALUE '  '.                  
017300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
017400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
017500     88  SEGMENT-END                         VALUE 'GB'.                  
017600     SKIP2                                                                
017700 01  GOOD-STATUSCODES.                                                    
017800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017900     SKIP3                                                                
018000 01  SSA1                        PIC X(64).                               
018100 01  SSA2                        PIC X(64).                               
018200 01  SSA3                        PIC X(64).                               
018300     EJECT                                                                
018400*    --- IMS FUNCTION CODES                                               
018500*01  -COPY W0003                                                          
018600     EJECT                                                                
018700*    ---  DLI INPUT-OUTPUT AREA                                           
018800                                                                          
018900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
019000 01  DLI-IO-WDB601.                                                       
019100*    03  -COPY WDB601                                                     
019200     EJECT                                                                
019300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB615'.                      
019400 01  DLI-IO-WDB615.                                                       
019500*    03  -COPY WDB615                                                     
019600     EJECT                                                                
019700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB603'.                      
019800 01  DLI-IO-WDB603.                                                       
019900*    03  -COPY WDB603                                                     
020000     EJECT                                                                
020100                                                                          
020200 LINKAGE SECTION.                                                         
020300*01  -COPY W0009   -PRE MSG-                                              
020400*01  -COPY W0008   -PRE USEA-                                             
020500     05  FILLER                  PIC X.                                   
020600                                                                          
020700*01  -COPY W0008  -PRE WDB6-                                              
020800     05  FILLER                  PIC X.                                   
020900     EJECT                                                                
021000                                                                          
021100 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDB6-PCB.                     
021200 MAIN SECTION.                                                            
021300     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDB6-PCB.                     
021400                                                                          
021500     PERFORM IMS-GET-MSG                                                  
021600     IF SEGMENT-FOUND                                                     
021700       PERFORM A-INIT                                                     
021800       PERFORM B-CHECK-KEYS                                               
021900       IF KEYS-OK                                                         
022000           IF MFS-UPDATE                                                  
022100              PERFORM G-CHECK-INPUT                                       
022200              IF SW-INPUT-RAETT = YES                                     
022300                 PERFORM H-UPDATE                                         
022400              END-IF                                                      
022500           ELSE                                                           
022600             IF MFS-FIRST                                                 
022700                PERFORM C-FIRST-PAGE                                      
022800             ELSE                                                         
022900                IF MFS-NEXT                                               
023000                   PERFORM D-NEXT-PAGE                                    
023100                ELSE                                                      
023200                   IF MFS-PREVIOUS                                        
023300                      SET SEGM-IX TO 1                                    
023400                      PERFORM I-PREVIOUS-PAGE                             
023500                   ELSE                                                   
023600                      PERFORM E-SAME-PAGE                                 
023700                   END-IF                                                 
023800                END-IF                                                    
023900             END-IF                                                       
024000           END-IF                                                         
024100           PERFORM F-LAES-VISA-INFO                                       
024200*WRITE INFO TO USER DATABASE,WHICH CAN BE USED FOR F8                     
024300           MOVE SPAR-AREA         TO MSGI-SPAR-AREA                       
024400           MOVE '002'             TO MSGI-KDCALL                          
024500           MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                    
024600           MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                          
024700           MOVE '2455'            TO MSGI-IDTRANS                         
024800           CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                     
024900*                                                                         
025000       END-IF                                                             
025100       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O45501 + 4                      
025200       PERFORM IMS-INSERT-MSG                                             
025300     END-IF                                                               
025400*                                                                         
025500     MOVE ZERO TO RETURN-CODE                                             
025600     GOBACK                                                               
025700     .                                                                    
025800     EJECT                                                                
025900*                                                                         
026000 A-INIT SECTION.                                                          
026100     MOVE 'A-INIT'                        TO CURRENT-SECTION              
026200                                                                          
026300     IF MSG-DUBBLA-TRANSKODER                                             
026400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I45501                 
026500       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
026600       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
026700     ELSE                                                                 
026800       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W2I45501                 
026900       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
027000       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
027100     END-IF                                                               
027200                                                                          
027300     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
027400     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
027500     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
027600                                                                          
027700     MOVE LOW-VALUE        TO MSG-AREA                                    
027800     MOVE 'W2O455N1'       TO MFS-IDMOD                                   
027900     MOVE '2455'           TO MOD-IDTRANS                                 
028000                                                                          
028100     MOVE MFS-RENSA-FAELT  TO MOD-TEMFSFEL                                
028200                              MOD-TEMFSINF                                
028300                              MOD-KVOT-RULL12HF-IN                        
028400                              MOD-KVVECKOR-FTL-IN                         
028500                              MOD-KVVECKOR-FTM-IN                         
028600                              MOD-KVVECKOR-FTH-IN                         
028700     IF OWN-MID OR HELP-MID                                               
028800       CONTINUE                                                           
028900     ELSE                                                                 
029000       MOVE SPACE TO MFS-KDTRTYP                                          
029100       MOVE '7'   TO MFS-IDPFK                                            
029200     END-IF                                                               
029300                                                                          
029400*---   GET TODAYS DATE                                                    
029500     ACCEPT TODAYS-DATE FROM DATE                                         
029600                                                                          
029700     .                                                                    
029800     EJECT                                                                
029900*                                                                         
030000 B-CHECK-KEYS SECTION.                                                    
030100     MOVE 'B-CHECK-KEYS'         TO CURRENT-SECTION                       
030200                                                                          
030300     MOVE ALL '+'                TO MSGI-WMSGINIT                         
030400     MOVE '001'                  TO MSGI-KDCALL                           
030500     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
030600     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
030700     MOVE '2455'                 TO MSGI-IDTRANS                          
030800     IF OWN-MID                                                           
030900        MOVE MID-IDDC-2455-IN    TO MSGI-IDDC-KEY                         
031000     END-IF                                                               
031100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
031200     MOVE MSGI-SPAR-AREA         TO SPAR-AREA                             
031300     IF SPAR-SEGM-KDANSKSEG NOT NUMERIC                                   
031400        MOVE ZERO TO SPAR-SEGM-KDANSKSEG                                  
031500     END-IF                                                               
031600     IF SPAR-SEGM-KDANSKSEG-FIRST NOT NUMERIC                             
031700        MOVE ZERO                                                         
031800          TO SPAR-SEGM-KDANSKSEG-FIRST                                    
031900     END-IF                                                               
032000                                                                          
032100*---   LANGUAGE TO BE USED BY WMEDCONV                                    
032200     MOVE 'GB  '                 TO MED-IDSKYLT                           
032300*                                                                         
032400     MOVE YES                    TO KEYS-SW                               
032500*                                                                         
032600     IF MID-IDDC-2455-IN = ALL '+'                                        
032700       IF MID-IDDC-2455-UT = ALL '+'                                      
032800          MOVE MSGI-IDDC-KEY     TO W-IDDC                                
032900       ELSE                                                               
033000          MOVE MID-IDDC-2455-UT  TO W-IDDC                                
033100       END-IF                                                             
033200     ELSE                                                                 
033300       MOVE MID-IDDC-2455-IN     TO W-IDDC                                
033400     END-IF                                                               
033500                                                                          
033600*--- CHECK OF IDDC                                                        
033700                                                                          
033800     IF W-IDDC NOT = SPACES                                               
033900       PERFORM IMS-GU-WDB601                                              
034000       IF SEGMENT-MISSING                                                 
034100         MOVE NOO                TO KEYS-SW                               
034200         MOVE ERR-WRONG-DC       TO MED-IDMFSFEL                          
034300         CALL WMEDKONV USING MED-WMEDAREA                                 
034400         MOVE MED-MFSFEL         TO MOD-TEMFSFEL                          
034500         PERFORM MFS-RENSA-FAELT-IN                                       
034600         PERFORM MFS-RENSA-FAELT-UT                                       
034700         PERFORM MFS-CLOSE-FIELD-IN                                       
034800       END-IF                                                             
034900     ELSE                                                                 
035000       MOVE NOO                  TO KEYS-SW                               
035100       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
035200       CALL WMEDKONV USING MED-WMEDAREA                                   
035300       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
035400     END-IF                                                               
035500     .                                                                    
035600     EJECT                                                                
035700*                                                                         
035800 C-FIRST-PAGE SECTION.                                                    
035900     MOVE 'C-FIRST-PAGE'         TO CURRENT-SECTION                       
036000                                                                          
036100     PERFORM MFS-RENSA-FAELT-IN                                           
036200     .                                                                    
036300     EJECT                                                                
036400*                                                                         
036500 D-NEXT-PAGE SECTION.                                                     
036600     MOVE 'D-NEXT-PAGE'              TO CURRENT-SECTION                   
036700                                                                          
036800     IF SPAR-IDDC NOT = SPACE                                             
036900       MOVE SPAR-IDDC                TO W-IDDC                            
037000       IF SPAR-SEGM-KDANSKSEG > ZERO                                      
037100          MOVE SPAR-SEGM-KDANSKSEG   TO W-KDANSKSEG                       
037200       ELSE                                                               
037300          MOVE SPAR-SEGM-KDANSKSEG-FIRST                                  
037400                                     TO W-KDANSKSEG                       
037500          MOVE ERR-LAST-PAGE-SHOWN   TO MED-IDMFSFEL                      
037600          CALL WMEDKONV USING MED-WMEDAREA                                
037700          MOVE MED-MFSFEL            TO MOD-TEMFSFEL                      
037800       END-IF                                                             
037900     END-IF                                                               
038000     .                                                                    
038100     EJECT                                                                
038200*                                                                         
038300 E-SAME-PAGE SECTION.                                                     
038400     MOVE 'E-SAME-PAGE'              TO CURRENT-SECTION                   
038500                                                                          
038600     IF SPAR-IDDC NOT = SPACE AND                                         
038700        ( MID-IDDC-2455-IN = ALL '+' OR SPACES)                           
038800        MOVE SPAR-IDDC                                                    
038900          TO W-IDDC                                                       
039000        MOVE SPAR-SEGM-KDANSKSEG-FIRST                                    
039100          TO W-KDANSKSEG                                                  
039200     END-IF                                                               
039300     .                                                                    
039400     EJECT                                                                
039500*                                                                         
039600 F-LAES-VISA-INFO SECTION.                                                
039700     MOVE 'F-LAES-VISA-INFO'   TO CURRENT-SECTION                         
039800                                                                          
039900     MOVE W-IDDC               TO MOD-IDDC-UT                             
040000     PERFORM IMS-GU-WDB601                                                
040100                                                                          
040200     IF SEGMENT-FOUND                                                     
040300*DATA FROM WDB601                                                         
040400        MOVE DCS-KVVECKOR-FTL  TO MOD-KVVECKOR-FTL-UT                     
040500        MOVE DCS-KVVECKOR-FTM  TO MOD-KVVECKOR-FTM-UT                     
040600        MOVE DCS-KVVECKOR-FTH  TO MOD-KVVECKOR-FTH-UT                     
040700        MOVE DCS-KVOT-RULL12HF TO MOD-KVOT-RULL12HF-UT                    
040800*TO SHOW LAST UPDATED TIME,PERSON DETAILS.                                
040900        PERFORM FA-LAST-UPDATED-DETAILS                                   
041000*                                                                         
041100        PERFORM IMS-GNP-WDB603-FIRST                                      
041200*                                                                         
041300        IF SEGMENT-FOUND                                                  
041400*WILL BE USED TO LOAD THE SAME PAGE AGAIN,WHEN PRESS ENTER AND            
041500*AFTER REACH THE LAST PAGE                                                
041600           MOVE SEGT-KDANSKSEG TO SPAR-SEGM-KDANSKSEG-FIRST               
041700        END-IF                                                            
041800                                                                          
041900        MOVE +1                TO IX-RAD                                  
042000        PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                      
042100          OR IX-RAD > 13                                                  
042200          IF SEGMENT-FOUND                                                
042300*                                                                         
042400             MOVE SEGT-KDANSKSEG                                          
042500               TO MOD-KDANSKSEG    (IX-RAD)                               
042600             INSPECT MOD-KDANSKSEG (IX-RAD)                               
042700               REPLACING LEADING SPACE BY ZERO                            
042800*--- TABLE TO GET PROCUREMENT SEGMENT DESCRIPTIONS ---*                   
042900             SEARCH ALL PSEGM-TAB-RECORD                                  
043000               WHEN PSEGM-ID (SEGM-IX) = SEGT-KDANSKSEG                   
043100               MOVE PSEGM-DESCRIPTION (SEGM-IX)                           
043200                 TO MOD-BEPSEGM (IX-RAD)                                  
043300             END-SEARCH                                                   
043400*--- TO GET PROCUREMENT FREQUENCY AND LEADTIME VALUES ---*                
043500             MOVE SEGT-IDREFTAB-LFL                                       
043600               TO MOD-IDREFTAB-LFL-UT  (IX-RAD)                           
043700             MOVE SEGT-IDREFTAB-LFM                                       
043800               TO MOD-IDREFTAB-LFM-UT  (IX-RAD)                           
043900             MOVE SEGT-IDREFTAB-LFH                                       
044000               TO MOD-IDREFTAB-LFH-UT  (IX-RAD)                           
044100             MOVE SEGT-IDREFTAB-LFXH                                      
044200               TO MOD-IDREFTAB-LFXH-UT (IX-RAD)                           
044300             MOVE SEGT-IDREFTAB-HFL                                       
044400               TO MOD-IDREFTAB-HFL-UT  (IX-RAD)                           
044500             MOVE SEGT-IDREFTAB-HFM                                       
044600               TO MOD-IDREFTAB-HFM-UT  (IX-RAD)                           
044700             MOVE SEGT-IDREFTAB-HFH                                       
044800               TO MOD-IDREFTAB-HFH-UT  (IX-RAD)                           
044900             MOVE SEGT-IDREFTAB-HFXH                                      
045000               TO MOD-IDREFTAB-HFXH-UT (IX-RAD)                           
045100          END-IF                                                          
045200          PERFORM IMS-GNP-WDB603                                          
045300          ADD  +1                    TO IX-RAD                            
045400        END-PERFORM                                                       
045500                                                                          
045600        IF SEGMENT-MISSING OR SEGMENT-END                                 
045700*                                                                         
045800           MOVE W-IDDC               TO SPAR-IDDC                         
045900           MOVE ZERO                 TO SPAR-SEGM-KDANSKSEG               
046000*                                                                         
046100           MOVE INF-LAST-PAGE        TO MED-IDMFSINF                      
046200        ELSE                                                              
046300*THESE VALUES WILL BE USED FOR F8 FUNCTION                                
046400           MOVE W-IDDC               TO SPAR-IDDC                         
046500           MOVE SEGT-KDANSKSEG       TO SPAR-SEGM-KDANSKSEG               
046600*                                                                         
046700           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
046800        END-IF                                                            
046900*                                                                         
047000        IF (FL-UPD = YES   AND                                            
047100            FL-ARRAY = NOO)                                               
047200            MOVE INF-UPDATE-DONE     TO MED-IDMFSINF                      
047300        END-IF                                                            
047400        CALL WMEDKONV USING MED-WMEDAREA                                  
047500        MOVE MED-TEMFSINF TO MOD-TEMFSINF                                 
047600*                                                                         
047700     END-IF                                                               
047800     .                                                                    
047900     EJECT                                                                
048000*                                                                         
048100 FA-LAST-UPDATED-DETAILS SECTION.                                         
048200        MOVE 'FA-LAST-UPDATED'   TO CURRENT-SECTION                       
048300                                                                          
048400*TO SHOW LAST UPDATED TIME,PERSON DETAILS.                                
048500        PERFORM IMS-GNP-WDB615                                            
048600        IF SEGMENT-FOUND                                                  
048700          MOVE LOGG-TIUPPDAT     TO MOD-TIUPPDAT                          
048800          MOVE LOGG-IDUSER       TO MOD-IDUSER                            
048900        ELSE                                                              
049000          MOVE ZERO              TO MOD-TIUPPDAT                          
049100          MOVE SPACE             TO MOD-IDUSER                            
049200        END-IF                                                            
049300     .                                                                    
049400     EJECT                                                                
049500*                                                                         
049600 G-CHECK-INPUT SECTION.                                                   
049700     MOVE 'G-CHECK-INPUT'         TO CURRENT-SECTION                      
049800     SKIP2                                                                
049900     MOVE YES                     TO SW-INPUT-RAETT                       
050000                                                                          
050100*--- MINIMUM PICKS ROLL 12 HF VALIDATION                                  
050200     IF (NOT (MID-KVOT-RULL12HF = ALL '+' ))                              
050300        IF MID-KVOT-RULL12HF IS NUMERIC                                   
050400           MOVE MFS-NUM-FAELT-RAETT                                       
050500             TO MOD-KVOT-RULL12HF-IN-ATTR                                 
050600        ELSE                                                              
050700           MOVE MFS-NUM-FAELT-FEL TO MOD-KVOT-RULL12HF-IN-ATTR            
050800           MOVE NOO               TO SW-INPUT-RAETT                       
050900        END-IF                                                            
051000        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVOT-RULL12HF-IN                 
051100     END-IF                                                               
051200                                                                          
051300*----------- MAXIMUM LEADTIMES VALIDATION -----------*                    
051400*--- MAX NUMBER OF WEEKS TO QUALIFY AS MEDIUM LEADTIME                    
051500     IF (NOT (MID-KVVECKOR-FTL = ALL '+' ))                               
051600        IF MID-KVVECKOR-FTL IS NUMERIC                                    
051700           MOVE MFS-NUM-FAELT-RAETT                                       
051800             TO MOD-KVVECKOR-FTL-IN-ATTR                                  
051900        ELSE                                                              
052000           MOVE MFS-NUM-FAELT-FEL TO MOD-KVVECKOR-FTL-IN-ATTR             
052100           MOVE NOO               TO SW-INPUT-RAETT                       
052200        END-IF                                                            
052300        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVVECKOR-FTL-IN                  
052400     END-IF                                                               
052500                                                                          
052600*--- MAX NUMBER OF WEEKS TO QUALIFY AS MEDIUM LEADTIME                    
052700     IF (NOT (MID-KVVECKOR-FTM = ALL '+' ))                               
052800        IF MID-KVVECKOR-FTM IS NUMERIC                                    
052900           MOVE MFS-NUM-FAELT-RAETT                                       
053000             TO MOD-KVVECKOR-FTM-IN-ATTR                                  
053100        ELSE                                                              
053200           MOVE MFS-NUM-FAELT-FEL TO MOD-KVVECKOR-FTM-IN-ATTR             
053300           MOVE NOO               TO SW-INPUT-RAETT                       
053400        END-IF                                                            
053500        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVVECKOR-FTM-IN                  
053600     END-IF                                                               
053700                                                                          
053800*--- MAX NUMBER OF WEEKS TO QUALIFY AS HIGH LEADTIME                      
053900     IF (NOT (MID-KVVECKOR-FTH = ALL '+' ))                               
054000        IF MID-KVVECKOR-FTH IS NUMERIC                                    
054100           MOVE MFS-NUM-FAELT-RAETT                                       
054200             TO MOD-KVVECKOR-FTH-IN-ATTR                                  
054300        ELSE                                                              
054400           MOVE MFS-NUM-FAELT-FEL TO MOD-KVVECKOR-FTH-IN-ATTR             
054500           MOVE NOO               TO SW-INPUT-RAETT                       
054600        END-IF                                                            
054700        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVVECKOR-FTH-IN                  
054800     END-IF                                                               
054900                                                                          
055000*------ PROCUREMENT TABLE FOR FREQUENCY AND LEADTIME -------*             
055100     MOVE +1              TO  IX-RAD                                      
055200     PERFORM UNTIL IX-RAD  >  RAD-MAX                                     
055300*--- TBL FOR LOW FREQ & LOW LEADTIME                                      
055400       IF (NOT (MID-IDREFTAB-LFL (IX-RAD) = ALL '+' ))                    
055500          IF MID-IDREFTAB-LFL (IX-RAD) IS ALPHABETIC                      
055600             MOVE MFS-NUM-FAELT-RAETT                                     
055700               TO MOD-IDREFTAB-LFL-IN-ATTR (IX-RAD)                       
055800          ELSE                                                            
055900             MOVE MFS-NUM-FAELT-FEL                                       
056000               TO MOD-IDREFTAB-LFL-IN-ATTR (IX-RAD)                       
056100             MOVE NOO                                                     
056200               TO SW-INPUT-RAETT                                          
056300          END-IF                                                          
056400          MOVE MFS-ROER-EJ-FAELT                                          
056500            TO MOD-IDREFTAB-LFL-IN (IX-RAD)                               
056600       END-IF                                                             
056700*--- TBL FOR LOW FREQ & LOW LEADTIME                                      
056800       IF (NOT (MID-IDREFTAB-LFL (IX-RAD) = ALL '+' ))                    
056900          IF MID-IDREFTAB-LFL (IX-RAD) IS ALPHABETIC                      
057000             MOVE MFS-NUM-FAELT-RAETT                                     
057100               TO MOD-IDREFTAB-LFL-IN-ATTR (IX-RAD)                       
057200          ELSE                                                            
057300             MOVE MFS-NUM-FAELT-FEL                                       
057400               TO MOD-IDREFTAB-LFL-IN-ATTR (IX-RAD)                       
057500             MOVE NOO                                                     
057600               TO SW-INPUT-RAETT                                          
057700          END-IF                                                          
057800          MOVE MFS-ROER-EJ-FAELT                                          
057900            TO MOD-IDREFTAB-LFL-IN (IX-RAD)                               
058000       END-IF                                                             
058100*--- TBL FOR LOW FREQ & MED LEADTIME                                      
058200       IF (NOT (MID-IDREFTAB-LFM (IX-RAD) = ALL '+' ))                    
058300          IF MID-IDREFTAB-LFM (IX-RAD) IS ALPHABETIC                      
058400             MOVE MFS-NUM-FAELT-RAETT                                     
058500               TO MOD-IDREFTAB-LFM-IN-ATTR (IX-RAD)                       
058600          ELSE                                                            
058700             MOVE MFS-NUM-FAELT-FEL                                       
058800               TO MOD-IDREFTAB-LFM-IN-ATTR (IX-RAD)                       
058900             MOVE NOO                                                     
059000               TO SW-INPUT-RAETT                                          
059100          END-IF                                                          
059200          MOVE MFS-ROER-EJ-FAELT                                          
059300            TO MOD-IDREFTAB-LFM-IN (IX-RAD)                               
059400       END-IF                                                             
059500*--- TBL FOR LOW FREQ & HIGH LEADTIME                                     
059600       IF (NOT (MID-IDREFTAB-LFH (IX-RAD) = ALL '+' ))                    
059700          IF MID-IDREFTAB-LFH (IX-RAD) IS ALPHABETIC                      
059800             MOVE MFS-NUM-FAELT-RAETT                                     
059900               TO MOD-IDREFTAB-LFH-IN-ATTR (IX-RAD)                       
060000          ELSE                                                            
060100             MOVE MFS-NUM-FAELT-FEL                                       
060200               TO MOD-IDREFTAB-LFH-IN-ATTR (IX-RAD)                       
060300             MOVE NOO                                                     
060400               TO SW-INPUT-RAETT                                          
060500          END-IF                                                          
060600          MOVE MFS-ROER-EJ-FAELT                                          
060700            TO MOD-IDREFTAB-LFH-IN (IX-RAD)                               
060800       END-IF                                                             
060900*--- TBL FOR LOW FREQ & XTRA HIGH LEADTIME                                
061000       IF (NOT (MID-IDREFTAB-LFXH (IX-RAD) = ALL '+' ))                   
061100          IF MID-IDREFTAB-LFXH (IX-RAD) IS ALPHABETIC                     
061200             MOVE MFS-NUM-FAELT-RAETT                                     
061300               TO MOD-IDREFTAB-LFXH-IN-ATTR (IX-RAD)                      
061400          ELSE                                                            
061500             MOVE MFS-NUM-FAELT-FEL                                       
061600               TO  MOD-IDREFTAB-LFXH-IN-ATTR (IX-RAD)                     
061700             MOVE NOO                                                     
061800               TO SW-INPUT-RAETT                                          
061900          END-IF                                                          
062000          MOVE MFS-ROER-EJ-FAELT                                          
062100            TO  MOD-IDREFTAB-LFXH-IN (IX-RAD)                             
062200       END-IF                                                             
062300*--- TBL FOR HIGH FREQ & LOW LEADTIME                                     
062400       IF (NOT (MID-IDREFTAB-HFL (IX-RAD) = ALL '+' ))                    
062500          IF MID-IDREFTAB-HFL (IX-RAD) IS ALPHABETIC                      
062600             MOVE MFS-NUM-FAELT-RAETT                                     
062700               TO MOD-IDREFTAB-HFL-IN-ATTR (IX-RAD)                       
062800          ELSE                                                            
062900             MOVE MFS-NUM-FAELT-FEL                                       
063000               TO  MOD-IDREFTAB-HFL-IN-ATTR (IX-RAD)                      
063100             MOVE NOO                                                     
063200               TO SW-INPUT-RAETT                                          
063300          END-IF                                                          
063400          MOVE MFS-ROER-EJ-FAELT                                          
063500            TO  MOD-IDREFTAB-HFL-IN (IX-RAD)                              
063600       END-IF                                                             
063700*--- TBL FOR HIGH FREQ & MED LEADTIME                                     
063800       IF (NOT (MID-IDREFTAB-HFM (IX-RAD) = ALL '+' ))                    
063900          IF MID-IDREFTAB-HFM (IX-RAD) IS ALPHABETIC                      
064000             MOVE MFS-NUM-FAELT-RAETT                                     
064100               TO MOD-IDREFTAB-HFM-IN-ATTR (IX-RAD)                       
064200          ELSE                                                            
064300             MOVE MFS-NUM-FAELT-FEL                                       
064400               TO MOD-IDREFTAB-HFM-IN-ATTR (IX-RAD)                       
064500             MOVE NOO                                                     
064600               TO SW-INPUT-RAETT                                          
064700          END-IF                                                          
064800          MOVE MFS-ROER-EJ-FAELT                                          
064900            TO MOD-IDREFTAB-HFM-IN (IX-RAD)                               
065000       END-IF                                                             
065100*--- TBL FOR HIGH FREQ & HIGH LEADTIME                                    
065200       IF (NOT (MID-IDREFTAB-HFH (IX-RAD) = ALL '+' ))                    
065300          IF MID-IDREFTAB-HFH (IX-RAD) IS ALPHABETIC                      
065400             MOVE MFS-NUM-FAELT-RAETT                                     
065500               TO MOD-IDREFTAB-HFH-IN-ATTR (IX-RAD)                       
065600          ELSE                                                            
065700             MOVE MFS-NUM-FAELT-FEL                                       
065800               TO MOD-IDREFTAB-HFH-IN-ATTR (IX-RAD)                       
065900             MOVE NOO                                                     
066000               TO SW-INPUT-RAETT                                          
066100          END-IF                                                          
066200          MOVE MFS-ROER-EJ-FAELT                                          
066300            TO MOD-IDREFTAB-HFH-IN (IX-RAD)                               
066400       END-IF                                                             
066500*--- TBL FOR HIGH FREQ & XTRA HIGH LEADTIME                               
066600       IF (NOT (MID-IDREFTAB-HFXH (IX-RAD) = ALL '+' ))                   
066700          IF MID-IDREFTAB-HFXH (IX-RAD) IS ALPHABETIC                     
066800             MOVE MFS-NUM-FAELT-RAETT                                     
066900               TO MOD-IDREFTAB-HFXH-IN-ATTR (IX-RAD)                      
067000          ELSE                                                            
067100             MOVE MFS-NUM-FAELT-FEL                                       
067200               TO  MOD-IDREFTAB-HFXH-IN-ATTR (IX-RAD)                     
067300             MOVE NOO                                                     
067400               TO SW-INPUT-RAETT                                          
067500          END-IF                                                          
067600          MOVE MFS-ROER-EJ-FAELT                                          
067700            TO  MOD-IDREFTAB-HFXH-IN (IX-RAD)                             
067800       END-IF                                                             
067900     ADD +1 TO IX-RAD                                                     
068000     END-PERFORM                                                          
068100*                                                                         
068200     IF SW-INPUT-RAETT = NOO AND MOD-TEMFSFEL = SPACES                    
068300        MOVE ERR-WRONG-KEY  TO MED-IDMFSFEL                               
068400        CALL WMEDKONV USING MED-WMEDAREA                                  
068500        MOVE MED-MFSFEL     TO MOD-TEMFSFEL                               
068600     ELSE                                                                 
068700        IF SW-INPUT-RAETT = NOO AND MOD-TEMFSFEL NOT = SPACES             
068800           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
068900           CALL WMEDKONV USING MED-WMEDAREA                               
069000           MOVE MED-MFSFEL     TO MOD-TEMFSFEL                            
069100        END-IF                                                            
069200        PERFORM E-SAME-PAGE                                               
069300     END-IF                                                               
069400     .                                                                    
069500     EJECT                                                                
069600*                                                                         
069700 H-UPDATE SECTION.                                                        
069800     MOVE 'H-UPDATE'           TO CURRENT-SECTION                         
069900                                                                          
070000     MOVE W-IDDC               TO MOD-IDDC-UT                             
070100                                                                          
070200     IF MID-COPY-IDDC = ALL '+' OR SPACES                                 
070300        PERFORM HA-UPDATE-FREQUENCY                                       
070400        PERFORM HB-UPDATE-TABLE                                           
070500*TO STAY IN THE SAME POSITION AFTER UPDATE                                
070600        MOVE MID-KDANSKSEG (1) TO W-KDANSKSEG                             
070700*                                                                         
070800     ELSE                                                                 
070900        PERFORM HC-COPY-DC                                                
071000     END-IF                                                               
071100                                                                          
071200     IF FL-UPD = YES                                                      
071300*TO CAPTURE UPDATED DATE, PERSON DETAILS.                                 
071400        PERFORM IMS-GHU-WDB615                                            
071500        IF SEGMENT-FOUND                                                  
071600          MOVE TODAYS-DATE  TO LOGG-TIUPPDAT                              
071700          MOVE MSGI-IDUSER  TO LOGG-IDUSER                                
071800          PERFORM IMS-REPL-WDB615                                         
071900        ELSE                                                              
072000          MOVE '2455'       TO LOGG-IDTRANS                               
072100          MOVE SPACE        TO LOGG-IDDC-REF                              
072200          MOVE TODAYS-DATE  TO LOGG-TIUPPDAT                              
072300          MOVE MSGI-IDUSER  TO LOGG-IDUSER                                
072400          PERFORM IMS-ISRT-WDB615                                         
072500        END-IF                                                            
072600*CLEAR THE SCREEN AFTER SUCCESSFUL UPDATE.                                
072700        PERFORM MFS-RENSA-FAELT-IN                                        
072800*                                                                         
072900     END-IF                                                               
073000     .                                                                    
073100     EJECT                                                                
073200*                                                                         
073300 HA-UPDATE-FREQUENCY SECTION.                                             
073400     MOVE 'HA-UPDATE-FREQ' TO CURRENT-SECTION                             
073500                                                                          
073600*UPDATE WDB601                                                            
073700     IF ( NOT (MID-KVOT-RULL12HF = ALL '+' )) OR                          
073800        ( NOT (MID-KVVECKOR-FTL = ALL '+' )) OR                           
073900        ( NOT (MID-KVVECKOR-FTM = ALL '+' )) OR                           
074000        ( NOT (MID-KVVECKOR-FTH = ALL '+' ))                              
074100                                                                          
074200        PERFORM IMS-GHU-WDB601                                            
074300                                                                          
074400        IF SEGMENT-FOUND                                                  
074500                                                                          
074600           IF (NOT (MID-KVVECKOR-FTL = ALL '+' ))                         
074700              MOVE MID-KVVECKOR-FTL                                       
074800                TO DCS-KVVECKOR-FTL                                       
074900           END-IF                                                         
075000           IF (NOT (MID-KVVECKOR-FTM = ALL '+' ))                         
075100              MOVE MID-KVVECKOR-FTM                                       
075200                TO DCS-KVVECKOR-FTM                                       
075300           END-IF                                                         
075400           IF (NOT (MID-KVVECKOR-FTH = ALL '+' ))                         
075500              MOVE MID-KVVECKOR-FTH                                       
075600                TO DCS-KVVECKOR-FTH                                       
075700           END-IF                                                         
075800           IF (NOT (MID-KVOT-RULL12HF = ALL '+' ))                        
075900              MOVE MID-KVOT-RULL12HF                                      
076000                TO DCS-KVOT-RULL12HF                                      
076100           END-IF                                                         
076200*                                                                         
076300           IF DCS-KVVECKOR-FTL  IS NUMERIC OR                             
076400              DCS-KVVECKOR-FTM  IS NUMERIC OR                             
076500              DCS-KVVECKOR-FTH  IS NUMERIC OR                             
076600              DCS-KVOT-RULL12HF IS NUMERIC                                
076700              PERFORM IMS-REPL-WDB601                                     
076800              MOVE YES           TO FL-UPD                                
076900           ELSE                                                           
077000              MOVE MED-2         TO MOD-TEMFSFEL                          
077100           END-IF                                                         
077200*                                                                         
077300        END-IF                                                            
077400     END-IF                                                               
077500     .                                                                    
077600     EJECT                                                                
077700*                                                                         
077800 HB-UPDATE-TABLE SECTION.                                                 
077900     MOVE 'HB-UPDATE-TABLE'   TO CURRENT-SECTION                          
078000                                                                          
078100*UPDATE WDB603                                                            
078200*UPDATE TABLE LF,TABLE,MF VALUES,HF VALUES,HF+ VALUES                     
078300     MOVE +1            TO  IX-RAD                                        
078400     PERFORM UNTIL IX-RAD      > RAD-MAX                                  
078500     IF (NOT (MID-KDANSKSEG (IX-RAD) = ALL '+' OR SPACES))                
078600       MOVE MID-KDANSKSEG (IX-RAD) TO W-KDANSKSEG                         
078700       PERFORM IMS-GHU-WDB603                                             
078800       IF (NOT (MID-IDREFTAB-LFL (IX-RAD) = ALL '+' OR SPACES))           
078900          IF (MID-IDREFTAB-LFL (IX-RAD) IS ALPHABETIC)                    
079000             MOVE MID-IDREFTAB-LFL (IX-RAD)                               
079100               TO SEGT-IDREFTAB-LFL                                       
079200          END-IF                                                          
079300       END-IF                                                             
079400       IF (NOT (MID-IDREFTAB-LFM (IX-RAD) = ALL '+' OR SPACES))           
079500          IF (MID-IDREFTAB-LFM (IX-RAD) IS ALPHABETIC)                    
079600             MOVE MID-IDREFTAB-LFM (IX-RAD)                               
079700               TO SEGT-IDREFTAB-LFM                                       
079800          END-IF                                                          
079900       END-IF                                                             
080000       IF (NOT (MID-IDREFTAB-LFH (IX-RAD) = ALL '+' OR SPACES))           
080100          IF (MID-IDREFTAB-LFH (IX-RAD) IS ALPHABETIC)                    
080200             MOVE MID-IDREFTAB-LFH (IX-RAD)                               
080300               TO SEGT-IDREFTAB-LFH                                       
080400          END-IF                                                          
080500       END-IF                                                             
080600       IF (NOT (MID-IDREFTAB-LFXH (IX-RAD) = ALL '+' OR SPACES))          
080700          IF (MID-IDREFTAB-LFXH (IX-RAD) IS ALPHABETIC)                   
080800             MOVE MID-IDREFTAB-LFXH (IX-RAD)                              
080900               TO SEGT-IDREFTAB-LFXH                                      
081000          END-IF                                                          
081100       END-IF                                                             
081200       IF (NOT (MID-IDREFTAB-HFL (IX-RAD) = ALL '+' OR SPACES))           
081300          IF (MID-IDREFTAB-HFL (IX-RAD) IS ALPHABETIC)                    
081400             MOVE MID-IDREFTAB-HFL (IX-RAD)                               
081500               TO SEGT-IDREFTAB-HFL                                       
081600          END-IF                                                          
081700       END-IF                                                             
081800       IF (NOT (MID-IDREFTAB-HFM (IX-RAD) = ALL '+' OR SPACES))           
081900          IF (MID-IDREFTAB-HFM (IX-RAD) IS ALPHABETIC)                    
082000             MOVE MID-IDREFTAB-HFM (IX-RAD)                               
082100               TO SEGT-IDREFTAB-HFM                                       
082200          END-IF                                                          
082300       END-IF                                                             
082400       IF (NOT (MID-IDREFTAB-HFH (IX-RAD) = ALL '+' OR SPACES))           
082500          IF (MID-IDREFTAB-HFH (IX-RAD) IS ALPHABETIC)                    
082600             MOVE MID-IDREFTAB-HFH (IX-RAD)                               
082700               TO SEGT-IDREFTAB-HFH                                       
082800          END-IF                                                          
082900       END-IF                                                             
083000       IF (NOT (MID-IDREFTAB-HFXH (IX-RAD) = ALL '+' OR SPACES))          
083100          IF (MID-IDREFTAB-HFXH (IX-RAD) IS ALPHABETIC)                   
083200             MOVE MID-IDREFTAB-HFXH (IX-RAD)                              
083300               TO SEGT-IDREFTAB-HFXH                                      
083400          END-IF                                                          
083500       END-IF                                                             
083600       PERFORM IMS-REPL-WDB603                                            
083700       MOVE YES TO FL-UPD                                                 
083800     ELSE                                                                 
083900*--- DISPLAY ERROR MESSAGE AT THE END OF ARRAY (IX-RAD >= 11)             
084000       IF ((MID-KDANSKSEG     (IX-RAD) = ALL '+' OR SPACES) AND           
084100           (MID-BEPSEGM       (IX-RAD) = ALL '+' OR SPACES) AND           
084200           (MID-IDREFTAB-LFL  (IX-RAD) NOT = ALL '+' )      OR            
084300           (MID-IDREFTAB-LFM  (IX-RAD) NOT = ALL '+' )      OR            
084400           (MID-IDREFTAB-LFH  (IX-RAD) NOT = ALL '+' )      OR            
084500           (MID-IDREFTAB-LFXH (IX-RAD) NOT = ALL '+' )      OR            
084600           (MID-IDREFTAB-HFL  (IX-RAD) NOT = ALL '+' )      OR            
084700           (MID-IDREFTAB-HFM  (IX-RAD) NOT = ALL '+' )      OR            
084800           (MID-IDREFTAB-HFH  (IX-RAD) NOT = ALL '+' )      OR            
084900           (MID-IDREFTAB-HFXH (IX-RAD) NOT = ALL '+' )                    
085000          )                                                               
085100           MOVE YES     TO FL-ARRAY                                       
085200           MOVE MED-1   TO MOD-TEMFSFEL                                   
085300       END-IF                                                             
085400     END-IF                                                               
085500     ADD +1 TO IX-RAD                                                     
085600     END-PERFORM                                                          
085700     .                                                                    
085800     EJECT                                                                
085900*                                                                         
086000 HC-COPY-DC SECTION.                                                      
086100     MOVE 'HC-COPY-DC'  TO CURRENT-SECTION                                
086200                                                                          
086300     PERFORM HD-GET-COPY-DC                                               
086400*--- LOAD THE SEGMENT(WDB603) OF FIELDS TO PERFORM COPY-DC                
086500     PERFORM IMS-GU-WDB601                                                
086600                                                                          
086700     IF SEGMENT-FOUND                                                     
086800        MOVE +1        TO INDX                                            
086900        PERFORM UNTIL INDX >= IX-RAD1                                     
087000          MOVE W-COPY-KDANSKSEG (INDX)                                    
087100            TO SEGT-KDANSKSEG                                             
087200          MOVE W-COPY-IDREFTAB-LFL (INDX)                                 
087300            TO SEGT-IDREFTAB-LFL                                          
087400          MOVE W-COPY-IDREFTAB-LFM (INDX)                                 
087500            TO SEGT-IDREFTAB-LFM                                          
087600          MOVE W-COPY-IDREFTAB-LFH (INDX)                                 
087700            TO SEGT-IDREFTAB-LFH                                          
087800          MOVE W-COPY-IDREFTAB-LFXH (INDX)                                
087900            TO SEGT-IDREFTAB-LFXH                                         
088000          MOVE W-COPY-IDREFTAB-HFL (INDX)                                 
088100            TO SEGT-IDREFTAB-HFL                                          
088200          MOVE W-COPY-IDREFTAB-HFM (INDX)                                 
088300            TO SEGT-IDREFTAB-HFM                                          
088400          MOVE W-COPY-IDREFTAB-HFH (INDX)                                 
088500            TO SEGT-IDREFTAB-HFH                                          
088600          MOVE W-COPY-IDREFTAB-HFXH (INDX)                                
088700            TO SEGT-IDREFTAB-HFXH                                         
088800          PERFORM IMS-ISRT-WDB603                                         
088900          MOVE YES            TO FL-UPD                                   
089000          ADD +1              TO INDX                                     
089100        END-PERFORM                                                       
089200     END-IF                                                               
089300     .                                                                    
089400     EJECT                                                                
089500                                                                          
089600 HD-GET-COPY-DC SECTION.                                                  
089700     MOVE 'HD-GET-COPY-DC'   TO CURRENT-SECTION                           
089800                                                                          
089900*--- PREPARE COPY-DC FROM THE ROOT SEGMENT(WDB601)                        
090000     MOVE MID-COPY-IDDC TO W-IDDC-COPY                                    
090100     PERFORM IMS-GU-WDB601-B                                              
090200     IF SEGMENT-FOUND                                                     
090300        PERFORM IMS-GNP-WDB603                                            
090400     END-IF                                                               
090500     IF SEGMENT-FOUND                                                     
090600        SET IX-RAD1            TO 1                                       
090700        PERFORM UNTIL SEGMENT-END OR IX-RAD1 > 270                        
090800          IF SEGT-KDANSKSEG > 0                                           
090900             MOVE SEGT-KDANSKSEG                                          
091000               TO W-COPY-KDANSKSEG (IX-RAD1)                              
091100             MOVE SEGT-IDREFTAB-LFL                                       
091200               TO W-COPY-IDREFTAB-LFL (IX-RAD1)                           
091300             MOVE SEGT-IDREFTAB-LFM                                       
091400               TO W-COPY-IDREFTAB-LFM (IX-RAD1)                           
091500             MOVE SEGT-IDREFTAB-LFH                                       
091600               TO W-COPY-IDREFTAB-LFH (IX-RAD1)                           
091700             MOVE SEGT-IDREFTAB-LFXH                                      
091800               TO W-COPY-IDREFTAB-LFXH (IX-RAD1)                          
091900             MOVE SEGT-IDREFTAB-HFL                                       
092000               TO W-COPY-IDREFTAB-HFL (IX-RAD1)                           
092100             MOVE SEGT-IDREFTAB-HFM                                       
092200               TO W-COPY-IDREFTAB-HFM (IX-RAD1)                           
092300             MOVE SEGT-IDREFTAB-HFH                                       
092400               TO W-COPY-IDREFTAB-HFH (IX-RAD1)                           
092500             MOVE SEGT-IDREFTAB-HFXH                                      
092600               TO W-COPY-IDREFTAB-HFXH (IX-RAD1)                          
092700          END-IF                                                          
092800          SET IX-RAD1 UP BY 1                                             
092900          PERFORM IMS-GNP-WDB603                                          
093000        END-PERFORM                                                       
093100     END-IF                                                               
093200     .                                                                    
093300     EJECT                                                                
093400*                                                                         
093500 I-PREVIOUS-PAGE SECTION.                                                 
093600     MOVE 'I-PREVIOUS-PAGE'               TO CURRENT-SECTION              
093700                                                                          
093800*--- LOAD PREVIOUS PAGE SEGMENTS (F6 - SCROLLS BACKWARD)                  
093900     IF SPAR-IDDC NOT = SPACE                                             
094000        MOVE SPAR-IDDC                    TO W-IDDC                       
094100        IF SPAR-SEGM-KDANSKSEG-FIRST > ZERO                               
094200           SEARCH ALL PSEGM-TAB-RECORD                                    
094300             WHEN PSEGM-ID (SEGM-IX) = SPAR-SEGM-KDANSKSEG-FIRST          
094400              SET SEGM-IX DOWN BY 13                                      
094500               IF SEGM-IX < 1                                             
094600                  MOVE INF-NO-MORE-F6     TO MED-IDMFSFEL                 
094700                  CALL WMEDKONV USING MED-WMEDAREA                        
094800                  MOVE MED-MFSFEL         TO MOD-TEMFSFEL                 
094900               ELSE                                                       
095000                  MOVE PSEGM-ID (SEGM-IX) TO W-KDANSKSEG                  
095100               END-IF                                                     
095200           END-SEARCH                                                     
095300        END-IF                                                            
095400     END-IF                                                               
095500     .                                                                    
095600     EJECT                                                                
095700*                                                                         
095800 MFS-RENSA-FAELT-IN SECTION.                                              
095900                                                                          
096000     MOVE MFS-RENSA-FAELT    TO MOD-IDDC-IN                               
096100     MOVE MFS-RENSA-FAELT    TO MOD-KVOT-RULL12HF-IN                      
096200     MOVE MFS-RENSA-FAELT    TO MOD-KVVECKOR-FTL-IN                       
096300     MOVE MFS-RENSA-FAELT    TO MOD-KVVECKOR-FTM-IN                       
096400     MOVE MFS-RENSA-FAELT    TO MOD-KVVECKOR-FTH-IN                       
096500                                                                          
096600     MOVE +1 TO IX-RAD                                                    
096700     PERFORM UNTIL IX-RAD > RAD-MAX                                       
096800       MOVE MFS-RENSA-FAELT                                               
096900         TO MOD-KDANSKSEG (IX-RAD)                                        
097000       MOVE MFS-RENSA-FAELT                                               
097100         TO MOD-BEPSEGM (IX-RAD)                                          
097200       MOVE MFS-RENSA-FAELT                                               
097300         TO MOD-IDREFTAB-LFL-IN (IX-RAD)                                  
097400       MOVE MFS-RENSA-FAELT                                               
097500         TO MOD-IDREFTAB-LFM-IN (IX-RAD)                                  
097600       MOVE MFS-RENSA-FAELT                                               
097700         TO MOD-IDREFTAB-LFH-IN (IX-RAD)                                  
097800       MOVE MFS-RENSA-FAELT                                               
097900         TO MOD-IDREFTAB-LFXH-IN (IX-RAD)                                 
098000       MOVE MFS-RENSA-FAELT                                               
098100         TO MOD-IDREFTAB-HFL-IN (IX-RAD)                                  
098200       MOVE MFS-RENSA-FAELT                                               
098300         TO MOD-IDREFTAB-HFM-IN (IX-RAD)                                  
098400       MOVE MFS-RENSA-FAELT                                               
098500         TO MOD-IDREFTAB-HFH-IN (IX-RAD)                                  
098600       MOVE MFS-RENSA-FAELT                                               
098700         TO MOD-IDREFTAB-HFXH-IN (IX-RAD)                                 
098800       ADD +1 TO IX-RAD                                                   
098900     END-PERFORM                                                          
099000     .                                                                    
099100     EJECT                                                                
099200*                                                                         
099300 MFS-RENSA-FAELT-UT  SECTION.                                             
099400*--- ALL OUTPUT FIELDS                                                    
099500     MOVE MFS-RENSA-FAELT    TO MOD-KVOT-RULL12HF-UT                      
099600     MOVE MFS-RENSA-FAELT    TO MOD-KVVECKOR-FTL-UT                       
099700     MOVE MFS-RENSA-FAELT    TO MOD-KVVECKOR-FTM-UT                       
099800     MOVE MFS-RENSA-FAELT    TO MOD-KVVECKOR-FTH-UT                       
099900                                                                          
100000     MOVE +1 TO IX-RAD                                                    
100100     PERFORM UNTIL IX-RAD > RAD-MAX                                       
100200       MOVE MFS-ERASE-FIELD                                               
100300         TO MOD-KDANSKSEG (IX-RAD)                                        
100400       MOVE MFS-ERASE-FIELD                                               
100500         TO MOD-BEPSEGM (IX-RAD)                                          
100600       MOVE MFS-ERASE-FIELD                                               
100700         TO MOD-IDREFTAB-LFL-UT (IX-RAD)                                  
100800       MOVE MFS-ERASE-FIELD                                               
100900         TO MOD-IDREFTAB-LFM-UT (IX-RAD)                                  
101000       MOVE MFS-ERASE-FIELD                                               
101100         TO MOD-IDREFTAB-LFH-UT (IX-RAD)                                  
101200       MOVE MFS-ERASE-FIELD                                               
101300         TO MOD-IDREFTAB-LFXH-UT (IX-RAD)                                 
101400       MOVE MFS-ERASE-FIELD                                               
101500         TO MOD-IDREFTAB-HFL-UT (IX-RAD)                                  
101600       MOVE MFS-ERASE-FIELD                                               
101700         TO MOD-IDREFTAB-HFM-UT (IX-RAD)                                  
101800       MOVE MFS-ERASE-FIELD                                               
101900         TO MOD-IDREFTAB-HFH-UT (IX-RAD)                                  
102000       MOVE MFS-ERASE-FIELD                                               
102100         TO MOD-IDREFTAB-HFXH-UT (IX-RAD)                                 
102200       ADD +1 TO IX-RAD                                                   
102300     END-PERFORM                                                          
102400     .                                                                    
102500     EJECT                                                                
102600                                                                          
102700 MFS-CLOSE-FIELD-IN SECTION.                                              
102800*--- ALLA INDATA-FÄLT                                                     
102900     MOVE MFS-CLOSE-FIELD    TO MOD-COPY-IDDC-ATTR                        
103000     MOVE MFS-CLOSE-FIELD    TO MOD-KVOT-RULL12HF-IN-ATTR                 
103100     MOVE MFS-CLOSE-FIELD    TO MOD-KVVECKOR-FTL-IN-ATTR                  
103200     MOVE MFS-CLOSE-FIELD    TO MOD-KVVECKOR-FTM-IN-ATTR                  
103300     MOVE MFS-CLOSE-FIELD    TO MOD-KVVECKOR-FTH-IN-ATTR                  
103400     MOVE +1 TO IX-RAD                                                    
103500     PERFORM UNTIL IX-RAD > RAD-MAX                                       
103600       MOVE MFS-CLOSE-FIELD                                               
103700         TO MOD-IDREFTAB-LFL-IN-ATTR (IX-RAD)                             
103800       MOVE MFS-CLOSE-FIELD                                               
103900         TO MOD-IDREFTAB-LFM-IN-ATTR (IX-RAD)                             
104000       MOVE MFS-CLOSE-FIELD                                               
104100         TO MOD-IDREFTAB-LFH-IN-ATTR (IX-RAD)                             
104200       MOVE MFS-CLOSE-FIELD                                               
104300         TO MOD-IDREFTAB-LFXH-IN-ATTR (IX-RAD)                            
104400       MOVE MFS-CLOSE-FIELD                                               
104500         TO MOD-IDREFTAB-HFL-IN-ATTR (IX-RAD)                             
104600       MOVE MFS-CLOSE-FIELD                                               
104700         TO MOD-IDREFTAB-HFM-IN-ATTR (IX-RAD)                             
104800       MOVE MFS-CLOSE-FIELD                                               
104900         TO MOD-IDREFTAB-HFH-IN-ATTR (IX-RAD)                             
105000       MOVE MFS-CLOSE-FIELD                                               
105100         TO MOD-IDREFTAB-HFXH-IN-ATTR (IX-RAD)                            
105200       ADD +1 TO IX-RAD                                                   
105300     END-PERFORM                                                          
105400     .                                                                    
105500     EJECT                                                                
105600                                                                          
105700*--- IMS SECTIONS ---                                                     
105800     SKIP3                                                                
105900 IMS-GET-MSG SECTION.                                                     
106000                                                                          
106100     MOVE '  QC' TO GOOD-STATUSCODES                                      
106200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
106300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
106400     PERFORM IMS-STATUSCHECK                                              
106500     .                                                                    
106600     SKIP3                                                                
106700 IMS-INSERT-MSG SECTION.                                                  
106800                                                                          
106900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
107000     MOVE SPACE TO GOOD-STATUSCODES                                       
107100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
107200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
107300     PERFORM IMS-STATUSCHECK                                              
107400     .                                                                    
107500     EJECT                                                                
107600 IMS-GU-WDB601    SECTION.                                                
107700     MOVE 'GU-WDB601' TO CURRENT-IMS-SECTION                              
107800                                                                          
107900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
108000          DELIMITED BY SIZE INTO SSA1                                     
108100     MOVE '  GE' TO GOOD-STATUSCODES                                      
108200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
108300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
108400     PERFORM IMS-STATUSCHECK                                              
108500     .                                                                    
108600     EJECT                                                                
108700 IMS-GU-WDB601-B  SECTION.                                                
108800     MOVE 'GU-WDB601-B' TO CURRENT-IMS-SECTION                            
108900                                                                          
109000     STRING 'WDB601  (IDDC     =' W-IDDC-COPY-X ')'                       
109100          DELIMITED BY SIZE INTO SSA1                                     
109200     MOVE '  GE' TO GOOD-STATUSCODES                                      
109300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
109400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
109500     PERFORM IMS-STATUSCHECK                                              
109600     .                                                                    
109700     EJECT                                                                
109800 IMS-GHU-WDB601 SECTION.                                                  
109900     MOVE 'GHU-WDB601' TO CURRENT-IMS-SECTION                             
110000                                                                          
110100     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
110200          DELIMITED BY SIZE INTO SSA1                                     
110300     MOVE '  GE' TO GOOD-STATUSCODES                                      
110400     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB601 SSA1                   
110500     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
110600     PERFORM IMS-STATUSCHECK                                              
110700     .                                                                    
110800     EJECT                                                                
110900                                                                          
111000 IMS-REPL-WDB601 SECTION.                                                 
111100     MOVE 'REPL-WDB601' TO CURRENT-IMS-SECTION                            
111200                                                                          
111300     MOVE '  ' TO GOOD-STATUSCODES                                        
111400     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB601                       
111500     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
111600     PERFORM IMS-STATUSCHECK                                              
111700     .                                                                    
111800     EJECT                                                                
111900 IMS-GNP-WDB615 SECTION.                                                  
112000     MOVE 'GNP-WDB615' TO CURRENT-IMS-SECTION                             
112100                                                                          
112200     STRING 'WDB615  (IDTRANS  =' W-IDTRANS-B6-X ')'                      
112300     DELIMITED BY SIZE INTO SSA1                                          
112400     MOVE '  GE' TO GOOD-STATUSCODES                                      
112500     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB615 SSA1                   
112600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
112700     PERFORM IMS-STATUSCHECK                                              
112800     .                                                                    
112900     EJECT                                                                
113000 IMS-GHU-WDB615 SECTION.                                                  
113100     MOVE 'GHU-WDB615' TO CURRENT-IMS-SECTION                             
113200                                                                          
113300     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
113400          DELIMITED BY SIZE INTO SSA1                                     
113500     STRING 'WDB615  (IDTRANS  =' W-IDTRANS-B6-X ')'                      
113600          DELIMITED BY SIZE INTO SSA2                                     
113700     MOVE '  GE' TO GOOD-STATUSCODES                                      
113800     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB615 SSA1 SSA2              
113900     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
114000     PERFORM IMS-STATUSCHECK                                              
114100     .                                                                    
114200     EJECT                                                                
114300 IMS-REPL-WDB615 SECTION.                                                 
114400     MOVE 'REPL-WDB615' TO CURRENT-IMS-SECTION                            
114500                                                                          
114600     MOVE '  ' TO GOOD-STATUSCODES                                        
114700     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB615                       
114800     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
114900     PERFORM IMS-STATUSCHECK                                              
115000     .                                                                    
115100     EJECT                                                                
115200 IMS-ISRT-WDB615 SECTION.                                                 
115300     MOVE 'ISRT-WDB615' TO CURRENT-IMS-SECTION                            
115400                                                                          
115500     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
115600          DELIMITED BY SIZE INTO SSA1                                     
115700     MOVE 'WDB615  ' TO SSA2                                              
115800     MOVE '    ' TO GOOD-STATUSCODES                                      
115900     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB615 SSA1 SSA2             
116000     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
116100     PERFORM IMS-STATUSCHECK                                              
116200     .                                                                    
116300     EJECT                                                                
116400 IMS-GNP-WDB603-FIRST SECTION.                                            
116500     MOVE 'GNP-WDB603' TO CURRENT-IMS-SECTION                             
116600                                                                          
116700     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
116800          DELIMITED BY SIZE INTO SSA1                                     
116900     STRING 'WDB603  (KDANSKSG =' W-KDANSKSEG-X                           
117000                    '!KDANSKSG >' W-KDANSKSEG-X ')'                       
117100          DELIMITED BY SIZE INTO SSA2                                     
117200     MOVE '  GE' TO GOOD-STATUSCODES                                      
117300     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB603 SSA1 SSA2              
117400     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
117500     PERFORM IMS-STATUSCHECK                                              
117600     .                                                                    
117700     EJECT                                                                
117800 IMS-GNP-WDB603 SECTION.                                                  
117900     MOVE 'GNP-WDB603' TO CURRENT-IMS-SECTION                             
118000                                                                          
118100     MOVE 'WDB603  '       TO SSA1                                        
118200     MOVE '  GE' TO GOOD-STATUSCODES                                      
118300     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB603 SSA1                   
118400     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
118500     PERFORM IMS-STATUSCHECK                                              
118600     .                                                                    
118700     EJECT                                                                
118800 IMS-GHU-WDB603 SECTION.                                                  
118900     MOVE 'GHU-WDB603' TO CURRENT-IMS-SECTION                             
119000                                                                          
119100     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
119200          DELIMITED BY SIZE INTO SSA1                                     
119300     STRING 'WDB603  (KDANSKSG =' W-KDANSKSEG-X ')'                       
119400          DELIMITED BY SIZE INTO SSA2                                     
119500     MOVE '  ' TO GOOD-STATUSCODES                                        
119600     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB603 SSA1 SSA2              
119700     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
119800     PERFORM IMS-STATUSCHECK                                              
119900     .                                                                    
120000     EJECT                                                                
120100 IMS-ISRT-WDB603 SECTION.                                                 
120200     MOVE 'ISRT-WDB603' TO CURRENT-IMS-SECTION                            
120300                                                                          
120400     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
120500          DELIMITED BY SIZE INTO SSA1                                     
120600     MOVE 'WDB603 ' TO SSA2                                               
120700     MOVE '  II' TO GOOD-STATUSCODES                                      
120800     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB603 SSA1 SSA2             
120900     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
121000     PERFORM IMS-STATUSCHECK                                              
121100     .                                                                    
121200     EJECT                                                                
121300 IMS-REPL-WDB603 SECTION.                                                 
121400     MOVE 'REPL-WDB603' TO CURRENT-IMS-SECTION                            
121500                                                                          
121600     MOVE '  ' TO GOOD-STATUSCODES                                        
121700     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB603                       
121800     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
121900     PERFORM IMS-STATUSCHECK                                              
122000     .                                                                    
122100     EJECT                                                                
122200 IMS-STATUSCHECK SECTION.                                                 
122300                                                                          
122400     SET STATUS-IX TO 1                                                   
122500     SEARCH GODK-STATUS                                                   
122600       AT END                                                             
122700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
122800         DELIMITED BY SIZE INTO FELTEXT                                   
122900         CALL FELLOG                                                      
123000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
123100         CONTINUE                                                         
123200     END-SEARCH                                                           
123300     .                                                                    
123400     EJECT                                                                
