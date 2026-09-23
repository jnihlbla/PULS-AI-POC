000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1181500.                                                
000300 AUTHOR.         DADHICH PRERNA.                                          
000400 DATE-WRITTEN.   24/12/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        PROGRAM READS W1181101 INPUT FILE WITH                           
001000*        PART ATTRIBUTES TO                                               
001100*        UPDATE A EXISTING PART IN WDK6                                   
001200*        AND INSERT PART IN WDD5,WDK2                                     
001300*                                                                         
001400*        THE PROGRAM UPDATES  WDK6                                        
001500*        THE PROGRAM UPDATES  WDD5                                        
001600*        THE PROGRAM UPDATES  WDK2                                        
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200 77  IDPGM                       PIC X(8)    VALUE 'W1181500'.            
003300 77  KDRC-DISPLAY                PIC Z(5).                                
003400 77  YES                         PIC X       VALUE 'J'.                   
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003900 77  WS-IDOUTREC                 PIC X(30)   VALUE SPACES.                
004000*                                                                         
004100 01  WS-WORK.                                                             
004200     03  WS-IDPSN                PIC 9(3)    VALUE ZERO.                  
004300     03  WS-KDARTHNT             PIC S9(7)   COMP-3 VALUE ZERO.           
004400     03  WS-KDEMBKOD-2           PIC S9(3)   COMP-3 VALUE ZERO.           
004500     03  WS-KVART                PIC 9(6)    VALUE ZERO.                  
004600     03  WS-KDFARLIG             PIC 9(1)    VALUE ZERO.                  
004700     03  WS-KDYTBEH              PIC 9(3)    VALUE ZERO.                  
004800     03  WS-FLVKART-NTO          PIC X       VALUE SPACES.                
004900     03  WS-FLDAP                PIC X       VALUE SPACES.                
005000     03  WS-TEARTNOT-1           PIC X(40)   VALUE SPACES.                
005100     03  WS-TEARTNOT-3           PIC X(40)   VALUE SPACES.                
005200     03  WS-TEARTNOT-7           PIC X(40)   VALUE SPACES.                
005300     03  WS-IDCDS                PIC X(8)    VALUE SPACES.                
005400     03  WS-KDARTSYS             PIC X(2)    VALUE SPACES.                
005500*                                                                         
005600 01  WS-SAVE-IDCOM-ERROR-MAIL    PIC S9(9)   COMP VALUE ZERO.             
005700     SKIP2                                                                
005800 01  CHKP-VAR.                                                            
005900     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
006000     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
006100     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
006200     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
006300     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
006400     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
006500     03 GSAM-PCB-LENGTH          PIC S9(9)   VALUE +48 COMP SYNC.         
006600                                                                          
006700 77  TYPE-OF-RUN                 PIC X       VALUE 'N'.                   
006800     88 NORMAL-RUN                           VALUE 'N'.                   
006900     88 RESTART-RUN                          VALUE 'R'.                   
007000                                                                          
007100 77  SW-UPD-WDK60111             PIC X       VALUE 'N'.                   
007200     88 UPD-WDK60111-JA                      VALUE 'J'.                   
007300                                                                          
007400 77  SW-UPD-WDK625               PIC X       VALUE 'N'.                   
007500     88 UPD-WDK625-JA                        VALUE 'J'.                   
007600*                                                                         
007700 77  SW-UPD-EMBKOD               PIC X       VALUE 'N'.                   
007800     88 UPD-EMBKOD-JA                        VALUE 'J'.                   
007900                                                                          
008000 77  WS-KDEMBKOD-CHK             PIC 9(3).                                
008100     88 KDEMBKOD-CHK-VALID                   VALUE 0 60 70.               
008200                                                                          
008300 01  WS-NOTE-CHANGE.                                                      
008400     03 WS-WDK625-NOT-1          PIC X       VALUE 'N'.                   
008500     03 WS-WDK625-NOT-3          PIC X       VALUE 'N'.                   
008600     03 WS-WDK625-NOT-7          PIC X       VALUE 'N'.                   
008700                                                                          
008710*USED FOR RESTART FILE. BUT ONLY 3000 BYTES ARE ALLOWED.                  
008720*SO, THE REMAINING DATA(SUPERSESSION) WILL NOT BE FETCHED                 
008730*FROM INPUT FILE.                                                         
008800 01  IN-REC.                                                              
008900     03  IN-DATA                 PIC X(30000).                            
009000                                                                          
009100 01  ERROR-TEXT.                                                          
009200     03  FILLER                  PIC X(8)    VALUE 'ERROR:'.              
009300     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
009400                                                                          
009500 77  W11811-EMPTY-SW             PIC X       VALUE 'Y'.                   
009600 77  INDATA-EOF-SW               PIC X       VALUE 'N'.                   
009700     88  END-OF-INDATA                       VALUE 'Y'.                   
009800     EJECT                                                                
009900 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
010000 01  FILLER REDEFINES TODAYS-DATE.                                        
010100     03  TODAYS-DATE-YEAR        PIC 9(2).                                
010200     03  TODAYS-DATE-MONTH       PIC 9(2).                                
010300     03  TODAYS-DATE-DAY         PIC 9(2).                                
010400     EJECT                                                                
010500                                                                          
010600 01  W-VIMSID.                                                            
010700     03  W-IMSID                 PIC X(4)    VALUE SPACE.                 
010800     03  FILLER                  PIC X(4)    VALUE SPACE.                 
010900     EJECT                                                                
011000                                                                          
011100 01  HDR-AREA.                                                            
011200*    03  -COPY WZ01REQU                                                   
011300*    03  -COPY WZ04HDR                                                    
011400                                                                          
011500*    --- PARAMETRAR TILL ABEND                                            
011600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011900     EJECT                                                                
012000 01  GENERAL-SUBPROGRAMS.                                                 
012100*                                                                         
012200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012400     03  VIMSID                  PIC X(8)    VALUE 'VIMSID  '.            
012500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012600     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
012700     EJECT                                                                
012800*    --- PARAMETRAR TILL POSTSUM                                          
012900*                                                                         
013000*01  -COPY W0005   -PRE  POSTSUM-                                         
013100     EJECT                                                                
013200 01  W11811-AREA-START           PIC X(24)   VALUE                        
013300                                              'W11811-AREA-START'.        
013400     SKIP2                                                                
013500*01  AREA -COPY W1181101   -PRE IN-                                       
013600*                                                                         
013700 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
013800 01  SEND-AREA.                                                           
013900*    03  -COPY WZ01SEND                                                   
014000     EJECT                                                                
014100                                                                          
014200 01  SEND-RAD.                                                            
014300   03  STYRTECKEN-RAD          PIC X.                                     
014400   03  MAIL-RAD                PIC X(80)  VALUE SPACE.                    
014500*    --- CONTROL CHARACTERS                                               
014600 01  WS-SKIP1                    PIC X       VALUE ' '.                   
014700 01  WS-SKIP2                    PIC X       VALUE '0'.                   
014800 01  WS-SKIP3                    PIC X       VALUE '-'.                   
014900 01  WS-PAGESKIP                 PIC X       VALUE '1'.                   
015000                                                                          
015100     EJECT                                                                
015200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015300     SKIP3                                                                
015400 01  KEYS-TILL-DLI.                                                       
015500     03  W-IDARTNR-X.                                                     
015600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015700     03  W-KDSEGKEY-X.                                                    
015800         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
015900     03  W-KDNOTTYP-X.                                                    
016000         05  W-KDNOTTYP          PIC S9(1)   COMP-3 VALUE ZERO.           
016100     03  W-KDEMBAL-X.                                                     
016200         05  W-KDEMBAL           PIC X(3)    VALUE SPACE.                 
016300     03  W-WDD5-X.                                                        
016400         05  W-IDARTNR-WDD5      PIC S9(9)   VALUE ZERO COMP-3.           
016500     03  W-WDK2-X.                                                        
016600         05  W-IDARTNR-WDK2      PIC S9(9)   VALUE ZERO COMP-3.           
016700     SKIP2                                                                
016800*    --- STATUS-KOD FRÅN IMS                                              
016900 01  STATUS-WS                   PIC XX.                                  
017000     88  SEGMENT-FOUND                       VALUE '  '.                  
017100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
017200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
017300     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
017400     88  IMS-NOT-OK                          VALUE 'XD'.                  
017500     SKIP2                                                                
017600 01  GOOD-STATUSCODES.                                                    
017700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017800     SKIP3                                                                
017900 01  SSA1                        PIC X(64).                               
018000 01  SSA2                        PIC X(64).                               
018100 01  SSA3                        PIC X(64).                               
018200     EJECT                                                                
018300*    --- IMS FUNCTION CODES                                               
018400*01  -COPY W0003                                                          
018500     EJECT                                                                
018600*    ---  DLI INPUT-OUTPUT AREA                                           
018700                                                                          
018800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK60111'.                    
018900 01  DLI-IO-WDK60111.                                                     
019000*    03  -COPY WDK601                                                     
019100*    03  -COPY WDK611                                                     
019200     EJECT                                                                
019300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK613'.                      
019400 01  DLI-IO-WDK613.                                                       
019500*    03  -COPY WDK613                                                     
019600     EJECT                                                                
019700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK625'.                      
019800 01  DLI-IO-WDK625.                                                       
019900*    03  -COPY WDK625                                                     
020000     EJECT                                                                
020100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD501'.                      
020200 01  DLI-IO-WDD501.                                                       
020300*    03  -COPY WDD501  -PRE ARTN01-                                       
020400                                                                          
020500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK201'.                      
020600 01  DLI-IO-WDK201.                                                       
020700*    03  -COPY WDK201                                                     
020800                                                                          
020900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK212'.                      
021000 01  DLI-IO-WDK212.                                                       
021100*    03  -COPY WDK212                                                     
021200                                                                          
021300     EJECT                                                                
021400 LINKAGE SECTION.                                                         
021500                                                                          
021600*01  -COPY W0009   -PRE MSG-                                              
021700                                                                          
021800*01  -COPY W0008  -PRE WDK6A-                                             
021900     05  FILLER                  PIC X.                                   
022000*01  -COPY W0008  -PRE WDK6-                                              
022100     05  FILLER                  PIC X.                                   
022200                                                                          
022300*01  -COPY W0008  -PRE WDD5-                                              
022400     05  FILLER                  PIC X.                                   
022500                                                                          
022600*01  -COPY W0008  -PRE WDK2-                                              
022700     05  FILLER                  PIC X.                                   
022800     EJECT                                                                
022900*01  -COPY W0009  -PRE DISTRDOC-                                          
023000     SKIP2                                                                
023100*01  -COPY W0008  -PRE IN-GSAM-                                           
023200     05  KEYFB-RSA               PIC X(12).                               
023300* KEYFB-RSA WILL HAVE THE POSITION OF LAST READ RECORD FROM INPUT         
023400* GSAM FILE. THIS IS REQUIRED TO REPOSITION THE POINTER IN INPUT          
023500* FILE DURING RESTART AFTER AN ABEND RUN.                                 
023600                                                                          
023700 01  DISP-PCB                    PIC X.                                   
023800     SKIP2                                                                
023900 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB WDK6A-PCB WDK6-PCB        
024000                           WDD5-PCB WDK2-PCB IN-GSAM-PCB.                 
024100                                                                          
024200 MAIN SECTION.                                                            
024300     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB WDK6A-PCB WDK6-PCB        
024400                           WDD5-PCB WDK2-PCB IN-GSAM-PCB.                 
024500                                                                          
024600     SKIP2                                                                
024700     PERFORM A-INIT                                                       
024800                                                                          
024900     IF NORMAL-RUN                                                        
025000       PERFORM IMS-GN-INDATA                                              
025100     ELSE                                                                 
025200       PERFORM IMS-GU-INDATA                                              
025300     END-IF                                                               
025400                                                                          
025500     IF END-OF-INDATA                                                     
025600       CONTINUE                                                           
025700     ELSE                                                                 
025800       PERFORM UNTIL END-OF-INDATA                                        
025900                                                                          
026000         IF CHKP-ANT = CHKP-MAX                                           
026100           PERFORM X-TAKE-CHECKPOINT                                      
026200         END-IF                                                           
026300                                                                          
026400         IF IN-IDPTYP = 'INS' OR                                          
026500                        'UPD' OR                                          
026600                        'SSI' OR                                          
026700                        'SSU' OR                                          
026800                        'EXI' OR                                          
026900                        'EXU' OR                                          
027000                        'EXS' OR                                          
027100                        'SWI' OR                                          
027200                        'SWU' OR                                          
027300                        'SWS'                                             
027400           MOVE IN-IDARTNR       TO W-IDARTNR                             
027500                                    W-IDARTNR-WDD5                        
027600                                    W-IDARTNR-WDK2                        
027700                                                                          
027800           PERFORM IMS-GU-WDK60111                                        
027900                                                                          
028000           IF SEGMENT-FOUND                                               
028100              PERFORM B-GET-WDK6                                          
028200*                                                                         
028300***           WHEN DANGEOUS GOODS INSERT OR UPD WT/VOL                    
028400*                                                                         
028500              IF IN-KDFARLIG = 4 OR 6                                     
028600                 PERFORM C-UPDATE-WDD5                                    
028700              END-IF                                                      
028800*                                                                         
028900              IF IN-VKART-NTO > ZERO                                      
029000                 PERFORM D-UPDATE-WDK2                                    
029100              END-IF                                                      
029200           ELSE                                                           
029300              MOVE YES           TO WS-FLDAP                              
029400              DISPLAY IN-IDARTNR ' IS MISSING IN WDK6'                    
029500           END-IF                                                         
029600         END-IF                                                           
029700         ADD +1                  TO CHKP-ANT                              
029800                                                                          
029900         PERFORM IMS-GN-INDATA                                            
030000       END-PERFORM                                                        
030100     END-IF                                                               
030200                                                                          
030300     IF WS-FLDAP  = YES                                                   
030400*SEND MAIL TO WSYST                                                       
030500        PERFORM E-FEL-TRANS-MAIL                                          
030600     END-IF                                                               
030700                                                                          
030800                                                                          
030900     PERFORM Z-FINIT                                                      
031000                                                                          
031100     MOVE ZERO TO RETURN-CODE                                             
031200     GOBACK                                                               
031300     .                                                                    
031400     EJECT                                                                
031500 A-INIT SECTION.                                                          
031600     SKIP2                                                                
031700                                                                          
031800     PERFORM IMS-RESTART                                                  
031900                                                                          
032000     ACCEPT DAGENS-DATUM       FROM DATE                                  
032100                                                                          
032200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
032300                                                                          
032400     MOVE NOO   TO WS-FLDAP                                               
032500                                                                          
032600     CALL VIMSID            USING W-IMSID                                 
032700     IF W-IMSID(1:3) = 'IMG'                                              
032800       MOVE 'QASE' TO W-IMSID                                             
032900     END-IF                                                               
033000     IF W-IMSID(1:3) = 'IMP'                                              
033100       MOVE 'DEVE' TO W-IMSID                                             
033200     END-IF                                                               
033300     IF W-IMSID(1:3) = 'IMY'                                              
033400       MOVE 'IGRT' TO W-IMSID                                             
033500     END-IF                                                               
033600     IF W-IMSID(1:3) = 'IMD'                                              
033700       MOVE 'XDEV' TO W-IMSID                                             
033800     END-IF                                                               
033900     IF W-IMSID(1:3) = 'IMB'                                              
034000       MOVE 'ACPT' TO W-IMSID                                             
034100     END-IF                                                               
034200     .                                                                    
034300     EJECT                                                                
034400*****************************************************************         
034500**CALLS WDK6 PROGRAM WITH THE NEW PART WHICH IS ALREADY INSERTED          
034600*FROM FILE W11811 AND IF ATTRIBUTE IS UPDATED THEN UPDATES IN WDK6        
034700*****************************************************************         
034800 B-GET-WDK6 SECTION.                                                      
034900                                                                          
035000     PERFORM S91-INIT-WORK                                                
035100                                                                          
035200     MOVE ART-IDCDS       TO WS-IDCDS                                     
035300     MOVE ART-KDARTSYS    TO WS-KDARTSYS                                  
035400     MOVE CLAG-IDPSN      TO WS-IDPSN                                     
035500     MOVE CLAG-KDARTHNT   TO WS-KDARTHNT                                  
035600     MOVE CLAG-KDEMBKOD-2 TO WS-KDEMBKOD-2                                
035700                             WS-KDEMBKOD-CHK                              
035800     MOVE CLAG-KDFARLIG   TO WS-KDFARLIG                                  
035900     MOVE CLAG-KDYTBEH    TO WS-KDYTBEH                                   
036000                                                                          
036100     IF (IN-VKART-NTO  > CLAG-VKART     AND                               
036200         CLAG-VKART    > ZERO )         OR                                
036300        (IN-VKART-NTO  = CLAG-VKART-NTO AND                               
036400         CLAG-KDUVKNTO = '3'  )         OR                                
036500        (CLAG-KDUVKNTO = '1' OR '2')                                      
036600        MOVE NOO          TO WS-FLVKART-NTO                               
036700     ELSE                                                                 
036800        MOVE YES          TO WS-FLVKART-NTO                               
036900     END-IF                                                               
037000                                                                          
037100*                                                                         
037200***  IF ATTRIBUTE CHANGED THEN UPDATE                                     
037300*                                                                         
037400     IF IN-IDPSN       = WS-IDPSN       AND                               
037500        IN-KDARTHNT    = WS-KDARTHNT    AND                               
037600        IN-KDFARLIG    = WS-KDFARLIG    AND                               
037700        IN-KDYTBEH     = WS-KDYTBEH     AND                               
037800        WS-IDCDS       = WS-IDCDS       AND                               
037900        WS-KDARTSYS    = WS-KDARTSYS    AND                               
038000        WS-FLVKART-NTO = NOO                                              
038100        MOVE NEJ          TO SW-UPD-WDK60111                              
038200     ELSE                                                                 
038300        MOVE JA           TO SW-UPD-WDK60111                              
038400     END-IF                                                               
038500*                                                                         
038600***  KDEMBKOD-2 IS OWNED BY VSIM. FOR PART UPDATE, WHEN                   
038700***  THE STORED VALUES IN WDK6 IS 0, 60 OR 70,                            
038800***  TCPLM UPD IS ALLOWED                                                 
038900*                                                                         
039000     IF IN-IDPTYP = 'SWI' OR 'SWU' OR 'SWS'                               
039100        CONTINUE                                                          
039200     ELSE                                                                 
039300        IF KDEMBKOD-CHK-VALID                                             
039400           IF IN-KDEMBKOD-2 NOT = WS-KDEMBKOD-2                           
039500              MOVE JA        TO SW-UPD-WDK60111                           
039600                                SW-UPD-EMBKOD                             
039700           END-IF                                                         
039800        END-IF                                                            
039900     END-IF                                                               
040000                                                                          
040100***  CHECK IF NOTES CHANGED                                               
040200*                                                                         
040300     IF IN-IDPTYP = 'UPD' OR 'SSU' OR 'EXU' OR 'EXS' OR                   
040400                    'SWU' OR 'SWS'                                        
040500        PERFORM BC-CHECK-WDK625                                           
040600     END-IF                                                               
040700                                                                          
040800*                                                                         
040900***  PROCESS UPDATE                                                       
041000*                                                                         
041100     IF UPD-WDK60111-JA                                                   
041200     OR UPD-WDK625-JA                                                     
041300        PERFORM BA-UPDATE-WDK6                                            
041400        IF UPD-EMBKOD-JA                                                  
041500           PERFORM BB-UPDATE-WDK613                                       
041600        END-IF                                                            
041700     END-IF                                                               
041800     .                                                                    
041900     EJECT                                                                
042000 BA-UPDATE-WDK6 SECTION.                                                  
042100******************************************************************        
042200*UPDATE WDK6 WITH NEW VALUES FOR EXISTING PART                   *        
042300******************************************************************        
042400     PERFORM IMS-GHU-WDK60111                                             
042500                                                                          
042600     IF SEGMENT-FOUND                                                     
042700        MOVE IN-IDCDS          TO ART-IDCDS                               
042800        MOVE IN-KDARTSYS       TO ART-KDARTSYS                            
042900        MOVE IN-IDPSN          TO CLAG-IDPSN                              
043000        MOVE IN-KDARTHNT       TO CLAG-KDARTHNT                           
043100        MOVE IN-KDFARLIG       TO CLAG-KDFARLIG                           
043200        MOVE IN-KDYTBEH        TO CLAG-KDYTBEH                            
043300                                                                          
043400        IF UPD-EMBKOD-JA                                                  
043500           MOVE IN-KDEMBKOD-2  TO CLAG-KDEMBKOD-2                         
043600        END-IF                                                            
043700                                                                          
043800        IF WS-FLVKART-NTO = YES                                           
043900           MOVE '3'            TO CLAG-KDUVKNTO                           
044000           MOVE 'TCPLM'        TO CLAG-IDUSER-VUPD                        
044100           MOVE IN-VKART-NTO   TO CLAG-VKART-NTO                          
044200           IF CLAG-VKART  = ZERO                                          
044300              MOVE IN-VKART-NTO                                           
044400                               TO CLAG-VKART                              
044500           END-IF                                                         
044600           MOVE DAGENS-DATUM   TO CLAG-TIUPPDAT-VUPD                      
044700        END-IF                                                            
044800                                                                          
044900        PERFORM IMS-REPL-WDK60111                                         
045000     END-IF                                                               
045100*                                                                         
045200***  UPDATE PLANNER NOTE AND PROCURER NOTE                                
045300*                                                                         
045400     IF UPD-WDK625-JA                                                     
045500        IF WS-WDK625-NOT-1 = JA                                           
045600           MOVE +1                     TO W-KDNOTTYP                      
045700           PERFORM IMS-GHU-WDK625                                         
045800           IF SEGMENT-FOUND                                               
045900              IF WS-TEARTNOT-1 NOT > SPACES                               
046000                 PERFORM IMS-DELETE-WDK625                                
046100              ELSE                                                        
046200                 MOVE WS-TEARTNOT-1    TO NOT-TEARTNOT                    
046300                 PERFORM IMS-REPL-WDK625                                  
046400              END-IF                                                      
046500           ELSE                                                           
046600              IF WS-TEARTNOT-1     > SPACES                               
046700                 MOVE +1               TO NOT-KDNOTTYP                    
046800                 MOVE WS-TEARTNOT-1    TO NOT-TEARTNOT                    
046900                 PERFORM IMS-ISRT-WDK625                                  
047000              END-IF                                                      
047100           END-IF                                                         
047200        END-IF                                                            
047300*                                                                         
047400        IF WS-WDK625-NOT-3 = JA                                           
047500           MOVE +3                     TO W-KDNOTTYP                      
047600           PERFORM IMS-GHU-WDK625                                         
047700           IF SEGMENT-FOUND                                               
047800              IF WS-TEARTNOT-3 NOT > SPACES                               
047900                 PERFORM IMS-DELETE-WDK625                                
048000              ELSE                                                        
048100                 MOVE WS-TEARTNOT-3    TO NOT-TEARTNOT                    
048200                 PERFORM IMS-REPL-WDK625                                  
048300              END-IF                                                      
048400           ELSE                                                           
048500              IF WS-TEARTNOT-3     > SPACES                               
048600                 MOVE +3               TO NOT-KDNOTTYP                    
048700                 MOVE WS-TEARTNOT-3    TO NOT-TEARTNOT                    
048800                 PERFORM IMS-ISRT-WDK625                                  
048900              END-IF                                                      
049000           END-IF                                                         
049100        END-IF                                                            
049200*                                                                         
049300        IF WS-WDK625-NOT-7 = JA                                           
049400           MOVE +7                     TO W-KDNOTTYP                      
049500           PERFORM IMS-GHU-WDK625                                         
049600           IF SEGMENT-FOUND                                               
049700              IF WS-TEARTNOT-7 NOT > SPACES                               
049800                 PERFORM IMS-DELETE-WDK625                                
049900              ELSE                                                        
050000                 MOVE WS-TEARTNOT-7    TO NOT-TEARTNOT                    
050100                 PERFORM IMS-REPL-WDK625                                  
050200              END-IF                                                      
050300           ELSE                                                           
050400              IF WS-TEARTNOT-7 > SPACES                                   
050500                 MOVE +7               TO NOT-KDNOTTYP                    
050600                 MOVE WS-TEARTNOT-7    TO NOT-TEARTNOT                    
050700                 PERFORM IMS-ISRT-WDK625                                  
050800              END-IF                                                      
050900           END-IF                                                         
051000        END-IF                                                            
051100     END-IF                                                               
051200     .                                                                    
051300     EJECT                                                                
051400 BB-UPDATE-WDK613 SECTION.                                                
051500******************************************************************        
051600*UPDATE WDK613 WITH NEW VALUES FOR EXISTING PART                 *        
051700******************************************************************        
051800     MOVE 'Q2 '               TO W-KDEMBAL                                
051900     PERFORM IMS-GHU-WDK613-EMB                                           
052000     IF SEGMENT-FOUND                                                     
052100        MOVE IN-KDEMBKOD-2    TO EMB-KDEMBKOD                             
052200        PERFORM IMS-REPL-WDK613-EMB                                       
052300     ELSE                                                                 
052400        MOVE 'Q2 '            TO EMB-KDEMBKEY                             
052500        MOVE ZERO             TO EMB-IDARTNR-EMB                          
052600                                 EMB-KVQPACK-EMB                          
052700        MOVE IN-KDEMBKOD-2    TO EMB-KDEMBKOD                             
052800        PERFORM IMS-ISRT-WDK613-EMB                                       
052900     END-IF                                                               
053000     .                                                                    
053100     EJECT                                                                
053200 BC-CHECK-WDK625 SECTION.                                                 
053300******************************************************************        
053400*CHECK WDK625 FOR UPDATE TO NOTES                                *        
053500******************************************************************        
053600     MOVE +1                    TO W-KDNOTTYP                             
053700     PERFORM IMS-GU-WDK625                                                
053800     IF SEGMENT-FOUND                                                     
053900        IF NOT-TEARTNOT  = IN-TEARTNOT-4                                  
054000           CONTINUE                                                       
054100        ELSE                                                              
054200           MOVE IN-TEARTNOT-4   TO WS-TEARTNOT-1                          
054300           MOVE JA              TO SW-UPD-WDK625                          
054400                                   WS-WDK625-NOT-1                        
054500        END-IF                                                            
054600     ELSE                                                                 
054700        IF IN-TEARTNOT-4 > SPACES                                         
054800           MOVE IN-TEARTNOT-4   TO WS-TEARTNOT-1                          
054900           MOVE JA              TO SW-UPD-WDK625                          
055000                                   WS-WDK625-NOT-1                        
055100        END-IF                                                            
055200     END-IF                                                               
055300*                                                                         
055400     MOVE +3                    TO W-KDNOTTYP                             
055500     PERFORM IMS-GU-WDK625                                                
055600     IF SEGMENT-FOUND                                                     
055700        IF NOT-TEARTNOT  = IN-TEARTNOT-2                                  
055800           CONTINUE                                                       
055900        ELSE                                                              
056000           MOVE IN-TEARTNOT-2   TO WS-TEARTNOT-3                          
056100           MOVE JA              TO SW-UPD-WDK625                          
056200                                   WS-WDK625-NOT-3                        
056300        END-IF                                                            
056400     ELSE                                                                 
056500        IF IN-TEARTNOT-2 > SPACES                                         
056600           MOVE IN-TEARTNOT-2   TO WS-TEARTNOT-3                          
056700           MOVE JA              TO SW-UPD-WDK625                          
056800                                   WS-WDK625-NOT-3                        
056900        END-IF                                                            
057000     END-IF                                                               
057100*                                                                         
057200     MOVE +7                    TO W-KDNOTTYP                             
057300     PERFORM IMS-GU-WDK625                                                
057400     IF SEGMENT-FOUND                                                     
057500        IF NOT-TEARTNOT  = IN-TEARTNOT-7                                  
057600           CONTINUE                                                       
057700        ELSE                                                              
057800           MOVE IN-TEARTNOT-7   TO WS-TEARTNOT-7                          
057900           MOVE JA              TO SW-UPD-WDK625                          
058000                                   WS-WDK625-NOT-7                        
058100        END-IF                                                            
058200     ELSE                                                                 
058300        IF IN-TEARTNOT-7 > SPACES                                         
058400           MOVE IN-TEARTNOT-7   TO WS-TEARTNOT-7                          
058500           MOVE JA              TO SW-UPD-WDK625                          
058600                                   WS-WDK625-NOT-7                        
058700        END-IF                                                            
058800     END-IF                                                               
058900     .                                                                    
059000     EJECT                                                                
059100******************************************************************        
059200*UPDATE/INSERTS WDD5 WITH VALUE FROM PRINS/TCPLM                 *        
059300******************************************************************        
059400 C-UPDATE-WDD5 SECTION.                                                   
059500     PERFORM IMS-GET-WDD501                                               
059600     IF SEGMENT-FOUND                                                     
059700        IF  IN-VLFG         = ARTN01-ART-VLFG                             
059800        AND IN-KDSORT-VLFG  = ARTN01-ART-KDSORT-VLFG                      
059900            CONTINUE                                                      
060000        ELSE                                                              
060100            MOVE IN-VLFG        TO ARTN01-ART-VLFG                        
060200            MOVE IN-KDSORT-VLFG TO ARTN01-ART-KDSORT-VLFG                 
060300            PERFORM IMS-REPL-WDD501                                       
060400        END-IF                                                            
060500     ELSE                                                                 
060600         MOVE IN-IDARTNR     TO ARTN01-ART-IDARTNR                        
060700         MOVE SPACE          TO ARTN01-ART-BEEMBMAT                       
060800         MOVE NOO            TO ARTN01-ART-FLFROST                        
060900         MOVE NOO            TO ARTN01-ART-FLTACTIL                       
061000         MOVE SPACE          TO ARTN01-ART-FLVARINF                       
061100         MOVE SPACE          TO ARTN01-ART-FLVARINF-SDS                   
061200         MOVE SPACE          TO ARTN01-ART-IDAO-FG                        
061300         MOVE SPACE          TO ARTN01-ART-IDVARINF                       
061400         MOVE SPACE          TO ARTN01-ART-IDVARINF-SDS                   
061500         MOVE ZERO           TO ARTN01-ART-KVFLAMP                        
061600         MOVE ZERO           TO ARTN01-ART-KVNTOFG                        
061700         MOVE ZERO           TO ARTN01-ART-KVVOC                          
061800         MOVE ZERO           TO ARTN01-ART-SUEQFG                         
061900         MOVE ZERO           TO ARTN01-ART-VKART-FG                       
062000         MOVE ZERO           TO ARTN01-ART-VKFORSFG                       
062100         MOVE SPACE          TO ARTN01-ART-TENOTE(1)                      
062200         MOVE SPACE          TO ARTN01-ART-TENOTE(2)                      
062300         MOVE SPACE          TO ARTN01-ART-KDLACK                         
062400         MOVE SPACE          TO ARTN01-ART-KDSORT-KVNTOFG                 
062500         MOVE DAGENS-DATUM   TO ARTN01-ART-TIREGDAT                       
062600         MOVE ZERO           TO ARTN01-ART-KDFARG                         
062700         MOVE ZERO           TO ARTN01-ART-KDFGPRIO                       
062800         MOVE ZERO           TO ARTN01-ART-IDARTNR-RECEPT                 
062900         MOVE ZERO           TO ARTN01-ART-REKSIFFR-ANMNR                 
063000         MOVE ZERO           TO ARTN01-ART-VLFG                           
063100         MOVE ZERO           TO ARTN01-ART-IDANMNR                        
063200         MOVE IN-KDSORT-VLFG TO ARTN01-ART-KDSORT-VLFG                    
063300         MOVE IN-VLFG        TO ARTN01-ART-VLFG                           
063400         PERFORM IMS-ISRT-WDD501                                          
063500     END-IF                                                               
063600     .                                                                    
063700     EJECT                                                                
063800*TO UPDATE NET WEIGHT INFO                                                
063900 D-UPDATE-WDK2 SECTION.                                                   
064000                                                                          
064100     PERFORM IMS-GET-WDK201                                               
064200                                                                          
064300     IF SEGMENT-MISSING                                                   
064400       MOVE IN-IDARTNR       TO ARTM-IDARTNR                              
064500       PERFORM IMS-ISRT-WDK201                                            
064600                                                                          
064700       MOVE '1'              TO KDP-KDSEGKEY                              
064800       MOVE  1               TO KDP-KVANTAL                               
064900       MOVE DAGENS-DATUM     TO KDP-TIUPPDAT                              
065000       MOVE IN-VKART-NTO     TO KDP-VKART-NTO                             
065100       PERFORM IMS-ISRT-WDK212                                            
065200     ELSE                                                                 
065300       PERFORM IMS-GHNP-WDK212                                            
065400       IF SEGMENT-MISSING                                                 
065500         MOVE '1'            TO KDP-KDSEGKEY                              
065600         MOVE  1             TO KDP-KVANTAL                               
065700         MOVE DAGENS-DATUM   TO KDP-TIUPPDAT                              
065800         MOVE IN-VKART-NTO   TO KDP-VKART-NTO                             
065900         PERFORM IMS-ISRT-WDK212                                          
066000       ELSE                                                               
066100         IF IN-VKART-NTO = KDP-VKART-NTO                                  
066200            CONTINUE                                                      
066300         ELSE                                                             
066400           MOVE DAGENS-DATUM TO KDP-TIUPPDAT                              
066500           MOVE IN-VKART-NTO TO KDP-VKART-NTO                             
066600           PERFORM IMS-REPL-WDK212                                        
066700         END-IF                                                           
066800       END-IF                                                             
066900     END-IF                                                               
067000     .                                                                    
067100     EJECT                                                                
067200                                                                          
067300 E-FEL-TRANS-MAIL SECTION.                                                
067400                                                                          
067500     PERFORM S81-OPEN-DP                                                  
067600                                                                          
067700     MOVE 'TCPLM-ERR'      TO WS-IDOUTREC                                 
067800     PERFORM S82-PUT-HDR                                                  
067900                                                                          
068000     MOVE SPACE            TO SEND-RAD                                    
068100     MOVE WS-SKIP1         TO STYRTECKEN-RAD                              
068200     MOVE 'CHECK W118J015 DISPLAY MESSAGES IN SPOOL'                      
068300                           TO MAIL-RAD                                    
068400     PERFORM S83-PUT-LINE                                                 
068500                                                                          
068600     MOVE SPACE            TO SEND-RAD                                    
068700     MOVE WS-SKIP1 TO STYRTECKEN-RAD                                      
068800     STRING 'ENVIRONMENT : ' W-IMSID                                      
068900     DELIMITED BY SIZE INTO MAIL-RAD                                      
069000     PERFORM S83-PUT-LINE                                                 
069100                                                                          
069200     PERFORM S84-CLOSE-DP                                                 
069300     .                                                                    
069400     EJECT                                                                
069500 Z-FINIT SECTION.                                                         
069600                                                                          
069700     PERFORM X-TAKE-CHECKPOINT                                            
069800     SKIP2                                                                
069900     MOVE 'S' TO POSTSUM-OPKOD                                            
070000     CALL POSTSUM USING POSTSUM-PARM                                      
070100     .                                                                    
070200     EJECT                                                                
070300 S81-OPEN-DP SECTION.                                                     
070400                                                                          
070500     MOVE 'OPEN'                  TO SEND-KDFUNC                          
070600     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
070700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
070800                         SEND-OPEN-AREA                                   
070900     IF SEND-KDRC > 0                                                     
071000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
071100       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
071200       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
071300       CALL FELLOG USING RKOD-ABEND-MED-DUMP                              
071400     END-IF                                                               
071500                                                                          
071600     MOVE SEND-IDCOM              TO WS-SAVE-IDCOM-ERROR-MAIL             
071700     .                                                                    
071800     EJECT                                                                
071900 S82-PUT-HDR SECTION.                                                     
072000                                                                          
072100     MOVE 1                       TO REQU-IDMSGVER                        
072200     MOVE ' '                     TO REQU-KDPGMACT                        
072300     MOVE IDPGM                   TO REQU-IDUSER                          
072400     MOVE 'W11815-001    '        TO HDR-IDOUTTYPE                        
072500     MOVE WS-IDOUTREC             TO HDR-IDOUTREC                         
072600     MOVE SPACE                   TO HDR-IDLIST                           
072700     MOVE 'PUT'                   TO SEND-KDFUNC                          
072800     MOVE WS-SAVE-IDCOM-ERROR-MAIL                                        
072900                                  TO SEND-IDCOM                           
073000     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
073100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
073200                         SEND-KVDLEN                                      
073300                         HDR-AREA                                         
073400     IF SEND-KDRC > ZERO                                                  
073500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
073600       STRING 'WZ01SEND PUT-HDR ERROR RC=' KDRC-DISPLAY                   
073700       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
073800       CALL FELLOG USING RKOD-ABEND-MED-DUMP                              
073900     END-IF                                                               
074000     .                                                                    
074100     EJECT                                                                
074200 S83-PUT-LINE SECTION.                                                    
074300                                                                          
074400     MOVE 'PUT'                           TO SEND-KDFUNC                  
074500     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
074600     MOVE WS-SAVE-IDCOM-ERROR-MAIL        TO SEND-IDCOM                   
074700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
074800                         SEND-KVDLEN                                      
074900                         SEND-RAD                                         
075000     IF SEND-KDRC > ZERO                                                  
075100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
075200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
075300       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
075400       CALL FELLOG USING RKOD-ABEND-MED-DUMP                              
075500     END-IF                                                               
075600     .                                                                    
075700     EJECT                                                                
075800 S91-INIT-WORK SECTION.                                                   
075900                                                                          
076000     INITIALIZE           WS-FLVKART-NTO                                  
076100                          WS-FLDAP                                        
076200                          WS-TEARTNOT-1                                   
076300                          WS-TEARTNOT-3                                   
076400                          WS-TEARTNOT-7                                   
076500                          WS-IDCDS                                        
076600                          WS-KDARTSYS                                     
076700*                                                                         
076800     MOVE ALL ZEROES   TO WS-IDPSN                                        
076900                          WS-KDARTHNT                                     
077000                          WS-KDEMBKOD-2                                   
077100                          WS-KVART                                        
077200                          WS-KDFARLIG                                     
077300                          WS-KDYTBEH                                      
077400                          WS-KDEMBKOD-CHK                                 
077500*                                                                         
077600     MOVE NEJ          TO SW-UPD-WDK60111                                 
077700                          SW-UPD-WDK625                                   
077800                          WS-WDK625-NOT-1                                 
077900                          WS-WDK625-NOT-3                                 
078000                          WS-WDK625-NOT-7                                 
078100                          SW-UPD-EMBKOD                                   
078200     .                                                                    
078300     EJECT                                                                
078400 S84-CLOSE-DP SECTION.                                                    
078500                                                                          
078600     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
078700     MOVE WS-SAVE-IDCOM-ERROR-MAIL   TO SEND-IDCOM                        
078800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
078900                                                                          
079000     IF SEND-KDRC > 0                                                     
079100       MOVE SEND-KDRC                TO KDRC-DISPLAY                      
079200       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
079300       DELIMITED BY SIZE INTO ERROR-TEXT-STR                              
079400       CALL FELLOG USING RKOD-ABEND-MED-DUMP                              
079500     END-IF                                                               
079600     .                                                                    
079700     EJECT                                                                
079800 X-TAKE-CHECKPOINT   SECTION.                                             
079900                                                                          
080000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
080100     MOVE '  XD' TO GOOD-STATUSCODES                                      
080200     CALL CBLTDLI USING CHKP MSG-PCB                                      
080300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
080400                        CHKP-AREA-LENGTH CHKP-AREA                        
080500                        GSAM-PCB-LENGTH IN-GSAM-PCB                       
080600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
080700     PERFORM IMS-STATUSCHECK                                              
080800                                                                          
080900     IF IMS-NOT-OK                                                        
081000       MOVE 'IMS CONTROL REGION NOT ACCESSIBLE' TO ERROR-TEXT-STR         
081100       DISPLAY ERROR-TEXT                                                 
081200       CALL FELLOG                                                        
081300     ELSE                                                                 
081400       MOVE +0            TO CHKP-ANT                                     
081500     END-IF                                                               
081600     .                                                                    
081700     EJECT                                                                
081800* --- IMS SECTIONS  ---                                                   
081900                                                                          
082000     EJECT                                                                
082100 IMS-GU-INDATA SECTION.                                                   
082200* EXECUTES DURING A RESTART TO READ THE FIRST RECORD AFTER THE            
082300* LAST CHECKPOINT                                                         
082400                                                                          
082500     MOVE 'IMS-GU-INDATA'        TO SSA1                                  
082600     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
082700     CALL CBLTDLI             USING GU                                    
082800                                    IN-GSAM-PCB                           
082900                                    IN-REC                                
083000                                    KEYFB-RSA                             
083100     MOVE IN-GSAM-STATUS-CODE    TO STATUS-WS                             
083200     IF SEGMENT-FOUND                                                     
083300       MOVE IN-DATA              TO IN-AREA                               
083400       MOVE NOO      TO W11811-EMPTY-SW                                   
083500       MOVE 'W11811' TO POSTSUM-FDNAMN                                    
083600       MOVE 'W11815D1' TO POSTSUM-DDNAMN2                                 
083700       MOVE 'IN'      TO POSTSUM-TRANSTYP                                 
083800       CALL POSTSUM USING POSTSUM-PARM                                    
083900     ELSE                                                                 
084000       SET END-OF-INDATA         TO TRUE                                  
084100       MOVE SPACES               TO IN-AREA                               
084200     END-IF                                                               
084300     PERFORM IMS-STATUSCHECK                                              
084400     .                                                                    
084500                                                                          
084600 IMS-GN-INDATA SECTION.                                                   
084700     MOVE 'IMS-GN-INDATA'        TO SSA1                                  
084800     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
084900     CALL CBLTDLI             USING GN                                    
085000                                    IN-GSAM-PCB                           
085100                                    IN-REC                                
085200     MOVE IN-GSAM-STATUS-CODE    TO STATUS-WS                             
085300     IF SEGMENT-FOUND                                                     
085400       MOVE IN-DATA              TO IN-AREA                               
085500       MOVE NOO      TO W11811-EMPTY-SW                                   
085600       MOVE 'W11811' TO POSTSUM-FDNAMN                                    
085700       MOVE 'W11815D1' TO POSTSUM-DDNAMN2                                 
085800       MOVE 'IN'      TO POSTSUM-TRANSTYP                                 
085900       CALL POSTSUM USING POSTSUM-PARM                                    
086000     ELSE                                                                 
086100       SET END-OF-INDATA         TO TRUE                                  
086200       MOVE SPACES               TO IN-AREA                               
086300     END-IF                                                               
086400     PERFORM IMS-STATUSCHECK                                              
086500      .                                                                   
086600                                                                          
086700 IMS-GU-WDK60111 SECTION.                                                 
086800                                                                          
086900     STRING 'WDK601  *D(IDARTNR  =' W-IDARTNR-X ')'                       
087000            DELIMITED BY SIZE INTO SSA1                                   
087100     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
087200          DELIMITED BY SIZE INTO SSA2                                     
087300     MOVE   '  GE'               TO GOOD-STATUSCODES                      
087400     CALL CBLTDLI USING GU WDK6A-PCB DLI-IO-WDK60111 SSA1 SSA2            
087500     MOVE WDK6A-STATUS-CODE      TO STATUS-WS                             
087600     PERFORM IMS-STATUSCHECK                                              
087700     .                                                                    
087800 IMS-GHU-WDK60111 SECTION.                                                
087900                                                                          
088000     STRING 'WDK601  *D(IDARTNR  =' W-IDARTNR-X ')'                       
088100            DELIMITED BY SIZE INTO SSA1                                   
088200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
088300          DELIMITED BY SIZE INTO SSA2                                     
088400     MOVE '  '                  TO GOOD-STATUSCODES                       
088500     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK60111 SSA1 SSA2            
088600     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
088700     PERFORM IMS-STATUSCHECK                                              
088800     .                                                                    
088900                                                                          
089000 IMS-REPL-WDK60111 SECTION.                                               
089100                                                                          
089200     MOVE '  '                  TO GOOD-STATUSCODES                       
089300                                                                          
089400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK60111                     
089500     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
089600     PERFORM IMS-STATUSCHECK                                              
089700     .                                                                    
089800     EJECT                                                                
089900 IMS-GHU-WDK613-EMB  SECTION.                                             
090000                                                                          
090100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
090200            DELIMITED BY SIZE INTO SSA1                                   
090300     STRING 'WDK613  (KDEMBAL  =' W-KDEMBAL-X ')'                         
090400            DELIMITED BY SIZE INTO SSA2                                   
090500     MOVE   '  GE'              TO GOOD-STATUSCODES                       
090600     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK613 SSA1 SSA2              
090700     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
090800     PERFORM IMS-STATUSCHECK                                              
090900     .                                                                    
091000     SKIP3                                                                
091100 IMS-REPL-WDK613-EMB SECTION.                                             
091200                                                                          
091300     MOVE '  '                  TO GOOD-STATUSCODES                       
091400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK613                       
091500     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
091600     PERFORM IMS-STATUSCHECK                                              
091700     .                                                                    
091800     SKIP3                                                                
091900 IMS-ISRT-WDK613-EMB SECTION.                                             
092000                                                                          
092100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
092200            DELIMITED BY SIZE INTO SSA1                                   
092300     MOVE   'WDK613   '         TO SSA2                                   
092400     MOVE   '  '                TO GOOD-STATUSCODES                       
092500     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK613 SSA1 SSA2             
092600     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
092700     PERFORM IMS-STATUSCHECK                                              
092800     .                                                                    
092900     SKIP3                                                                
093000 IMS-GU-WDK625  SECTION.                                                  
093100                                                                          
093200     INITIALIZE  DLI-IO-WDK625                                            
093300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
093400            DELIMITED BY SIZE       INTO SSA1                             
093500     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
093600          DELIMITED BY SIZE         INTO SSA2                             
093700     STRING 'WDK625  (KDNOTTYP =' W-KDNOTTYP-X ')'                        
093800          DELIMITED BY SIZE         INTO SSA3                             
093900     MOVE '  GE'                      TO GOOD-STATUSCODES                 
094000     CALL CBLTDLI USING GU WDK6A-PCB DLI-IO-WDK625 SSA1 SSA2 SSA3         
094100     MOVE WDK6A-STATUS-CODE           TO STATUS-WS                        
094200     PERFORM IMS-STATUSCHECK                                              
094300     .                                                                    
094400     SKIP3                                                                
094500 IMS-GHU-WDK625 SECTION.                                                  
094600                                                                          
094700     INITIALIZE  DLI-IO-WDK625                                            
094800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
094900            DELIMITED BY SIZE       INTO SSA1                             
095000     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
095100          DELIMITED BY SIZE         INTO SSA2                             
095200     STRING 'WDK625  (KDNOTTYP =' W-KDNOTTYP-X ')'                        
095300            DELIMITED BY SIZE       INTO SSA3                             
095400     MOVE '  GE'                      TO GOOD-STATUSCODES                 
095500     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK625 SSA1 SSA2 SSA3         
095600     MOVE WDK6-STATUS-CODE            TO STATUS-WS                        
095700     PERFORM IMS-STATUSCHECK                                              
095800     .                                                                    
095900     SKIP3                                                                
096000 IMS-ISRT-WDK625 SECTION.                                                 
096100                                                                          
096200     MOVE 'WDK625   '                 TO SSA1                             
096300     MOVE '  '                        TO GOOD-STATUSCODES                 
096400     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK625 SSA1                  
096500     MOVE WDK6-STATUS-CODE            TO STATUS-WS                        
096600     PERFORM IMS-STATUSCHECK                                              
096700     .                                                                    
096800     SKIP3                                                                
096900 IMS-REPL-WDK625 SECTION.                                                 
097000                                                                          
097100     MOVE '  '                        TO GOOD-STATUSCODES                 
097200     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK625                       
097300     MOVE WDK6-STATUS-CODE            TO STATUS-WS                        
097400     PERFORM IMS-STATUSCHECK                                              
097500     .                                                                    
097600     SKIP3                                                                
097700 IMS-DELETE-WDK625 SECTION.                                               
097800                                                                          
097900     MOVE '  '                        TO GOOD-STATUSCODES                 
098000     CALL CBLTDLI USING DLET WDK6-PCB DLI-IO-WDK625                       
098100     MOVE WDK6-STATUS-CODE            TO STATUS-WS                        
098200     PERFORM IMS-STATUSCHECK                                              
098300     .                                                                    
098400     EJECT                                                                
098500 IMS-GET-WDD501 SECTION.                                                  
098600     STRING 'WDD501  (IDARTNR  =' W-WDD5-X ')'                            
098700          DELIMITED BY SIZE INTO SSA1                                     
098800     MOVE '  GE' TO GOOD-STATUSCODES                                      
098900     CALL CBLTDLI USING GHU WDD5-PCB DLI-IO-WDD501 SSA1                   
099000     MOVE WDD5-STATUS-CODE TO STATUS-WS                                   
099100     PERFORM IMS-STATUSCHECK                                              
099200     .                                                                    
099300     SKIP3                                                                
099400 IMS-ISRT-WDD501 SECTION.                                                 
099500     MOVE 'WDD501 ' TO SSA1                                               
099600     MOVE '  ' TO GOOD-STATUSCODES                                        
099700     CALL CBLTDLI USING ISRT WDD5-PCB DLI-IO-WDD501 SSA1                  
099800     MOVE WDD5-STATUS-CODE TO STATUS-WS                                   
099900     PERFORM IMS-STATUSCHECK                                              
100000     .                                                                    
100100     SKIP3                                                                
100200 IMS-REPL-WDD501 SECTION.                                                 
100300     MOVE '  ' TO GOOD-STATUSCODES                                        
100400     CALL CBLTDLI USING REPL WDD5-PCB DLI-IO-WDD501                       
100500     MOVE WDD5-STATUS-CODE TO STATUS-WS                                   
100600     PERFORM IMS-STATUSCHECK                                              
100700     .                                                                    
100800     SKIP3                                                                
100900 IMS-GET-WDK201 SECTION.                                                  
101000     STRING 'WDK201  (IDARTNR  =' W-WDK2-X ')'                            
101100          DELIMITED BY SIZE INTO SSA1                                     
101200     MOVE '  GE'           TO GOOD-STATUSCODES                            
101300     CALL CBLTDLI USING GU WDK2-PCB DLI-IO-WDK201 SSA1                    
101400     MOVE WDK2-STATUS-CODE TO STATUS-WS                                   
101500     PERFORM IMS-STATUSCHECK                                              
101600     .                                                                    
101700                                                                          
101800 IMS-ISRT-WDK201 SECTION.                                                 
101900     MOVE 'WDK201  '       TO SSA1                                        
102000     MOVE '  '             TO GOOD-STATUSCODES                            
102100     CALL CBLTDLI USING ISRT WDK2-PCB DLI-IO-WDK201 SSA1                  
102200     MOVE WDK2-STATUS-CODE TO STATUS-WS                                   
102300     PERFORM IMS-STATUSCHECK                                              
102400     .                                                                    
102500                                                                          
102600 IMS-GHNP-WDK212 SECTION.                                                 
102700     MOVE 'WDK212  '       TO SSA1                                        
102800     MOVE '  GE'           TO GOOD-STATUSCODES                            
102900     CALL CBLTDLI USING GHNP WDK2-PCB DLI-IO-WDK212 SSA1                  
103000     MOVE WDK2-STATUS-CODE TO STATUS-WS                                   
103100     PERFORM IMS-STATUSCHECK                                              
103200     .                                                                    
103300                                                                          
103400 IMS-ISRT-WDK212 SECTION.                                                 
103500     STRING 'WDK201  (IDARTNR  =' W-WDK2-X ')'                            
103600       DELIMITED BY SIZE INTO SSA1                                        
103700     MOVE 'WDK212  '       TO SSA2                                        
103800     MOVE '  '             TO GOOD-STATUSCODES                            
103900     CALL CBLTDLI USING ISRT WDK2-PCB DLI-IO-WDK212 SSA1 SSA2             
104000     MOVE WDK2-STATUS-CODE TO STATUS-WS                                   
104100     PERFORM IMS-STATUSCHECK                                              
104200     .                                                                    
104300                                                                          
104400 IMS-REPL-WDK212 SECTION.                                                 
104500     MOVE '  '             TO GOOD-STATUSCODES                            
104600     CALL CBLTDLI USING REPL WDK2-PCB DLI-IO-WDK212                       
104700     MOVE WDK2-STATUS-CODE TO STATUS-WS                                   
104800     PERFORM IMS-STATUSCHECK                                              
104900     .                                                                    
105000     EJECT                                                                
105100 IMS-RESTART SECTION.                                                     
105200     SKIP2                                                                
105300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
105400     MOVE '  ' TO GOOD-STATUSCODES                                        
105500     CALL CBLTDLI USING XRST MSG-PCB                                      
105600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
105700                        CHKP-AREA-LENGTH CHKP-AREA                        
105800                        GSAM-PCB-LENGTH IN-GSAM-PCB                       
105900                                                                          
106000     IF CHKP-MSG-IO-AREA = SPACES                                         
106100       SET NORMAL-RUN            TO TRUE                                  
106200     ELSE                                                                 
106300       SET RESTART-RUN           TO TRUE                                  
106400     END-IF                                                               
106500                                                                          
106600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
106700     PERFORM IMS-STATUSCHECK                                              
106800     .                                                                    
106900     SKIP3                                                                
107000 IMS-STATUSCHECK SECTION.                                                 
107100     SKIP2                                                                
107200     SET STATUS-IX TO 1                                                   
107300     SEARCH GOOD-STATUS                                                   
107400       AT END                                                             
107500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
107600           DELIMITED BY SIZE INTO ERROR-TEXT                              
107700         DISPLAY ERROR-TEXT                                               
107800         CALL FELLOG                                                      
107900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
108000         CONTINUE                                                         
108100     END-SEARCH                                                           
108200     .                                                                    
