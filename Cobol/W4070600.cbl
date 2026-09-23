001300 ID DIVISION.                                                             
001400                                                                          
001500 PROGRAM-ID.     W4070600.                                                
001600 AUTHOR.         LARS CALAIS.                                             
001700 DATE-WRITTEN.   95/11/28.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNCTION:                                                            
002100*        READS AND UPDATES ACTIONFILE WL4107                              
002200*                                                                         
002310*        THE PROGRAM UPDATES   WL4107                                     
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSACTION: W4T706                                              
002700*        MID:         W4I70601                                            
002800*                                                                         
002900*    OUTDATA.                                                             
003000*        MOD:         W4O70601                                            
003100                                                                          
003200                                                                          
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003601*    -- CHECKED BY WY2000                                                 
003610     SKIP3                                                                
003700 77  IDPGM                       PIC X(08)   VALUE 'W4070600'.            
003800                                                                          
003900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004000 77  ERROR-TEXT                             PIC X(80) VALUE SPACE.        
004100                                                                          
004200 77  YES                         PIC X       VALUE 'Y'.                   
004300 77  NOO                         PIC X       VALUE 'N'.                   
004400                                                                          
004600*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004800                                                                          
004901 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
004902     88  INDATA-OK                           VALUE 'Y'.                   
004910     88  INDATA-WRONG                        VALUE 'N'.                   
005000                                                                          
005100 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005200     88  KEYS-OK                             VALUE 'Y'.                   
005300     88  KEYS-WRONG                          VALUE 'N'.                   
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  OWN-MID                             VALUE '4706'.                
005700     88  GOOD-MID                            VALUE '4701' '4702'          
005800                                                   '4703' '4704'          
005900                                                   '4705' '4706'          
006000                                                   '4707'.                
006200     88  HELP-MID                            VALUE '0551'.                
006300     EJECT                                                                
006400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERAL-SUBPROGRAM.                                                  
006600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     EJECT                                                                
007200*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007300*01 -COPY WMEDAREA                                                        
007400                                                                          
007500 01  MESSAGE-CODES.                                                       
007601     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007602     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007603     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007610     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008000     EJECT                                                                
008700*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000                                                                          
009100*01  MID -COPY W4I70601                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400                                                                          
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W4O70601                                                 
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100                                                                          
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800                                                                          
011200 01  KEYS-TO-DLI.                                                         
011301     03  W-WDGXKEY-X.                                                     
011302         05  W-IDHTYP            PIC X(4)    VALUE '4107'.                
011303         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
011304     03  W-KDSEGKEY-X.                                                    
011310         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
011400                                                                          
011500*    --- STATUS-KOD FRÅN IMS                                              
011600 01  STATUS-WS                   PIC XX.                                  
011700     88  SEGMENT-FOUND                       VALUE '  '.                  
011800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
011900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012000                                                                          
012100 01  GOOD-STATUSCODES.                                                    
012200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012300                                                                          
012400 01  SSA1                        PIC X(64).                               
012500 01  SSA2                        PIC X(64).                               
012600     EJECT                                                                
012700*    --- IMS FUNCTION CODES                                               
012800*01  -COPY W0003                                                          
013000     EJECT                                                                
013100*    ---  DLI INPUT-OUTPUT AREA                                           
013200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013300                                                                          
013400 01  DLI-IO-AREA.                                                         
013500     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
013601                                                                          
013605     03  WL410711 REDEFINES IO-AREA.                                      
013610*        05  -COPY WDGX4108                                               
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100                                                                          
014200*01  -COPY W0009   -PRE MSG-                                              
014300*01  -COPY W0008   -PRE USEA-                                             
014400     05  FILLER                  PIC X.                                   
014501     EJECT                                                                
014502*01  -COPY W0008  -PRE 4107-                                              
014510     05  FILLER                  PIC X.                                   
014600     EJECT                                                                
014701 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB 4107-PCB.                     
014702 MAIN SECTION.                                                            
014710     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB 4107-PCB.                     
014800                                                                          
015000     PERFORM IMS-GET-MSG                                                  
015100     IF SEGMENT-FOUND                                                     
015200       PERFORM A-INIT                                                     
015501       IF MFS-UPDATE                                                      
015502         PERFORM G-CHECK-INPUT                                            
015503         IF INDATA-OK                                                     
015504           PERFORM H-UPDATE                                               
015505         END-IF                                                           
015506       ELSE                                                               
015507         PERFORM MFS-ERASE-FIELD-IN                                       
015510       END-IF                                                             
015900       PERFORM F-READ-SHOW-INFO                                           
016200       PERFORM IMS-INSERT-MSG                                             
016300     END-IF                                                               
016500                                                                          
016600     MOVE ZERO TO RETURN-CODE                                             
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
017000 A-INIT SECTION.                                                          
017100                                                                          
017200     IF MSG-DOUBLE-TRANSACTIONS                                           
017300       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I70601                 
017400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017600     ELSE                                                                 
017700       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I70601                  
017800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018000     END-IF                                                               
018100                                                                          
018200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018500                                                                          
018600     MOVE LOW-VALUE TO MSG-AREA                                           
018700     MOVE 'W4O70601' TO MFS-IDMOD                                         
018800     MOVE '4706' TO MOD-IDTRANS                                           
018900     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
019000                                                                          
019300     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O70601 + 4                        
019400                                                                          
019500     IF OWN-MID OR HELP-MID                                               
019600       CONTINUE                                                           
019700     ELSE                                                                 
019800       MOVE SPACE TO MFS-KDTRTYP                                          
019900       MOVE '7' TO MFS-IDPFK                                              
020000     END-IF                                                               
020300     .                                                                    
020400     EJECT                                                                
023100 F-READ-SHOW-INFO SECTION.                                                
023200                                                                          
023300     PERFORM IMS-GET-WL410701                                             
023310     PERFORM IMS-GHNP-WL410711                                            
023400                                                                          
023500     MOVE 4108-KVDAGAR-LAP      TO MOD-KVDAGAR-LAP-UT                     
023600     MOVE 4108-KVDAGAR-RTAOSEA  TO MOD-KVDAGAR-RTAOSEA-UT                 
023610     MOVE 4108-KVDAGAR-RTAOVR   TO MOD-KVDAGAR-RTAOVR-UT                  
023620     MOVE 4108-KVDAGAR-RTM      TO MOD-KVDAGAR-RTM-UT                     
023630     MOVE 4108-KVDAGAR-RTP      TO MOD-KVDAGAR-RTP-UT                     
023640     MOVE 4108-KVDAGAR-KLIARB   TO MOD-KVDAGAR-KLIARB-UT                  
023650     MOVE 4108-KVDAGAR-KLIAVV   TO MOD-KVDAGAR-KLIAVV-UT                  
024300     .                                                                    
024400     EJECT                                                                
025302 G-CHECK-INPUT SECTION.                                                   
025303                                                                          
025304     MOVE YES  TO INDATA-SW                                               
025305     IF MID-KVDAGAR-LAP         = ALL '+' AND                             
025306        MID-KVDAGAR-RTAOSEA     = ALL '+' AND                             
025307        MID-KVDAGAR-RTAOVR      = ALL '+' AND                             
025308        MID-KVDAGAR-RTM         = ALL '+' AND                             
025309        MID-KVDAGAR-RTP         = ALL '+' AND                             
025310        MID-KVDAGAR-KLIARB      = ALL '+' AND                             
025311        MID-KVDAGAR-KLIAVV      = ALL '+'                                 
025312       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
025313       CALL WMEDKONV USING MED-WMEDAREA                                   
025314       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
025315       PERFORM MFS-DO-NOT-TOUCH-FIELD-IN                                  
025317       MOVE NOO TO INDATA-SW                                              
025318     ELSE                                                                 
025319                                                                          
025320       IF MID-KVDAGAR-LAP       = ALL '+'                                 
025322          MOVE MFS-NUM-FIELD-OK TO MOD-KVDAGAR-LAP-IN-ATTR                
025323       ELSE                                                               
025324         IF MID-KVDAGAR-LAP     NUMERIC                                   
025325           MOVE MFS-NUM-FIELD-OK TO MOD-KVDAGAR-LAP-IN-ATTR               
025328         ELSE                                                             
025329           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVDAGAR-LAP-IN-ATTR            
025330           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVDAGAR-LAP-IN              
025331           MOVE NOO TO INDATA-SW                                          
025332         END-IF                                                           
025333       END-IF                                                             
025334                                                                          
025335       IF MID-KVDAGAR-RTAOSEA   = ALL '+'                                 
025336          MOVE MFS-NUM-FIELD-OK TO MOD-KVDAGAR-RTAOSEA-IN-ATTR            
025337       ELSE                                                               
025338         IF MID-KVDAGAR-RTAOSEA NUMERIC                                   
025339           MOVE MFS-NUM-FIELD-OK TO MOD-KVDAGAR-RTAOSEA-IN-ATTR           
025340         ELSE                                                             
025350           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVDAGAR-RTAOSEA-IN-ATTR        
025351           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVDAGAR-RTAOSEA-IN          
025352           MOVE NOO TO INDATA-SW                                          
025353         END-IF                                                           
025354       END-IF                                                             
025355                                                                          
025356       IF MID-KVDAGAR-RTAOVR    = ALL '+'                                 
025357          MOVE MFS-NUM-FIELD-OK TO MOD-KVDAGAR-RTAOVR-IN-ATTR             
025358       ELSE                                                               
025359         IF MID-KVDAGAR-RTAOVR  NUMERIC                                   
025360           MOVE MFS-NUM-FIELD-OK TO MOD-KVDAGAR-RTAOVR-IN-ATTR            
025361         ELSE                                                             
025362           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVDAGAR-RTAOVR-IN-ATTR         
025363           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVDAGAR-RTAOVR-IN           
025364           MOVE NOO TO INDATA-SW                                          
025365         END-IF                                                           
025366       END-IF                                                             
025367                                                                          
025368       IF MID-KVDAGAR-RTM       = ALL '+'                                 
025369          MOVE MFS-NUM-FIELD-OK TO MOD-KVDAGAR-RTM-IN-ATTR                
025370       ELSE                                                               
025371         IF MID-KVDAGAR-RTM     NUMERIC                                   
025372           MOVE MFS-NUM-FIELD-OK TO MOD-KVDAGAR-RTM-IN-ATTR               
025373         ELSE                                                             
025374           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVDAGAR-RTM-IN-ATTR            
025375           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVDAGAR-RTM-IN              
025376           MOVE NOO TO INDATA-SW                                          
025377         END-IF                                                           
025378       END-IF                                                             
025379                                                                          
025380       IF MID-KVDAGAR-RTP       = ALL '+'                                 
025381          MOVE MFS-NUM-FIELD-OK TO MOD-KVDAGAR-RTP-IN-ATTR                
025382       ELSE                                                               
025383         IF MID-KVDAGAR-RTP     NUMERIC                                   
025384           MOVE MFS-NUM-FIELD-OK TO MOD-KVDAGAR-RTP-IN-ATTR               
025385         ELSE                                                             
025386           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVDAGAR-RTP-IN-ATTR            
025387           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVDAGAR-RTP-IN              
025388           MOVE NOO TO INDATA-SW                                          
025389         END-IF                                                           
025390       END-IF                                                             
025391                                                                          
025392       IF MID-KVDAGAR-KLIARB    = ALL '+'                                 
025393          MOVE MFS-NUM-FIELD-OK TO MOD-KVDAGAR-KLIARB-IN-ATTR             
025394       ELSE                                                               
025395         IF MID-KVDAGAR-KLIARB  NUMERIC                                   
025396           MOVE MFS-NUM-FIELD-OK TO MOD-KVDAGAR-KLIARB-IN-ATTR            
025397         ELSE                                                             
025398           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVDAGAR-KLIARB-IN-ATTR         
025399           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVDAGAR-KLIARB-IN           
025400           MOVE NOO TO INDATA-SW                                          
025401         END-IF                                                           
025402       END-IF                                                             
025403                                                                          
025404       IF MID-KVDAGAR-KLIAVV    = ALL '+'                                 
025405          MOVE MFS-NUM-FIELD-OK TO MOD-KVDAGAR-KLIAVV-IN-ATTR             
025406       ELSE                                                               
025407         IF MID-KVDAGAR-KLIAVV  NUMERIC                                   
025408           MOVE MFS-NUM-FIELD-OK TO MOD-KVDAGAR-KLIAVV-IN-ATTR            
025409         ELSE                                                             
025410           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVDAGAR-KLIAVV-IN-ATTR         
025411           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVDAGAR-KLIAVV-IN           
025412           MOVE NOO TO INDATA-SW                                          
025413         END-IF                                                           
025414       END-IF                                                             
025415                                                                          
025416       IF INDATA-WRONG                                                    
025417         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
025418         CALL WMEDKONV USING MED-WMEDAREA                                 
025419         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
025421         PERFORM MFS-DO-NOT-TOUCH-FIELD-IN                                
025432       END-IF                                                             
025433     END-IF                                                               
025434     .                                                                    
025435     EJECT                                                                
025436 H-UPDATE SECTION.                                                        
025437                                                                          
025438     PERFORM IMS-GET-WL410701                                             
025439     PERFORM IMS-GHNP-WL410711                                            
025440                                                                          
025442     IF MID-KVDAGAR-LAP         NOT = ALL '+'                             
025443       MOVE MID-KVDAGAR-LAP     TO 4108-KVDAGAR-LAP                       
025444       MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVDAGAR-LAP-UT-ATTR              
025447     END-IF                                                               
025448                                                                          
025453     IF MID-KVDAGAR-RTAOSEA     NOT = ALL '+'                             
025454       MOVE MID-KVDAGAR-RTAOSEA TO 4108-KVDAGAR-RTAOSEA                   
025455       MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVDAGAR-RTAOSEA-UT-ATTR          
025459     END-IF                                                               
025460                                                                          
025465     IF MID-KVDAGAR-RTAOVR      NOT = ALL '+'                             
025466       MOVE MID-KVDAGAR-RTAOVR  TO 4108-KVDAGAR-RTAOVR                    
025467       MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVDAGAR-RTAOVR-UT-ATTR           
025470     END-IF                                                               
025471                                                                          
025476     IF MID-KVDAGAR-RTM         NOT = ALL '+'                             
025477       MOVE MID-KVDAGAR-RTM     TO 4108-KVDAGAR-RTM                       
025478       MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVDAGAR-RTM-UT-ATTR              
025481     END-IF                                                               
025482                                                                          
025487     IF MID-KVDAGAR-RTP         NOT = ALL '+'                             
025488       MOVE MID-KVDAGAR-RTP     TO 4108-KVDAGAR-RTP                       
025489       MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVDAGAR-RTP-UT-ATTR              
025492     END-IF                                                               
025493                                                                          
025498     IF MID-KVDAGAR-KLIARB      NOT = ALL '+'                             
025499       MOVE MID-KVDAGAR-KLIARB  TO 4108-KVDAGAR-KLIARB                    
025500       MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVDAGAR-KLIARB-UT-ATTR           
025503     END-IF                                                               
025504                                                                          
025509     IF MID-KVDAGAR-KLIAVV      NOT = ALL '+'                             
025510       MOVE MID-KVDAGAR-KLIAVV  TO 4108-KVDAGAR-KLIAVV                    
025511       MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVDAGAR-KLIAVV-UT-ATTR           
025514     END-IF                                                               
025515                                                                          
025516     PERFORM IMS-REPL-WL410711                                            
025517                                                                          
025518     MOVE INF-UPDATE-DONE       TO MED-IDMFSINF                           
025519     CALL WMEDKONV           USING MED-WMEDAREA                           
025520     MOVE MED-MFSINF            TO MOD-TEMFSINF                           
025521     PERFORM MFS-FORM-ATTR                                                
025522     PERFORM MFS-ERASE-FIELD-IN                                           
025524     .                                                                    
025525     EJECT                                                                
025530 MFS-ERASE-FIELD-OUT SECTION.                                             
025600                                                                          
025700*    --- ALLA UTDATA-FÄLT                                                 
025800     MOVE MFS-ERASE-FIELD TO MOD-KVDAGAR-LAP-UT                           
025900                             MOD-KVDAGAR-RTAOSEA-UT                       
026000                             MOD-KVDAGAR-RTAOVR-UT                        
026100                             MOD-KVDAGAR-RTM-UT                           
026110                             MOD-KVDAGAR-RTP-UT                           
026120                             MOD-KVDAGAR-KLIARB-UT                        
026130                             MOD-KVDAGAR-KLIAVV-UT                        
026200     .                                                                    
026400                                                                          
026500 MFS-ERASE-FIELD-IN SECTION.                                              
026600                                                                          
026700*    --- ALLA INDATA-FÄLT                                                 
026900     MOVE MFS-ERASE-FIELD TO MOD-KVDAGAR-LAP-IN                           
026910                             MOD-KVDAGAR-RTAOSEA-IN                       
026920                             MOD-KVDAGAR-RTAOVR-IN                        
026930                             MOD-KVDAGAR-RTM-IN                           
026940                             MOD-KVDAGAR-RTP-IN                           
026950                             MOD-KVDAGAR-KLIARB-IN                        
026960                             MOD-KVDAGAR-KLIAVV-IN                        
027000     .                                                                    
027100     EJECT                                                                
028200 MFS-DO-NOT-TOUCH-FIELD-IN  SECTION.                                      
028300                                                                          
028400*    --- ALLA INDATA-FÄLT                                                 
028510     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVDAGAR-LAP-IN                    
028520                                    MOD-KVDAGAR-RTAOSEA-IN                
028530                                    MOD-KVDAGAR-RTAOVR-IN                 
028540                                    MOD-KVDAGAR-RTM-IN                    
028550                                    MOD-KVDAGAR-RTP-IN                    
028560                                    MOD-KVDAGAR-KLIARB-IN                 
028570                                    MOD-KVDAGAR-KLIAVV-IN                 
028700     .                                                                    
028800     EJECT                                                                
028900 MFS-FORM-ATTR SECTION.                                                   
029000                                                                          
029100*    --- ALL INDATA-FIELDS                                                
029300     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KVDAGAR-LAP-IN-ATTR              
029310                                     MOD-KVDAGAR-RTAOSEA-IN-ATTR          
029320                                     MOD-KVDAGAR-RTAOVR-IN-ATTR           
029330                                     MOD-KVDAGAR-RTM-IN-ATTR              
029340                                     MOD-KVDAGAR-RTP-IN-ATTR              
029350                                     MOD-KVDAGAR-KLIARB-IN-ATTR           
029360                                     MOD-KVDAGAR-KLIAVV-IN-ATTR           
029400     .                                                                    
029500     EJECT                                                                
030300* --- IMS SECTIONS ---                                                    
030400                                                                          
030500 IMS-GET-MSG SECTION.                                                     
030600                                                                          
030700     MOVE '  QC' TO GOOD-STATUSCODES                                      
030800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031000     PERFORM IMS-STATUSCHECK                                              
031100     .                                                                    
031200                                                                          
031300 IMS-INSERT-MSG SECTION.                                                  
031400                                                                          
031500     IF ENGLISH-TEXT                                                      
031600       MOVE 'N' TO MFS-KDHUVOMR                                           
031700     END-IF                                                               
031800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031900     MOVE SPACE TO GOOD-STATUSCODES                                       
032000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032200     PERFORM IMS-STATUSCHECK                                              
032300     .                                                                    
032401     EJECT                                                                
032402 IMS-GET-WL410701               SECTION.                                  
032403                                                                          
032404     STRING 'WL410701(WDGXKEY  =' W-WDGXKEY-X ')'                         
032405          DELIMITED BY SIZE INTO SSA1                                     
032406     MOVE '    ' TO GOOD-STATUSCODES                                      
032407     CALL CBLTDLI USING GU 4107-PCB DLI-IO-AREA SSA1                      
032408     MOVE 4107-STATUS-CODE TO STATUS-WS                                   
032409     PERFORM IMS-STATUSCHECK                                              
032410     .                                                                    
032411                                                                          
032412 IMS-GHNP-WL410711               SECTION.                                 
032413                                                                          
032414     STRING 'WL410711(KDSEGKEY =' W-KDSEGKEY-X ')'                        
032415          DELIMITED BY SIZE INTO SSA1                                     
032416     MOVE '    ' TO GOOD-STATUSCODES                                      
032417     CALL CBLTDLI USING GHNP 4107-PCB DLI-IO-AREA SSA1                    
032418     MOVE 4107-STATUS-CODE TO STATUS-WS                                   
032419     PERFORM IMS-STATUSCHECK                                              
032420     .                                                                    
032421                                                                          
032422 IMS-REPL-WL410711              SECTION.                                  
032423                                                                          
032424     MOVE '  ' TO GOOD-STATUSCODES                                        
032425     CALL CBLTDLI USING REPL 4107-PCB DLI-IO-AREA                         
032426     MOVE 4107-STATUS-CODE TO STATUS-WS                                   
032427     PERFORM IMS-STATUSCHECK                                              
032430     .                                                                    
032500     EJECT                                                                
032600 IMS-STATUSCHECK SECTION.                                                 
032700                                                                          
032800     SET STATUS-IX TO 1                                                   
032900     SEARCH GOOD-STATUS                                                   
033000       AT END                                                             
033100         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
033200         DELIMITED BY SIZE INTO ERROR-TEXT                                
033300         CALL FELLOG                                                      
033400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
033500         CONTINUE                                                         
033600     END-SEARCH                                                           
033700     .                                                                    
