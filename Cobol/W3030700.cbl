001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W3030700.                                                
001600 AUTHOR.         KARANDE DIGAMBAR.                                        
001700 DATE-WRITTEN.   02/03/25.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNCTION:                                                            
002100*        INSERT,REPLACE OR DELETE THE MAIL INFO FOR THE NON PRICED        
002200*        PARTS                                                            
002300*                                                                         
002410*        THE PROGRAM UPDATES   WDR4                                       
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSACTION: W3T307                                              
002800*        MID:         W30307I1                                            
002900*                                                                         
003000*    OUTDATA.                                                             
003100*        MOD:         W30307O1                                            
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500                                                                          
003600 DATA DIVISION.                                                           
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W3030700'.            
004000                                                                          
004100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004200 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004300                                                                          
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004510 77  W-ADDISPABS-ASYNC           PIC X(50)   VALUE SPACE.                 
004520 77  W-ADDISPABS-SYNC            PIC X(50)   VALUE SPACE.                 
004600                                                                          
004800*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005000                                                                          
005101 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005102     88  INDATA-OK                           VALUE 'J'.                   
005110     88  INDATA-WRONG                        VALUE 'N'.                   
005200                                                                          
005300 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005400     88  KEYS-OK                             VALUE 'J'.                   
005500     88  KEYS-WRONG                          VALUE 'N'.                   
005600                                                                          
005610 77  UPDATE-DONE                 PIC X       VALUE 'J'.                   
005620     88  UPDATE-OK                           VALUE 'J'.                   
005630     88  UPDATE-WRONG                        VALUE 'N'.                   
005640                                                                          
005700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005800     88  OWN-MID                             VALUE '3307'.                
005900     88  GOOD-MID                            VALUE '3301' '3302'          
006000                                                   '3303' '3304'          
006100                                                   '3305' '3306'          
006200                                                   '3307' '3308'          
006300                                                   '3309'.                
006400     88  HELP-MID                            VALUE '0551'.                
006500     EJECT                                                                
006600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006700 01  GENERAL-SUBPROGRAMS.                                                 
006800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007210     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007220     03  W009EMAD                PIC X(8)    VALUE 'W009EMAD'.            
007300     EJECT                                                                
007400*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007500*01 -COPY WMEDAREA                                                        
007600     SKIP3                                                                
007700 01  MESSAGE-CODES.                                                       
007801     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007802     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007803     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '005'.                 
007804     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007810     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008101     EJECT                                                                
008110*01  -COPY WDATAREA                                                       
008200     EJECT                                                                
008220*    --- PARAMETRAR TILL SUBPROGRAM W009EMAD (E-ADDRESS VALIDITY)         
008230*                                                                         
008240 01  FILLER                      PIC X(16)  VALUE 'W009EMAD-AREA'.        
008250                                                                          
008260*01 -COPY W009EMAD                                                        
008270     EJECT                                                                
008300*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008400*                                                                         
008800     EJECT                                                                
008900*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
009000*                                                                         
009100 01  SAVE-AREA.                                                           
009200     03  SAVE-IDTRANS           PIC X(4)    VALUE '3307'.                 
009400     EJECT                                                                
009500*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
009600*                                                                         
009700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009800     SKIP3                                                                
009900*01  MID -COPY W30307I1                                                   
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010200     SKIP3                                                                
010300*01  -COPY WMSGAREA                                                       
010400     EJECT                                                                
010500     03  MOD REDEFINES MSG-AREA.                                          
010600*      05  -COPY W30307O1                                                 
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010900     SKIP3                                                                
011000*01  -COPY WMFSAREA                                                       
011100     EJECT                                                                
011200*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011300*                                                                         
011400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011500     SKIP3                                                                
011600 01  KEYS-TO-DLI.                                                         
011700     03  W-WDGXKEY-X.                                                     
011701         05  FILLER              PIC X(4)    VALUE '3101'.                
011702         05  W-IDDISTR           PIC 9(4)    VALUE ZERO.                  
011703         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
011800     SKIP2                                                                
011900*    --- STATUS-KOD FRÅN IMS                                              
012000 01  STATUS-WS                   PIC XX.                                  
012100     88  SEGMENT-FOUND                       VALUE '  '.                  
012200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012400     SKIP2                                                                
012500 01  GOOD-STATUSCODES.                                                    
012600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012700     SKIP3                                                                
012800 01  SSA1                        PIC X(64).                               
012900 01  SSA2                        PIC X(64).                               
013000     EJECT                                                                
013100*    --- IMS FUNCTION CODES                                               
013200*01  -COPY W0003                                                          
013400     EJECT                                                                
013500*    ---  DLI INPUT-OUTPUT AREA                                           
013600                                                                          
013701 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR401'.                      
013702 01  DLI-IO-WDR401.                                                       
013703*    03  -COPY WDGX3101                                                   
013704     EJECT                                                                
013705 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3102'.                    
013706 01  DLI-IO-WDGX3102.                                                     
013710*    03  -COPY WDGX3102                                                   
014000     EJECT                                                                
014100 LINKAGE SECTION.                                                         
014200*01  -COPY W0009   -PRE MSG-                                              
014502*01  -COPY W0008   -PRE WDR4-                                             
014510     05  FILLER                  PIC X.                                   
014600     EJECT                                                                
014701 PROCEDURE DIVISION  USING MSG-PCB WDR4-PCB.                              
014702 MAIN SECTION.                                                            
014710     ENTRY 'DLITCBL' USING MSG-PCB WDR4-PCB.                              
014800                                                                          
015000     PERFORM IMS-GET-MSG                                                  
015100     IF SEGMENT-FOUND                                                     
015200       PERFORM A-INIT                                                     
015300       PERFORM B-CHECK-KEYS                                               
015400       IF KEYS-OK                                                         
015501         IF MFS-UPDATE                                                    
015502           PERFORM G-CHECK-INPUT                                          
015503           IF INDATA-OK                                                   
015504             PERFORM H-UPDATE                                             
015505           END-IF                                                         
015510         ELSE                                                             
015705             PERFORM F-READ-SHOW-INFO                                     
015810         END-IF                                                           
016000       END-IF                                                             
016300       COMPUTE MSG-KVLL = LENGTH OF MOD-W30307O1 + 4                      
016400       PERFORM IMS-INSERT-MSG                                             
016500     END-IF                                                               
016700                                                                          
016800     MOVE ZERO TO RETURN-CODE                                             
016900     GOBACK                                                               
017000     .                                                                    
017100     EJECT                                                                
017200 A-INIT SECTION.                                                          
017300                                                                          
017400     IF MSG-DOUBLE-TRANSACTIONS                                           
017500       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W30307I1                 
017600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017800     ELSE                                                                 
017900       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W30307I1                  
018000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018200     END-IF                                                               
018300                                                                          
018400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018700                                                                          
018800     MOVE LOW-VALUE TO MSG-AREA                                           
018900     MOVE 'W3O307N1' TO MFS-IDMOD                                         
019000     MOVE '3307' TO MOD-IDTRANS                                           
019100     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
019110     MOVE ZEROS           TO W-IDDISTR                                    
019200                                                                          
019300     IF OWN-MID OR HELP-MID                                               
019400       CONTINUE                                                           
019500     ELSE                                                                 
019600       MOVE SPACE TO  MFS-KDTRTYP                                         
019700       MOVE '7'   TO  MFS-IDPFK                                           
019800     END-IF                                                               
020100     .                                                                    
020200     EJECT                                                                
020300 B-CHECK-KEYS SECTION.                                                    
020400                                                                          
021900     MOVE YES TO KEYS-SW                                                  
022000                                                                          
022103     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
022104                                                                          
022112     INSPECT MID-IDDISTR-IN REPLACING LEADING SPACE BY ZERO               
022113     IF MID-IDDISTR-IN NOT = ALL '+'                                      
022114       IF MID-IDDISTR-IN NUMERIC                                          
022115          IF MID-IDDISTR-IN  > 0                                          
022116             MOVE MID-IDDISTR-IN TO W-IDDISTR                             
022117          ELSE                                                            
022118            MOVE NOO TO KEYS-SW                                           
022121          END-IF                                                          
022122       ELSE                                                               
022123          MOVE NOO TO KEYS-SW                                             
022124       END-IF                                                             
022125     ELSE                                                                 
022126       INSPECT MID-IDDISTR-UT REPLACING LEADING SPACE BY ZERO             
022127       IF MID-IDDISTR-UT NUMERIC                                          
022128         IF MID-IDDISTR-UT  > 0                                           
022129            MOVE MID-IDDISTR-UT TO W-IDDISTR                              
022130         ELSE                                                             
022131            MOVE NOO TO KEYS-SW                                           
022132         END-IF                                                           
022133       ELSE                                                               
022134         MOVE NOO TO KEYS-SW                                              
022135       END-IF                                                             
022140     END-IF                                                               
022201                                                                          
022202     IF GOOD-MID OR KEYS-OK                                               
022203       MOVE W-IDDISTR       TO MOD-IDDISTR-UT                             
022204     ELSE                                                                 
022205       MOVE MFS-ERASE-FIELD TO MID-IDDISTR-UT                             
022210     END-IF                                                               
022300                                                                          
022400     IF KEYS-WRONG                                                        
022500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
022600       CALL WMEDKONV USING MED-WMEDAREA                                   
022700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
022800       PERFORM MFS-ERASE-FIELD-IN                                         
022900       PERFORM MFS-ERASE-FIELD-OUT                                        
023000     END-IF                                                               
023100     .                                                                    
023300     EJECT                                                                
023400                                                                          
025702 G-CHECK-INPUT SECTION.                                                   
025703                                                                          
025704     MOVE YES    TO INDATA-SW                                             
025705     MOVE SPACES TO W-ADDISPABS-ASYNC                                     
025706                    W-ADDISPABS-SYNC                                      
025707     IF MID-W30307I1 = ALL '+'                                            
025708       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
025709       CALL WMEDKONV USING MED-WMEDAREA                                   
025710       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
025711       MOVE NOO TO INDATA-SW                                              
025712     ELSE                                                                 
025725       IF MID-CD        NOT = ALL '+'                                     
025726          IF MID-CD     NOT = ( 'I' AND 'R'  AND 'D' )                    
025727             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-CD-ATTR                    
025731             MOVE NOO TO INDATA-SW                                        
025732          ELSE                                                            
025733             MOVE MFS-ALPHA-FIELD-OK TO MOD-CD-ATTR                       
025734          END-IF                                                          
025735       ELSE                                                               
025736          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-CD-ATTR                       
025742          MOVE NOO TO INDATA-SW                                           
025743       END-IF                                                             
025744                                                                          
025747       IF MID-IDMAIL NOT = ALL '+'                                        
025748          MOVE MID-IDMAIL             TO EMAD-IDMAIL                      
025749          CALL W009EMAD USING EMAD-W009EMAD                               
025750          MOVE EMAD-IDMAIL  TO MID-IDMAIL    MOD-IDMAIL                   
025751          IF EMAD-KDSVAR > SPACE                                          
025752             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDMAIL-ATTR                
025753             MOVE NOO                   TO INDATA-SW                      
025754          ELSE                                                            
025755             MOVE MFS-ALPHA-FIELD-OK    TO MOD-IDMAIL-ATTR                
025756          END-IF                                                          
025757       ELSE                                                               
025759          IF MID-CD     = ( 'I' OR 'R'  )                                 
025760             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDMAIL-ATTR                
025761             MOVE NOO                   TO INDATA-SW                      
025762          END-IF                                                          
025764       END-IF                                                             
025765                                                                          
025766       IF MID-ADDISPABS-ASYNC    NOT = ALL '+'                            
025767          MOVE MID-ADDISPABS-ASYNC     TO W-ADDISPABS-ASYNC               
025769          MOVE MFS-ALPHA-FIELD-OK      TO                                 
025770                             MOD-ADDISPABS-ASYNC-ATTR                     
025771       ELSE                                                               
025772          MOVE SPACES TO W-ADDISPABS-ASYNC                                
025773          MOVE MFS-ALPHA-FIELD-OK      TO                                 
025774                             MOD-ADDISPABS-ASYNC-ATTR                     
025783       END-IF                                                             
025784                                                                          
025785       IF MID-ADDISPABS-SYNC    NOT = ALL '+'                             
025786          MOVE MID-ADDISPABS-SYNC     TO W-ADDISPABS-SYNC                 
025787          MOVE MFS-ALPHA-FIELD-OK     TO                                  
025788                             MOD-ADDISPABS-SYNC-ATTR                      
025790       ELSE                                                               
025791          MOVE SPACES                 TO W-ADDISPABS-SYNC                 
025792          MOVE MFS-ALPHA-FIELD-OK     TO                                  
025793                             MOD-ADDISPABS-SYNC-ATTR                      
025794                                                                          
025795       END-IF                                                             
025803                                                                          
025804     END-IF                                                               
025805                                                                          
025806     IF INDATA-OK                                                         
025807       PERFORM IMS-GHU-WDR401                                             
025808       IF SEGMENT-FOUND                                                   
025809          IF MID-CD    = 'I'                                              
025810             MOVE MFS-ALPHA-FIELD-WRONG    TO  MOD-CD-ATTR                
025811             MOVE       'DISTRICT EXIST- TO ADD/CHANGE MAIL INFO          
025812-                 ' USE   R'                                              
025813                                           TO  MOD-TEMFSINF               
025814             MOVE NOO                      TO INDATA-SW                   
025815          END-IF                                                          
025816       ELSE                                                               
025817          IF MID-CD    = 'R'  OR 'D'                                      
025818             MOVE MFS-ALPHA-FIELD-WRONG    TO  MOD-CD-ATTR                
025819             MOVE 'DISTRICT NOT EXIST'     TO MOD-TEMFSINF                
025820             MOVE NOO                      TO INDATA-SW                   
025821          END-IF                                                          
025822       END-IF                                                             
025823     END-IF                                                               
025824                                                                          
025825     IF INDATA-WRONG                                                      
025826       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
025827       CALL WMEDKONV USING MED-WMEDAREA                                   
025828       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
025830       PERFORM MFS-DONT-TOUCH-FIELD                                       
025831       MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                             
025832     END-IF                                                               
025833     .                                                                    
025840     EJECT                                                                
025849                                                                          
025850 H-UPDATE SECTION.                                                        
025851                                                                          
025852     MOVE     NOO  TO UPDATE-DONE                                         
025853     EVALUATE MID-CD                                                      
025854                                                                          
025855         WHEN 'I'                                                         
025856              MOVE  W-WDGXKEY-X             TO 3101-WDGX3101              
025857              PERFORM IMS-ISRT-WDR401                                     
025858              IF SEGMENT-FOUND                                            
025859                 MOVE MID-IDMAIL            TO  3102-IDMAIL               
025861                 MOVE W-ADDISPABS-ASYNC     TO                            
025862                                       3102-ADDISPABS-ASYNC               
025863                 MOVE W-ADDISPABS-SYNC      TO                            
025864                                       3102-ADDISPABS-SYNC                
025865                 PERFORM IMS-ISRT-WDGX3102                                
025866                 MOVE YES TO UPDATE-DONE                                  
025868              END-IF                                                      
025869                                                                          
025870         WHEN 'R'                                                         
025872              PERFORM IMS-GHNP-WDGX3102                                   
025873              MOVE MID-IDMAIL              TO  3102-IDMAIL                
025874              MOVE W-ADDISPABS-ASYNC       TO                             
025875                                       3102-ADDISPABS-ASYNC               
025876              MOVE W-ADDISPABS-SYNC        TO                             
025877                                       3102-ADDISPABS-SYNC                
025878              IF SEGMENT-FOUND                                            
025879                 PERFORM IMS-REPL-WDGX3102                                
025881                 MOVE YES                   TO UPDATE-DONE                
025882              ELSE                                                        
025884                 PERFORM IMS-ISRT-WDGX3102                                
025885                 MOVE YES                   TO UPDATE-DONE                
025887              END-IF                                                      
025888                                                                          
025889         WHEN 'D'                                                         
025893              PERFORM IMS-DLET-WDR401                                     
025894              MOVE YES                   TO UPDATE-DONE                   
025896     END-EVALUATE                                                         
025898                                                                          
025899     IF UPDATE-OK                                                         
025900       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
025901       CALL WMEDKONV USING MED-WMEDAREA                                   
025902       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
025903       PERFORM MFS-FORM-ATTR                                              
025904       PERFORM MFS-ERASE-FIELD-IN                                         
025905       PERFORM MFS-ERASE-FIELD-OUT                                        
025907     END-IF                                                               
025908     .                                                                    
025909     EJECT                                                                
025910                                                                          
025911 F-READ-SHOW-INFO SECTION.                                                
025912                                                                          
025914     PERFORM IMS-GHU-WDR401                                               
025915     IF SEGMENT-FOUND                                                     
025916        PERFORM IMS-GHNP-WDGX3102                                         
025917        IF SEGMENT-FOUND                                                  
025918           MOVE 3102-IDMAIL          TO  MOD-IDMAIL                       
025919           MOVE 3102-ADDISPABS-ASYNC TO  MOD-ADDISPABS-ASYNC              
025920           MOVE 3102-ADDISPABS-SYNC  TO  MOD-ADDISPABS-SYNC               
025923        ELSE                                                              
025926           PERFORM MFS-ERASE-FIELD-IN                                     
025930        END-IF                                                            
025931        MOVE INF-PRESS-PF11 TO MED-IDMFSINF                               
025932        CALL WMEDKONV USING MED-WMEDAREA                                  
025933        MOVE MED-MFSINF TO MOD-TEMFSFEL                                   
025934     ELSE                                                                 
025935       MOVE KEYS-ARE-MISSING TO MED-IDMFSFEL                              
025936       CALL WMEDKONV USING MED-WMEDAREA                                   
025937       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
025938       PERFORM MFS-ERASE-FIELD-IN                                         
025939     END-IF                                                               
025947     .                                                                    
025948                                                                          
025950 MFS-ERASE-FIELD-OUT SECTION.                                             
026000                                                                          
026100*    --- ALLA UTDATA-FÄLT                                                 
026300     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-UT                               
026600     .                                                                    
026700     SKIP3                                                                
026800 MFS-ERASE-FIELD-IN SECTION.                                              
026900                                                                          
027000*    --- ALLA INDATA-FÄLT                                                 
027100     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
027120                             MOD-IDMAIL                                   
027150                             MOD-ADDISPABS-ASYNC                          
027170                             MOD-ADDISPABS-SYNC                           
027180                             MOD-CD                                       
027300     .                                                                    
027400     EJECT                                                                
027500 MFS-DONT-TOUCH-FIELD  SECTION.                                           
027600                                                                          
027700*    --- ALLA UTDATA-FÄLT                                                 
027900     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR-IN                        
028000                                    MOD-IDMAIL                            
028100                                    MOD-ADDISPABS-ASYNC                   
028110                                    MOD-ADDISPABS-SYNC                    
028130                                    MOD-CD                                
028200     .                                                                    
028300     SKIP3                                                                
029100 MFS-FORM-ATTR SECTION.                                                   
029200                                                                          
029300*    --- ALL INDATA-FIELDS                                                
029500     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-IDMAIL-ATTR                      
029510                                     MOD-ADDISPABS-ASYNC-ATTR             
029520                                     MOD-ADDISPABS-SYNC-ATTR              
029540                                     MOD-CD-ATTR                          
029600     .                                                                    
029700     SKIP2                                                                
030500* --- IMS SECTIONS ---                                                    
030600     SKIP3                                                                
030700 IMS-GET-MSG SECTION.                                                     
030800                                                                          
030900     MOVE '  QC' TO GOOD-STATUSCODES                                      
031000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031200     PERFORM IMS-STATUSCHECK                                              
031300     .                                                                    
031400     SKIP3                                                                
031500 IMS-INSERT-MSG SECTION.                                                  
031600                                                                          
032000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032100     MOVE SPACE TO GOOD-STATUSCODES                                       
032200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032400     PERFORM IMS-STATUSCHECK                                              
032500     .                                                                    
032601     EJECT                                                                
032602 IMS-GHU-WDR401 SECTION.                                                  
032603                                                                          
032604     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
032605          DELIMITED BY SIZE INTO SSA1                                     
032606     MOVE '  GE' TO GOOD-STATUSCODES                                      
032607     CALL CBLTDLI USING GHU WDR4-PCB DLI-IO-WDR401 SSA1                   
032608     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
032609     PERFORM IMS-STATUSCHECK                                              
032610     .                                                                    
032611     SKIP3                                                                
032612 IMS-ISRT-WDR401 SECTION.                                                 
032613                                                                          
032614     MOVE 'WDR401 ' TO SSA1                                               
032615     MOVE '  II' TO GOOD-STATUSCODES                                      
032616     CALL CBLTDLI USING ISRT WDR4-PCB DLI-IO-WDR401 SSA1                  
032617     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
032618     PERFORM IMS-STATUSCHECK                                              
032619     .                                                                    
032620     SKIP3                                                                
032629 IMS-DLET-WDR401 SECTION.                                                 
032630                                                                          
032631     MOVE '  ' TO GOOD-STATUSCODES                                        
032632     CALL CBLTDLI USING DLET WDR4-PCB DLI-IO-WDR401                       
032633     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
032634     PERFORM IMS-STATUSCHECK                                              
032635     .                                                                    
032636     EJECT                                                                
032637 IMS-GHNP-WDGX3102 SECTION.                                               
032638                                                                          
032639     MOVE   'WDGX3102 ' TO SSA1                                           
032641     MOVE '  GE' TO GOOD-STATUSCODES                                      
032642     CALL CBLTDLI USING GHNP WDR4-PCB DLI-IO-WDGX3102 SSA1                
032643     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
032644     PERFORM IMS-STATUSCHECK                                              
032645     .                                                                    
032646     SKIP3                                                                
032647 IMS-ISRT-WDGX3102 SECTION.                                               
032648                                                                          
032651     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
032652          DELIMITED BY SIZE INTO SSA1                                     
032653     MOVE 'WDGX3102 ' TO SSA2                                             
032654     MOVE '  ' TO GOOD-STATUSCODES                                        
032655     CALL CBLTDLI USING ISRT WDR4-PCB DLI-IO-WDGX3102 SSA1 SSA2           
032656     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
032657     PERFORM IMS-STATUSCHECK                                              
032658     .                                                                    
032659     SKIP3                                                                
032660 IMS-REPL-WDGX3102 SECTION.                                               
032661                                                                          
032662     MOVE '  ' TO GOOD-STATUSCODES                                        
032663     CALL CBLTDLI USING REPL WDR4-PCB DLI-IO-WDGX3102                     
032664     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
032665     PERFORM IMS-STATUSCHECK                                              
032666     .                                                                    
032667     SKIP3                                                                
032800 IMS-STATUSCHECK SECTION.                                                 
032900                                                                          
033000     SET STATUS-IX TO 1                                                   
033100     SEARCH GOOD-STATUS                                                   
033200       AT END                                                             
033300         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
033400         DELIMITED BY SIZE INTO ERROR-TEXT                                
033500         CALL FELLOG                                                      
033600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
033700         CONTINUE                                                         
033800     END-SEARCH                                                           
033900     .                                                                    
