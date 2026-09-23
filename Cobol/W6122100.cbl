000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6122100.                                                
000300 AUTHOR.         BOHLIN HÅKAN.                                            
000400 DATE-WRITTEN.   21/09/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SELECT CONTAINERS (FREIGHT BY BOAT) WHERE WE SHOULD              
000900*        CALL PROJECT44 TO GET ETA INFO.                                  
001000*                                                                         
001100*        THE PROGRAM READS     WDR5 (6301/WDGX6302)                       
001200*                              WDM7                                       
001300*                              WDQ2                                       
001400*                              WDR1 (4433/WDGX4434)                       
001500*                                                                         
001600*    ABENDCODES:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- OUTPUT FILE WITH CONTAINERS TO SEND TO PROJECT44.          
002900*          --- WILL CALL P44 WITH BOOKING NO AND TRANSPORTER.             
003000     SELECT W612211                    ASSIGN TO W61221D1.                
003100*          --- OUTPUT FILE WITH CONTAINERS TO SEND TO PROJECT44.          
003200*          --- WILL CALL P44 WITH CONTAINER NO AND TRANSPORTER.           
003300     SELECT W612212                    ASSIGN TO W61221D2.                
003400*          --- OUTPUT FILE WITH CONTAINERS WHERE PULS WANT                
003500*          --- PROJECT44 TO STOP SEND PUSHEVENTS.                         
003600*          --- WILL CALL P44 WITH SHIPMENT ID (P44 UNIQUE ID).            
003700     SELECT W612213                    ASSIGN TO W61221D3.                
003800*          --- OUTPUT FILE WITH WDR5 INFO TO AZURE DATALAKE.              
003900     SELECT W61221X                    ASSIGN TO W61221D4.                
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP2                                                                
004300 FILE SECTION.                                                            
004400     SKIP3                                                                
004500 FD  W612211                                                              
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900*01  RECORD -COPY W61221 -PRE  OUT1-  -L.                                 
005000     SKIP3                                                                
005100 FD  W612212                                                              
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400                                                                          
005500*01  RECORD -COPY W61221 -PRE  OUT2-  -L.                                 
005600     SKIP3                                                                
005700 FD  W612213                                                              
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS  0.                                                   
006000                                                                          
006100*01  RECORD -COPY W61222 -PRE  OUT3-  -L.                                 
006200     SKIP3                                                                
006300 FD  W61221X                                                              
006400     RECORDING       F                                                    
006500     BLOCK CONTAINS  0.                                                   
006600                                                                          
006700*01  RECORD -COPY W61225 -PRE  OUT4-  -L.                                 
006800     EJECT                                                                
006900 WORKING-STORAGE SECTION.                                                 
007000                                                                          
007100 77  IDPGM                       PIC X(8)    VALUE 'W6122100'.            
007200 77  YES                         PIC X       VALUE 'J'.                   
007300 77  NOO                         PIC X       VALUE 'N'.                   
007400                                                                          
007500 77  CARRIER-CODE                PIC X.                                   
007600     88 CARRIER-VALID-CHAR    VALUE                                       
007700     'A' 'B' 'C' 'D' 'E' 'F' 'G' 'H' 'I' 'J' 'K' 'L' 'M' 'N' 'O'          
007800     'P' 'Q' 'R' 'S' 'T' 'U' 'V' 'W' 'X' 'Y' 'Z' 'Å' 'Ä' 'Ö'.             
007900                                                                          
008000 77  CARRIER-SW                  PIC X.                                   
008100     88 CARRIER-OK               VALUE 'J'.                               
008200                                                                          
008300***  CHANGE-CALL-OK WHEN WE CHANGE FROM CALLING WITH                      
008400***  BOOKING NO/SCAC TO CONTAINER NO/SCAC                                 
008500 77  CHANGE-CALL-SW              PIC X       VALUE 'N'.                   
008600     88 CHANGE-CALL-OK           VALUE 'J'.                               
008700     88 CHANGE-CALL-NOK          VALUE 'N'.                               
008800                                                                          
008900 01  W-IDBOKN                    PIC X(15)   VALUE SPACE.                 
009000 01  W-BETRPFIR                  PIC X(15)   VALUE SPACE.                 
009100                                                                          
009200 01  W-IDLBBET.                                                           
009300     03 W-IDLBBET-1.                                                      
009400        05 W-IDLBBET-1-POS1      PIC X(1).                                
009500        05 W-IDLBBET-1-POS2      PIC X(1).                                
009600        05 W-IDLBBET-1-POS3      PIC X(1).                                
009700        05 W-IDLBBET-1-POS4      PIC X(1).                                
009800     03 W-IDLBBET-2              PIC X(7).                                
009900     03 FILLER                   PIC X(1).                                
010000                                                                          
010100     EJECT                                                                
010200 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
010300 01  FILLER REDEFINES TODAYS-DATE.                                        
010400     05  TODAYS-DATE-YEAR        PIC 9(2).                                
010500     05  TODAYS-DATE-MONTH       PIC 9(2).                                
010600     05  TODAYS-DATE-DAY         PIC 9(2).                                
010700     EJECT                                                                
010800 01  -COPY WWDC99                                                         
010900     EJECT                                                                
011000 01  -COPY WWDCKONS                                                       
011100     EJECT                                                                
011200*    --- PARAMETRAR TILL WDAGKONV                                         
011300*01  -COPY WDAGAREA                                                       
011400     EJECT                                                                
011500 01  GENERAL-SUBPROGRAMS.                                                 
011600*                                                                         
011700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012100     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
012200     SKIP2                                                                
012300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
012400                                                                          
012500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012800     SKIP2                                                                
012900 01  ERROR-TEXT.                                                          
013000     03  FILLER                  PIC X(11)   VALUE 'ERROR-TEXT:'.         
013100     03  ERROR-TEXT-STR          PIC X(69)   VALUE SPACE.                 
013200     EJECT                                                                
013300*    --- PARAMETRAR TILL POSTSUM                                          
013400*                                                                         
013500*01  -COPY W0005   -PRE  POSTSUM-                                         
013600     EJECT                                                                
013700 01  OUT1-AREA-START             PIC X(24)   VALUE                        
013800                                 'OUT1-AREA-START  '.                     
013900     SKIP2                                                                
014000                                                                          
014100*01  AREA -COPY W61221     -PRE OUT1-                                     
014200     EJECT                                                                
014300 01  OUT2-AREA-START             PIC X(24)   VALUE                        
014400                                 'OUT2-AREA-START  '.                     
014500     SKIP2                                                                
014600                                                                          
014700*01  AREA -COPY W61221     -PRE OUT2-                                     
014800     EJECT                                                                
014900 01  OUT3-AREA-START             PIC X(24)   VALUE                        
015000                                 'OUT3-AREA-START  '.                     
015100     SKIP2                                                                
015200                                                                          
015300*01  AREA -COPY W61222     -PRE OUT3-                                     
015400                                                                          
015500     SKIP2                                                                
015600                                                                          
015700*01  AREA -COPY W61225     -PRE OUT4-                                     
015800     EJECT                                                                
015900*    --- AREAS FOR IMS-SECTIONS                                           
016000*                                                                         
016100     EJECT                                                                
016200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016300     SKIP3                                                                
016400 01  KEYS-FOR-DLI.                                                        
016500     03  W-6302KEY-X.                                                     
016600         05  W-6302-DABERANK     PIC 9(8).                                
016700         05  W-6302-IDFAKT       PIC S9(7)   COMP-3.                      
016800                                                                          
016900     03  W-WDM701KY-MIN-X.                                                
017000         05  W-IDFAKT-MIN        PIC S9(7)   VALUE ZERO COMP-3.           
017100         05  W-IDORDNR7-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
017200         05  FILLER              PIC X(7)    VALUE LOW-VALUES.            
017300     03  W-WDM701KY-MAX-X.                                                
017400         05  W-IDFAKT-MAX        PIC S9(7)   VALUE ZERO COMP-3.           
017500         05  W-IDORDNR7-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
017600         05  FILLER              PIC X(7)    VALUE HIGH-VALUE.            
017700     03  W-IDBOKN-X              PIC X(15)   VALUE SPACE.                 
017800                                                                          
017900     03  W-WDQ2CSEQ-X.                                                    
018000         05  W-IDDISTR           PIC S9(5)   VALUE +0 COMP-3.             
018100         05  W-IDKUNDNR          PIC S9(7)   VALUE +0 COMP-3.             
018200         05  W-IDKUNDRF.                                                  
018300           07  W-IDORDNR         PIC 9(7)    VALUE ZERO.                  
018400           07  FILLER            PIC X(3)    VALUE SPACE.                 
018500     03  W-IDDC-X                PIC X(2)    VALUE SPACE.                 
018600                                                                          
018700     03  W-4433KEY-X.                                                     
018800         05  W-4433-IDHTYP       PIC X(4)    VALUE '4433'.                
018900         05  W-4433-IDDC         PIC X(2).                                
019000         05  W-4433-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
019100                                                                          
019200     03  W-4434KEY-MIN-X.                                                 
019300         05  W-4434-IDTRP-MIN     PIC X(5)    VALUE SPACE.                
019400         05  W-4434-LOW-VALUE-MIN PIC X(5)    VALUE LOW-VALUE.            
019500     03  W-4434KEY-MAX-X.                                                 
019600         05  W-4434-IDTRP-MAX     PIC X(5)    VALUE SPACE.                
019700         05  W-4434-LOW-VALUE-MAX PIC X(5)    VALUE HIGH-VALUE.           
019800                                                                          
019900                                                                          
020000     SKIP2                                                                
020100*    --- STATUS-KOD FRÅN IMS                                              
020200 01  STATUS-WS                   PIC XX.                                  
020300     88  SEGMENT-FOUND                       VALUE '  '.                  
020400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
020500     88  SEGMENT-END                         VALUE 'GB'.                  
020600     SKIP2                                                                
020700 01  GOOD-STATUSCODES.                                                    
020800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020900     SKIP3                                                                
021000 01  SSA1                        PIC X(128).                              
021100 01  SSA2                        PIC X(128).                              
021200     EJECT                                                                
021300*    --- IMS FUNCTION CODES                                               
021400*01  -COPY W0003                                                          
021500     EJECT                                                                
021600*    ---  DLI INPUT-OUTPUT AREA                                           
021700     EJECT                                                                
021800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6302'.                    
021900 01  DLI-IO-WDGX6302.                                                     
022000*    03  -COPY WDGX6302                                                   
022100     EJECT                                                                
022200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM701'.                      
022300 01  DLI-IO-WDM701.                                                       
022400*    03  -COPY WDM701                                                     
022500     EJECT                                                                
022600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ201'.                      
022700 01  DLI-IO-WDQ201.                                                       
022800*    03  -COPY WDQ201                                                     
022900     EJECT                                                                
023000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ212'.                      
023100 01  DLI-IO-WDQ212.                                                       
023200*    03  -COPY WDQ212                                                     
023300     EJECT                                                                
023400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4434'.                    
023500 01  DLI-IO-WDGX4434.                                                     
023600*    03  -COPY WDGX4434                                                   
023700     EJECT                                                                
023800 LINKAGE SECTION.                                                         
023900                                                                          
024000*01  -COPY W0008  -PRE 6301-                                              
024100     05  6301-KFB-IDHTYP         PIC X(4).                                
024200     05  6301-KFB-IDDC           PIC X(2).                                
024300                                                                          
024400*01  -COPY W0008  -PRE WDM7-                                              
024500     05  FILLER                  PIC X.                                   
024600                                                                          
024700*01  -COPY W0008  -PRE WDQ2-                                              
024800     05  FILLER                  PIC X.                                   
024900                                                                          
025000*01  -COPY W0008  -PRE 4433-                                              
025100     05  FILLER                  PIC X.                                   
025200     EJECT                                                                
025300 PROCEDURE DIVISION  USING 6301-PCB WDM7-PCB WDQ2-PCB 4433-PCB.           
025400 MAIN SECTION.                                                            
025500     ENTRY 'DLITCBL' USING 6301-PCB WDM7-PCB WDQ2-PCB 4433-PCB.           
025600                                                                          
025700                                                                          
025800     PERFORM A-INIT                                                       
025900                                                                          
026000     PERFORM IMS-GN-WDGX6302                                              
026100     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                         
026200       PERFORM D-CREATE-DLAKE-RECORDS                                     
026300       PERFORM B-CHECK-CARRIER                                            
026400       IF CARRIER-OK                                                      
026500         PERFORM C-CREATE-P44-RECORDS                                     
026600       END-IF                                                             
026700       PERFORM IMS-GN-WDGX6302                                            
026800     END-PERFORM                                                          
026900                                                                          
027000                                                                          
027100     PERFORM Z-FINIT                                                      
027200                                                                          
027300     MOVE ZERO TO RETURN-CODE                                             
027400     GOBACK                                                               
027500     .                                                                    
027600     EJECT                                                                
027700 A-INIT SECTION.                                                          
027800                                                                          
027900     OPEN OUTPUT W612211                                                  
028000                 W612212                                                  
028100                 W612213                                                  
028200                 W61221X                                                  
028300                                                                          
028400     ACCEPT TODAYS-DATE  FROM DATE                                        
028500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
028600     .                                                                    
028700     EJECT                                                                
028800 B-CHECK-CARRIER SECTION.                                                 
028900                                                                          
029000     MOVE YES TO CARRIER-SW                                               
029100     MOVE NOO TO CHANGE-CALL-SW                                           
029200                                                                          
029300***  CHECK IF WE ALREADY HAVE RECEIVED THIS TRANSPORT ***                 
029400     IF 6302-KDTRPSTA = SPACE                                             
029500       PERFORM BB-CHECK-ETADATES                                          
029600***  CHECK IF WE ALREADY HAVE CREATED A SUBSCRIPTION  ***                 
029700       IF 6302-IDSUBSCR > ZERO                                            
029800         IF ((CDC OR DDC-SE)  AND 6302-IDDC-LEV = SPACE)                  
029900           IF CHANGE-CALL-NOK                                             
030000             MOVE NOO TO CARRIER-SW                                       
030100           END-IF                                                         
030200         ELSE                                                             
030300           MOVE NOO TO CARRIER-SW                                         
030400         END-IF                                                           
030500       END-IF                                                             
030600     ELSE                                                                 
030700       IF 6302-KDTRPSTA = 'R' AND 6302-IDSUBSCR > ZERO AND                
030800          6302-IDCONTNR > ZERO                                            
030900         PERFORM BA-CREATE-P44-CER                                        
031000       END-IF                                                             
031100       MOVE NOO TO CARRIER-SW                                             
031200     END-IF                                                               
031300                                                                          
031400     IF CARRIER-OK                                                        
031500***    CHECK IF VALID CONTAINER NO   ***                                  
031600       MOVE 6302-IDLBBET TO W-IDLBBET                                     
031700       MOVE W-IDLBBET-1-POS1 TO CARRIER-CODE                              
031800       IF CARRIER-VALID-CHAR                                              
031900         MOVE W-IDLBBET-1-POS2 TO CARRIER-CODE                            
032000         IF CARRIER-VALID-CHAR                                            
032100           MOVE W-IDLBBET-1-POS3 TO CARRIER-CODE                          
032200           IF CARRIER-VALID-CHAR                                          
032300             MOVE W-IDLBBET-1-POS4 TO CARRIER-CODE                        
032400             IF CARRIER-VALID-CHAR                                        
032500               IF W-IDLBBET-2 NUMERIC                                     
032600                 CONTINUE                                                 
032700               ELSE                                                       
032800                 MOVE NOO TO CARRIER-SW                                   
032900               END-IF                                                     
033000             ELSE                                                         
033100               MOVE NOO TO CARRIER-SW                                     
033200             END-IF                                                       
033300           ELSE                                                           
033400             MOVE NOO TO CARRIER-SW                                       
033500           END-IF                                                         
033600         ELSE                                                             
033700           MOVE NOO TO CARRIER-SW                                         
033800         END-IF                                                           
033900       ELSE                                                               
034000         MOVE NOO TO CARRIER-SW                                           
034100       END-IF                                                             
034200     END-IF                                                               
034300                                                                          
034400***  WHEN WH OUTBOUND INSTALL THERE STORY TO                              
034500***  STORE DATA ON WDM7 LONGER THEN BELOW CODE                            
034600***  SHOULD BE EXECUTED. FROM WDM7 WE FETCH BOOKING NO.                   
034700***  IF CARRIER-OK                                                        
034800***  CHECK IF VALID INVOICE DATE  ***                                     
034900***    MOVE 002         TO DAG-KDCALL                                     
035000***    MOVE 6302-TIFAKT TO DAG-TIAAMMDD-FOM                               
035100***    MOVE 14          TO DAG-KVKALDAG                                   
035200***    CALL WDAGKONV USING DAG-KDCALL                                     
035300***                        DAG-DATUM-AREA                                 
035400***                        DAG-KDSVAR                                     
035500***    IF DAG-KDSVAR = SPACE                                              
035600***       CONTINUE                                                        
035700***    ELSE                                                               
035800***      DISPLAY 'WRONG CALL TO WDAGKONV '                                
035900***      CALL ABEND USING RKOD-ABEND-NO-DUMP                              
036000***    END-IF                                                             
036100***    IF TODAYS-DATE < DAG-TIAAMMDD-TOM                                  
036200***      MOVE NOO TO CARRIER-SW                                           
036300***    END-IF                                                             
036400***  END-IF                                                               
036500     .                                                                    
036600     EJECT                                                                
036700 BA-CREATE-P44-CER SECTION.                                               
036800     MOVE 6302-IDCONTNR TO OUT3-IDCONTNR                                  
036900     MOVE 6302-IDSUBSCR TO OUT3-IDSUBSCR                                  
037000     MOVE 6302-IDLBBET  TO OUT3-IDLBBET                                   
037100     MOVE 6302-DABERANK TO OUT3-DABERANK                                  
037200     MOVE 6302-IDFAKT   TO OUT3-IDFAKT                                    
037300     MOVE 6301-KFB-IDDC TO OUT3-IDDC-REC                                  
037400     PERFORM S13-WRITE-W612213                                            
037500     .                                                                    
037600     EJECT                                                                
037700 BB-CHECK-ETADATES SECTION.                                               
037800                                                                          
037900     MOVE 6302-IDDC-SEND TO WS-IDDC                                       
038000     IF (CDC OR DDC-SE)  AND 6302-IDDC-LEV = SPACE                        
038100       PERFORM BBA-CHECK-INVDATE                                          
038200       IF 6302-IDSUBSCR > ZERO                                            
038300         MOVE NOO TO CHANGE-CALL-SW                                       
038400         IF 6302-IDCONTNR = ZERO AND                                      
038500            6302-IDBOKN   > SPACE AND                                     
038600            6302-DABERANK-LIFDEPPL = ZERO AND                             
038700            6302-DABERANK-LIFDEPAC = ZERO AND                             
038800            6302-DABERANK-PODDEPPL = ZERO AND                             
038900            6302-DABERANK-PODDEPAC = ZERO AND                             
039000            6302-DABERANK-DLVDELPL = ZERO AND                             
039100            6302-DABERANK-DLVDELAC = ZERO AND                             
039200            6302-DABERANK-PODDISPL = ZERO AND                             
039300            6302-DABERANK-PODDISAC = ZERO AND                             
039400            6302-DABERANK-PODARRPL = ZERO AND                             
039500            6302-DABERANK-LIFARRAC = ZERO AND                             
039600            TODAYS-DATE > DAG-TIAAMMDD-TOM                                
039700           MOVE YES TO CHANGE-CALL-SW                                     
039800         END-IF                                                           
039900       ELSE                                                               
040000         IF 6302-IDCONTNR = ZERO AND                                      
040100            6302-DABERANK-LIFDEPPL = ZERO AND                             
040200            6302-DABERANK-LIFDEPAC = ZERO AND                             
040300            6302-DABERANK-PODDEPPL = ZERO AND                             
040400            6302-DABERANK-PODDEPAC = ZERO AND                             
040500            6302-DABERANK-DLVDELPL = ZERO AND                             
040600            6302-DABERANK-DLVDELAC = ZERO AND                             
040700            6302-DABERANK-PODDISPL = ZERO AND                             
040800            6302-DABERANK-PODDISAC = ZERO AND                             
040900            6302-DABERANK-PODARRPL = ZERO AND                             
041000            6302-DABERANK-LIFARRAC = ZERO AND                             
041100            TODAYS-DATE > DAG-TIAAMMDD-TOM                                
041200           MOVE YES TO CHANGE-CALL-SW                                     
041300         ELSE                                                             
041400           MOVE NOO TO CHANGE-CALL-SW                                     
041500         END-IF                                                           
041600       END-IF                                                             
041701     ELSE                                                                 
041801       PERFORM BBB-CHECK-INVDATE                                          
041901       IF 6302-IDSUBSCR = ZERO AND                                        
042001          6302-IDCONTNR = ZERO AND                                        
042101          6302-DABERANK-LIFDEPPL = ZERO AND                               
042201          6302-DABERANK-LIFDEPAC = ZERO AND                               
042301          6302-DABERANK-PODDEPPL = ZERO AND                               
042401          6302-DABERANK-PODDEPAC = ZERO AND                               
042501          6302-DABERANK-DLVDELPL = ZERO AND                               
042601          6302-DABERANK-DLVDELAC = ZERO AND                               
042701          6302-DABERANK-PODDISPL = ZERO AND                               
042801          6302-DABERANK-PODDISAC = ZERO AND                               
042901          6302-DABERANK-PODARRPL = ZERO AND                               
043001          6302-DABERANK-LIFARRAC = ZERO AND                               
043101          TODAYS-DATE > DAG-TIAAMMDD-TOM                                  
043201          CONTINUE                                                        
043301       ELSE                                                               
043401          MOVE NOO TO CARRIER-SW                                          
043501       END-IF                                                             
043600     END-IF                                                               
043700     .                                                                    
043800     EJECT                                                                
043900 BBA-CHECK-INVDATE SECTION.                                               
044000                                                                          
044100     MOVE 002         TO DAG-KDCALL                                       
044200     MOVE 6302-TIFAKT TO DAG-TIAAMMDD-FOM                                 
044300     MOVE 14          TO DAG-KVKALDAG                                     
044400     CALL WDAGKONV USING DAG-KDCALL                                       
044500                         DAG-DATUM-AREA                                   
044600                         DAG-KDSVAR                                       
044700     IF DAG-KDSVAR = SPACE                                                
044800        CONTINUE                                                          
044900     ELSE                                                                 
045000       DISPLAY 'WRONG CALL TO WDAGKONV '                                  
045100       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
045200     END-IF                                                               
045300     .                                                                    
045400     EJECT                                                                
045501 BBB-CHECK-INVDATE SECTION.                                               
045601                                                                          
045701     MOVE 002         TO DAG-KDCALL                                       
045801     MOVE 6302-TIFAKT TO DAG-TIAAMMDD-FOM                                 
045901     MOVE 14          TO DAG-KVKALDAG                                     
046001     CALL WDAGKONV USING DAG-KDCALL                                       
046101                         DAG-DATUM-AREA                                   
046201                         DAG-KDSVAR                                       
046301     IF DAG-KDSVAR = SPACE                                                
046401        CONTINUE                                                          
046501     ELSE                                                                 
046601       DISPLAY 'WRONG CALL TO WDAGKONV '                                  
046701       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
046801     END-IF                                                               
046901     .                                                                    
047001     EJECT                                                                
047100 C-CREATE-P44-RECORDS SECTION.                                            
047200                                                                          
047300     MOVE 6302-IDDC-SEND TO WS-IDDC                                       
047400                                                                          
047500     IF ((CDC OR DDC-SE)  AND 6302-IDDC-LEV = SPACE) AND                  
047600        CHANGE-CALL-NOK                                                   
047700       IF 6302-IDBOKN = SPACE                                             
047800         PERFORM CA-GET-BOOKINGNO                                         
047900       ELSE                                                               
048000         MOVE 6302-IDBOKN TO W-IDBOKN                                     
048100       END-IF                                                             
048200       IF 6302-BETRPFIR = SPACE                                           
048300         PERFORM CB-GET-TRANSPORTER                                       
048400       ELSE                                                               
048500         MOVE 6302-BETRPFIR  TO W-BETRPFIR                                
048600       END-IF                                                             
048700       IF W-IDBOKN > SPACE AND W-BETRPFIR > SPACE                         
048800         MOVE 6302-DABERANK  TO OUT1-DABERANK                             
048900         MOVE 6302-IDFAKT    TO OUT1-IDFAKT                               
049000         MOVE 6302-IDDISTR   TO OUT1-IDDISTR                              
049100         MOVE 6302-IDKUNDNR  TO OUT1-IDKUNDNR                             
049200         MOVE 6302-IDKUNDRF  TO OUT1-IDKUNDRF                             
049300         MOVE 6301-KFB-IDDC  TO OUT1-IDDC-REC                             
049400         MOVE 6302-IDDC-SEND TO OUT1-IDDC-SEND                            
049500         MOVE 6302-IDLBBET   TO OUT1-IDLBBET                              
049600         MOVE W-IDBOKN       TO OUT1-IDBOKN                               
049700         MOVE W-BETRPFIR     TO OUT1-BETRPFIR                             
049800         MOVE 6302-IDSUBSCR  TO OUT1-IDSUBSCR                             
049900         PERFORM S11-WRITE-W612211                                        
050000       END-IF                                                             
050100     ELSE                                                                 
050200       IF 6302-BETRPFIR = SPACE                                           
050300         PERFORM CB-GET-TRANSPORTER                                       
050400       ELSE                                                               
050500         MOVE 6302-BETRPFIR TO W-BETRPFIR                                 
050600       END-IF                                                             
050700       IF W-BETRPFIR > SPACE                                              
050800         MOVE 6302-DABERANK  TO OUT2-DABERANK                             
050900         MOVE 6302-IDFAKT    TO OUT2-IDFAKT                               
051000         MOVE 6302-IDDISTR   TO OUT2-IDDISTR                              
051100         MOVE 6302-IDKUNDNR  TO OUT2-IDKUNDNR                             
051200         MOVE 6302-IDKUNDRF  TO OUT2-IDKUNDRF                             
051300         MOVE 6301-KFB-IDDC  TO OUT2-IDDC-REC                             
051400         IF 6302-IDDC-LEV > SPACE                                         
051500           MOVE 6302-IDDC-LEV  TO OUT2-IDDC-SEND                          
051600         ELSE                                                             
051700           MOVE 6302-IDDC-SEND TO OUT2-IDDC-SEND                          
051800         END-IF                                                           
051900         MOVE 6302-IDLBBET   TO OUT2-IDLBBET                              
052000         MOVE 6302-IDBOKN    TO OUT2-IDBOKN                               
052100         MOVE W-BETRPFIR     TO OUT2-BETRPFIR                             
052200         MOVE 6302-IDSUBSCR  TO OUT2-IDSUBSCR                             
052300         PERFORM S12-WRITE-W612212                                        
052400       END-IF                                                             
052500     END-IF                                                               
052600                                                                          
052700     .                                                                    
052800     EJECT                                                                
052900 CA-GET-BOOKINGNO SECTION.                                                
053000                                                                          
053100     MOVE 6302-IDFAKT      TO W-IDFAKT-MIN                                
053200                              W-IDFAKT-MAX                                
053300     MOVE 6302-IDORDNR5    TO W-IDORDNR7-MIN                              
053400                              W-IDORDNR7-MAX                              
053500     PERFORM IMS-GU-WDM701                                                
053600     IF SEGMENT-FOUND                                                     
053700       MOVE HUV-IDBOKN TO W-IDBOKN                                        
053800     ELSE                                                                 
053900       MOVE SPACE      TO W-IDBOKN                                        
054000     END-IF                                                               
054100     .                                                                    
054200     EJECT                                                                
054300 CB-GET-TRANSPORTER SECTION.                                              
054400                                                                          
054500     MOVE 6302-IDDISTR   TO W-IDDISTR                                     
054600     MOVE 6302-IDKUNDNR  TO W-IDKUNDNR                                    
054700     MOVE 6302-IDORDNR5  TO W-IDORDNR                                     
054800     PERFORM IMS-GU-WDQ201-CSEQ                                           
054900     IF SEGMENT-FOUND                                                     
055000       IF 6302-IDDC-LEV = SPACE                                           
055100         IF DDC-SE                                                        
055200           MOVE WC-CDC-SE      TO W-IDDC-X                                
055300                                  W-4433-IDDC                             
055400         ELSE                                                             
055500           MOVE 6302-IDDC-SEND TO W-IDDC-X                                
055600                                  W-4433-IDDC                             
055700         END-IF                                                           
055800       ELSE                                                               
055900         MOVE 6302-IDDC-LEV    TO W-IDDC-X                                
056000                                  W-4433-IDDC                             
056100       END-IF                                                             
056200       PERFORM IMS-GNP-WDQ212                                             
056300       IF SEGMENT-FOUND                                                   
056400          MOVE ARB-IDTRP    TO W-4434-IDTRP-MIN                           
056500                               W-4434-IDTRP-MAX                           
056600          PERFORM IMS-GU-WDGX4434                                         
056700          IF SEGMENT-FOUND                                                
056800            MOVE 4434-BETRPFIR TO W-BETRPFIR                              
056900          ELSE                                                            
057000            MOVE SPACE         TO W-BETRPFIR                              
057100          END-IF                                                          
057200       ELSE                                                               
057300         MOVE SPACE TO W-BETRPFIR                                         
057400       END-IF                                                             
057500     ELSE                                                                 
057600       MOVE SPACE   TO W-BETRPFIR                                         
057700     END-IF                                                               
057800     .                                                                    
057900     EJECT                                                                
058000 D-CREATE-DLAKE-RECORDS SECTION.                                          
058100     MOVE 6301-KFB-IDDC          TO OUT4-IDDC-REC                         
058200     MOVE 6302-DABERANK          TO OUT4-DABERANK                         
058300     MOVE 6302-IDFAKT            TO OUT4-IDFAKT                           
058400     MOVE 6302-ADINLOMR          TO OUT4-ADINLOMR                         
058500     MOVE 6302-IDDC-SEND         TO OUT4-IDDC-SEND                        
058600     MOVE 6302-IDDISTR           TO OUT4-IDDISTR                          
058700     MOVE 6302-IDKUNDNR          TO OUT4-IDKUNDNR                         
058800     MOVE 6302-IDKUNDRF          TO OUT4-IDKUNDRF                         
058900     MOVE 6302-IDLBBET           TO OUT4-IDLBBET                          
059000     MOVE 6302-IDLEVNR           TO OUT4-IDLEVNR                          
059100     MOVE 6302-KDTRPSTA          TO OUT4-KDTRPSTA                         
059200     MOVE 6302-KVKOLLI-FAKT      TO OUT4-KVKOLLI-FAKT                     
059300     MOVE 6302-KVKOLLI-MOT       TO OUT4-KVKOLLI-MOT                      
059400     MOVE 6302-KVRADER-FAKT      TO OUT4-KVRADER-FAKT                     
059500     MOVE 6302-KVRADER-MOT       TO OUT4-KVRADER-MOT                      
059600     MOVE 6302-KVRADER-PRIO      TO OUT4-KVRADER-PRIO                     
059700     MOVE 6302-TIFAKT            TO OUT4-TIFAKT                           
059800     MOVE 6302-IDSHIPM           TO OUT4-IDSHIPM                          
059900     MOVE 6302-IDDC-LEV          TO OUT4-IDDC-LEV                         
060000     MOVE 6302-IDBOKN            TO OUT4-IDBOKN                           
060100     MOVE 6302-BETRPFIR          TO OUT4-BETRPFIR                         
060200     MOVE 6302-FLMANETA          TO OUT4-FLMANETA                         
060300     MOVE 6302-IDUSER-MANETA     TO OUT4-IDUSER-MANETA                    
060400     MOVE 6302-DABERANK-DISCH    TO OUT4-DABERANK-DISCH                   
060500     MOVE 6302-DABERANK-PROP     TO OUT4-DABERANK-PROP                    
060600     MOVE 6302-TILST-CALLP44     TO OUT4-TILST-CALLP44                    
060700     MOVE 6302-TILST-PUSHEVNT    TO OUT4-TILST-PUSHEVNT                   
060800     MOVE 6302-IDSUBSCR          TO OUT4-IDSUBSCR                         
060900     MOVE 6302-IDCONTNR          TO OUT4-IDCONTNR                         
061000     MOVE 6302-DABERANK-LIFDEPPL TO OUT4-DABERANK-LIFDEPPL                
061100     MOVE 6302-DABERANK-LIFDEPAC TO OUT4-DABERANK-LIFDEPAC                
061200     MOVE 6302-DABERANK-PODDEPPL TO OUT4-DABERANK-PODDEPPL                
061300     MOVE 6302-DABERANK-PODDEPAC TO OUT4-DABERANK-PODDEPAC                
061400     MOVE 6302-DABERANK-DLVDELPL TO OUT4-DABERANK-DLVDELPL                
061500     MOVE 6302-DABERANK-DLVDELAC TO OUT4-DABERANK-DLVDELAC                
061600     MOVE 6302-DABERANK-PODDISPL TO OUT4-DABERANK-PODDISPL                
061700     MOVE 6302-DABERANK-PODDISAC TO OUT4-DABERANK-PODDISAC                
061800     MOVE 6302-DABERANK-PODARRPL TO OUT4-DABERANK-PODARRPL                
061900     MOVE 6302-DABERANK-LIFARRAC TO OUT4-DABERANK-LIFARRAC                
062000     PERFORM S14-WRITE-W61221X                                            
062100     .                                                                    
062200     EJECT                                                                
062300 Z-FINIT SECTION.                                                         
062400     CLOSE W612211                                                        
062500           W612212                                                        
062600           W612213                                                        
062700           W61221X                                                        
062800                                                                          
062900     MOVE 'S' TO POSTSUM-OPKOD                                            
063000     CALL POSTSUM USING POSTSUM-PARM                                      
063100     .                                                                    
063200     EJECT                                                                
063300 S11-WRITE-W612211 SECTION.                                               
063400                                                                          
063500     WRITE OUT1-RECORD FROM OUT1-AREA                                     
063600                                                                          
063700     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
063800     MOVE 'W612211' TO POSTSUM-FDNAMN                                     
063900     MOVE 'W61221D1' TO POSTSUM-DDNAMN2                                   
064000     CALL POSTSUM USING POSTSUM-PARM                                      
064100     .                                                                    
064200     EJECT                                                                
064300 S12-WRITE-W612212 SECTION.                                               
064400                                                                          
064500     WRITE OUT2-RECORD FROM OUT2-AREA                                     
064600                                                                          
064700     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
064800     MOVE 'W612212'   TO POSTSUM-FDNAMN                                   
064900     MOVE 'W61221D2'  TO POSTSUM-DDNAMN2                                  
065000     CALL POSTSUM USING POSTSUM-PARM                                      
065100     .                                                                    
065200     EJECT                                                                
065300 S13-WRITE-W612213 SECTION.                                               
065400                                                                          
065500     WRITE OUT3-RECORD FROM OUT3-AREA                                     
065600                                                                          
065700     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
065800     MOVE 'W612213'   TO POSTSUM-FDNAMN                                   
065900     MOVE 'W61221D3'  TO POSTSUM-DDNAMN2                                  
066000     CALL POSTSUM USING POSTSUM-PARM                                      
066100     .                                                                    
066200     EJECT                                                                
066300 S14-WRITE-W61221X SECTION.                                               
066400                                                                          
066500     WRITE OUT4-RECORD FROM OUT4-AREA                                     
066600                                                                          
066700     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
066800     MOVE 'W61221X'   TO POSTSUM-FDNAMN                                   
066900     MOVE 'W61221D4'  TO POSTSUM-DDNAMN2                                  
067000     CALL POSTSUM USING POSTSUM-PARM                                      
067100     .                                                                    
067200     EJECT                                                                
067300 S99-ABEND SECTION.                                                       
067400                                                                          
067500     SKIP2                                                                
067600     MOVE 'S' TO POSTSUM-OPKOD                                            
067700     CALL POSTSUM USING POSTSUM-PARM                                      
067800     CALL ABEND USING RKOD-ABEND                                          
067900     .                                                                    
068000     EJECT                                                                
068100* --- IMS SECTIONS  ---                                                   
068200                                                                          
068300     EJECT                                                                
068400 IMS-GN-WDGX6302 SECTION.                                                 
068500                                                                          
068600     MOVE 'WDGX6302' TO SSA1                                              
068700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
068800     CALL CBLTDLI USING GN 6301-PCB DLI-IO-WDGX6302 SSA1                  
068900     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
069000     PERFORM IMS-STATUSCHECK                                              
069100     .                                                                    
069200     EJECT                                                                
069300 IMS-GU-WDM701 SECTION.                                                   
069400                                                                          
069500     STRING 'WDM701  (WDM701KY=>' W-WDM701KY-MIN-X                        
069600                    '&WDM701KY=<' W-WDM701KY-MAX-X ')'                    
069700          DELIMITED BY SIZE INTO SSA1                                     
069800     MOVE '  GE' TO GOOD-STATUSCODES                                      
069900     CALL CBLTDLI USING GU WDM7-PCB DLI-IO-WDM701 SSA1                    
070000     MOVE WDM7-STATUS-CODE TO STATUS-WS                                   
070100     PERFORM IMS-STATUSCHECK                                              
070200     .                                                                    
070300     EJECT                                                                
070400 IMS-GU-WDQ201-CSEQ SECTION.                                              
070500                                                                          
070600     STRING  'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
070700             DELIMITED BY SIZE INTO SSA1                                  
070800     MOVE '  GE' TO GOOD-STATUSCODES                                      
070900     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
071000     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
071100     PERFORM IMS-STATUSCHECK                                              
071200     .                                                                    
071300     EJECT                                                                
071400 IMS-GNP-WDQ212 SECTION.                                                  
071500                                                                          
071600     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
071700            DELIMITED BY SIZE INTO SSA1                                   
071800     MOVE  '  GE'            TO GOOD-STATUSCODES                          
071900     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-WDQ212 SSA1                   
072000     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
072100     PERFORM IMS-STATUSCHECK                                              
072200     .                                                                    
072300     EJECT                                                                
072400 IMS-GU-WDGX4434 SECTION.                                                 
072500                                                                          
072600     STRING 'WDR101  (WDGXKEY  =' W-4433KEY-X ')'                         
072700          DELIMITED BY SIZE INTO SSA1                                     
072800     STRING 'WDR130  (WDGXKEY =>' W-4434KEY-MIN-X                         
072900                    '&WDGXKEY =<' W-4434KEY-MAX-X ')'                     
073000          DELIMITED BY SIZE INTO SSA2                                     
073100     MOVE '  GE' TO GOOD-STATUSCODES                                      
073200     CALL CBLTDLI USING GU 4433-PCB DLI-IO-WDGX4434 SSA1 SSA2             
073300     MOVE 4433-STATUS-CODE TO STATUS-WS                                   
073400     PERFORM IMS-STATUSCHECK                                              
073500     .                                                                    
073600     EJECT                                                                
073700     EJECT                                                                
073800 IMS-STATUSCHECK SECTION.                                                 
073900                                                                          
074000     SET STATUS-IX TO 1                                                   
074100     SEARCH GOOD-STATUS                                                   
074200       AT END                                                             
074300         STRING ' WRONG STATUSCODE FROM IMS: ' STATUS-WS                  
074400           DELIMITED BY SIZE INTO ERROR-TEXT-STR                          
074500         DISPLAY ERROR-TEXT                                               
074600         CALL FELLOG                                                      
074700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
074800         CONTINUE                                                         
074900     END-SEARCH                                                           
075000     .                                                                    
