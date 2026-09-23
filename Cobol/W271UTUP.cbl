000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W271UTUP.                                                
000400 AUTHOR.         ARUP DATTA.                                              
000500 DATE-WRITTEN.   19/07/02.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*                                                                         
001000*        THE SUBPROGRAM IS USED TO IMPLEMENT OBJECT ORIENTED              
001100*        PROGRAMMING                                                      
001200*        THE PROGRAM IS USED TO GET THE LEAD TIME ADJUSTED                
001300*        DEMAND FOR XDCS.TAKES INTO ACCOUNT THE FUTURE                    
001400*        FORECAST WITHIN THE LEADTIME FOR WEEKLY DEMAND                   
001500*        CALCULATION                                                      
001600*                                                                         
001700*                                                                         
001800*                                                                         
001900*        PROGRAM READS     WDB6                                           
002000*                          WDK7                                           
002100*                                                                         
002200*                                                                         
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 DATA DIVISION.                                                           
002700                                                                          
002800 WORKING-STORAGE SECTION.                                                 
002900*    -COPY WY2000W1                                                       
003000     SKIP3                                                                
003100 77  IDPGM                       PIC X(8)    VALUE 'W271UTUP'.            
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400 77  IX                          PIC 9(3)    VALUE ZERO.                  
003500 77  AKTIV                       PIC X       VALUE 'A'.                   
003600 77  PASSIV                      PIC X       VALUE 'P'.                   
003700 77  6ARBDAG-SW                  PIC X       VALUE 'N'.                   
003800     EJECT                                                                
003900                                                                          
004000 01  WORKING-FIELDS.                                                      
004100     03 WS-IDDC-SPAR             PIC X(2)    VALUE SPACES.                
004200     03 WS-CDC-SE                PIC X(2)    VALUE '11'.                  
004300     03 WS-IDLEVNR               PIC X(5)    VALUE SPACES.                
004400     03 WS-KVPB-PLAN             PIC 9(6)V9  VALUE ZERO.                  
004500     03 ERR-TXT-STR              PIC X(70)   VALUE SPACES.                
004600*                                                                         
004700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004800 01  FILLER REDEFINES DAGENS-DATUM.                                       
004900     03 DAGENS-AA                PIC 9(2).                                
005000     03 DAGENS-MAANAD            PIC 9(2).                                
005100     03 DAGENS-DAG               PIC 9(2).                                
005200 01  WS-DAGENS-DATUM-AAVV        PIC 9(4)    VALUE ZERO.                  
005300 01  WS-DAGENS-DAGNR             PIC 9       VALUE ZERO.                  
005400 01  WS-TIAAMMDD-IN              PIC 9(6)    VALUE ZERO.                  
005500 01  WS-TIAAMMDD-IN-CURR         PIC 9(6)    VALUE ZERO.                  
005600*                                                                         
005700 01  WS-LTDATE-TIAAVV            PIC 9(4)    VALUE ZERO.                  
005800 01  WS-LTDATE-AAMMDD            PIC 9(6)    VALUE ZERO.                  
005900 01  WS-DAT-TID                  PIC 9       VALUE ZERO.                  
006000 01  WS-TIME                     PIC 9(8)    VALUE ZERO.                  
006100 01  WS-TIDATE2.                                                          
006200     03 WS-TIDATE2-LTDATE        PIC X(6)    VALUE ZERO.                  
006300     03 FILLER                   PIC X(14)   VALUE ZERO.                  
006400*                                                                         
006500 01  DAGENS-DATUM-CENTURY        PIC 9(8)    VALUE ZERO.                  
006600 01  WS-TIPBDAT                  PIC S9(5)   VALUE ZERO COMP-3.           
006700*                                                                         
006800 01  INDX-L                      PIC S9(3)   VALUE +0   COMP SYNC.        
006900 01  INDX1-L                     PIC S9(3)   VALUE +0   COMP SYNC.        
007000 01  INDX2-L                     PIC S9(3)   VALUE +0   COMP SYNC.        
007100 01  IX-V                        PIC S9(3)   VALUE ZERO COMP SYNC.        
007200 01  IX-VECKA-MAX                PIC S9(3)   VALUE +156 COMP SYNC.        
007300 01  INDEX-ONE                   PIC S9(3)   VALUE +1   COMP SYNC.        
007400 01  LAST-IX                     PIC 9(3)    VALUE ZERO.                  
007500*                                                                         
007600 01  WS-FLFLYG                   PIC X       VALUE 'N'.                   
007700 01  WS-KVDAYS                   PIC S9(9)   VALUE ZERO COMP SYNC.        
007800 01  WS-KVDAYS-SEVEN             PIC S9(9)   VALUE +7   COMP SYNC.        
007900 01  WS-KVDAYS-ONE               PIC S9(9)   VALUE +1   COMP SYNC.        
008000 01  WS-LEADTIME-WEEKS           PIC 9(3)    VALUE ZERO.                  
008100 01  WS-LEADTIME-WEEKS-PLUS1     PIC 9(3)    VALUE ZERO.                  
008200 01  WS-REST-DAYS                PIC 9(2)    VALUE ZERO.                  
008300 01  WS-REST-DAYS-ADJ            PIC 9(2)    VALUE ZERO.                  
008400 01  WS-REST-DAYS-CURR           PIC 9(9)    VALUE ZERO.                  
008500 01  WS-KVDAYS-LT                PIC 9(9)    VALUE ZERO.                  
008600 01  WS-FACTOR-PER-V             PIC S9V9(2) VALUE +4.33 COMP-3.          
008700*                                                                         
008800 01  WS-TIAAVVD-L222             PIC 9(5)    VALUE ZERO.                  
008900 01  FILLER REDEFINES WS-TIAAVVD-L222.                                    
009000     03  WS-TIAAVV-L222          PIC 9(4).                                
009100     03  WS-TID-L222             PIC 9(1).                                
009200 01  WS-TIAAVV-L222-CURR         PIC 9(4).                                
009300*                                                                         
009400 01  WS-NONEED-DAYS              PIC 9       VALUE ZERO.                  
009500 01  W-DAGAR-KVAR                PIC 9       VALUE ZERO.                  
009600 01  WS-LAST-MINUS-NEED          PIC S9(7)V9(2)                           
009700                                             VALUE ZERO COMP-3.           
009800 01  WS-DAY-NEED-LAST            PIC S9(7)V9(2)                           
009900                                             VALUE ZERO COMP-3.           
010000 01  WS-LEDTIDSBEHOV             PIC S9(7)V9(2)                           
010100                                             VALUE ZERO COMP-3.           
010200 01  WS-DEMAND-WEEKLY            PIC S9(7)V9(2)                           
010300                                             VALUE ZERO COMP-3.           
010400 01  WS-DEMAND-WEEKLY-CLNDR      PIC S9(7)V9(2)                           
010500                                             VALUE ZERO COMP-3.           
010600 01  W-VECKODEL                  PIC S9(1)V9(2)                           
010700                                             VALUE ZERO COMP-3.           
010800*                                                                         
010900 01  SW-LOCAL-PART               PIC X       VALUE 'N'.                   
011000     88 LOCAL-PART-JA                        VALUE 'J'.                   
011100     88 LOCAL-PART-NEJ                       VALUE 'N'.                   
011200*                                                                         
011300 01  SW-FIRST-EXECUTE            PIC X(1)    VALUE 'J'.                   
011400     88 FIRST-RUN-JA                         VALUE 'J'.                   
011500     88 FIRST-RUN-NEJ                        VALUE 'N'.                   
011600*                                                                         
011700 01  SW-WEEK-DAY-CHECK           PIC X       VALUE 'N'.                   
011800     88 WEEK-FIRST-DAY-JA                    VALUE 'J'.                   
011900     88 WEEK-FIRST-DAY-NEJ                   VALUE 'N'.                   
012000*                                                                         
012100 01  SW-REFILL-DC                PIC X       VALUE 'J'.                   
012200     88 REFILL-DC-FOUND                      VALUE 'J'.                   
012300     88 REFILL-DC-MISSING                    VALUE 'N'.                   
012400*                                                                         
012500 01  SW-LT-DC-SRCH               PIC X       VALUE 'N'.                   
012600     88 LT-DC-SRCH-JA                        VALUE 'J'.                   
012700     88 LT-DC-SRCH-NEJ                       VALUE 'N'.                   
012800*                                                                         
012900 01  SW-GET-LTID-BEHOV           PIC X       VALUE 'N'.                   
013000     88 GET-LTID-BEHOV-JA                    VALUE 'J'.                   
013100*                                                                         
013200 01  WDK711-ACCESS-STATUS        PIC X       VALUE 'N'.                   
013300     88 WDK711-FOUND                         VALUE 'J'.                   
013400                                                                          
013500 01  W222-BEHOV-TAB.                                                      
013600     03  W-BEHOV-LT              OCCURS 156.                              
013700         05  W-BEHOV-LT-VECKA    PIC S9(8)V9(2)                           
013800                                             VALUE ZERO COMP-3.           
013900         05  W-BEHOV-LT-TOT      PIC S9(8)V9(2)                           
014000                                             VALUE ZERO COMP-3.           
014100                                                                          
014200 01  W222-BEHOV-TAB-INIT.                                                 
014300     03  FILLER                  OCCURS 156.                              
014400         05  FILLER              PIC S9(8)V9(2)                           
014500                                             VALUE ZERO COMP-3.           
014600         05  FILLER              PIC S9(8)V9(2)                           
014700                                             VALUE ZERO COMP-3.           
014800*                                                                         
014900 01  FILLER               PIC X(16)   VALUE 'B616-TABELL'.                
015000 01  MAX-B601-IX          PIC S9(4)   COMP SYNC VALUE +110.               
015100 01  MAX-B616-IX          PIC S9(4)   COMP SYNC VALUE +99.                
015200 01  B616-TABELL.                                                         
015300     03  TAB-B601  OCCURS 110 ASCENDING KEY IS TAB-DCS-IDDC               
015400                              INDEXED BY B601-IX.                         
015500*      05  -COPY WDB601  -PRE TAB-                                        
015600       05  TAB-B616  OCCURS  99 ASCENDING KEY IS TAB-REF-IDDC-REF         
015700                                INDEXED BY B616-IX.                       
015800*          07  -COPY WDB616  -PRE TAB-                                    
015900*                                                                         
016000                                                                          
016100**** GENERAL SUBROUTINE                                                   
016200 01  DYNAMISKA-SUBPROGRAM.                                                
016300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
016600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016800     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
016900     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
017000     03  W271UTIL                PIC X(8)    VALUE 'W271UTIL'.            
017100                                                                          
017200*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
017300*01 -COPY WDATAREA                                                        
017400     EJECT                                                                
017500*   --- PARAMETRAR TILL SUBPROGRAM W005INIT                               
017600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
017700*01 -COPY WMSGINIT                                                        
017800     EJECT                                                                
017900                                                                          
018000*    ---PARAMETRAR TILL WZ20DAYS                                          
018100                                                                          
018200 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
018300*01  -COPY WZ20DAYS                                                       
018400     EJECT                                                                
018500 01  FILLER                      PIC X(16)   VALUE 'W271UTIL'.            
018600*    ---PARAMETRAR TILL W271UTIL                                          
018700*01 -COPY W271UTIL                                                        
018800     EJECT                                                                
018900                                                                          
019000* VARIABLES TO SUBPROGRAM W009VADD                                        
019100 01  DATUM-AAVV                  PIC S9(5)   COMP-3.                      
019200 01  ANTAL-VECKOR                PIC S9(3)   COMP-3.                      
019300     EJECT                                                                
019400*                                                                         
019500*   --- PARAMETRAR TILL ABEND                                             
019600                                                                          
019700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
019800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
019900     SKIP2                                                                
020000 01  FELTEXT.                                                             
020100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
020200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
020300     EJECT                                                                
020400*                                                                         
020500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020600*                                                                         
020700 01  NYCKLAR-TILL-DLI.                                                    
020800     03  W-IDARTNR-X.                                                     
020900         05  W-IDARTNR           PIC S9(9)   COMP-3.                      
021000     03  W-IDDC-X.                                                        
021100         05  W-IDDC              PIC X(2)    VALUE ZERO.                  
021200     03  W-KDSEGKEY-X.                                                    
021300         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
021400     03  W-IDDC-B6-X.                                                     
021500         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
021600     03  W-IDDC-B616-X.                                                   
021700         05  W-IDDC-B616         PIC X(2)    VALUE SPACE.                 
021800                                                                          
021900*    --- STATUS-KOD FRÅN IMS                                              
022000 01  IMS-WS.                                                              
022100     03  FILLER                  PIC X(8)    VALUE 'IMS-WS  '.            
022200                                                                          
022300*                            *** STATUSKOD FRÅN IMS                       
022400     03 STATUS-WS                PIC XX.                                  
022500         88  SEGMENT-FOUND                   VALUE '  '.                  
022600         88  SEGMENT-MISSING                 VALUE 'GE' 'GB'.             
022700                                                                          
022800     03  GODK-STATUSKODER.                                                
022900         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.            
023000                                                                          
023100     03  SSA1                    PIC X(128).                              
023200     03  SSA2                    PIC X(128).                              
023300     03  SSA3                    PIC X(128).                              
023400     EJECT                                                                
023500                                                                          
023600*    --- IMS FUNKTIONSKODER                                               
023700*01  -COPY W0003                                                          
023800     EJECT                                                                
023900                                                                          
024000*    --- DLI INPUT-OUTPUT AREA                                            
024100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
024200 01  DLI-IO-AREA-WDK601.                                                  
024300*    03  -COPY WDK601                                                     
024400     EJECT                                                                
024500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
024600 01  DLI-IO-AREA-WDK611.                                                  
024700*    03  -COPY WDK611                                                     
024800     EJECT                                                                
024900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK701'.             
025000 01  DLI-IO-AREA-WDK701.                                                  
025100*    03  -COPY WDK701                                                     
025200     EJECT                                                                
025300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
025400 01  DLI-IO-AREA-WDK711.                                                  
025500*    03  -COPY WDK711                                                     
025600     EJECT                                                                
025700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB601'.             
025800 01  DLI-IO-AREA-WDB601.                                                  
025900*    03  -COPY WDB601                                                     
026000     EJECT                                                                
026100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB616'.             
026200 01  DLI-IO-AREA-WDB616.                                                  
026300*    03  -COPY WDB616                                                     
026400     EJECT                                                                
026500 LINKAGE SECTION.                                                         
026600                                                                          
026700*01 -COPY W271UTUP                                                        
026800     EJECT                                                                
026900*01  -COPY W0008  -PRE WDK7-                                              
027000     05  FILLER                  PIC X.                                   
027100     EJECT                                                                
027200*01  -COPY W0008  -PRE WDB6-                                              
027300     05  FILLER                  PIC X.                                   
027400     EJECT                                                                
027500 01  UTIL-WDK6-PCB               PIC X.                                   
027600 01  UTIL-WDK7-PCB               PIC X.                                   
027700 01  UTIL-WDB6-PCB               PIC X.                                   
027800     EJECT                                                                
027900                                                                          
028000 PROCEDURE DIVISION USING UTUP-W271UTUP  WDK7-PCB                         
028100                          WDB6-PCB                                        
028200                          UTIL-WDK6-PCB                                   
028300                          UTIL-WDK7-PCB                                   
028400                          UTIL-WDB6-PCB.                                  
028500                                                                          
028600     PERFORM A-INIT                                                       
028700                                                                          
028800***  KDCALL 001 AND 002 IS USED IN W272REFL                               
028900***  TO MAINTAIN DISTINCT FUNCTIONALITY, MUTUALLY                         
029000***  EXCLUSIVE KDCALL IS USED IN EACH OF THESE PROGRAMS.                  
029100***  KDCALL 003 IS USED TO GET LEADTIME ADJUSTED DEMAND FOR 3 YRS.        
029110***  KDCALL 004 IS USED TO GET LEADTIME ADJUSTED FOR CURRENT WEEK.        
029200                                                                          
029300     IF UTUP-KDSVAR-OK                                                    
029400        EVALUATE UTUP-KDCALL                                              
029500          WHEN 003                                                        
029600            PERFORM B-GET-LEDTID-BEHOV-XDC                                
029700          WHEN 004                                                        
029800            PERFORM B-GET-LEDTID-BEHOV-XDC                                
029900          WHEN OTHER                                                      
030000            SET UTUP-KDSVAR-FEL   TO TRUE                                 
030100            MOVE 'ERROR KDCALL'   TO UTUP-TEXT                            
030200            DISPLAY 'W271UTUP ERROR KDCALL'                               
030300        END-EVALUATE                                                      
030400     END-IF                                                               
030500                                                                          
030600     SET FIRST-RUN-NEJ            TO TRUE                                 
030700     MOVE ZERO TO RETURN-CODE                                             
030800     GOBACK                                                               
030900     .                                                                    
031000     EJECT                                                                
031100                                                                          
031200 A-INIT SECTION.                                                          
031300                                                                          
031400     MOVE SPACES                     TO UTUP-KDSVAR                       
031500     MOVE NEJ                        TO SW-WEEK-DAY-CHECK                 
031600                                        SW-LOCAL-PART                     
031700     MOVE JA                         TO SW-REFILL-DC                      
031800*                                                                         
031900***  GET CURRENT DATE AND CALCULATE CURRENT DAY NUMBER AND WEEK           
032000*                                                                         
032100     IF FIRST-RUN-JA                                                      
032200        MOVE FUNCTION CURRENT-DATE(3:6)                                   
032300                                     TO DAGENS-DATUM                      
032400        MOVE DAGENS-DATUM            TO DAT-I-TIDATUM                     
032500        MOVE 'AAMMDD'                TO DAT-KDDATFORM                     
032600                                                                          
032700        CALL WDATKONV   USING DAT-KDDATFORM DAT-I-TIDATUM                 
032800                              DAT-O-TIDATUM DAT-KDSVAR                    
032900                                                                          
033000        IF DAT-KDSVAR-OK                                                  
033100          MOVE DAT-TIAAVVD           TO WS-TIPBDAT                        
033200                                        WS-TIAAVVD-L222                   
033300          MOVE DAT-TIAAVV-GRP        TO WS-TIAAVV-L222                    
033400                                        WS-TIAAVV-L222-CURR               
033500          MOVE DAT-TIAAMMDD          TO WS-TIAAMMDD-IN-CURR               
033600          MOVE DAT-TIAAVV-GRP        TO WS-DAGENS-DATUM-AAVV              
033700          MOVE DAT-TID               TO WS-DAGENS-DAGNR                   
033800        ELSE                                                              
033900          STRING ' FEL FRÅN WDATKONV IN A-INIT ' STATUS-WS                
034000          DELIMITED BY SIZE INTO FELTEXT                                  
034100          CALL FELLOG                                                     
034200        END-IF                                                            
034300*                                                                         
034400***     LOAD WORKING TABLE WITH ALL DC INFO FROM WDB6                     
034500*                                                                         
034600        PERFORM AA-LOAD-TAB-WDB6                                          
034700     END-IF                                                               
034800*                                                                         
034900     MOVE WS-TIAAMMDD-IN-CURR        TO WS-TIAAMMDD-IN                    
035000*                                                                         
035100     IF  UTUP-IDARTNR    IS NUMERIC                                       
035200     AND UTUP-IDARTNR     > ZERO                                          
035300         CONTINUE                                                         
035400     ELSE                                                                 
035500         SET UTUP-KDSVAR-FEL         TO TRUE                              
035600         MOVE 'INVALID PART NO'      TO UTUP-TEXT                         
035700     END-IF                                                               
035800*                                                                         
035900***  CHECK IF WE SHOULD USE 5 DAY WEEK OR 6 DAY WEEK.                     
036000*                                                                         
036100     IF UTUP-KDSVAR-OK                                                    
036200        PERFORM S21-SRCH-B601-IDLAND                                      
036300     END-IF                                                               
036400     .                                                                    
036500     EJECT                                                                
036600                                                                          
036700 AA-LOAD-TAB-WDB6  SECTION.                                               
036800                                                                          
036900     SET B601-IX                  TO +1                                   
037000     SET B616-IX                  TO +1                                   
037100     PERFORM IMS-GN-WDB601                                                
037200                                                                          
037300     PERFORM UNTIL SEGMENT-MISSING                                        
037400       IF B601-IX  <= MAX-B601-IX                                         
037500          MOVE DCS-WDB601         TO TAB-DCS-WDB601 (B601-IX)             
037600*                                                                         
037700          SET B616-IX             TO +1                                   
037800          PERFORM IMS-GNP-WDB616                                          
037900          PERFORM UNTIL SEGMENT-MISSING                                   
038000            MOVE REF-WDB616                                               
038100                             TO TAB-REF-WDB616 (B601-IX,B616-IX)          
038200            SORT TAB-B616 (B601-IX)  ASCENDING  TAB-REF-IDDC-REF          
038300            PERFORM IMS-GNP-WDB616                                        
038400            SET B616-IX        UP BY +1                                   
038500          END-PERFORM                                                     
038600          PERFORM IMS-GN-WDB601                                           
038700          SET B601-IX          UP BY +1                                   
038800       ELSE                                                               
038900          MOVE 'DC TABLE END, MAX LIM REACHED' TO ERR-TXT-STR             
039000          DISPLAY ERR-TXT-STR                                             
039100          CALL FELLOG                                                     
039200       END-IF                                                             
039300     END-PERFORM                                                          
039400                                                                          
039500     SORT TAB-B601         ASCENDING TAB-DCS-IDDC                         
039600     .                                                                    
039700     EJECT                                                                
039800                                                                          
039900 B-GET-LEDTID-BEHOV-XDC SECTION.                                          
040000                                                                          
040100     MOVE NEJ                        TO SW-GET-LTID-BEHOV                 
040200     IF UTUP-KDCALL = 004                                                 
040300        MOVE JA                      TO SW-GET-LTID-BEHOV                 
040400     END-IF                                                               
040500                                                                          
040600     PERFORM S01-CALC-DEMAND-XDC                                          
040700     .                                                                    
040800     EJECT                                                                
040900                                                                          
041000 S01-CALC-DEMAND-XDC SECTION.                                             
041100                                                                          
041200     INITIALIZE UTUP-UTDATA                                               
041300     MOVE +1                         TO INDX-L                            
041400                                        IX-V                              
041500     PERFORM S01A-LEDTID-ADJMT                                            
041600     IF WDK711-FOUND                                                      
041700        INITIALIZE UTIL-W271UTIL                                          
041800        MOVE UTUP-IDARTNR            TO UTIL-IDARTNR                      
041900        MOVE UTUP-IDDC               TO UTIL-IDDC                         
042000        MOVE UTUP-IDDC-REF           TO UTIL-IDDC-REF                     
042100        MOVE NEJ                     TO UTIL-FLSIM                        
042200        MOVE WS-DAGENS-DAGNR         TO WS-TID-L222                       
042300        MOVE 005                     TO UTIL-KDCALL                       
042400*                                                                         
042500****    IF DEMAND SIMULATION REQUESTED FROM 2352, 2372 OR 2382            
042600*                                                                         
042700        IF UTUP-FLSIM  = JA                                               
042800           MOVE UTUP-FLSIM           TO UTIL-FLSIM                        
042900           MOVE UTUP-KVPB-REF        TO UTIL-KVPB-REF                     
043000           MOVE UTUP-KVPBREOI        TO UTIL-KVPBREOI                     
043100        END-IF                                                            
043200*                                                                         
043300        CALL W271UTIL             USING UTIL-W271UTIL                     
043400                                        UTIL-WDK6-PCB                     
043500                                        UTIL-WDK7-PCB                     
043600                                        UTIL-WDB6-PCB                     
043700*                                                                         
043800        MOVE UTIL-FLPB-JUST          TO UTUP-FLPB-JUST                    
043900        MOVE UTIL-FLFFC              TO UTUP-FLFFC                        
044000        MOVE UTIL-FLSEAS             TO UTUP-FLSEAS                       
044100*                                                                         
044200        PERFORM UNTIL IX-V > IX-VECKA-MAX                                 
044300          MOVE W222-BEHOV-TAB-INIT   TO W222-BEHOV-TAB                    
044400          MOVE ZERO                  TO WS-LEDTIDSBEHOV                   
044500                                                                          
044600          MOVE +1                    TO INDX-L                            
044700          MOVE IX-V                  TO INDX1-L                           
044800*                                                                         
044900***       FIELD W-BEHOV-LT-TOT SHOULD HAVE CUMULATIVE TOTAL               
045000***       LEADTIME TIME DEMAND                                            
045100***       FIELD UTUP-KVBEHOV-V SHOULD HAVE ONLY WEEKLY DEMAND             
045200***       OF THE START WEEK                                               
045300*                                                                         
045400          PERFORM UNTIL INDX-L > WS-LEADTIME-WEEKS-PLUS1                  
045500                                                                          
045600            IF UTIL-KDSVAR-OK                                             
045700               PERFORM S01B-CONV-DEMAND-WEEKLY                            
045800            END-IF                                                        
045900*                                                                         
046000            ADD +1                   TO INDX-L                            
046100                                        INDX1-L                           
046200          END-PERFORM                                                     
046300****                                                                      
046400****                                                                      
046500          MOVE W-BEHOV-LT-TOT (INDX-L - 1)                                
046600                                     TO WS-LEDTIDSBEHOV                   
046700          PERFORM S01C-LAST-WEEK-NEED                                     
046800          COMPUTE WS-LEDTIDSBEHOV     = WS-LEDTIDSBEHOV                   
046900                                      - WS-LAST-MINUS-NEED                
047000*                                                                         
047100          MOVE WS-LEDTIDSBEHOV       TO UTUP-LT-BEHOV-V (IX-V)            
047200*                                                                         
047300***       LEADTID-BEHOV IS CALCULATED STARTING FIRST WEEK                 
047400*                                                                         
047500          IF IX-V = INDEX-ONE                                             
047600             MOVE WS-LEDTIDSBEHOV    TO UTUP-LEADTID-BEHOV                
047700          END-IF                                                          
047800*                                                                         
047900***       IF LEADTID-BEHOV IS REQUESTED, END THE LOOP                     
048000*                                                                         
048100          IF GET-LTID-BEHOV-JA                                            
048200             MOVE IX-VECKA-MAX       TO IX-V                              
048300          END-IF                                                          
048400*                                                                         
048500***       CURRENT WEEK STARTS FROM CURRENT WEEKDAY                        
048600***       ALL SUBSEQUENT WEEKS START FROM MONDAY.                         
048700***       WEEK-FIRST-DAY IS SET TO YES FOR SUBSEQUENT WEEKS               
048800*                                                                         
048900          IF IX-V = INDEX-ONE                                             
049000***          BELOW EXECUTED ONLY ONCE AS ALL WEEKS STARTS ON              
049100***          MONDAY AND LEADTIME ADJUSTED END WEEK IS ALWAYS              
049200***          ON THE SAME DAY                                              
049300*                                                                         
049400             SET WEEK-FIRST-DAY-JA   TO TRUE                              
049500             MOVE WS-TIAAVV-L222     TO DATUM-AAVV                        
049600             MOVE 1                  TO ANTAL-VECKOR                      
049700                                                                          
049800             CALL W009VADD        USING DATUM-AAVV                        
049900                                        ANTAL-VECKOR                      
050000                                                                          
050100             MOVE DATUM-AAVV         TO WS-TIAAVV-L222                    
050200             MOVE 1                  TO WS-TID-L222                       
050300*                                                                         
050400***          CONVERT DATE FORMAT FROM AAVVD TO AAMMDD                     
050500*                                                                         
050600             MOVE WS-TIAAVVD-L222    TO DAT-I-TIDATUM                     
050700             MOVE 'AAVVD'            TO DAT-KDDATFORM                     
050800                                                                          
050900             CALL WDATKONV   USING DAT-KDDATFORM DAT-I-TIDATUM            
051000                                   DAT-O-TIDATUM DAT-KDSVAR               
051100                                                                          
051200             IF DAT-KDSVAR-OK                                             
051300                MOVE DAT-TIAAMMDD    TO WS-TIAAMMDD-IN                    
051400             ELSE                                                         
051500               STRING ' FEL FRÅN DATUMRUTIN WDATKONV-2 ' STATUS-WS        
051600               DELIMITED BY SIZE INTO FELTEXT                             
051700               CALL FELLOG                                                
051800             END-IF                                                       
051900*                                                                         
052000***          CALL BELOW SECTION TO GET LEADTIME TIME ADJ LAST WEEK        
052100***          START WEEK IS WEEK + 1.                                      
052200*                                                                         
052300             PERFORM S01AA-LEDTID-CALC                                    
052400          END-IF                                                          
052500          ADD +1                     TO IX-V                              
052600        END-PERFORM                                                       
052700     ELSE                                                                 
052800        SET UTUP-KDSVAR-FEL          TO TRUE                              
052900        MOVE 'ERROR WDK711 ACCESS '                                       
053000                                     TO UTUP-TEXT                         
053100        DISPLAY 'W271UTUP ERROR WDK711 ACCESS'                            
053200     END-IF                                                               
053300     .                                                                    
053400     EJECT                                                                
053500 S01A-LEDTID-ADJMT  SECTION.                                              
053600                                                                          
053700***  CALCULATE THE LEADTIME ADJUSTED WEEK AND DAY                         
053800***  THE LEADTIME ADJUSTED DAY IS USED TO ADJUST THE LAST                 
053900***  WEEK NEED                                                            
054000***  FOR LOCALLY PURCHASED PARTS THE LEADTIME IS PASSED                   
054100***  FROM THE CALLING PROGRAM AS THE INFORMATION IS NOT                   
054200***  AVAILBLE IN WDB6                                                     
054300*                                                                         
054400     MOVE NEJ                      TO SW-LT-DC-SRCH                       
054500     MOVE UTUP-IDARTNR             TO W-IDARTNR                           
054600     MOVE UTUP-IDDC                TO W-IDDC                              
054700     PERFORM IMS-GU-WDK711                                                
054800     IF SEGMENT-FOUND                                                     
054900        IF SLAG-IDDC-REF  = UTUP-IDDC-REF                                 
055000           MOVE SLAG-FLFLYG        TO WS-FLFLYG                           
055100           SET WDK711-FOUND        TO TRUE                                
055200           IF SLAG-IDDC-REF    NOT  > SPACES                              
055300              SET LOCAL-PART-JA    TO TRUE                                
055400           END-IF                                                         
055500        END-IF                                                            
055600     END-IF                                                               
055700*                                                                         
055800     IF WDK711-FOUND                                                      
055900        PERFORM S01AA-LEDTID-CALC                                         
056000     END-IF                                                               
056100     .                                                                    
056200     EJECT                                                                
056300                                                                          
056400 S01AA-LEDTID-CALC  SECTION.                                              
056500                                                                          
056600***  IF NOT LOCALLY PURCHASED, GET LEADTIME FROM WDB6                     
056700***  IF LOCALLY PURCHASED, LEADTIME SHOULD BE PASSED                      
056800***  AS INPUT FROM CALLING PROGRAM                                        
056900*                                                                         
057000*    SW-LT-DC-SRCH FACILATES SEARCHING FOR LT ONLY ONCE PER DC            
057100*                                                                         
057200     IF LT-DC-SRCH-NEJ                                                    
057300        IF LOCAL-PART-NEJ                                                 
057400           PERFORM S21-SRCH-B601-IDLAND                                   
057500           IF REFILL-DC-FOUND                                             
057600              IF WS-FLFLYG = JA                                           
057700                 MOVE TAB-REF-KVDLTID-AIRETA (B601-IX,B616-IX)            
057800                                      TO WS-KVDAYS                        
057900              ELSE                                                        
058000                 MOVE TAB-REF-KVDLTID-TOT (B601-IX,B616-IX)               
058100                                      TO WS-KVDAYS                        
058200              END-IF                                                      
058300           ELSE                                                           
058400              MOVE ZERO               TO WS-KVDAYS                        
058500           END-IF                                                         
058600        ELSE                                                              
058700           MOVE UTUP-LEADTIME         TO WS-KVDAYS                        
058800        END-IF                                                            
058900        SET LT-DC-SRCH-JA             TO TRUE                             
059000     END-IF                                                               
059100*                                                                         
059200     IF WS-KVDAYS  >  ZERO                                                
059300        DIVIDE WS-KVDAYS   BY WS-KVDAYS-SEVEN                             
059400                       GIVING WS-LEADTIME-WEEKS                           
059500                    REMAINDER WS-REST-DAYS                                
059600        MOVE WS-KVDAYS                TO DAYS-KVDAYS                      
059700     ELSE                                                                 
059800        MOVE ZERO                     TO DAYS-KVDAYS                      
059900                                         WS-LEADTIME-WEEKS                
060000                                         WS-REST-DAYS                     
060100     END-IF                                                               
060200                                                                          
060300***  IF AUTO REFILL IS YES, THEN REFILL HAPPENS ON CURRENT DAY            
060400***  SO ONE DAY SHOULD BE REDUCED FROM LEADTIME                           
060500*                                                                         
060600     IF DAYS-KVDAYS            > ZERO                                     
060700        IF SLAG-FLREFBEO       = JA                                       
060800           COMPUTE DAYS-KVDAYS         = (DAYS-KVDAYS - 1)                
060900        END-IF                                                            
061000     END-IF                                                               
061100     MOVE DAYS-KVDAYS                 TO WS-KVDAYS-LT                     
061200*                                                                         
061300***  CALCULATE HOW MANY WEEKS COMPRISES THE LEADTIME.                     
061400***  ALSO CALCULATION TAKES INTO ACCOUNT CURRENT DAY IN THE WEEK          
061500***  AND ADJUSTS ACCORDINGLY TO DETERMINE IF WE END UP                    
061600***  IN A NEW WEEK (IF START DATE IS CURRENT WEEK).                       
061700*                                                                         
061800     IF WEEK-FIRST-DAY-NEJ                                                
061900        COMPUTE WS-REST-DAYS = WS-REST-DAYS + WS-DAGENS-DAGNR             
062000     END-IF                                                               
062100                                                                          
062200     IF SLAG-FLREFBEO       = JA                                          
062300        IF WS-REST-DAYS > ZERO                                            
062400           COMPUTE WS-REST-DAYS = WS-REST-DAYS - 1                        
062500        END-IF                                                            
062600     END-IF                                                               
062700                                                                          
062800     PERFORM S02-GET-LT-ADJ-WEEK                                          
062900     .                                                                    
063000     EJECT                                                                
063100 S01B-CONV-DEMAND-WEEKLY SECTION.                                         
063200                                                                          
063300***  DEMAND FROM W271UTIL IS ALWAYS FULL WEEK DEMAND                      
063400***  IF CURRENT WEEK, THEN DEMAND CALCULATED FOR REMAINING                
063500***  DAYS OF THE WEEK                                                     
063600***  FOR TOTAL LEADTIME TIME DEMAND, THE CURRENT WEEK DEMAND              
063700***  IS ADJUSTED CONSIDERING 7 CALENDER DAYS                              
063800***  WEEKLY DEMAND CALCULATION IS BASED ON WORKDAYS                       
063900*                                                                         
064000***  IF DEMAND NEEDED FROM W271UTIL EXCEEDS 156 WEEKS                     
064100***  THEN USE 156TH OCCURANCE. THERE WILL NO CHANGE IN WEEKLY             
064200***  DEMAND BEYOND 156 WEEEKS IN FUTURE.                                  
064300*                                                                         
064500     IF INDX1-L > IX-VECKA-MAX                                            
064600        MOVE IX-VECKA-MAX             TO INDX2-L                          
064700     ELSE                                                                 
064800        MOVE INDX1-L                  TO INDX2-L                          
064900     END-IF                                                               
065000*                                                                         
065100     MOVE ZERO                        TO WS-DEMAND-WEEKLY                 
065300     COMPUTE WS-DEMAND-WEEKLY                                             
065400                    = (UTIL-PBTOT-V (INDX2-L) / WS-FACTOR-PER-V)          
065500     MOVE WS-DEMAND-WEEKLY            TO WS-DEMAND-WEEKLY-CLNDR           
065600*                                                                         
065700***  IF CURRENT WEEK AND FIRST DAY FLAG IS OFF, THEN ADJUST               
065800***  DEMAND FOR THE REMAINING DAYS OF THE WEEK                            
065900*                                                                         
066000     IF INDX-L = 1            AND                                         
066100        WEEK-FIRST-DAY-NEJ                                                
066200        IF 6ARBDAG-SW = JA                                                
066300           IF WS-DAGENS-DAGNR = 6                                         
066400           OR WS-DAGENS-DAGNR > 6                                         
066500              MOVE ZERO               TO WS-DEMAND-WEEKLY                 
066600           ELSE                                                           
066700              COMPUTE WS-REST-DAYS-CURR                                   
066800                                       = 6 - WS-DAGENS-DAGNR              
066900              COMPUTE W-VECKODEL       = WS-REST-DAYS-CURR / 6            
067000              COMPUTE WS-DEMAND-WEEKLY =                                  
067100                          WS-DEMAND-WEEKLY * W-VECKODEL                   
067200           END-IF                                                         
067300        ELSE                                                              
067400           IF WS-DAGENS-DAGNR = 5                                         
067500           OR WS-DAGENS-DAGNR > 5                                         
067600              MOVE ZERO               TO WS-DEMAND-WEEKLY                 
067700           ELSE                                                           
067800              COMPUTE W-DAGAR-KVAR     = 5 - WS-DAGENS-DAGNR              
067900              COMPUTE W-VECKODEL       = W-DAGAR-KVAR / 5                 
068000              COMPUTE WS-DEMAND-WEEKLY =                                  
068100                                 WS-DEMAND-WEEKLY * W-VECKODEL            
068200           END-IF                                                         
068300        END-IF                                                            
068400*                                                                         
068500***     CALCULATE THE CURRENT WEEK DEMAND USING CALENDER DAYS             
068600***     ADD 1 DAY TO ACCOUNT FOR CURRENT DAY IF AUTO-REF IS YES           
068700***     THIS WEEKLY DEMAND BASED ON CALENDER DAYS IS USED FOR             
068800***     TOTAL LEADTIME TIME DEMAND                                        
068900*                                                                         
069000        COMPUTE WS-REST-DAYS-CURR  = (7 - WS-DAGENS-DAGNR) + 1            
069100        IF WS-REST-DAYS-CURR      >= WS-KVDAYS                            
069200           IF WS-REST-DAYS-CURR    > WS-KVDAYS AND                        
069300              SLAG-FLREFBEO NOT    = JA                                   
069400                COMPUTE WS-REST-DAYS-CURR                                 
069500                                   = WS-KVDAYS + WS-KVDAYS-ONE            
069600           ELSE                                                           
069700              MOVE WS-KVDAYS      TO WS-REST-DAYS-CURR                    
069800           END-IF                                                         
069900        END-IF                                                            
070000                                                                          
070100        COMPUTE W-VECKODEL = (WS-REST-DAYS-CURR / WS-KVDAYS-SEVEN)        
070200                                                                          
070300        COMPUTE WS-DEMAND-WEEKLY-CLNDR ROUNDED                            
070500                     = ((UTIL-PBTOT-V (INDX2-L) / WS-FACTOR-PER-V)        
070600                                 * W-VECKODEL)                            
070700     END-IF                                                               
070800*                                                                         
070900     MOVE WS-DEMAND-WEEKLY            TO W-BEHOV-LT-VECKA(INDX-L)         
071000     MOVE WS-DEMAND-WEEKLY-CLNDR      TO W-BEHOV-LT-TOT(INDX-L)           
071100                                                                          
071200     IF INDX-L  = INDEX-ONE                                               
071300        IF IX-V = INDEX-ONE                                               
071400           MOVE W-BEHOV-LT-VECKA(INDX-L)                                  
071500                                      TO UTUP-KVBEHOV-VECKA               
071600        END-IF                                                            
071700*                                                                         
071800        MOVE W-BEHOV-LT-VECKA(INDX-L)                                     
071900                                      TO UTUP-KVBEHOV-V (IX-V)            
072000     ELSE                                                                 
072100        ADD W-BEHOV-LT-TOT(INDX-L - 1)                                    
072200                                      TO W-BEHOV-LT-TOT(INDX-L)           
072300     END-IF                                                               
072400     .                                                                    
072500     EJECT                                                                
072600                                                                          
072700 S01C-LAST-WEEK-NEED SECTION.                                             
072800                                                                          
072900***  ADJUST LAST WEEK DEMAND BASED ON NUMBER OF DAYS TO BE                
073000***  CONSIRED IN THE LAST WEEK                                            
073100*                                                                         
073200     INITIALIZE WS-DAY-NEED-LAST                                          
073300                WS-NONEED-DAYS                                            
073400                WS-LAST-MINUS-NEED                                        
073500                                                                          
073600     IF  LAST-IX > 1                                                      
073700     OR (LAST-IX = 1 AND                                                  
073800         WEEK-FIRST-DAY-JA )                                              
073900        IF WS-REST-DAYS-ADJ > 0                                           
074000           IF WS-LEADTIME-WEEKS-PLUS1  > ZERO                             
074100              COMPUTE WS-NONEED-DAYS   = 7 - WS-REST-DAYS-ADJ             
074200              COMPUTE WS-DAY-NEED-LAST =                                  
074300                                 W-BEHOV-LT-VECKA (LAST-IX) / 7           
074400              COMPUTE WS-LAST-MINUS-NEED ROUNDED                          
074500                                       = (WS-NONEED-DAYS *                
074600                                          WS-DAY-NEED-LAST)               
074700           ELSE                                                           
074800              MOVE ZERO               TO WS-LAST-MINUS-NEED               
074900           END-IF                                                         
075000        END-IF                                                            
075100     END-IF                                                               
075200     .                                                                    
075300     EJECT                                                                
075400 S02-GET-LT-ADJ-WEEK  SECTION.                                            
075500                                                                          
075600*                                                                         
075700***  CALL SUB PGM TO ADD LEADTIME TO THE START DATE TO GET                
075800***  THE LEADTIME ADJUSTED DATE                                           
075900*                                                                         
076000     MOVE WS-TIAAMMDD-IN              TO DAYS-TIDATE1                     
076100     MOVE 'YYMMDD'                    TO DAYS-KDDATFMT1                   
076200     MOVE 'YYMMDD'                    TO DAYS-KDDATFMT2                   
076300     MOVE SPACE                       TO DAYS-TIDATE2                     
076400                                         DAYS-IDCALEND                    
076500                                         WS-TIDATE2                       
076600                                                                          
076700     CALL WZ20DAYS   USING DAYS-WZ20DAYS                                  
076800                                                                          
076900     IF DAYS-KDRC = 8                                                     
077000        STRING 'FEL VID ANROP TILL WZ20DAYS - SEC S02- '                  
077100                      DELIMITED BY SIZE INTO FELTEXT-STR                  
077200        DISPLAY FELTEXT                                                   
077300        CALL ABEND   USING RKOD-ABEND-UTAN-DUMP                           
077400     ELSE                                                                 
077600        MOVE DAYS-TIDATE2             TO WS-TIDATE2                       
077700        MOVE WS-TIDATE2-LTDATE        TO WS-LTDATE-AAMMDD                 
077800     END-IF                                                               
077900*                                                                         
078000***  CALL SUB PROGRAM TO GET THE LEADTIME ADJUSTED DATE                   
078100***  IN DIFFRENT FORMATS                                                  
078200*                                                                         
078300     MOVE 'AAMMDD'                    TO DAT-KDDATFORM                    
078400     MOVE WS-LTDATE-AAMMDD            TO DAT-I-TIDATUM                    
078500                                                                          
078600     CALL WDATKONV   USING DAT-KDDATFORM DAT-I-TIDATUM                    
078700                           DAT-O-TIDATUM DAT-KDSVAR                       
078800                                                                          
078900     IF DAT-KDSVAR-OK                                                     
079000                                                                          
079100        MOVE DAT-TIAAVV-GRP           TO WS-LTDATE-TIAAVV                 
079200        MOVE DAT-TID                  TO WS-DAT-TID                       
079300                                         WS-REST-DAYS-ADJ                 
079400     ELSE                                                                 
079500        STRING ' FEL FRÅN WDATKONV I W271UTUP'                            
079600               ' (SEC - S02- )'                                           
079700                        DELIMITED BY SIZE INTO FELTEXT-STR                
079800        DISPLAY FELTEXT                                                   
079900        CALL ABEND   USING RKOD-ABEND-UTAN-DUMP                           
080000     END-IF                                                               
080100                                                                          
080200     IF   WS-REST-DAYS     > ZERO                                         
080300     OR  (WS-REST-DAYS     = ZERO                                         
080400     AND  WS-REST-DAYS-ADJ < 7)                                           
080500         COMPUTE WS-LEADTIME-WEEKS-PLUS1                                  
080600                                       = WS-LEADTIME-WEEKS + 1            
080700         IF WS-REST-DAYS  > 7                                             
080800            COMPUTE WS-LEADTIME-WEEKS-PLUS1                               
080900                                 = WS-LEADTIME-WEEKS-PLUS1 + 1            
081000         END-IF                                                           
081100     ELSE                                                                 
081200       MOVE WS-LEADTIME-WEEKS         TO WS-LEADTIME-WEEKS-PLUS1          
081300     END-IF                                                               
081400                                                                          
081500     IF WS-LEADTIME-WEEKS-PLUS1     = ZERO                                
081600        MOVE 1                        TO WS-LEADTIME-WEEKS-PLUS1          
081700     END-IF                                                               
081800                                                                          
081900     IF WS-REST-DAYS-ADJ > 0                                              
082000        IF WS-LEADTIME-WEEKS-PLUS1  > ZERO                                
082100           MOVE WS-LEADTIME-WEEKS-PLUS1                                   
082200                                      TO LAST-IX                          
082300        END-IF                                                            
082400     END-IF                                                               
082500     .                                                                    
082600     EJECT                                                                
082700                                                                          
082800 S21-SRCH-B601-IDLAND SECTION.                                            
082900                                                                          
083000*                                                                         
083100***  BELOW NDCS AND SDCS HAS 6 WORKING DAYS                               
083200***  SWITCH 6ARBDAG-SW IS SET ACCORDINGLY                                 
083300*                                                                         
083400     MOVE NEJ                     TO 6ARBDAG-SW                           
083500     SEARCH ALL TAB-B601                                                  
083600       AT END                                                             
083700         MOVE 'DC NOT FOUND IN B601 TAB'    TO ERR-TXT-STR                
083800         DISPLAY ERR-TXT-STR                                              
083900         CALL FELLOG                                                      
084000       WHEN TAB-DCS-IDDC (B601-IX) = UTUP-IDDC                            
084100         IF (TAB-DCS-SDC  (B601-IX) AND                                   
084200            TAB-DCS-CHINA (B601-IX))                                      
084300         OR TAB-DCS-NDC-CN     (B601-IX)                                  
084400         OR TAB-DCS-JAPAN      (B601-IX)                                  
084500         OR TAB-DCS-ENGLAND    (B601-IX)                                  
084600         OR TAB-DCS-INDIA      (B601-IX)                                  
084700         OR TAB-DCS-EMIRATES   (B601-IX)                                  
084800            MOVE JA              TO 6ARBDAG-SW                            
084900         END-IF                                                           
085000         IF LOCAL-PART-NEJ  AND                                           
085100            UTUP-IDDC-REF > SPACES                                        
085200            PERFORM S21A-SRCH-B616-REFILL                                 
085300         END-IF                                                           
085400     END-SEARCH                                                           
085500     .                                                                    
085600     EJECT                                                                
085700                                                                          
085800 S21A-SRCH-B616-REFILL SECTION.                                           
085900                                                                          
086000*                                                                         
086100***  SEARCH REFILLING DC. WHEN MATCH, CORRESPONDING                       
086200***  LEAD TIME WILL BE USED IN CALCULATION OF LEADTIME DEMAND             
086300*                                                                         
086400     SEARCH ALL TAB-B616                                                  
086500       AT END                                                             
086600          MOVE NEJ               TO SW-REFILL-DC                          
086700       WHEN TAB-REF-IDDC-REF (B601-IX,B616-IX)                            
086800                                  = UTUP-IDDC-REF                         
086900          CONTINUE                                                        
087000     END-SEARCH                                                           
087100     .                                                                    
087200     EJECT                                                                
087300                                                                          
087400*****                                                                     
087500*****  IMS SECTIONER   ****                                               
087600*****                                                                     
087700 IMS-GN-WDB601  SECTION.                                                  
087800                                                                          
087900     MOVE 'WDB601  '          TO SSA1                                     
088000     MOVE '  GB'              TO GODK-STATUSKODER                         
088100     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-WDB601 SSA1               
088200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
088300     PERFORM IMS-STATUSKONTROLL                                           
088400     .                                                                    
088500     SKIP3                                                                
088600 IMS-GNP-WDB616    SECTION.                                               
088700                                                                          
088800     MOVE 'WDB616  '          TO SSA1                                     
088900     MOVE '  GE'              TO GODK-STATUSKODER                         
089000     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-WDB616 SSA1              
089100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
089200     PERFORM IMS-STATUSKONTROLL                                           
089300     .                                                                    
089400     SKIP3                                                                
089500 IMS-GU-WDK711 SECTION.                                                   
089600                                                                          
089700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
089800          DELIMITED BY SIZE INTO SSA1                                     
089900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
090000          DELIMITED BY SIZE INTO SSA2                                     
090100     MOVE '  GE'            TO GODK-STATUSKODER                           
090200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
090300     MOVE WDK7-STATUS-CODE  TO STATUS-WS                                  
090400     PERFORM IMS-STATUSKONTROLL                                           
090500     .                                                                    
090600     SKIP3                                                                
090700                                                                          
090800 IMS-STATUSKONTROLL SECTION.                                              
090900                                                                          
091000     SET STATUS-IX TO 1                                                   
091100     SEARCH GODK-STATUS                                                   
091200       AT END                                                             
091300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
091400         DELIMITED BY SIZE INTO FELTEXT                                   
091500         CALL FELLOG                                                      
091600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
091700         CONTINUE                                                         
091800     END-SEARCH                                                           
091900     .                                                                    
092000     EJECT                                                                
092100*    -COPY WY2000P1                                                       
