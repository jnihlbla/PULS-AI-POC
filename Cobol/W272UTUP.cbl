000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W272UTUP.                                                
000400 AUTHOR.         ARUP DATTA.                                              
000500 DATE-WRITTEN.   19/07/02.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SUBPROGRAM TO UPDATE IMS DB                                      
001000*        THIS PROGRAM ALSO READS THE DEMAND MODULE W2222 WITH             
001100*        KDCALL = 02                                                      
001200*                                                                         
001300*        THE SUBPROGRAM IS USED TO IMPLEMENT OBJECT ORIENTED              
001400*        PROGRAMMING                                                      
001500*        THE PROGRAM IS TO HANDLE COMMON UPDATES IN REFILL AREA           
001600*        WHEN CALLING THIS SUBPROGRAM CORRECT KDCALL VALUES               
001700*        NEEDS TO BE PASSED SO AS TO PROCESS THE REQUIRED                 
001800*        UPDATE FUNCTIONALITY                                             
001900*                                                                         
002000*                                                                         
002100*                                                                         
002200*        PROGRAM READS     WDK6                                           
002300*                          WDK7                                           
002400*        PROGRAM UPDATES   WDK6                                           
002500*                          WDK7                                           
002600*                                                                         
002700*                                                                         
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000                                                                          
003100 DATA DIVISION.                                                           
003200                                                                          
003300 WORKING-STORAGE SECTION.                                                 
003400*    -COPY WY2000W1                                                       
003500     SKIP3                                                                
003600 77  IDPGM                       PIC X(8)    VALUE 'W272UTUP'.            
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 77  IX                          PIC 9(3)    VALUE ZERO.                  
004000 77  AKTIV                       PIC X       VALUE 'A'.                   
004100 77  PASSIV                      PIC X       VALUE 'P'.                   
004200     EJECT                                                                
004300                                                                          
004400 01  WORKING-FIELDS.                                                      
004500     03 WS-IDDC-SPAR             PIC X(2)    VALUE SPACES.                
004600     03 WS-CDC-SE                PIC X(2)    VALUE '11'.                  
004700     03 WS-IDLEVNR               PIC X(5)    VALUE SPACES.                
004800     03 WS-KVPB-PLAN             PIC 9(6)V9  VALUE ZERO.                  
004900*                                                                         
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03 DAGENS-AA                PIC 9(2).                                
005300     03 DAGENS-MAANAD            PIC 9(2).                                
005400     03 DAGENS-DAG               PIC 9(2).                                
005500 01  WS-DAGENS-DATUM-AAVV        PIC 9(4)    VALUE ZERO.                  
005600 01  WS-DAGENS-DAGNR             PIC 9       VALUE ZERO.                  
005700*                                                                         
005800 01  WS-LTDATE-TIAAVV            PIC 9(4)    VALUE ZERO.                  
005900 01  WS-LTDATE-AAMMDD            PIC 9(6)    VALUE ZERO.                  
006000 01  WS-DAT-TID                  PIC 9       VALUE ZERO.                  
006100 01  WS-TIME                     PIC 9(8)    VALUE ZERO.                  
006200*                                                                         
006300 01  DAGENS-DATUM-CENTURY        PIC 9(8)    VALUE ZERO.                  
006400 01  WS-TIPBDAT                  PIC S9(5)   VALUE ZERO COMP-3.           
006500*                                                                         
006600 01  INDX-L                      PIC S9(3)   VALUE +0.                    
006700 01  INDX1-L                     PIC S9(3)   VALUE +0.                    
006800 01  INDX1-L-MAX                 PIC S9(3)   VALUE +52.                   
006900 01  LAST-IX                     PIC 9(3)    VALUE ZERO.                  
007000                                                                          
007100**** LEADTIME CALCULATION FOR DEMAND FROM W2222200                        
007200 01  WS-FLFLYG               PIC X        VALUE 'N'.                      
007300 01  WS-KVDAYS               PIC S9(9)    VALUE ZERO COMP.                
007400 01  WS-LEADTIME-WEEKS       PIC 9(3)     VALUE ZERO.                     
007500 01  WS-LEADTIME-WEEKS-PLUS1 PIC 9(3)     VALUE ZERO.                     
007600 01  WS-CALC-AAVV            PIC 9(4)     VALUE ZERO.                     
007700 01  WS-REST-DAYS            PIC 9(2)     VALUE ZERO.                     
007800 01  WS-REST-DAYS-ADJ        PIC 9(2)     VALUE ZERO.                     
007900 01  WS-TIVV-TEMP            PIC 9(2)     VALUE ZERO.                     
008000 01  WS-TIAAVV-L222          PIC 9(4)     VALUE ZERO.                     
008100 01  WS-TIAAVV-PLUS1         PIC 9(4)     VALUE ZERO.                     
008200 01  WS-TIAAVVD-PLUS1        PIC 9(4)     VALUE ZERO.                     
008300 01  WS-ANTAL-DAG-LT         PIC 9(3)     VALUE ZERO.                     
008400 01  WS-NONEED-DAYS          PIC 9        VALUE ZERO.                     
008500 01  WS-KVDAGAR-KVAR         PIC 9(3)     VALUE ZERO COMP-3.              
008600 01  WS-FAKTOR               PIC S9(1)V9(3)          COMP-3.              
008700 01  WS-LAST-MINUS-NEED   PIC S9(7)V9     VALUE ZERO COMP-3.              
008800 01  WS-DAY-NEED-LAST     PIC S9(7)V9     VALUE ZERO COMP-3.              
008900 01  WS-LEDTIDSBEHOV      PIC S9(7)V9(1)  VALUE ZERO COMP-3.              
009000 01  WS-VECKO-SEP-BEHOV   PIC S9(7)V9(2)  VALUE ZERO COMP-3.              
009100 01  WS-DAG-SEP-BEHOV     PIC S9(7)V9(2)  VALUE ZERO COMP-3.              
009200 01  WS-KVPB-DESSUTOM     PIC S9(7)V9(2)  VALUE ZERO COMP-3.              
009300                                                                          
009400 01  W222-BEHOV-TAB.                                                      
009500     03  W-BEHOV-LT                       OCCURS 52.                      
009600         05  W-BEHOV-LT-VECKA                                             
009700                              PIC S9(8)V9 COMP-3 VALUE ZERO.              
009800         05  W-BEHOV-LT-TOT                                               
009900                              PIC S9(8)V9 COMP-3 VALUE ZERO.              
010000                                                                          
010100                                                                          
010200**** GENERAL SUBROUTINE                                                   
010300 01  DYNAMISKA-SUBPROGRAM.                                                
010400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010900     03  W272REFL                PIC X(8)    VALUE 'W272REFL'.            
011000     03  W222PBTO                PIC X(8)    VALUE 'W222PBTO'.            
011100     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
011200     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
011300     03  W22222                  PIC X(8)    VALUE 'W22222'.              
011400                                                                          
011500*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
011600*01 -COPY WDATAREA                                                        
011700     EJECT                                                                
011800*   --- PARAMETRAR TILL SUBPROGRAM W005INIT                               
011900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012000*01 -COPY WMSGINIT                                                        
012100     EJECT                                                                
012200                                                                          
012300*   --- PARAMETRAR TILL W272REFL                                          
012400*                                                                         
012500 01  FILLER                      PIC X(16)   VALUE 'W272REFL'.            
012600*01 -COPY W272REFL   -PRE CDC-                                            
012700     EJECT                                                                
012800                                                                          
012900*   --- PARAMETRAR TILL SUBPROGRAM W222PBTO                               
013000*                                                                         
013100 01  FILLER                      PIC X(16)   VALUE 'W222PBTO'.            
013200*01 -COPY W222PBTO                                                        
013300     EJECT                                                                
013400                                                                          
013500*    ---PARAMETRAR TILL WZ20DAYS                                          
013600                                                                          
013700 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
013800*01  -COPY WZ20DAYS                                                       
013900     EJECT                                                                
014000                                                                          
014100*    ---LINKAGE AREA FOR PGM W2222200                                     
014200                                                                          
014300 01  FILLER                      PIC X(16)   VALUE 'W22222  '.            
014400*01  AREA  -COPY W222L222   -PRE LINK-.                                   
014500     EJECT                                                                
014600                                                                          
014700* VARIABLES TO SUBPROGRAM W009VADD                                        
014800 01  DATUM-AAVV                  PIC S9(5)   COMP-3.                      
014900 01  ANTAL-VECKOR                PIC S9(3)   COMP-3.                      
015000     EJECT                                                                
015100*                                                                         
015200 01  FILLER                      PIC X(16)   VALUE                        
015300                                             'KDBEHOV W2222200'.          
015400 01  KDBEHOV-W2222200.                                                    
015500     03  SEP-SATS-TPO-LEV-SDC-NDC                                         
015600                                 PIC  X(2)   VALUE '19'.                  
015700     EJECT                                                                
015800 01  WDK61129-ACCESS-STATUS      PIC XX      VALUE 'N'.                   
015900     88 WDK61129-FOUND                       VALUE 'J'.                   
016000                                                                          
016100*   --- PARAMETRAR TILL ABEND                                             
016200                                                                          
016300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
016400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
016500     SKIP2                                                                
016600 01  FELTEXT.                                                             
016700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
016800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
016900     EJECT                                                                
017000*                                                                         
017100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017200*                                                                         
017300 01  NYCKLAR-TILL-DLI.                                                    
017400     03  W-IDARTNR-X.                                                     
017500         05  W-IDARTNR           PIC S9(9)   COMP-3.                      
017600     03  W-IDDC-X.                                                        
017700         05  W-IDDC              PIC X(2)    VALUE ZERO.                  
017800     03  W-KDSEGKEY-X.                                                    
017900         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
018000     03  W-IDDC-B6-X.                                                     
018100         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
018200     03  W-IDDC-B616-X.                                                   
018300         05  W-IDDC-B616         PIC X(2)    VALUE SPACE.                 
018400                                                                          
018500*    --- STATUS-KOD FRÅN IMS                                              
018600 01  IMS-WS.                                                              
018700     03  FILLER                  PIC X(8)    VALUE 'IMS-WS  '.            
018800                                                                          
018900*                            *** STATUSKOD FRÅN IMS                       
019000     03 STATUS-WS                PIC XX.                                  
019100         88  SEGMENT-FOUND                   VALUE '  '.                  
019200         88  SEGMENT-MISSING                 VALUE 'GE'.                  
019300         88  SEGMENT-EXISTS                  VALUE 'II'.                  
019400                                                                          
019500     03  GODK-STATUSKODER.                                                
019600         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.            
019700                                                                          
019800     03  SSA1                    PIC X(128).                              
019900     03  SSA2                    PIC X(128).                              
020000     03  SSA3                    PIC X(128).                              
020100     EJECT                                                                
020200                                                                          
020300*    --- IMS FUNKTIONSKODER                                               
020400*01  -COPY W0003                                                          
020500     EJECT                                                                
020600                                                                          
020700*    --- DLI INPUT-OUTPUT AREA                                            
020800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
020900 01  DLI-IO-AREA-WDK601.                                                  
021000*    03  -COPY WDK601                                                     
021100     EJECT                                                                
021200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
021300 01  DLI-IO-AREA-WDK611.                                                  
021400*    03  -COPY WDK611                                                     
021500     EJECT                                                                
021600 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDK61129'.          
021700     SKIP3                                                                
021800 01  DLI-IO-AREA-WDK61129.                                                
021900*    03  -COPY WDK611 -PRE K6-                                            
022000*    03  -COPY WDK629 -PRE K6-                                            
022100     EJECT                                                                
022200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK701'.             
022300 01  DLI-IO-AREA-WDK701.                                                  
022400*    03  -COPY WDK701                                                     
022500     EJECT                                                                
022600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
022700 01  DLI-IO-AREA-WDK711.                                                  
022800*    03  -COPY WDK711                                                     
022900     EJECT                                                                
023000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB601'.             
023100 01  DLI-IO-AREA-WDB601.                                                  
023200*    03  -COPY WDB601                                                     
023300     EJECT                                                                
023400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB616'.             
023500 01  DLI-IO-AREA-WDB616.                                                  
023600*    03  -COPY WDB616                                                     
023700     EJECT                                                                
023800 LINKAGE SECTION.                                                         
023900                                                                          
024000*01 -COPY W272UTUP                                                        
024100     EJECT                                                                
024200*01  -COPY W0008  -PRE WDK6-                                              
024300     05  FILLER                       PIC X.                              
024400     EJECT                                                                
024500*01  -COPY W0008  -PRE WDB6-                                              
024600     05  FILLER                       PIC X.                              
024700     EJECT                                                                
024800 01  PBTO-W222-WDK6-PCB               PIC X.                              
024900 01  PBTO-W222-WDK7-PCB               PIC X.                              
025000 01  PBTO-W222-ARTM-PCB               PIC X.                              
025100 01  PBTO-W222-2501-PCB               PIC X.                              
025200 01  PBTO-W222-WDB6R-PCB              PIC X.                              
025300 01  PBTO-W222-WDK7R-PCB              PIC X.                              
025400 01  PBTO-W222-WDB6-PCB               PIC X.                              
025500 01  PBTO-W222-WDD7-PCB               PIC X.                              
025600 01  PBTO-W222-WDK7E-PCB              PIC X.                              
025700 01  PBTO-W222-UTIL-WDK6-PCB          PIC X.                              
025800 01  PBTO-W222-UTIL-WDK7-PCB          PIC X.                              
025900 01  PBTO-W222-UTIL-WDB6-PCB          PIC X.                              
026000 01  PBTO-W222-UTUP-WDK7-PCB          PIC X.                              
026100 01  PBTO-W222-UTUP-WDB6-PCB          PIC X.                              
026200 01  PBTO-W222-UTUP-UTIL-WDK6-PCB     PIC X.                              
026300 01  PBTO-W222-UTUP-UTIL-WDK7-PCB     PIC X.                              
026400 01  PBTO-W222-UTUP-UTIL-WDB6-PCB     PIC X.                              
026500     EJECT                                                                
026600 01  CDC-REFL-2501-PCB                PIC X.                              
026700 01  CDC-REFL-WDB6-PCB                PIC X.                              
026800 01  UTIL-WDK6-PCB                    PIC X.                              
026900 01  UTIL-WDK7-PCB                    PIC X.                              
027000 01  UTIL-WDB6-PCB                    PIC X.                              
027100                                                                          
027200     EJECT                                                                
027300 01  W222-WDK6-PCB                    PIC X.                              
027400 01  W222-WDK7-PCB                    PIC X.                              
027500 01  W222-ARTM-PCB                    PIC X.                              
027600 01  W222-2501-PCB                    PIC X.                              
027700 01  W222-WDB6R-PCB                   PIC X.                              
027800 01  W222-WDK7R-PCB                   PIC X.                              
027900 01  W222-WDB6-PCB                    PIC X.                              
028000 01  W222-WDD7-PCB                    PIC X.                              
028100 01  W222-WDK7E-PCB                   PIC X.                              
028200 01  W222-UTIL-WDK6-PCB               PIC X.                              
028300 01  W222-UTIL-WDK7-PCB               PIC X.                              
028400 01  W222-UTIL-WDB6-PCB               PIC X.                              
028500 01  W222-UTUP-WDK7-PCB               PIC X.                              
028600 01  W222-UTUP-WDB6-PCB               PIC X.                              
028700 01  W222-UTUP-UTIL-WDK6-PCB          PIC X.                              
028800 01  W222-UTUP-UTIL-WDK7-PCB          PIC X.                              
028900 01  W222-UTUP-UTIL-WDB6-PCB          PIC X.                              
029000     EJECT                                                                
029100                                                                          
029200 PROCEDURE DIVISION USING UTUP-W272UTUP  WDK6-PCB WDB6-PCB                
029300                          PBTO-W222-WDK6-PCB  PBTO-W222-WDK7-PCB          
029400                          PBTO-W222-ARTM-PCB  PBTO-W222-2501-PCB          
029500                          PBTO-W222-WDB6R-PCB PBTO-W222-WDK7R-PCB         
029600                          PBTO-W222-WDB6-PCB  PBTO-W222-WDD7-PCB          
029700                          PBTO-W222-WDK7E-PCB                             
029800                          PBTO-W222-UTIL-WDK6-PCB                         
029900                          PBTO-W222-UTIL-WDK7-PCB                         
030000                          PBTO-W222-UTIL-WDB6-PCB                         
030100                          PBTO-W222-UTUP-WDK7-PCB                         
030200                          PBTO-W222-UTUP-WDB6-PCB                         
030300                          PBTO-W222-UTUP-UTIL-WDK6-PCB                    
030400                          PBTO-W222-UTUP-UTIL-WDK7-PCB                    
030500                          PBTO-W222-UTUP-UTIL-WDB6-PCB                    
030600                          CDC-REFL-2501-PCB                               
030700                          CDC-REFL-WDB6-PCB                               
030800                          UTIL-WDK6-PCB                                   
030900                          UTIL-WDK7-PCB                                   
031000                          UTIL-WDB6-PCB                                   
031100                          W222-WDK6-PCB                                   
031200                          W222-WDK7-PCB                                   
031300                          W222-ARTM-PCB                                   
031400                          W222-2501-PCB                                   
031500                          W222-WDB6R-PCB                                  
031600                          W222-WDK7R-PCB                                  
031700                          W222-WDB6-PCB                                   
031800                          W222-WDD7-PCB                                   
031900                          W222-WDK7E-PCB                                  
032000                          W222-UTIL-WDK6-PCB                              
032100                          W222-UTIL-WDK7-PCB                              
032200                          W222-UTIL-WDB6-PCB                              
032300                          W222-UTUP-WDK7-PCB                              
032400                          W222-UTUP-WDB6-PCB                              
032500                          W222-UTUP-UTIL-WDK6-PCB                         
032600                          W222-UTUP-UTIL-WDK7-PCB                         
032700                          W222-UTUP-UTIL-WDB6-PCB.                        
032800                                                                          
032900     PERFORM A-INIT                                                       
033000                                                                          
033100     EVALUATE UTUP-KDCALL                                                 
033200        WHEN 001                                                          
033300          PERFORM B-UPD-PROGNOS                                           
033400        WHEN 002                                                          
033500          PERFORM C-GET-WEEKLY-DEMAND                                     
033600        WHEN OTHER                                                        
033700          SET UTUP-KDSVAR-FEL   TO TRUE                                   
033800          MOVE 'ERROR KDCALL'   TO UTUP-TEXT                              
033900          DISPLAY 'W272UTUP ERROR KDCALL'                                 
034000     END-EVALUATE                                                         
034100                                                                          
034200     MOVE ZERO TO RETURN-CODE                                             
034300     GOBACK                                                               
034400     .                                                                    
034500     EJECT                                                                
034600                                                                          
034700 A-INIT SECTION.                                                          
034800                                                                          
034900     MOVE SPACES                  TO UTUP-KDSVAR                          
035000*                                                                         
035100     MOVE FUNCTION CURRENT-DATE(1:8)                                      
035200                                  TO DAGENS-DATUM-CENTURY                 
035300                                                                          
035400     ACCEPT DAGENS-DATUM        FROM DATE                                 
035500     ACCEPT WS-TIME             FROM TIME                                 
035600                                                                          
035700     MOVE 'AAMMDD'                TO DAT-KDDATFORM                        
035800     MOVE DAGENS-DATUM            TO DAT-I-TIDATUM                        
035900                                                                          
036000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
036100                     DAT-O-TIDATUM DAT-KDSVAR                             
036200                                                                          
036300     IF DAT-KDSVAR-OK                                                     
036400                                                                          
036500       MOVE DAT-TIAAVVD       TO WS-TIPBDAT                               
036600       MOVE DAT-TID           TO WS-DAGENS-DAGNR                          
036700       MOVE DAT-TIAAVV-GRP    TO WS-DAGENS-DATUM-AAVV                     
036800                                                                          
036900     ELSE                                                                 
037000         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
037100         DELIMITED BY SIZE INTO FELTEXT                                   
037200         CALL FELLOG                                                      
037300     END-IF                                                               
037400*                                                                         
037500     IF  UTUP-IDARTNR IS NUMERIC                                          
037600     AND UTUP-IDARTNR  > ZERO                                             
037700         CONTINUE                                                         
037800     ELSE                                                                 
037900         SET UTUP-KDSVAR-FEL  TO TRUE                                     
038000         MOVE 'INVALID PART NO'                                           
038100                              TO UTUP-TEXT                                
038200     END-IF                                                               
038300     .                                                                    
038400     EJECT                                                                
038500                                                                          
038600 B-UPD-PROGNOS   SECTION.                                                 
038700                                                                          
038800     PERFORM S-GET-WEEKLY-DEMAND                                          
038900*                                                                         
039000     PERFORM BA-UPD-PROGNOS-CDC                                           
039100     .                                                                    
039200     EJECT                                                                
039300 BA-UPD-PROGNOS-CDC SECTION.                                              
039400                                                                          
039500     MOVE UTUP-IDARTNR           TO W-IDARTNR                             
039600     PERFORM IMS-GHU-WDK61129                                             
039700     IF SEGMENT-FOUND                                                     
039800        MOVE    WS-TIPBDAT       TO K6-CLAG-TIPBDAT                       
039900        IF K6-CLAG-DAPBPLAN      >= DAGENS-DATUM-CENTURY                  
040000           MOVE K6-CLAG-KVPB-PLAN                                         
040100                                 TO WS-KVPB-PLAN                          
040200        ELSE                                                              
040300           MOVE W-IDARTNR        TO PBTO-IDARTNR                          
040400           CALL W222PBTO      USING PBTO-W222PBTO                         
040500                                    PBTO-W222-WDK6-PCB                    
040600                                    PBTO-W222-WDK7-PCB                    
040700                                    PBTO-W222-ARTM-PCB                    
040800                                    PBTO-W222-2501-PCB                    
040900                                    PBTO-W222-WDB6R-PCB                   
041000                                    PBTO-W222-WDK7R-PCB                   
041100                                    PBTO-W222-WDB6-PCB                    
041200                                    PBTO-W222-WDD7-PCB                    
041300                                    PBTO-W222-WDK7E-PCB                   
041400                                    PBTO-W222-UTIL-WDK6-PCB               
041500                                    PBTO-W222-UTIL-WDK7-PCB               
041600                                    PBTO-W222-UTIL-WDB6-PCB               
041700                                    PBTO-W222-UTUP-WDK7-PCB               
041800                                    PBTO-W222-UTUP-WDB6-PCB               
041900                                    PBTO-W222-UTUP-UTIL-WDK6-PCB          
042000                                    PBTO-W222-UTUP-UTIL-WDK7-PCB          
042100                                    PBTO-W222-UTUP-UTIL-WDB6-PCB          
042200                                                                          
042300           IF PBTO-KDSVAR = JA                                            
042400              MOVE PBTO-KVPB-PLAN                                         
042500                                 TO WS-KVPB-PLAN                          
042600                                    K6-CREF-KVPB-PLAN                     
042700           ELSE                                                           
042800              SET UTUP-KDSVAR-FEL                                         
042900                                 TO TRUE                                  
043000              MOVE 'ERROR W222PBTO'                                       
043100                                 TO UTUP-TEXT                             
043200              DISPLAY 'W272UTUP ERROR W222PBTO'                           
043300           END-IF                                                         
043400        END-IF                                                            
043500                                                                          
043600        MOVE W-IDARTNR           TO CDC-REFL-IDARTNR                      
043700        MOVE WS-CDC-SE           TO CDC-REFL-IDDC                         
043800        MOVE K6-CREF-IDDC-REF    TO CDC-REFL-IDDC-REF                     
043900        MOVE K6-CREF-IDREFTAB    TO CDC-REFL-IDREFTAB                     
044000        MOVE K6-CREF-FLREFBEO    TO CDC-REFL-FLREFBEO                     
044100        MOVE K6-CREF-FLWILSON    TO CDC-REFL-FLWILSON                     
044200        MOVE K6-CLAG-PRARTSTD    TO CDC-REFL-PRARTBES                     
044300        MOVE K6-CREF-FLFLYG      TO CDC-REFL-FLFLYG                       
044400                                                                          
044500        MOVE K6-CREF-TIREFPKT    TO TMP1-YYMMDD                           
044600        MOVE DAGENS-DATUM        TO TMP2-YYMMDD                           
044700        PERFORM WY2000P1                                                  
044800                                                                          
044900        IF TMP1-YYMMDD     >= TMP2-YYMMDD                                 
045000           MOVE K6-CREF-KVREFPKT TO CDC-REFL-IN-KVREFPKT                  
045100        ELSE                                                              
045200           MOVE ZERO             TO CDC-REFL-IN-KVREFPKT                  
045300        END-IF                                                            
045400                                                                          
045500        MOVE K6-CREF-TIREFPAF    TO TMP1-YYMMDD                           
045600        MOVE DAGENS-DATUM        TO TMP2-YYMMDD                           
045700        PERFORM WY2000P1                                                  
045800                                                                          
045900        IF TMP1-YYMMDD     >= TMP2-YYMMDD                                 
046000           MOVE K6-CLAG-KVQ      TO CDC-REFL-IN-KVREFBER                  
046100        ELSE                                                              
046200         MOVE ZERO               TO CDC-REFL-IN-KVREFBER                  
046300        END-IF                                                            
046400        MOVE WS-LEDTIDSBEHOV     TO CDC-REFL-IN-LEADTID-BEHOV             
046500                                                                          
046600        CALL W272REFL         USING CDC-REFL-W272REFL                     
046700                                    CDC-REFL-2501-PCB                     
046800                                    CDC-REFL-WDB6-PCB                     
046900                                    UTIL-WDK6-PCB                         
047000                                    UTIL-WDK7-PCB                         
047100                                    UTIL-WDB6-PCB                         
047200                                                                          
047300        MOVE K6-CREF-TIREFPKT    TO TMP1-YYMMDD                           
047400        MOVE DAGENS-DATUM        TO TMP2-YYMMDD                           
047500        PERFORM WY2000P1                                                  
047600        IF TMP1-YYMMDD     >= TMP2-YYMMDD                                 
047700*---       NO UPDATE OF KVREFPKT IF MANUAL DATE IS SET                    
047800           CONTINUE                                                       
047900        ELSE                                                              
048000           MOVE CDC-REFL-KVREFPKT                                         
048100                                 TO K6-CREF-KVREFPKT                      
048200        END-IF                                                            
048300                                                                          
048400        MOVE K6-CREF-TIREFPAF    TO TMP1-YYMMDD                           
048500        MOVE DAGENS-DATUM        TO TMP2-YYMMDD                           
048600        PERFORM WY2000P1                                                  
048700        IF TMP1-YYMMDD     >= TMP2-YYMMDD                                 
048800*---       NO UPDATE OF KVREFBER IF MANUAL DATE IS SET                    
048900           CONTINUE                                                       
049000        ELSE                                                              
049100           MOVE CDC-REFL-KVREFBER                                         
049200                                 TO K6-CLAG-KVQ                           
049300        END-IF                                                            
049400                                                                          
049500        MOVE CDC-REFL-KVREFOVL   TO K6-CREF-KVREFOVL                      
049600        MOVE CDC-REFL-KVSLAGER   TO K6-CLAG-KVSLAGER                      
049700                                                                          
049800        IF   K6-CREF-KDREFSTA     = PASSIV                                
049900        AND (K6-CLAG-KVPB-SEP     > ZERO                                  
050000        OR   K6-CLAG-KVPB-PLAN    > ZERO)                                 
050100             MOVE AKTIV          TO K6-CREF-KDREFSTA                      
050200             MOVE DAGENS-DATUM   TO K6-CREF-TIREFSTA                      
050300        END-IF                                                            
050400        PERFORM IMS-REPL-WDK61129                                         
050500     ELSE                                                                 
050600        SET UTUP-KDSVAR-FEL      TO TRUE                                  
050700        MOVE 'ERROR WDK61129 ACCESS'                                      
050800                                 TO UTUP-TEXT                             
050900        DISPLAY 'W272UTUP ERROR WDK61129 ACCESS'                          
051000     END-IF                                                               
051100     .                                                                    
051200     EJECT                                                                
051300 C-GET-WEEKLY-DEMAND SECTION.                                             
051400                                                                          
051500     PERFORM S-GET-WEEKLY-DEMAND                                          
051600     .                                                                    
051700     EJECT                                                                
051800 S-GET-WEEKLY-DEMAND SECTION.                                             
051900                                                                          
052000     INITIALIZE LINK-W222L222                                             
052100                                                                          
052200     PERFORM SA-LEDTID-ADJMT                                              
052300     MOVE UTUP-IDARTNR              TO LINK-IDARTNR                       
052400                                       W-IDARTNR                          
052500     MOVE WS-DAGENS-DAGNR           TO LINK-TID-AKTUELL                   
052600     MOVE SPACE                     TO LINK-IDDC                          
052700     MOVE NEJ                       TO LINK-FLINKLDIRLEV                  
052800     MOVE SEP-SATS-TPO-LEV-SDC-NDC  TO LINK-KDBEHOV                       
052900     MOVE WS-LEADTIME-WEEKS-PLUS1   TO LINK-KVVECKOR-BEHOV                
053000     MOVE WS-TIAAVV-L222            TO LINK-TIAAVV-AKTUELL                
053100                                       LINK-TIBEHOV-START                 
053200                                       DATUM-AAVV                         
053300     MOVE 1                         TO ANTAL-VECKOR                       
053400                                                                          
053500     CALL W009VADD USING  DATUM-AAVV                                      
053600                          ANTAL-VECKOR                                    
053700                                                                          
053800     MOVE DATUM-AAVV                TO LINK-TIBEHOV-START                 
053900                                                                          
054000     MOVE ZERO                      TO WS-LEDTIDSBEHOV                    
054100                                                                          
054200     CALL W22222 USING LINK-AREA W222-WDK6-PCB W222-WDK7-PCB              
054300                       W222-ARTM-PCB                                      
054400                       W222-2501-PCB  W222-WDB6R-PCB                      
054500                       W222-WDK7R-PCB W222-WDB6-PCB                       
054600                       W222-WDD7-PCB  W222-WDK7E-PCB                      
054700                       W222-UTIL-WDK6-PCB                                 
054800                       W222-UTIL-WDK7-PCB                                 
054900                       W222-UTIL-WDB6-PCB                                 
055000                       W222-UTUP-WDK7-PCB                                 
055100                       W222-UTUP-WDB6-PCB                                 
055200                       W222-UTUP-UTIL-WDK6-PCB                            
055300                       W222-UTUP-UTIL-WDK7-PCB                            
055400                       W222-UTUP-UTIL-WDB6-PCB                            
055500                                                                          
055600     IF LINK-ANROP-FEL                                                    
055700        PERFORM SB-NOLLA-W22222                                           
055800     ELSE                                                                 
055900        MOVE +1                     TO INDX-L                             
056000                                       INDX1-L                            
056100        MOVE LINK-KVBEHOV-DESSUTOM  TO W-BEHOV-LT-VECKA(INDX1-L)          
056200                                       W-BEHOV-LT-TOT(INDX1-L)            
056300*                                                                         
056400        ADD +1                      TO INDX1-L                            
056500        PERFORM UNTIL INDX1-L > LAST-IX                                   
056600          MOVE LINK-KVBEHOV-VECKA(INDX-L)                                 
056700                                    TO W-BEHOV-LT-VECKA(INDX1-L)          
056800          MOVE W-BEHOV-LT-TOT(INDX1-L - 1)                                
056900                                    TO W-BEHOV-LT-TOT(INDX1-L)            
057000          ADD  LINK-KVBEHOV-VECKA(INDX-L)                                 
057100                                    TO W-BEHOV-LT-TOT(INDX1-L)            
057200          ADD +1                    TO INDX-L                             
057300                                       INDX1-L                            
057400        END-PERFORM                                                       
057500        MOVE W-BEHOV-LT-TOT(INDX1-L - 1)                                  
057600                                    TO WS-LEDTIDSBEHOV                    
057700        PERFORM SC-LAST-WEEK-NEED                                         
057800        COMPUTE WS-LEDTIDSBEHOV      = WS-LEDTIDSBEHOV                    
057900                                     - WS-LAST-MINUS-NEED                 
058000*                                                                         
058100        PERFORM SD-SEP-BEHOV-INNEV-VECKA                                  
058200        MOVE WS-LEDTIDSBEHOV        TO UTUP-LEADTID-BEHOV                 
058300     END-IF                                                               
058400     .                                                                    
058500     EJECT                                                                
058600 SA-LEDTID-ADJMT  SECTION.                                                
058700                                                                          
058800     MOVE UTUP-IDARTNR             TO W-IDARTNR                           
058900     PERFORM IMS-GU-WDK61129                                              
059000     IF SEGMENT-FOUND                                                     
059100        MOVE K6-CREF-FLFLYG        TO WS-FLFLYG                           
059200        SET WDK61129-FOUND         TO TRUE                                
059300     END-IF                                                               
059400*                                                                         
059500     MOVE WS-CDC-SE                TO W-IDDC-B6                           
059600     MOVE UTUP-IDDC-REF            TO W-IDDC-B616                         
059700     PERFORM IMS-GU-WDB616                                                
059800     IF SEGMENT-FOUND                                                     
059900        IF WS-FLFLYG = JA                                                 
060000           MOVE REF-KVDLTID-AIRETA TO WS-KVDAYS                           
060100        ELSE                                                              
060200           MOVE REF-KVDLTID-TOT    TO WS-KVDAYS                           
060300        END-IF                                                            
060400     ELSE                                                                 
060500        MOVE ZERO                  TO WS-KVDAYS                           
060600     END-IF                                                               
060700*                                                                         
060800     IF WS-KVDAYS > ZERO                                                  
060900        DIVIDE WS-KVDAYS BY 7                                             
061000                              GIVING WS-LEADTIME-WEEKS                    
061100                           REMAINDER WS-REST-DAYS                         
061200        MOVE WS-KVDAYS            TO DAYS-KVDAYS                          
061300     ELSE                                                                 
061400        MOVE ZERO                 TO DAYS-KVDAYS                          
061500                                     WS-LEADTIME-WEEKS                    
061600                                     WS-REST-DAYS                         
061700     END-IF                                                               
061800                                                                          
061900     IF DAYS-KVDAYS > ZERO                                                
062000        COMPUTE DAYS-KVDAYS = (DAYS-KVDAYS - 1)                           
062100     END-IF                                                               
062200                                                                          
062300     MOVE WS-DAGENS-DATUM-AAVV    TO WS-TIAAVV-L222                       
062400*                                                                         
062500     MOVE DAGENS-DATUM            TO DAYS-TIDATE1                         
062600     MOVE 'YYMMDD'                TO DAYS-KDDATFMT1                       
062700     MOVE 'YYMMDD'                TO DAYS-KDDATFMT2                       
062800     MOVE SPACE                   TO DAYS-TIDATE2                         
062900                                     DAYS-IDCALEND                        
063000                                                                          
063100     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
063200                                                                          
063300     IF DAYS-KDRC = 8                                                     
063400        STRING 'FEL VID ANROP TILL WZ20DAYS - SEC SA- '                   
063500        DELIMITED BY SIZE INTO FELTEXT-STR                                
063600        DISPLAY FELTEXT                                                   
063700        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
063800     ELSE                                                                 
063900        MOVE DAYS-TIDATE2(1:6)    TO WS-LTDATE-AAMMDD                     
064000     END-IF                                                               
064100                                                                          
064200     MOVE 'AAMMDD'                TO DAT-KDDATFORM                        
064300     MOVE WS-LTDATE-AAMMDD        TO DAT-I-TIDATUM                        
064400                                                                          
064500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
064600                         DAT-O-TIDATUM DAT-KDSVAR                         
064700                                                                          
064800     IF DAT-KDSVAR-OK                                                     
064900                                                                          
065000        MOVE DAT-TIAAVV-GRP       TO WS-LTDATE-TIAAVV                     
065100        MOVE DAT-TID              TO WS-DAT-TID                           
065200                                     WS-REST-DAYS-ADJ                     
065300     ELSE                                                                 
065400        STRING ' FEL FRÅN WDATKONV I W272UTUP'                            
065500               ' (SEC - SA- )'                                            
065600        DELIMITED BY SIZE INTO FELTEXT-STR                                
065700        DISPLAY FELTEXT                                                   
065800        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
065900     END-IF                                                               
066000                                                                          
066100     COMPUTE WS-REST-DAYS = WS-REST-DAYS + WS-DAGENS-DAGNR                
066200     COMPUTE WS-REST-DAYS = WS-REST-DAYS - 1                              
066300                                                                          
066400     IF  WS-REST-DAYS     > ZERO                                          
066500         COMPUTE WS-LEADTIME-WEEKS-PLUS1                                  
066600                                   = WS-LEADTIME-WEEKS + 1                
066700         IF WS-REST-DAYS  > 7                                             
066800            COMPUTE WS-LEADTIME-WEEKS-PLUS1                               
066900                             = WS-LEADTIME-WEEKS-PLUS1 + 1                
067000         END-IF                                                           
067100     ELSE                                                                 
067200       MOVE WS-LEADTIME-WEEKS     TO WS-LEADTIME-WEEKS-PLUS1              
067300     END-IF                                                               
067400                                                                          
067500     IF WS-LEADTIME-WEEKS-PLUS1    = ZERO                                 
067600        MOVE 1                    TO WS-LEADTIME-WEEKS-PLUS1              
067700     END-IF                                                               
067800                                                                          
067900     IF WS-REST-DAYS-ADJ > 0                                              
068000        IF WS-LEADTIME-WEEKS-PLUS1 > ZERO                                 
068100           MOVE WS-LEADTIME-WEEKS-PLUS1                                   
068200                                  TO LAST-IX                              
068300        END-IF                                                            
068400     END-IF                                                               
068500     .                                                                    
068600     EJECT                                                                
068700 SB-NOLLA-W22222 SECTION.                                                 
068800                                                                          
068900     MOVE +0                      TO LINK-KVBEHOV-SUMMA                   
069000                                     LINK-KVBEHOV-DESSUTOM                
069100                                     LINK-TIBEHOV-FIRST                   
069200                                                                          
069300     MOVE +1                      TO INDX-L                               
069400     PERFORM UNTIL INDX-L > 156                                           
069500       MOVE +0                    TO LINK-KVBEHOV-VECKA(INDX-L)           
069600                                                                          
069700       ADD +1                     TO INDX-L                               
069800     END-PERFORM                                                          
069900     .                                                                    
070000     EJECT                                                                
070100 SC-LAST-WEEK-NEED SECTION.                                               
070200                                                                          
070300     IF WS-REST-DAYS-ADJ > 0                                              
070400        IF WS-LEADTIME-WEEKS-PLUS1  > ZERO                                
070500           COMPUTE WS-NONEED-DAYS   = 7 - WS-REST-DAYS-ADJ                
070600           COMPUTE WS-DAY-NEED-LAST =                                     
070700                                 W-BEHOV-LT-VECKA (LAST-IX) / 7           
070800           COMPUTE WS-LAST-MINUS-NEED ROUNDED                             
070900                                    = (WS-NONEED-DAYS *                   
071000                                      WS-DAY-NEED-LAST)                   
071100       ELSE                                                               
071200           MOVE ZERO               TO WS-LAST-MINUS-NEED                  
071300       END-IF                                                             
071400     END-IF                                                               
071500     .                                                                    
071600     EJECT                                                                
071700 SD-SEP-BEHOV-INNEV-VECKA  SECTION.                                       
071800                                                                          
071900     MOVE ZERO                        TO WS-KVPB-DESSUTOM                 
072000*                                                                         
072100     IF WDK61129-FOUND                                                    
072200        COMPUTE WS-VECKO-SEP-BEHOV ROUNDED                                
072300                                       = K6-CLAG-KVPB-SEP / 4.33          
072400        COMPUTE WS-DAG-SEP-BEHOV   ROUNDED                                
072500                                       = WS-VECKO-SEP-BEHOV / 5           
072600                                                                          
072700        IF   WS-DAGENS-DAGNR = 7                                          
072800        OR   WS-DAGENS-DAGNR = 6                                          
072900        OR  (WS-DAGENS-DAGNR = 5                                          
073000        AND  WS-TIME (1:4)   > 1700)                                      
073100             MOVE ZERO                TO WS-KVPB-DESSUTOM                 
073200        ELSE                                                              
073300             COMPUTE WS-KVDAGAR-KVAR   = (5 - WS-DAGENS-DAGNR)            
073400             IF WS-TIME(1:4)          <= 1700                             
073500                ADD +1                TO WS-KVDAGAR-KVAR                  
073600             END-IF                                                       
073700                                                                          
073800             MOVE +1                  TO WS-FAKTOR                        
073900             SUBTRACT K6-CLAG-REDIRLEV                                    
074000                                    FROM WS-FAKTOR                        
074100                                                                          
074200             COMPUTE WS-KVPB-DESSUTOM  = WS-DAG-SEP-BEHOV *               
074300                                         WS-KVDAGAR-KVAR  *               
074400                                         WS-FAKTOR                        
074500        END-IF                                                            
074600     END-IF                                                               
074700     COMPUTE WS-LEDTIDSBEHOV           = WS-LEDTIDSBEHOV                  
074800                                       + WS-KVPB-DESSUTOM                 
074900     .                                                                    
075000     EJECT                                                                
075100                                                                          
075200*****                                                                     
075300*****  IMS SECTIONER   ****                                               
075400*****                                                                     
075500 IMS-GHU-WDK61129   SECTION.                                              
075600                                                                          
075700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
075800          DELIMITED BY SIZE INTO SSA1                                     
075900     STRING 'WDK611  *D(KDSEGKEY =' W-KDSEGKEY-X ')'                      
076000          DELIMITED BY SIZE INTO SSA2                                     
076100     MOVE 'WDK629  '          TO SSA3                                     
076200     MOVE '  ' TO GODK-STATUSKODER                                        
076300     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA-WDK61129                 
076400                            SSA1 SSA2 SSA3                                
076500     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
076600     PERFORM IMS-STATUSKONTROLL                                           
076700     .                                                                    
076800     SKIP3                                                                
076900 IMS-GU-WDK61129   SECTION.                                               
077000                                                                          
077100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
077200          DELIMITED BY SIZE INTO SSA1                                     
077300     STRING 'WDK611  *D(KDSEGKEY =' W-KDSEGKEY-X ')'                      
077400          DELIMITED BY SIZE INTO SSA2                                     
077500     MOVE 'WDK629  '          TO SSA3                                     
077600     MOVE '  ' TO GODK-STATUSKODER                                        
077700     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-AREA-WDK61129                 
077800                            SSA1 SSA2 SSA3                                
077900     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
078000     PERFORM IMS-STATUSKONTROLL                                           
078100     .                                                                    
078200     SKIP3                                                                
078300 IMS-REPL-WDK61129  SECTION.                                              
078400                                                                          
078500     MOVE '  ' TO GODK-STATUSKODER                                        
078600     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-WDK61129                
078700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
078800     PERFORM IMS-STATUSKONTROLL                                           
078900     .                                                                    
079000     SKIP3                                                                
079100 IMS-GU-WDB616    SECTION.                                                
079200                                                                          
079300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
079400          DELIMITED BY SIZE INTO SSA1                                     
079500     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
079600          DELIMITED BY SIZE INTO SSA2                                     
079700     MOVE '  ' TO GODK-STATUSKODER                                        
079800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-WDB616 SSA1 SSA2          
079900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
080000     PERFORM IMS-STATUSKONTROLL                                           
080100     .                                                                    
080200     SKIP3                                                                
080300                                                                          
080400 IMS-STATUSKONTROLL SECTION.                                              
080500                                                                          
080600     SET STATUS-IX TO 1                                                   
080700     SEARCH GODK-STATUS                                                   
080800       AT END                                                             
080900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
081000         DELIMITED BY SIZE INTO FELTEXT                                   
081100         CALL FELLOG                                                      
081200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
081300         CONTINUE                                                         
081400     END-SEARCH                                                           
081500     .                                                                    
081600     EJECT                                                                
081700*    -COPY WY2000P1                                                       
