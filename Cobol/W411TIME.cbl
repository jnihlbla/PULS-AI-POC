000010*COMPOPT STDSUB=YES                                                       
000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W411TIME.                                                
000400 AUTHOR.         ANNELIE ENGLUND.                                         
000500 DATE-WRITTEN.   MAJ 1990                                                 
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*                                                                         
001100*    PROGRAMMET ÄR ETT SUBPROGRAM SOM ANVÄNDS AV WOPS OCH                 
001200*    ORDER-ENTRY. DET BERÄKNAR STARTTID UTIFRÅN ANGIVEN STOPP-            
001300*    TID OCH ANTAL ARBETSTIMMAR, ELLER STOPPTID UTIFRÅN ANGIVEN           
001400*    STARTTID OCH ANTAL ARBETSTIMMAR, ELLER ANTAL ARBETSTIMMAR            
001500*    FRÅN GIVEN STARTTID OCH STOPPTID.                                    
001600*                                                                         
001700*                                                                         
001800*                                                                         
001900*                                                                         
002000*    PROGRAMMET LÄSER    WL443701 (WDR101)  ARBETSTIDSTABELL              
002100*                        WL443711 (WDR110)                                
002200*                                                                         
002300*    LÄNKAREA: W411TIME                                                   
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
002901*    -COPY WY2000W1                                                       
002902     SKIP3                                                                
003000 77  IDPGM                       PIC X(08)   VALUE 'W411TIME'.            
003100 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500                                                                          
003600 77  OK                          PIC X       VALUE ' '.                   
003700 77  FEL                         PIC X       VALUE 'F'.                   
003800                                                                          
003900 77  ANTAL-NOLL-FAELT            PIC 9       VALUE ZERO.                  
004000                                                                          
004100 77  TABELL-ANTAL-MIN            PIC 9(5)    VALUE ZERO.                  
004200                                                                          
004300 77  TIME-ANTAL-MIN              PIC 9(5)    VALUE ZERO.                  
004400                                                                          
004500 77  MIN-RAKNARE                 PIC 9(5)    VALUE ZERO.                  
004600                                                                          
004700 77  STARTTID                    PIC 9(5)    VALUE ZERO.                  
004800                                                                          
004900 77  STOPTID                     PIC 9(5)    VALUE ZERO.                  
005000                                                                          
005100 77  W-DIFFERENS                 PIC 9(5)    VALUE ZERO.                  
005200                                                                          
005300 77  TOT-ANTAL-TIM               PIC 9(4)    VALUE ZERO.                  
005400                                                                          
005500 77  TOT-ANTAL-MIN               PIC 9(5)    VALUE ZERO.                  
005600                                                                          
005700 77  W-TIAAMMDD                  PIC 9(6)    VALUE ZERO.                  
005800                                                                          
005900 77  ANTAL-HELA-MIN              PIC 9(2)    VALUE ZERO.                  
006000                                                                          
006100 77  W-TIARBMIN                  PIC 9(5)    VALUE ZERO.                  
006200                                                                          
006300 01  W-TIARB                     PIC 9(5)    VALUE ZERO.                  
006400 01  ARBTID REDEFINES W-TIARB.                                            
006500     03  TIARB-HHH               PIC 9(3).                                
006600     03  TIARB-MM                PIC 9(2).                                
006700                                                                          
006800 01  TIDTYP                      PIC 9(2).                                
006900     88  PAC-TID                 VALUE 11.                                
007000     88  ADM-TID                 VALUE 12.                                
007100     88  LAST-TID                VALUE 13.                                
007200                                                                          
007300 01  DATRAKN-TIAADDD             PIC 9(5)    VALUE ZERO.                  
007400 01  DATRAKN-DAT REDEFINES DATRAKN-TIAADDD.                               
007500     03  DATRAKN-AA              PIC 9(2).                                
007600     03  DATRAKN-DDD             PIC 9(3).                                
007700                                                                          
007800 01  W-TIME-START-TIAAMMDD       PIC 9(6)    VALUE ZERO.                  
007900 01  W-TIME-START REDEFINES W-TIME-START-TIAAMMDD.                        
008000     03  W-TIME-START-AA         PIC 9(2).                                
008100     03  W-TIME-START-MMDD       PIC 9(4).                                
008200                                                                          
008300 01  W-TIME-STOP-TIAAMMDD       PIC 9(6)     VALUE ZERO.                  
008400 01  W-TIME-STOP REDEFINES W-TIME-STOP-TIAAMMDD.                          
008500     03  W-TIME-STOP-AA         PIC 9(2).                                 
008600     03  W-TIME-STOP-MMDD       PIC 9(4).                                 
008700                                                                          
008800 01  W-TIME-START-TIHHMMSS       PIC 9(6)    VALUE ZERO.                  
008900 01  W-TIME-START REDEFINES W-TIME-START-TIHHMMSS.                        
009000     03  W-TIME-START-HH         PIC 9(2).                                
009100     03  W-TIME-START-MM         PIC 9(2).                                
009200     03  W-TIME-START-SS         PIC 9(2).                                
009300                                                                          
009400 01  W-TIME-START-HHMM REDEFINES W-TIME-START-TIHHMMSS.                   
009500     03  W-START-HHMM            PIC 9(4).                                
009600     03  W-START-SS              PIC 9(2).                                
009700                                                                          
009800 01  W-TIME-STOP-TIHHMMSS        PIC 9(6)    VALUE ZERO.                  
009900 01  W-TIME-STOP REDEFINES W-TIME-STOP-TIHHMMSS.                          
010000     03  W-TIME-STOP-HH          PIC 9(2).                                
010100     03  W-TIME-STOP-MM          PIC 9(2).                                
010200     03  W-TIME-STOP-SS          PIC 9(2).                                
010300                                                                          
010400 01  W-TIME-STOP-HHMM REDEFINES W-TIME-STOP-TIHHMMSS.                     
010500     03  W-STOP-HHMM            PIC 9(4).                                 
010600     03  W-STOP-SS              PIC 9(2).                                 
010700                                                                          
010800 01  W-TISTAMIN                  PIC 9(5)    VALUE ZERO.                  
010900 01  W-TISTART REDEFINES W-TISTAMIN.                                      
011000     03  W-TISTAMIN-X            PIC 9.                                   
011100     03  W-TISTAMIN-HH           PIC 9(2).                                
011200     03  W-TISTAMIN-MM           PIC 9(2).                                
011300                                                                          
011400 01  W-TISTOMIN                  PIC 9(5)    VALUE ZERO.                  
011500 01  W-TISTOP REDEFINES W-TISTOMIN.                                       
011600     03  W-TISTOMIN-X            PIC 9.                                   
011700     03  W-TISTOMIN-HH           PIC 9(2).                                
011800     03  W-TISTOMIN-MM           PIC 9(2).                                
011900                                                                          
012000 01  GENERELLA-SUBPROGRAM.                                                
012100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012500     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
012600                                                                          
012700     EJECT                                                                
012800*    --- PARAMETRAR TILL ABEND                                            
012900                                                                          
013000 01  RKOD-ABEND                  PIC S9(4)  VALUE +33 COMP SYNC.          
013100                                                                          
013200*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
013300                                                                          
013400*01  -COPY WDATAREA                                                       
013600                                                                          
013700     EJECT                                                                
013800*    --- PARAMETRAR TILL SUBPROGRAM WORKDAY                               
013900                                                                          
014000*01  -COPY WORKAREA                                                       
014200                                                                          
014300     EJECT                                                                
014400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014500*                                                                         
014600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014700     SKIP3                                                                
014800 01  NYCKLAR-TILL-DLI.                                                    
014900                                                                          
015000     03  W-WDGX4437-X.                                                    
015100         05  W-IDHTYP-4437       PIC X(4)     VALUE '4437'.               
015200         05  W-IDDC-4437         PIC X(2)     VALUE '00'.                 
015300         05  W-IDPRC-4437        PIC X(4).                                
015400         05  W-VALFRI-4437       PIC X(20)    VALUE LOW-VALUE.            
015500                                                                          
015600     03  W-WDGX4438-X.                                                    
015700         05  W-DADATUM-4438      PIC 9(8)     VALUE ZERO.                 
015900                                                                          
016000     SKIP2                                                                
016100*                                                                         
016200*    --- STATUS-KOD FRÅN IMS                                              
016300 01  STATUS-WS                   PIC XX.                                  
016400     88  SEGMENT-FINNS                       VALUE '  '.                  
016500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016700     SKIP2                                                                
016800 01  GODK-STATUSKODER.                                                    
016900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017000     SKIP3                                                                
017100 01  SSA1                        PIC X(64).                               
017200 01  SSA2                        PIC X(64).                               
017300     EJECT                                                                
017400*    --- IMS FUNKTIONSKODER                                               
017500*01  -COPY W0003                                                          
017700     EJECT                                                                
017800*    ---  DLI INPUT-OUTPUT AREA                                           
017900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
018000     SKIP3                                                                
018100 01  DLI-IO-AREA.                                                         
018200     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
018300     SKIP3                                                                
018400     03  WL443701 REDEFINES IO-AREA.                                      
018500*        05  -COPY WDGX4437   -PRE 4437-                                  
018700     EJECT                                                                
018800     03  WL443711 REDEFINES IO-AREA.                                      
018900*        05  -COPY WDGX4438   -PRE 4437-                                  
019100     EJECT                                                                
019200 LINKAGE SECTION.                                                         
019300                                                                          
019400*   -COPY W411TIME                                                        
019600*                                                                         
019700                                                                          
019800*01  -COPY W0008      -PRE 4437-                                          
020000     05  FILLER                  PIC X.                                   
020100*                                                                         
020200     EJECT                                                                
020300 PROCEDURE DIVISION USING TIME-W411TIME 4437-PCB.                         
020400                                                                          
020500                                                                          
020600     PERFORM A-INIT                                                       
020700                                                                          
020800     EVALUATE TIME-KDCALL                                                 
020900     WHEN 11                                                              
021000         PERFORM B-BEHANDLA-WOPS-PAC                                      
021100     WHEN 12                                                              
021200         PERFORM C-BEHANDLA-WOPS-ADM                                      
021300     WHEN 13                                                              
021400         PERFORM D-BEHANDLA-WOPS-LAST                                     
021500     WHEN 22                                                              
021600         PERFORM E-BEHANDLA-ORDER-ENTRY                                   
021700     WHEN OTHER                                                           
021800         MOVE 'FEL FRÅN SUBPROGRAM W411TIME I HUVUDSLINGAN '              
021900         TO FELTEXT                                                       
022000         CALL ABEND USING RKOD-ABEND                                      
022100     END-EVALUATE                                                         
022200                                                                          
022300     GOBACK                                                               
022400                                                                          
022500     .                                                                    
022600     EJECT                                                                
022700                                                                          
022800 A-INIT SECTION.                                                          
022900                                                                          
023000     PERFORM AA-NOLLSTAELL-FAELT                                          
023100     IF TIME-START-TIAAMMDD = ZERO AND TIME-START-TIHHMMSS = ZERO         
023200       ADD 1 TO ANTAL-NOLL-FAELT                                          
023300     ELSE                                                                 
023400       PERFORM AB-KOLLA-STARTDAT                                          
023500     END-IF                                                               
023600                                                                          
023700     IF TIME-STOP-TIAAMMDD = ZERO AND TIME-STOP-TIHHMMSS = ZERO           
023800       ADD 1 TO ANTAL-NOLL-FAELT                                          
023900     ELSE                                                                 
024000       PERFORM AC-KOLLA-STOPDAT                                           
024100     END-IF                                                               
024200                                                                          
024300     IF TIME-TIARB = ZERO                                                 
024400       ADD 1 TO ANTAL-NOLL-FAELT                                          
024410                                                                          
024411       MOVE W-TIME-STOP-TIAAMMDD    TO TMP1-YYMMDD                        
024412       MOVE W-TIME-START-TIAAMMDD   TO TMP2-YYMMDD                        
024420       PERFORM WY2000P1                                                   
024500       IF TMP1-YYMMDD < TMP2-YYMMDD                                       
024510                                                                          
024600         IF W-TIME-START-AA NOT = 99 AND                                  
024700            W-TIME-STOP-AA NOT = 00                                       
024800           MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION A1'               
024900           TO FELTEXT                                                     
025000           CALL ABEND USING RKOD-ABEND                                    
025100         END-IF                                                           
025200       END-IF                                                             
025210                                                                          
025300       IF W-TIME-STOP-TIAAMMDD = W-TIME-START-TIAAMMDD                    
025310                                                                          
025410         IF W-TIME-STOP-TIHHMMSS < W-TIME-START-TIHHMMSS                  
025500           MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION A2'               
025600           TO FELTEXT                                                     
025700           CALL ABEND USING RKOD-ABEND                                    
025800         END-IF                                                           
025900       END-IF                                                             
026000     ELSE                                                                 
026010                                                                          
026020       IF TIME-TIARB NUMERIC                                              
026030         COMPUTE W-TIARB = TIME-TIARB * 100                               
026040                                                                          
026050         IF TIARB-MM > 59                                                 
026060           MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION A3'               
026070           TO FELTEXT                                                     
026080           CALL ABEND USING RKOD-ABEND                                    
026090         END-IF                                                           
026100       ELSE                                                               
026200         MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION A4'                 
026300         TO FELTEXT                                                       
026400         CALL ABEND USING RKOD-ABEND                                      
026500       END-IF                                                             
026600     END-IF                                                               
027400                                                                          
027500     IF ANTAL-NOLL-FAELT NOT = 1                                          
027600       MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION A5'                   
027700       TO FELTEXT                                                         
027800       CALL ABEND USING RKOD-ABEND                                        
027900     END-IF                                                               
028000                                                                          
028100     .                                                                    
028200     EJECT                                                                
028300                                                                          
028400 AA-NOLLSTAELL-FAELT SECTION.                                             
028500                                                                          
028600     MOVE ZERO TO ANTAL-NOLL-FAELT                                        
028700                  TABELL-ANTAL-MIN                                        
028800                  TIME-ANTAL-MIN                                          
028900                  MIN-RAKNARE                                             
029000                  STARTTID                                                
029100                  STOPTID                                                 
029200                  W-TIARB                                                 
029300                  W-TIARBMIN                                              
029400                  W-DIFFERENS                                             
029500                  TOT-ANTAL-TIM                                           
029600                  TOT-ANTAL-MIN                                           
029700                  W-TIAAMMDD                                              
029800                  ANTAL-HELA-MIN                                          
029900                  DATRAKN-TIAADDD                                         
030000                  W-TIME-START-TIAAMMDD                                   
030100                  W-TIME-STOP-TIAAMMDD                                    
030200                  W-TIME-START-TIHHMMSS                                   
030300                  W-TIME-STOP-TIHHMMSS                                    
030400                  W-TISTAMIN                                              
030500                  W-TISTOMIN                                              
030600     MOVE ' '  TO TIME-KDSVAR                                             
030700                                                                          
030800     .                                                                    
030900     EJECT                                                                
031000                                                                          
031100 AB-KOLLA-STARTDAT SECTION.                                               
031200                                                                          
031300     MOVE 'AAMMDD'            TO DAT-KDDATFORM                            
031400     MOVE TIME-START-TIAAMMDD TO DAT-I-TIDATUM                            
031500     CALL WDATKONV USING DAT-KDDATFORM                                    
031600                         DAT-I-TIDATUM                                    
031700                         DAT-O-TIDATUM                                    
031800                         DAT-KDSVAR                                       
031900     IF DAT-KDSVAR-FEL                                                    
032000       MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION AB1'                  
032100       TO FELTEXT                                                         
032200       CALL ABEND USING RKOD-ABEND                                        
032300     END-IF                                                               
032400                                                                          
032500     MOVE TIME-START-TIAAMMDD TO W-TIME-START-TIAAMMDD                    
032600     MOVE TIME-START-TIHHMMSS TO W-TIME-START-TIHHMMSS                    
032700                                                                          
032800     IF W-TIME-START-HH > 23 OR W-TIME-START-MM > 59                      
032900       MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION AB2'                  
033000       TO FELTEXT                                                         
033100       CALL ABEND USING RKOD-ABEND                                        
033200     END-IF                                                               
033300     .                                                                    
033310     EJECT                                                                
033400                                                                          
033500 AC-KOLLA-STOPDAT SECTION.                                                
033600                                                                          
033700     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
033800     MOVE TIME-STOP-TIAAMMDD TO DAT-I-TIDATUM                             
033900     CALL WDATKONV USING DAT-KDDATFORM                                    
034000                         DAT-I-TIDATUM                                    
034100                         DAT-O-TIDATUM                                    
034200                         DAT-KDSVAR                                       
034300     IF DAT-KDSVAR-FEL                                                    
034400       MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION AC1'                  
034500       TO FELTEXT                                                         
034600       CALL ABEND USING RKOD-ABEND                                        
034700     END-IF                                                               
034800                                                                          
034900     MOVE TIME-STOP-TIAAMMDD TO W-TIME-STOP-TIAAMMDD                      
035000     MOVE TIME-STOP-TIHHMMSS TO W-TIME-STOP-TIHHMMSS                      
035100                                                                          
035200     IF W-TIME-STOP-HH > 23 OR W-TIME-STOP-MM > 59                        
035300       MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION AC2'                  
035400       TO FELTEXT                                                         
035500       CALL ABEND USING RKOD-ABEND                                        
035600     END-IF                                                               
035700     .                                                                    
035710     EJECT                                                                
035800                                                                          
035900 B-BEHANDLA-WOPS-PAC SECTION.                                             
036000                                                                          
036100     MOVE 11 TO TIDTYP                                                    
036200                                                                          
036300     IF TIME-STARTDAT = ZERO                                              
036400       PERFORM S01-BERAEKNA-STARTDAT-WOPS                                 
036500     END-IF                                                               
036510                                                                          
036600     IF TIME-STOPDAT = ZERO                                               
036700       PERFORM S02-BERAEKNA-STOPDAT-WOPS                                  
036800     END-IF                                                               
036810                                                                          
036900     IF TIME-TIARB = ZERO                                                 
037000       PERFORM S03-BERAEKNA-TIARB-WOPS                                    
037100     END-IF                                                               
037300     .                                                                    
037400     EJECT                                                                
037500                                                                          
037600 C-BEHANDLA-WOPS-ADM SECTION.                                             
037700                                                                          
037800     MOVE 12 TO TIDTYP                                                    
037900                                                                          
038000     IF TIME-STARTDAT = ZERO                                              
038100       PERFORM S01-BERAEKNA-STARTDAT-WOPS                                 
038200     END-IF                                                               
038210                                                                          
038300     IF TIME-STOPDAT = ZERO                                               
038400       PERFORM S02-BERAEKNA-STOPDAT-WOPS                                  
038500     END-IF                                                               
038510                                                                          
038600     IF TIME-TIARB = ZERO                                                 
038700       PERFORM S03-BERAEKNA-TIARB-WOPS                                    
038800     END-IF                                                               
039000     .                                                                    
039100     EJECT                                                                
039200                                                                          
039300 D-BEHANDLA-WOPS-LAST SECTION.                                            
039400                                                                          
039500     MOVE 13 TO TIDTYP                                                    
039600                                                                          
039700     IF TIME-STARTDAT = ZERO                                              
039800       PERFORM S01-BERAEKNA-STARTDAT-WOPS                                 
039900     END-IF                                                               
039910                                                                          
040000     IF TIME-STOPDAT = ZERO                                               
040100       PERFORM S02-BERAEKNA-STOPDAT-WOPS                                  
040200     END-IF                                                               
040210                                                                          
040300     IF TIME-TIARB = ZERO                                                 
040400       PERFORM S03-BERAEKNA-TIARB-WOPS                                    
040500     END-IF                                                               
040700     .                                                                    
040800     EJECT                                                                
040900                                                                          
041000 E-BEHANDLA-ORDER-ENTRY SECTION.                                          
041100                                                                          
041200     MOVE ZERO TO TIME-IDPRC                                              
041300     IF TIME-STARTDAT = ZERO                                              
041400       PERFORM EA-BERAEKNA-STARTDAT-OE                                    
041500     END-IF                                                               
041510                                                                          
041600     IF TIME-STOPDAT = ZERO                                               
041700       PERFORM EB-BERAEKNA-STOPDAT-OE                                     
041800     END-IF                                                               
041810                                                                          
041900     IF TIME-TIARB = ZERO                                                 
042000       PERFORM EC-BERAEKNA-TIARB-OE                                       
042100     END-IF                                                               
042300     .                                                                    
042400     EJECT                                                                
042500                                                                          
042600 EA-BERAEKNA-STARTDAT-OE SECTION.                                         
042700                                                                          
042800     COMPUTE W-TIARBMIN = (TIARB-HHH * 60) + TIARB-MM                     
042900     MOVE TIME-STOP-TIHHMMSS TO W-TIME-STOP-TIHHMMSS                      
043000     PERFORM EAA-BERAEKNA-INNEVARANDE-DAG                                 
043100                                                                          
043200     PERFORM UNTIL MIN-RAKNARE NOT < W-TIARBMIN                           
043300       PERFORM EAB-RAEKNA-DAGAR-BAKAT                                     
043400     END-PERFORM                                                          
043500                                                                          
043600     MOVE W-TIAAMMDD TO TIME-START-TIAAMMDD                               
043700     PERFORM EAC-RAEKNA-OM-MINUTER                                        
043800     MOVE W-TIME-START-TIHHMMSS TO TIME-START-TIHHMMSS                    
043900                                                                          
044000     .                                                                    
044100     EJECT                                                                
044200                                                                          
044300 EAA-BERAEKNA-INNEVARANDE-DAG SECTION.                                    
044400                                                                          
044500     MOVE 001                TO WORK-KDCALL                               
044510     MOVE TIME-IDDC          TO WORK-IDDC                                 
044600     MOVE TIME-STOP-TIAAMMDD TO WORK-TIAAMMDD-FOM                         
044700                                WORK-TIAAMMDD-TOM                         
044800                                W-TIAAMMDD                                
044900     CALL WORKDAY  USING WORK-KDCALL                                      
045000                         WORK-DATE-AREA                                   
045100                         WORK-KDSVAR                                      
045200     IF WORK-KDSVAR-FEL                                                   
045300       MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION EAA '                 
045400       TO FELTEXT                                                         
045500       CALL ABEND USING RKOD-ABEND                                        
045600     END-IF                                                               
045610                                                                          
046700     IF WORK-KVWORKD = 1                                                  
046800                                                                          
046900       IF W-TIME-STOP-HH < 17                                             
047000                                                                          
047100         IF W-TIME-STOP-HH > 8                                            
047200           COMPUTE MIN-RAKNARE =                                          
047300           ((W-TIME-STOP-HH * 60) + W-TIME-STOP-MM) - (8 * 60)            
047400         END-IF                                                           
047500                                                                          
047510       ELSE                                                               
047520         COMPUTE MIN-RAKNARE = 60 * 9                                     
047530       END-IF                                                             
047600     END-IF                                                               
047700                                                                          
047800     .                                                                    
047900     EJECT                                                                
048000                                                                          
048100 EAB-RAEKNA-DAGAR-BAKAT SECTION.                                          
048200                                                                          
048300     MOVE 'AAMMDD'   TO DAT-KDDATFORM                                     
048400     MOVE W-TIAAMMDD TO DAT-I-TIDATUM                                     
048500                                                                          
048600     CALL WDATKONV USING DAT-KDDATFORM                                    
048700                         DAT-I-TIDATUM                                    
048800                         DAT-O-TIDATUM                                    
048900                         DAT-KDSVAR                                       
049000     IF DAT-KDSVAR-FEL                                                    
049100       MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION EAB1'                 
049200       TO FELTEXT                                                         
049300       CALL ABEND USING RKOD-ABEND                                        
049400     END-IF                                                               
049410                                                                          
049500     MOVE DAT-TIAA-DAGNR TO DATRAKN-AA                                    
049600     MOVE DAT-TIDDD      TO DATRAKN-DDD                                   
049700     MOVE DAT-TIAADDD    TO DATRAKN-TIAADDD                               
049800     COMPUTE DATRAKN-DDD = DATRAKN-DDD - 1                                
049810                                                                          
049900     IF DATRAKN-DDD = ZERO                                                
050000       PERFORM S06-BEHANDLA-AERSSKIFTE                                    
050100     END-IF                                                               
050110                                                                          
050200     MOVE 'AADDD '        TO DAT-KDDATFORM                                
050300     MOVE DATRAKN-TIAADDD TO DAT-I-TIDATUM                                
050400                                                                          
050500     CALL WDATKONV USING DAT-KDDATFORM                                    
050600                         DAT-I-TIDATUM                                    
050700                         DAT-O-TIDATUM                                    
050800                         DAT-KDSVAR                                       
050900     IF DAT-KDSVAR-FEL                                                    
051000       MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION EAB2'                 
051100       TO FELTEXT                                                         
051200       CALL ABEND USING RKOD-ABEND                                        
051300     END-IF                                                               
051310                                                                          
051400     MOVE 001          TO WORK-KDCALL                                     
051410     MOVE TIME-IDDC    TO WORK-IDDC                                       
051500     MOVE DAT-TIAAMMDD TO W-TIAAMMDD                                      
051600                          WORK-TIAAMMDD-FOM                               
051700                          WORK-TIAAMMDD-TOM                               
051800     CALL WORKDAY  USING  WORK-KDCALL                                     
051900                          WORK-DATE-AREA                                  
052000                          WORK-KDSVAR                                     
052100     IF WORK-KDSVAR-FEL                                                   
052200       MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION EAB3'                 
052300       TO FELTEXT                                                         
052400       CALL ABEND USING RKOD-ABEND                                        
052500     END-IF                                                               
052600                                                                          
053200     IF WORK-KVWORKD = 1                                                  
053300       COMPUTE MIN-RAKNARE =                                              
053400       MIN-RAKNARE + (9 * 60)                                             
053500     END-IF                                                               
053700     .                                                                    
053800     EJECT                                                                
053900                                                                          
054000 EAC-RAEKNA-OM-MINUTER SECTION.                                           
054100                                                                          
054200     COMPUTE W-DIFFERENS = MIN-RAKNARE - W-TIARBMIN                       
054300     COMPUTE TOT-ANTAL-TIM = W-DIFFERENS / 60                             
054400     COMPUTE ANTAL-HELA-MIN =                                             
054500     W-DIFFERENS - (TOT-ANTAL-TIM * 60)                                   
054600     COMPUTE TOT-ANTAL-TIM = 8 + TOT-ANTAL-TIM                            
054700     MOVE TOT-ANTAL-TIM  TO W-TIME-START-HH                               
054800     MOVE ANTAL-HELA-MIN TO W-TIME-START-MM                               
054900     MOVE ZERO           TO W-TIME-START-SS                               
055000                                                                          
055100     .                                                                    
055200     EJECT                                                                
055300                                                                          
055400 EB-BERAEKNA-STOPDAT-OE SECTION.                                          
055500                                                                          
055600     COMPUTE W-TIARBMIN = (TIARB-HHH * 60) + TIARB-MM                     
055700     MOVE TIME-START-TIHHMMSS TO W-TIME-START-TIHHMMSS                    
055800     PERFORM EBA-BERAEKNA-INNEVARANDE-DAG                                 
055900                                                                          
055920     PERFORM UNTIL MIN-RAKNARE NOT < W-TIARBMIN                           
056100       PERFORM EBB-RAEKNA-DAGAR-FRAMAT                                    
056200     END-PERFORM                                                          
056300                                                                          
056400     MOVE W-TIAAMMDD TO TIME-STOP-TIAAMMDD                                
056500     PERFORM EBC-BERAEKNA-INNEVARANDE-DAG                                 
056600     PERFORM EBD-RAEKNA-OM-MINUTER                                        
056700     MOVE W-TIME-STOP-TIHHMMSS TO TIME-STOP-TIHHMMSS                      
056800                                                                          
056900     .                                                                    
057000     EJECT                                                                
057100                                                                          
057200 EBA-BERAEKNA-INNEVARANDE-DAG SECTION.                                    
057300                                                                          
057400     MOVE 001                 TO WORK-KDCALL                              
057410     MOVE TIME-IDDC           TO WORK-IDDC                                
057500     MOVE TIME-START-TIAAMMDD TO WORK-TIAAMMDD-FOM                        
057600                                 WORK-TIAAMMDD-TOM                        
057700                                 W-TIAAMMDD                               
057800     CALL WORKDAY  USING WORK-KDCALL                                      
057900                         WORK-DATE-AREA                                   
058000                         WORK-KDSVAR                                      
058100     IF WORK-KDSVAR-FEL                                                   
058200       MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION EBA '                 
058300       TO FELTEXT                                                         
058400       CALL ABEND USING RKOD-ABEND                                        
058500     END-IF                                                               
058600                                                                          
059600     IF WORK-KVWORKD = 1 AND W-TIME-START-HH < 17                         
059610                                                                          
059700       IF W-TIME-START-HH > 8                                             
059800         COMPUTE MIN-RAKNARE =                                            
059900         (17 * 60) - (W-TIME-START-HH * 60 + W-TIME-START-MM)             
060000       ELSE                                                               
060100         COMPUTE MIN-RAKNARE = 9 * 60                                     
060200       END-IF                                                             
060300     END-IF                                                               
060400                                                                          
060500     .                                                                    
060600     EJECT                                                                
060700                                                                          
060800 EBB-RAEKNA-DAGAR-FRAMAT SECTION.                                         
060900                                                                          
061000     MOVE 001        TO WORK-KDCALL                                       
061010     MOVE TIME-IDDC  TO WORK-IDDC                                         
061100     MOVE W-TIAAMMDD TO WORK-TIAAMMDD-FOM                                 
061200                        WORK-TIAAMMDD-TOM                                 
061300     CALL WORKDAY  USING WORK-KDCALL                                      
061400                         WORK-DATE-AREA                                   
061500                         WORK-KDSVAR                                      
061600     IF WORK-KDSVAR-FEL                                                   
061700       MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION EBB '                 
061800       TO FELTEXT                                                         
061900       CALL ABEND USING RKOD-ABEND                                        
062000     END-IF                                                               
062100                                                                          
062200     MOVE WORK-TIAAMMDD-NEXT-WORKDAY TO W-TIAAMMDD                        
062700     IF WORK-KVWORKD = 1                                                  
062800       COMPUTE MIN-RAKNARE = MIN-RAKNARE + (9 * 60)                       
062900     END-IF                                                               
063000                                                                          
063100     .                                                                    
063200     EJECT                                                                
063300                                                                          
063400 EBC-BERAEKNA-INNEVARANDE-DAG SECTION.                                    
063500                                                                          
063600     COMPUTE W-DIFFERENS = MIN-RAKNARE - W-TIARBMIN                       
064100     IF WORK-KVWORKD = 1                                                  
064200       COMPUTE TOT-ANTAL-MIN =                                            
064300       (17 * 60) - W-DIFFERENS                                            
064400     END-IF                                                               
064600     .                                                                    
064700     EJECT                                                                
064800                                                                          
064900 EBD-RAEKNA-OM-MINUTER SECTION.                                           
065000                                                                          
065100     COMPUTE TOT-ANTAL-TIM =                                              
065200     TOT-ANTAL-MIN / 60                                                   
065300     COMPUTE ANTAL-HELA-MIN =                                             
065400     TOT-ANTAL-MIN - (TOT-ANTAL-TIM * 60)                                 
065500     MOVE TOT-ANTAL-TIM  TO W-TIME-STOP-HH                                
065600     MOVE ANTAL-HELA-MIN TO W-TIME-STOP-MM                                
065700     MOVE ZERO           TO W-TIME-STOP-SS                                
065800     .                                                                    
065900     EJECT                                                                
066000                                                                          
066100 EC-BERAEKNA-TIARB-OE SECTION.                                            
066200                                                                          
066300     MOVE 001                 TO WORK-KDCALL                              
066310     MOVE TIME-IDDC           TO WORK-IDDC                                
066400     MOVE TIME-START-TIAAMMDD TO WORK-TIAAMMDD-FOM                        
066500                                 WORK-TIAAMMDD-TOM                        
066600                                 W-TIAAMMDD                               
066700     CALL WORKDAY  USING WORK-KDCALL                                      
066800                         WORK-DATE-AREA                                   
066900                         WORK-KDSVAR                                      
067000     IF WORK-KDSVAR-FEL                                                   
067100       MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION EC '                  
067200       TO FELTEXT                                                         
067300       CALL ABEND USING RKOD-ABEND                                        
067400     END-IF                                                               
067500                                                                          
067600     IF TIME-START-TIAAMMDD = TIME-STOP-TIAAMMDD                          
067700       PERFORM ECA-BERAEKNA-SAMMA-DAG                                     
067800     ELSE                                                                 
067900                                                                          
068900       IF WORK-KVWORKD = 1 AND W-TIME-START-HH < 17                       
069000         IF W-TIME-START-HH > 8                                           
069100         COMPUTE MIN-RAKNARE =                                            
069200         (17 * 60) - (W-TIME-START-HH * 60 + W-TIME-START-MM)             
069300         ELSE                                                             
069400           COMPUTE MIN-RAKNARE = 9 * 60                                   
069500         END-IF                                                           
069600       END-IF                                                             
069610                                                                          
069700       PERFORM ECB-KONVERTERA-DATUM                                       
069800       PERFORM UNTIL TIME-STOP-TIAAMMDD = W-TIAAMMDD                      
069900                                                                          
070400         IF WORK-KVWORKD = 1                                              
070500           COMPUTE MIN-RAKNARE = MIN-RAKNARE + (9 * 60)                   
070600         END-IF                                                           
070700                                                                          
070800         PERFORM ECB-KONVERTERA-DATUM                                     
070900                                                                          
071000       END-PERFORM                                                        
071100                                                                          
071200       PERFORM ECC-BERAEKNA-INNEVARANDE-DAG                               
071300       PERFORM ECD-RAEKNA-OM-MINUTER                                      
071400                                                                          
071500     END-IF                                                               
071600                                                                          
071700     .                                                                    
071800     EJECT                                                                
071900                                                                          
072000 ECA-BERAEKNA-SAMMA-DAG SECTION.                                          
072100                                                                          
074200                                                                          
074300     IF WORK-KVWORKD = 1                                                  
074310                                                                          
074400       IF W-TIME-START-HH < 17                                            
074410                                                                          
074500         IF W-TIME-START-HH > 8                                           
074600           COMPUTE STARTTID =                                             
074700           W-TIME-START-HH * 60 + W-TIME-START-MM                         
074800         ELSE                                                             
074900           COMPUTE STARTTID = 8 * 60                                      
075000         END-IF                                                           
075010                                                                          
075100       END-IF                                                             
075110                                                                          
075200       IF W-TIME-STOP-HH < 17                                             
075210                                                                          
075300         IF W-TIME-STOP-HH > 8                                            
075400           COMPUTE STOPTID =                                              
075500           W-TIME-STOP-HH * 60 + W-TIME-STOP-MM                           
075600         ELSE                                                             
075700           COMPUTE STOPTID = 8 * 60                                       
075800         END-IF                                                           
075810                                                                          
075900       ELSE                                                               
076000         COMPUTE STOPTID = 17 * 60                                        
076100       END-IF                                                             
076200     END-IF                                                               
076300                                                                          
076400     COMPUTE TOT-ANTAL-MIN = (STOPTID - STARTTID)                         
076500     COMPUTE TOT-ANTAL-TIM =                                              
076600     TOT-ANTAL-MIN / 60                                                   
076700     COMPUTE ANTAL-HELA-MIN =                                             
076800     TOT-ANTAL-MIN - (TOT-ANTAL-TIM * 60)                                 
076900     MOVE TOT-ANTAL-TIM  TO TIARB-HHH                                     
077000     MOVE ANTAL-HELA-MIN TO TIARB-MM                                      
077100     COMPUTE TIME-TIARB ROUNDED = W-TIARB * 0.01                          
077200                                                                          
077300     .                                                                    
077400     EJECT                                                                
077500                                                                          
077600 ECB-KONVERTERA-DATUM SECTION.                                            
077700                                                                          
077800     MOVE 'AAMMDD'   TO DAT-KDDATFORM                                     
077900     MOVE W-TIAAMMDD TO DAT-I-TIDATUM                                     
078000                                                                          
078100     CALL WDATKONV USING DAT-KDDATFORM                                    
078200                         DAT-I-TIDATUM                                    
078300                         DAT-O-TIDATUM                                    
078400                         DAT-KDSVAR                                       
078500     IF DAT-KDSVAR-FEL                                                    
078600       MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION ECB1'                 
078700       TO FELTEXT                                                         
078800       CALL ABEND USING RKOD-ABEND                                        
078900     END-IF                                                               
079000                                                                          
079100     MOVE DAT-TIAADDD TO DATRAKN-TIAADDD                                  
079200     COMPUTE DATRAKN-DDD = DATRAKN-DDD + 1                                
079220     IF DATRAKN-DDD > 365                                                 
079310                                                                          
079400       IF DATRAKN-AA = 99                                                 
079500         MOVE 00 TO DATRAKN-AA                                            
079600       ELSE                                                               
079700         COMPUTE DATRAKN-AA = DATRAKN-AA + 1                              
079800       END-IF                                                             
079900       MOVE 1 TO DATRAKN-DDD                                              
080000     END-IF                                                               
080010                                                                          
080100     MOVE 'AADDD '        TO DAT-KDDATFORM                                
080200     MOVE DATRAKN-TIAADDD TO DAT-I-TIDATUM                                
080300                                                                          
080400     CALL WDATKONV USING DAT-KDDATFORM                                    
080500                         DAT-I-TIDATUM                                    
080600                         DAT-O-TIDATUM                                    
080700                         DAT-KDSVAR                                       
080800     IF DAT-KDSVAR-FEL                                                    
080900       MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION ECB2'                 
081000       TO FELTEXT                                                         
081100       CALL ABEND USING RKOD-ABEND                                        
081200     END-IF                                                               
081210                                                                          
081300     MOVE 001          TO WORK-KDCALL                                     
081310     MOVE TIME-IDDC    TO WORK-IDDC                                       
081400     MOVE DAT-TIAAMMDD TO W-TIAAMMDD                                      
081500                          WORK-TIAAMMDD-FOM                               
081600                          WORK-TIAAMMDD-TOM                               
081700     CALL WORKDAY  USING WORK-KDCALL                                      
081800                         WORK-DATE-AREA                                   
081900                         WORK-KDSVAR                                      
082000     IF WORK-KDSVAR-FEL                                                   
082100       MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION ECB3'                 
082200       TO FELTEXT                                                         
082300       CALL ABEND USING RKOD-ABEND                                        
082400     END-IF                                                               
082500                                                                          
082600     .                                                                    
082700     EJECT                                                                
082800                                                                          
082900 ECC-BERAEKNA-INNEVARANDE-DAG SECTION.                                    
083000                                                                          
084300     IF WORK-KVWORKD = 1                                                  
084310                                                                          
084400       IF W-TIME-STOP-HH < 17                                             
084410                                                                          
084500         IF W-TIME-STOP-HH > 8                                            
084600           COMPUTE MIN-RAKNARE =                                          
084700           MIN-RAKNARE +                                                  
084800           ((W-TIME-STOP-HH * 60 + W-TIME-STOP-MM) - (8 * 60))            
084900         END-IF                                                           
084910                                                                          
085000       ELSE                                                               
085100         COMPUTE MIN-RAKNARE = MIN-RAKNARE + (9 * 60)                     
085200       END-IF                                                             
085300     END-IF                                                               
085400                                                                          
085500     .                                                                    
085600     EJECT                                                                
085700                                                                          
085800 ECD-RAEKNA-OM-MINUTER SECTION.                                           
085900                                                                          
086000     COMPUTE TOT-ANTAL-TIM  =                                             
086100     MIN-RAKNARE / 60                                                     
086200     COMPUTE ANTAL-HELA-MIN =                                             
086300     MIN-RAKNARE - (TOT-ANTAL-TIM * 60)                                   
086400     MOVE TOT-ANTAL-TIM  TO TIARB-HHH                                     
086500     MOVE ANTAL-HELA-MIN TO TIARB-MM                                      
086600     COMPUTE TIME-TIARB ROUNDED = W-TIARB * 0.01                          
086700                                                                          
086800     .                                                                    
086900     EJECT                                                                
087000                                                                          
087100 S01-BERAEKNA-STARTDAT-WOPS SECTION.                                      
087200                                                                          
087300     COMPUTE W-TIARBMIN = (TIARB-HHH * 60) + TIARB-MM                     
087400                                                                          
087500     MOVE TIME-IDDC          TO W-IDDC-4437                               
087600     MOVE TIME-IDPRC         TO W-IDPRC-4437                              
087700     MOVE TIME-STOP-TIAAMMDD TO W-DADATUM-4438                            
087800                                W-TIAAMMDD                                
087810     IF W-TIAAMMDD < 500000                                               
087820       MOVE 20                 TO W-DADATUM-4438(1:2)                     
087830     ELSE                                                                 
087840       IF W-TIAAMMDD < 999999                                             
087850         MOVE 19               TO W-DADATUM-4438(1:2)                     
087860       ELSE                                                               
087870         MOVE 99999999         TO W-DADATUM-4438                          
087880       END-IF                                                             
087890     END-IF                                                               
087900                                                                          
088000     PERFORM IMS-01-GU-4437-WDR101                                        
088100                                                                          
088200     IF SEGMENT-SAKNAS                                                    
088300       PERFORM S04-SAETT-IDPRC-TILL-9999                                  
088400     END-IF                                                               
088500                                                                          
088510     IF TIME-KDSVAR-OK                                                    
088600       PERFORM IMS-04-GNP-4437-WDR110                                     
088700       IF SEGMENT-SAKNAS                                                  
088800         MOVE FEL TO TIME-KDSVAR                                          
088900       ELSE                                                               
089100          PERFORM S05-FLYTTA-AREA                                         
089200          MOVE TIME-STOP-TIHHMMSS TO W-TIME-STOP-TIHHMMSS                 
089300                                                                          
089400          IF W-TISTOMIN > ZERO                                            
089500            PERFORM  S01A-BERAEKNA-INNEVARANDE-DAG                        
089600          END-IF                                                          
089700                                                                          
089720          PERFORM UNTIL MIN-RAKNARE NOT < W-TIARBMIN  OR                  
089900                        TIME-KDSVAR-FEL                                   
090000                                                                          
090100            PERFORM S01B-RAEKNA-DAGAR-BAKAT                               
090200                                                                          
090300          END-PERFORM                                                     
090400          IF TIME-KDSVAR-OK                                               
090500            PERFORM S01C-RAEKNA-OM-MINUTER                                
090600            MOVE W-TIME-START-TIHHMMSS TO TIME-START-TIHHMMSS             
090700            MOVE W-TIAAMMDD            TO TIME-START-TIAAMMDD             
090800          END-IF                                                          
090801                                                                          
090810       END-IF                                                             
090900     ELSE                                                                 
091000       MOVE FEL TO TIME-KDSVAR                                            
091100     END-IF                                                               
091200                                                                          
091300     .                                                                    
091400     EJECT                                                                
091500                                                                          
091600 S01A-BERAEKNA-INNEVARANDE-DAG SECTION.                                   
091700                                                                          
091720     IF W-STOP-HHMM NOT < W-TISTOMIN                                      
091900       COMPUTE MIN-RAKNARE =                                              
092000       (W-TISTOMIN-HH * 60 + W-TISTOMIN-MM) -                             
092100       (W-TISTAMIN-HH * 60 + W-TISTAMIN-MM)                               
092200       MOVE MIN-RAKNARE TO TABELL-ANTAL-MIN                               
092300     END-IF                                                               
092400                                                                          
092420     IF W-STOP-HHMM > W-TISTAMIN AND                                      
092600        W-STOP-HHMM < W-TISTOMIN                                          
092700                                                                          
092800       COMPUTE MIN-RAKNARE =                                              
092900       (W-TIME-STOP-HH * 60 + W-TIME-STOP-MM) -                           
093000       (W-TISTAMIN-HH * 60 + W-TISTAMIN-MM)                               
093100       MOVE MIN-RAKNARE      TO TABELL-ANTAL-MIN                          
093200       MOVE W-TIME-STOP-HHMM TO W-TISTOMIN                                
093300     END-IF                                                               
093400                                                                          
093500     .                                                                    
093600     EJECT                                                                
093700                                                                          
093800  S01B-RAEKNA-DAGAR-BAKAT SECTION.                                        
093900                                                                          
094000     MOVE 'AAMMDD'   TO DAT-KDDATFORM                                     
094100     MOVE W-TIAAMMDD TO DAT-I-TIDATUM                                     
094200                                                                          
094300     CALL WDATKONV USING DAT-KDDATFORM                                    
094400                         DAT-I-TIDATUM                                    
094500                         DAT-O-TIDATUM                                    
094600                         DAT-KDSVAR                                       
094700     IF DAT-KDSVAR-FEL                                                    
094800       MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION S01B1'                
094900       TO FELTEXT                                                         
095000       CALL ABEND USING RKOD-ABEND                                        
095100     END-IF                                                               
095110                                                                          
095200     MOVE DAT-TIAADDD TO DATRAKN-TIAADDD                                  
095300     COMPUTE  DATRAKN-DDD = DATRAKN-DDD - 1                               
095310                                                                          
095400     IF DATRAKN-DDD = ZERO                                                
095500       PERFORM S06-BEHANDLA-AERSSKIFTE                                    
095600     END-IF                                                               
095610                                                                          
095700     MOVE 'AADDD '        TO DAT-KDDATFORM                                
095800     MOVE DATRAKN-TIAADDD TO DAT-I-TIDATUM                                
095900                                                                          
096000     CALL WDATKONV USING DAT-KDDATFORM                                    
096100                         DAT-I-TIDATUM                                    
096200                         DAT-O-TIDATUM                                    
096300                         DAT-KDSVAR                                       
096400     IF DAT-KDSVAR-FEL                                                    
096500       MOVE 'FEL FRÅN SUBPROGRAM W411TIME I SECTION S01B2'                
096600       TO FELTEXT                                                         
096700       CALL ABEND USING RKOD-ABEND                                        
096800     END-IF                                                               
096810                                                                          
096900     MOVE DAT-TIAAMMDD TO W-DADATUM-4438                                  
097000                          W-TIAAMMDD                                      
097010     IF W-TIAAMMDD < 500000                                               
097020       MOVE 20                 TO W-DADATUM-4438(1:2)                     
097030     ELSE                                                                 
097040       IF W-TIAAMMDD < 999999                                             
097050         MOVE 19               TO W-DADATUM-4438(1:2)                     
097060       ELSE                                                               
097070         MOVE 99999999         TO W-DADATUM-4438                          
097080       END-IF                                                             
097090     END-IF                                                               
097100                                                                          
097200     PERFORM IMS-04-GNP-4437-WDR110                                       
097300     IF SEGMENT-FINNS                                                     
097400       PERFORM S05-FLYTTA-AREA                                            
097500       COMPUTE TABELL-ANTAL-MIN =                                         
097600       ( W-TISTOMIN-HH * 60 + W-TISTOMIN-MM ) -                           
097700       ( W-TISTAMIN-HH * 60 + W-TISTAMIN-MM )                             
097800       ADD TABELL-ANTAL-MIN TO MIN-RAKNARE                                
097900     ELSE                                                                 
098000       MOVE FEL TO TIME-KDSVAR                                            
098100     END-IF                                                               
098200                                                                          
098300     .                                                                    
098400     EJECT                                                                
098500                                                                          
098600 S01C-RAEKNA-OM-MINUTER SECTION.                                          
098700                                                                          
098800     COMPUTE W-DIFFERENS = MIN-RAKNARE - W-TIARBMIN                       
098900     COMPUTE TOT-ANTAL-TIM =                                              
099000     W-DIFFERENS / 60                                                     
099100     COMPUTE ANTAL-HELA-MIN =                                             
099200     W-DIFFERENS - (TOT-ANTAL-TIM * 60)                                   
099300     COMPUTE TOT-ANTAL-TIM =                                              
099400     W-TISTAMIN-HH + TOT-ANTAL-TIM                                        
099500     COMPUTE ANTAL-HELA-MIN =                                             
099600     ANTAL-HELA-MIN + W-TISTAMIN-MM                                       
099610                                                                          
099700     IF ANTAL-HELA-MIN > 59                                               
099800       ADD 1 TO TOT-ANTAL-TIM                                             
099900       COMPUTE ANTAL-HELA-MIN = ANTAL-HELA-MIN - 60                       
100000     END-IF                                                               
100010                                                                          
100100     MOVE ANTAL-HELA-MIN TO W-TIME-START-MM                               
100200     MOVE TOT-ANTAL-TIM  TO W-TIME-START-HH                               
100300     MOVE ZERO           TO W-TIME-START-SS                               
100400                                                                          
100500     .                                                                    
100600     EJECT                                                                
100700                                                                          
100800 S02-BERAEKNA-STOPDAT-WOPS SECTION.                                       
100900                                                                          
101000     COMPUTE W-TIARBMIN = (TIARB-HHH * 60) + TIARB-MM                     
101100                                                                          
101200     MOVE TIME-IDDC     TO W-IDDC-4437                                    
101300     MOVE TIME-IDPRC    TO W-IDPRC-4437                                   
101400                                                                          
101500     PERFORM IMS-01-GU-4437-WDR101                                        
101600     IF SEGMENT-SAKNAS                                                    
101700       PERFORM S04-SAETT-IDPRC-TILL-9999                                  
101800     END-IF                                                               
101900                                                                          
102000     IF TIME-KDSVAR-OK                                                    
102100       MOVE TIME-START-TIAAMMDD TO W-DADATUM-4438                         
102200                                   W-TIAAMMDD                             
102210       IF W-TIAAMMDD < 500000                                             
102220         MOVE 20               TO W-DADATUM-4438(1:2)                     
102230       ELSE                                                               
102240         IF W-TIAAMMDD < 999999                                           
102250           MOVE 19             TO W-DADATUM-4438(1:2)                     
102260         ELSE                                                             
102270           MOVE 99999999       TO W-DADATUM-4438                          
102280         END-IF                                                           
102290       END-IF                                                             
102300       PERFORM IMS-04-GNP-4437-WDR110                                     
102400       IF SEGMENT-FINNS                                                   
102500         PERFORM S05-FLYTTA-AREA                                          
102600         MOVE TIME-START-TIHHMMSS TO W-TIME-START-TIHHMMSS                
102610                                                                          
102700         IF W-TISTOMIN > ZERO                                             
102800           PERFORM S02A-BERAEKNA-INNEVARANDE-DAG                          
102900         END-IF                                                           
103000                                                                          
103020         PERFORM UNTIL MIN-RAKNARE NOT < W-TIARBMIN                       
103200         OR TIME-KDSVAR-FEL                                               
103300                                                                          
103700         PERFORM IMS-03-GNP-4437-WDR110                                   
103810                                                                          
103900           IF SEGMENT-FINNS                                               
104000             PERFORM S05-FLYTTA-AREA                                      
104100             MOVE 4437-4438-DADATUM (3:6) TO W-TIAAMMDD                   
104200             COMPUTE TABELL-ANTAL-MIN =                                   
104300             ( W-TISTOMIN-HH * 60 + W-TISTOMIN-MM ) -                     
104400             ( W-TISTAMIN-HH * 60 + W-TISTAMIN-MM )                       
104500              ADD TABELL-ANTAL-MIN TO MIN-RAKNARE                         
104600           ELSE                                                           
104700             MOVE FEL TO TIME-KDSVAR                                      
104800           END-IF                                                         
104900         END-PERFORM                                                      
105000                                                                          
105100         IF TIME-KDSVAR-OK                                                
105200           PERFORM S02B-RAEKNA-OM-MINUTER                                 
105300           MOVE W-TIME-STOP-TIHHMMSS TO TIME-STOP-TIHHMMSS                
105400           MOVE W-TIAAMMDD           TO TIME-STOP-TIAAMMDD                
105500         END-IF                                                           
105510                                                                          
105600       ELSE                                                               
105700         MOVE FEL TO TIME-KDSVAR                                          
105800       END-IF                                                             
105900     ELSE                                                                 
106000       MOVE FEL TO TIME-KDSVAR                                            
106100     END-IF                                                               
106200                                                                          
106300     .                                                                    
106400     EJECT                                                                
106500                                                                          
106600 S02A-BERAEKNA-INNEVARANDE-DAG SECTION.                                   
106700                                                                          
106720     IF W-START-HHMM NOT > W-TISTAMIN                                     
106900       COMPUTE MIN-RAKNARE =                                              
107000       (W-TISTOMIN-HH * 60 + W-TISTOMIN-MM) -                             
107100       (W-TISTAMIN-HH * 60 + W-TISTAMIN-MM)                               
107200       MOVE MIN-RAKNARE TO TABELL-ANTAL-MIN                               
107300     END-IF                                                               
107400                                                                          
107420     IF W-START-HHMM > W-TISTAMIN AND                                     
107600        W-START-HHMM < W-TISTOMIN                                         
107700                                                                          
107800       COMPUTE MIN-RAKNARE =                                              
107900       (W-TISTOMIN-HH * 60 + W-TISTOMIN-MM) -                             
108000       (W-TIME-START-HH * 60 + W-TIME-START-MM)                           
108100       MOVE MIN-RAKNARE  TO TABELL-ANTAL-MIN                              
108200       MOVE W-START-HHMM TO W-TISTAMIN                                    
108300     END-IF                                                               
108400                                                                          
108500     .                                                                    
108600     EJECT                                                                
108700                                                                          
108800 S02B-RAEKNA-OM-MINUTER SECTION.                                          
108900                                                                          
109000     COMPUTE W-DIFFERENS = MIN-RAKNARE - W-TIARBMIN                       
109010     END-COMPUTE                                                          
109020                                                                          
109100     COMPUTE TOT-ANTAL-MIN =                                              
109200     TABELL-ANTAL-MIN - W-DIFFERENS                                       
109210     END-COMPUTE                                                          
109220                                                                          
109300     COMPUTE TOT-ANTAL-TIM =                                              
109400     TOT-ANTAL-MIN / 60                                                   
109410     END-COMPUTE                                                          
109420                                                                          
109500     COMPUTE ANTAL-HELA-MIN =                                             
109600     TOT-ANTAL-MIN - (TOT-ANTAL-TIM * 60)                                 
109610     END-COMPUTE                                                          
109620                                                                          
109700     COMPUTE TOT-ANTAL-TIM =                                              
109800     W-TISTAMIN-HH + TOT-ANTAL-TIM                                        
109810     END-COMPUTE                                                          
109820                                                                          
109900     COMPUTE ANTAL-HELA-MIN =                                             
110000     ANTAL-HELA-MIN + W-TISTAMIN-MM                                       
110010     END-COMPUTE                                                          
110100                                                                          
110200     IF ANTAL-HELA-MIN > 59                                               
110300       ADD 1 TO TOT-ANTAL-TIM                                             
110400       COMPUTE ANTAL-HELA-MIN = ANTAL-HELA-MIN - 60                       
110410       END-COMPUTE                                                        
110500     END-IF                                                               
110600                                                                          
110700     MOVE TOT-ANTAL-TIM  TO W-TIME-STOP-HH                                
110800     MOVE ANTAL-HELA-MIN TO W-TIME-STOP-MM                                
110900     MOVE ZERO           TO W-TIME-STOP-SS                                
111000                                                                          
111100     .                                                                    
111200     EJECT                                                                
111300                                                                          
111400 S03-BERAEKNA-TIARB-WOPS SECTION.                                         
111500                                                                          
111600     MOVE TIME-IDDC     TO W-IDDC-4437                                    
111700     MOVE TIME-IDPRC    TO W-IDPRC-4437                                   
111800                                                                          
111900     PERFORM IMS-01-GU-4437-WDR101                                        
112000     IF SEGMENT-SAKNAS                                                    
112100       PERFORM S04-SAETT-IDPRC-TILL-9999                                  
112200     END-IF                                                               
112300                                                                          
112400     IF TIME-KDSVAR-OK                                                    
112500       MOVE TIME-START-TIAAMMDD TO W-DADATUM-4438                         
112600                                   W-TIAAMMDD                             
112610       IF W-TIAAMMDD < 500000                                             
112620         MOVE 20               TO W-DADATUM-4438(1:2)                     
112630       ELSE                                                               
112640         IF W-TIAAMMDD < 999999                                           
112650           MOVE 19             TO W-DADATUM-4438(1:2)                     
112660         ELSE                                                             
112670           MOVE 99999999       TO W-DADATUM-4438                          
112680         END-IF                                                           
112690       END-IF                                                             
112700       PERFORM IMS-04-GNP-4437-WDR110                                     
112800       IF SEGMENT-FINNS                                                   
112900         PERFORM S05-FLYTTA-AREA                                          
113000         MOVE TIME-START-TIHHMMSS TO W-TIME-START-TIHHMMSS                
113010                                                                          
113100         IF TIME-START-TIAAMMDD = TIME-STOP-TIAAMMDD                      
113200           PERFORM S03A-BERAEKNA-SAMMA-DAG                                
113300         ELSE                                                             
113310                                                                          
113400           IF W-TISTOMIN > ZERO                                           
113500             PERFORM S03B-BERAEKNA-INNEVARANDE-DAG                        
113600           END-IF                                                         
113610                                                                          
114000           PERFORM IMS-03-GNP-4437-WDR110                                 
114110                                                                          
114200           IF SEGMENT-FINNS                                               
114300             MOVE 4437-4438-DADATUM (3:6) TO W-TIAAMMDD                   
114400             PERFORM S05-FLYTTA-AREA                                      
114500           ELSE                                                           
114600             MOVE FEL TO TIME-KDSVAR                                      
114700           END-IF                                                         
114710                                                                          
114800           PERFORM UNTIL W-TIAAMMDD = W-TIME-STOP-TIAAMMDD                
114900                         OR TIME-KDSVAR-FEL                               
115000                                                                          
115100               COMPUTE TABELL-ANTAL-MIN =                                 
115200               ( W-TISTOMIN-HH * 60 + W-TISTOMIN-MM ) -                   
115300               ( W-TISTAMIN-HH * 60 + W-TISTAMIN-MM )                     
115400               ADD TABELL-ANTAL-MIN TO MIN-RAKNARE                        
115800               PERFORM IMS-03-GNP-4437-WDR110                             
115910                                                                          
116000               IF SEGMENT-FINNS                                           
116100                 MOVE 4437-4438-DADATUM (3:6) TO W-TIAAMMDD               
116200                 PERFORM S05-FLYTTA-AREA                                  
116300               ELSE                                                       
116400                 MOVE FEL TO TIME-KDSVAR                                  
116500               END-IF                                                     
116510                                                                          
116600           END-PERFORM                                                    
116610                                                                          
116700           IF TIME-KDSVAR-OK                                              
116710                                                                          
116800             IF W-TISTOMIN > ZERO                                         
116900               PERFORM S03C-BERAEKNA-INNEVARANDE-DAG                      
117000             END-IF                                                       
117010                                                                          
117100             PERFORM S03D-RAEKNA-OM-MINUTER                               
117200           END-IF                                                         
117210                                                                          
117300         END-IF                                                           
117400       ELSE                                                               
117500         MOVE FEL TO TIME-KDSVAR                                          
117600       END-IF                                                             
117700     ELSE                                                                 
117800       MOVE FEL TO TIME-KDSVAR                                            
117900     END-IF                                                               
118000                                                                          
118100     .                                                                    
118200     EJECT                                                                
118300                                                                          
118400 S03A-BERAEKNA-SAMMA-DAG SECTION.                                         
118500                                                                          
118600     IF W-TISTOMIN > ZERO                                                 
118620       IF W-START-HHMM > W-TISTAMIN                                       
118800         COMPUTE STARTTID = (W-TIME-START-HH * 60 +                       
118900         W-TIME-START-MM)                                                 
119000       ELSE                                                               
119100         COMPUTE STARTTID = (W-TISTAMIN-HH * 60 +                         
119200         W-TISTAMIN-MM)                                                   
119300       END-IF                                                             
119400                                                                          
119420       IF W-STOP-HHMM < W-TISTOMIN                                        
119600         COMPUTE STOPTID = (W-TIME-STOP-HH * 60 +                         
119700         W-TIME-STOP-MM)                                                  
119800       ELSE                                                               
119900         COMPUTE STOPTID = (W-TISTOMIN-HH * 60 +                          
120000         W-TISTOMIN-MM)                                                   
120100       END-IF                                                             
120200                                                                          
120300       COMPUTE TOT-ANTAL-MIN = (STOPTID - STARTTID)                       
120400       COMPUTE TOT-ANTAL-TIM =                                            
120500       TOT-ANTAL-MIN / 60                                                 
120600       COMPUTE ANTAL-HELA-MIN =                                           
120700       TOT-ANTAL-MIN - (TOT-ANTAL-TIM * 60)                               
120800       MOVE ANTAL-HELA-MIN TO TIARB-MM                                    
120900       MOVE TOT-ANTAL-TIM  TO TIARB-HHH                                   
121000       COMPUTE TIME-TIARB ROUNDED = W-TIARB * 0.01                        
121100     ELSE                                                                 
121200       MOVE ZERO TO TIME-TIARB                                            
121300     END-IF                                                               
121400                                                                          
121500     .                                                                    
121600     EJECT                                                                
121700                                                                          
121800  S03B-BERAEKNA-INNEVARANDE-DAG SECTION.                                  
121900                                                                          
121920     IF W-START-HHMM NOT > W-TISTAMIN                                     
122100       COMPUTE MIN-RAKNARE =                                              
122200       (W-TISTOMIN-HH * 60 + W-TISTOMIN-MM) -                             
122300       (W-TISTAMIN-HH * 60 + W-TISTAMIN-MM)                               
122400       MOVE MIN-RAKNARE TO TABELL-ANTAL-MIN                               
122500     END-IF                                                               
122600                                                                          
122620     IF W-START-HHMM > W-TISTAMIN AND                                     
122800     W-START-HHMM < W-TISTOMIN                                            
122900       COMPUTE MIN-RAKNARE =                                              
123000       (W-TISTOMIN-HH * 60 + W-TISTOMIN-MM) -                             
123100       (W-TIME-START-HH * 60 + W-TIME-START-MM)                           
123200       MOVE MIN-RAKNARE TO TABELL-ANTAL-MIN                               
123300     END-IF                                                               
123400                                                                          
123500     .                                                                    
123600     EJECT                                                                
123700                                                                          
123800 S03C-BERAEKNA-INNEVARANDE-DAG SECTION.                                   
123900                                                                          
123920     IF W-STOP-HHMM  > W-TISTOMIN                                         
124100                                                                          
124200       COMPUTE TABELL-ANTAL-MIN =                                         
124300       (W-TISTOMIN-HH * 60 + W-TISTOMIN-MM) -                             
124400       (W-TISTAMIN-HH * 60 + W-TISTAMIN-MM)                               
124500       ADD TABELL-ANTAL-MIN TO MIN-RAKNARE                                
124600     END-IF                                                               
124700                                                                          
124720     IF W-STOP-HHMM < W-TISTOMIN                                          
124900     AND > W-TISTAMIN                                                     
125000       COMPUTE TABELL-ANTAL-MIN =                                         
125100       (W-TIME-STOP-HH * 60 + W-TIME-STOP-MM) -                           
125200       (W-TISTAMIN-HH * 60 + W-TISTAMIN-MM)                               
125300       ADD TABELL-ANTAL-MIN TO MIN-RAKNARE                                
125400     END-IF                                                               
125500                                                                          
125600     .                                                                    
125700     EJECT                                                                
125800                                                                          
125900 S03D-RAEKNA-OM-MINUTER SECTION.                                          
126000                                                                          
126100     COMPUTE TOT-ANTAL-TIM =                                              
126200     MIN-RAKNARE / 60                                                     
126300     COMPUTE ANTAL-HELA-MIN =                                             
126400     MIN-RAKNARE - (TOT-ANTAL-TIM * 60)                                   
126500     MOVE ANTAL-HELA-MIN TO TIARB-MM                                      
126600     MOVE TOT-ANTAL-TIM  TO TIARB-HHH                                     
126700     COMPUTE TIME-TIARB ROUNDED = W-TIARB * 0.01                          
126800                                                                          
126900     .                                                                    
127000     EJECT                                                                
127100                                                                          
127200 S04-SAETT-IDPRC-TILL-9999 SECTION.                                       
127300                                                                          
127400     MOVE 9999 TO W-IDPRC-4437                                            
127500                  TIME-IDPRC                                              
127600     PERFORM IMS-01-GU-4437-WDR101                                        
127700     IF SEGMENT-SAKNAS                                                    
127800       MOVE FEL TO TIME-KDSVAR                                            
127900     END-IF                                                               
128000                                                                          
128100     .                                                                    
128200     EJECT                                                                
128300                                                                          
128400 S05-FLYTTA-AREA SECTION.                                                 
128500                                                                          
128600     IF PAC-TID                                                           
128700       MOVE 4437-4438-TISTAMIN-PAC TO W-TISTAMIN                          
128800       MOVE 4437-4438-TISTOMIN-PAC TO W-TISTOMIN                          
128900     END-IF                                                               
129000                                                                          
129100     IF ADM-TID                                                           
129200       MOVE 4437-4438-TISTAMIN-ADM TO W-TISTAMIN                          
129300       MOVE 4437-4438-TISTOMIN-ADM TO W-TISTOMIN                          
129400     END-IF                                                               
129500                                                                          
129600     IF LAST-TID                                                          
129700       MOVE 4437-4438-TISTAMIN-LAST TO W-TISTAMIN                         
129800       MOVE 4437-4438-TISTOMIN-LAST TO W-TISTOMIN                         
129900     END-IF                                                               
130000                                                                          
130100     .                                                                    
130200     EJECT                                                                
130300                                                                          
130400 S06-BEHANDLA-AERSSKIFTE SECTION.                                         
130500                                                                          
130600     IF DATRAKN-AA = 00                                                   
130700       MOVE 99 TO DATRAKN-AA                                              
130800     ELSE                                                                 
130900       COMPUTE DATRAKN-AA = DATRAKN-AA - 1                                
131000     END-IF                                                               
131100     MOVE 365 TO DATRAKN-DDD                                              
131200                                                                          
131300     .                                                                    
131400     EJECT                                                                
131500                                                                          
131600                                                                          
131700* --- IMS SEKTIONER ---                                                   
131800     SKIP3                                                                
131900 IMS-01-GU-4437-WDR101 SECTION.                                           
132000                                                                          
132100     STRING 'WL443701(WDGXKEY  =' W-WDGX4437-X ')'                        
132200          DELIMITED BY SIZE INTO SSA1                                     
132300     MOVE '  GE' TO GODK-STATUSKODER                                      
132400     CALL CBLTDLI USING GU 4437-PCB DLI-IO-AREA SSA1                      
132500     MOVE 4437-STATUS-CODE TO STATUS-WS                                   
132600     PERFORM IMS-STATUSKONTROLL                                           
132700     .                                                                    
132800     EJECT                                                                
134200                                                                          
134300 IMS-03-GNP-4437-WDR110 SECTION.                                          
134400                                                                          
134500     MOVE 'WL443711 ' TO SSA1                                             
134600     MOVE '  GE' TO GODK-STATUSKODER                                      
134700     CALL CBLTDLI USING GNP 4437-PCB DLI-IO-AREA SSA1                     
134800     MOVE 4437-STATUS-CODE TO STATUS-WS                                   
134900     PERFORM IMS-STATUSKONTROLL                                           
135000     .                                                                    
135100     EJECT                                                                
135200 IMS-04-GNP-4437-WDR110 SECTION.                                          
135300                                                                          
135400     STRING 'WL443711*F(DADATUM  =' W-WDGX4438-X ')'                      
135500          DELIMITED BY SIZE INTO SSA1                                     
135600     MOVE '  GE' TO GODK-STATUSKODER                                      
135700     CALL CBLTDLI USING GNP 4437-PCB DLI-IO-AREA SSA1                     
135800     MOVE 4437-STATUS-CODE TO STATUS-WS                                   
135900     PERFORM IMS-STATUSKONTROLL                                           
136000     .                                                                    
136100     EJECT                                                                
137300 IMS-STATUSKONTROLL SECTION.                                              
137400                                                                          
137500     SET STATUS-IX TO 1                                                   
137600     SEARCH GODK-STATUS                                                   
137700       AT END                                                             
137800       STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                             
137900       DELIMITED BY SIZE INTO FELTEXT                                     
138000       CALL FELLOG                                                        
138100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
138200     END-SEARCH                                                           
138300     .                                                                    
138310     EJECT                                                                
138500*    -COPY WY2000P1                                                       
