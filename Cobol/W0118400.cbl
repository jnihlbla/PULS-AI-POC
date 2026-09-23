000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W0118400.                                                
000400 AUTHOR.         STEFAN KIHLBERG  UMESH JAIN                              
000500 DATE-WRITTEN.   94/10/26         12/08/28                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER WDK7 MED SB                                                
001000*        SKAPAR FILER MED SAMTLIGA DATAELEMENT FRÅN WDK701                
001100*        EN FIL FÖR VARJE TYP AV LAGER                                    
001200*        LDC         W011.LDC.W01184                                      
001300*        SDC         W011.SDC.W01184                                      
001400*        NDC         W011.NDC.W01184                                      
001500*        NDC         W011.NDC.W01183 (WDK723)                             
001600*        NDC         W011.NDC.W01185 (WDK724)                             
001700*        NDC         W011.NDC.W01188 (WDK728)                             
001800*        NDC         W011.NDC.W01182 (WDK722)                             
001900*                                                                         
002000*    ABENDKODER:                                                          
002100*        U0016 -  . . . .                                                 
002200*        U1000 -  . . . .                                                 
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*          --- SAMTLIGA DATAELEMENT FRÅN WDK711, IDDC = LDC               
003300     SELECT LDC-W01184                 ASSIGN TO W01184D1.                
003400                                                                          
003500*          --- SAMTLIGA DATAELEMENT FRÅN WDK711, IDDC = SDC               
003600     SELECT SDC-W01184                 ASSIGN TO W01184D2.                
003700                                                                          
003800*          --- SAMTLIGA DATAELEMENT FRÅN WDK711, IDDC = NDC               
003900     SELECT NDC-W01184                 ASSIGN TO W01184D3.                
004000                                                                          
004100*          --- SAMTLIGA DATAELEMENT FRÅN WDK723 IDDC = NDC                
004200     SELECT NDC-W01183                 ASSIGN TO W01184D4.                
004300                                                                          
004400*          --- SAMTLIGA DATAELEMENT FRÅN WDK724 IDDC = NDC                
004500     SELECT NDC-W01185                 ASSIGN TO W01184D5.                
004600                                                                          
004700*          --- SAMTLIGA DATAELEMENT FRÅN WDK728 IDDC = NDC                
004800     SELECT NDC-W01188                 ASSIGN TO W01184D6.                
004900                                                                          
005000*          --- SAMTLIGA DATAELEMENT FRÅN WDK722 IDDC = NDC                
005100     SELECT NDC-W01182                 ASSIGN TO W01184D7.                
005200                                                                          
005300     EJECT                                                                
005400 DATA DIVISION.                                                           
005500     SKIP2                                                                
005600 FILE SECTION.                                                            
005700     SKIP3                                                                
005800 FD  LDC-W01184                                                           
005900     RECORDING       F                                                    
006000     BLOCK CONTAINS  0.                                                   
006100                                                                          
006200*01  POST -COPY W01184 -PRE  LDC-W01184- -L.                              
006300                                                                          
006400 FD  SDC-W01184                                                           
006500     RECORDING       F                                                    
006600     BLOCK CONTAINS  0.                                                   
006700                                                                          
006800*01  POST -COPY W01184 -PRE  SDC-W01184- -L.                              
006900                                                                          
007000 FD  NDC-W01184                                                           
007100     RECORDING       F                                                    
007200     BLOCK CONTAINS  0.                                                   
007300                                                                          
007400*01  POST -COPY W01184 -PRE  NDC-W01184-    -L.                           
007500                                                                          
007600     EJECT                                                                
007700 FD  NDC-W01183                                                           
007800     RECORDING       F                                                    
007900     BLOCK CONTAINS  0.                                                   
008000                                                                          
008100*01  POST -COPY W01183 -PRE  NDC-W01183-    -L.                           
008200                                                                          
008300 FD  NDC-W01185                                                           
008400     RECORDING       F                                                    
008500     BLOCK CONTAINS  0.                                                   
008600                                                                          
008700*01  POST -COPY W01185 -PRE  NDC-W01185-    -L.                           
008800                                                                          
008900 FD  NDC-W01188                                                           
009000     RECORDING       F                                                    
009100     BLOCK CONTAINS  0.                                                   
009200                                                                          
009300*01  POST -COPY W01188 -PRE  NDC-W01188-    -L.                           
009400                                                                          
009500 FD  NDC-W01182                                                           
009600     RECORDING       F                                                    
009700     BLOCK CONTAINS  0.                                                   
009800                                                                          
009900*01  POST -COPY W01182 -PRE   NDC-W01182-   -L.                           
010000                                                                          
010100     EJECT                                                                
010200 WORKING-STORAGE SECTION.                                                 
010300*    -- CHECKED BY WY2000                                                 
010400 77  IDPGM                       PIC X(8)    VALUE 'W0118400'.            
010500 77  JA                          PIC X       VALUE 'J'.                   
010600 77  NEJ                         PIC X       VALUE 'N'.                   
010700                                                                          
010800 01  ARBETSAREOR.                                                         
010900     03 WS-FIRST                 PIC X(1)    VALUE 'Y'.                   
011000     03 WS-PREV-IDLAND           PIC X(2)    VALUE SPACE.                 
011100     03 IX                       PIC 9(2)    VALUE ZERO.                  
011200     03 WS-B6-IX                 PIC 9(3)    VALUE ZERO.                  
011300     03 MAX-B6-IX                PIC 9(3)    VALUE ZERO.                  
011400                                                                          
011500     EJECT                                                                
011600*      --- VALID IDDC CODES                                               
011700*                                                                         
011800*01    -COPY WWDC99                                                       
011900       EJECT                                                              
012000 01  WS-TABELL.                                                           
012100     03 WS-IDLAND-TAB  OCCURS 200 INDEXED BY TAB-IX.                      
012200        05 TAB-IDDC            PIC X(2).                                  
012300        05 TAB-IDLANDX2        PIC X(2).                                  
012400                                                                          
012500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
012600 01  FILLER REDEFINES DAGENS-DATUM.                                       
012700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
012800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
013000     EJECT                                                                
013100 01  DYNAMISKA-SUBPROGRAM.                                                
013200*                                                                         
013300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013700     SKIP2                                                                
013800*    --- PARAMETRAR TILL ABEND                                            
013900                                                                          
014000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
014100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
014200     SKIP2                                                                
014300 01  FELTEXT.                                                             
014400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014600     EJECT                                                                
014700*    --- PARAMETRAR TILL POSTSUM                                          
014800*                                                                         
014900*01  -COPY W0005   -PRE  POSTSUM-                                         
015000     EJECT                                                                
015100 01  W01184-AREA.                                                         
015200     03  SDC-UPPGIFTER.                                                   
015300         05  -COPY W01184                                                 
015400     EJECT                                                                
015500 01  W01183-AREA.                                                         
015600     03  -COPY W01183                                                     
015700     EJECT                                                                
015800 01  W01185-AREA.                                                         
015900     03  -COPY W01185                                                     
016000     EJECT                                                                
016100 01  W01188-AREA.                                                         
016200     03  -COPY W01188                                                     
016300     EJECT                                                                
016400 01  W01182-AREA.                                                         
016500     03  -COPY W01182   -PRE W01182-                                      
016600     EJECT                                                                
016700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016800*                                                                         
016900     EJECT                                                                
017000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017100     SKIP3                                                                
017200 01  NYCKLAR-TILL-DLI.                                                    
017300     03  W-IDARTNR-X.                                                     
017400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017500     03  W-IDDC-X.                                                        
017600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
017700     03  W-IDLAND-X.                                                      
017800         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
017900     SKIP2                                                                
018000*    --- STATUS-KOD FRÅN IMS                                              
018100 01  STATUS-WS                   PIC XX.                                  
018200     88  SEGMENT-FINNS                       VALUE '  '.                  
018300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
018400     SKIP2                                                                
018500 01  GODK-STATUSKODER.                                                    
018600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018700     SKIP3                                                                
018800 01  SSA1                        PIC X(64).                               
018900 01  SSA2                        PIC X(64).                               
019000     EJECT                                                                
019100*    --- IMS FUNKTIONSKODER                                               
019200*01  -COPY W0003                                                          
019300     EJECT                                                                
019400*    ---  DLI INPUT-OUTPUT AREA                                           
019500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
019600     SKIP3                                                                
019700 01  DLI-IO-AREA.                                                         
019800     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
019900     SKIP3                                                                
020000     03  WDK701   REDEFINES IO-AREA.                                      
020100*        05  -COPY WDK701                                                 
020200                                                                          
020300     03  WDK711   REDEFINES IO-AREA.                                      
020400*        05  -COPY WDK711                                                 
020500                                                                          
020600     03  WDK722   REDEFINES IO-AREA.                                      
020700*        05  -COPY WDK722                                                 
020800                                                                          
020900     03  WDK723   REDEFINES IO-AREA.                                      
021000*        05  -COPY WDK723                                                 
021100                                                                          
021200     03  WDK724   REDEFINES IO-AREA.                                      
021300*        05  -COPY WDK724                                                 
021400                                                                          
021500     03  WDK727   REDEFINES IO-AREA.                                      
021600*        05  -COPY WDK727                                                 
021700                                                                          
021800     03  WDK728   REDEFINES IO-AREA.                                      
021900*        05  -COPY WDK728                                                 
022000*                                                                         
022100 01  FILLER               PIC X(16)   VALUE 'WDK712 AREA'.                
022200 01   DLI-IO-WDK712.                                                      
022300*     03  -COPY WDK712                                                    
022400                                                                          
022500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
022600 01   DLI-IO-AREA-B601.                                                   
022700*     03  -COPY WDB601                                                    
022800                                                                          
022900 LINKAGE SECTION.                                                         
023000                                                                          
023100     EJECT                                                                
023200*01  -COPY W0008  -PRE WDK7-1-                                            
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500*01  -COPY W0008  -PRE WDK7-2-                                            
023600     05  FILLER                  PIC X.                                   
023700     EJECT                                                                
023800*01  -COPY W0008  -PRE WDB6-                                              
023900     05  FILLER                  PIC X.                                   
024000     EJECT                                                                
024100 PROCEDURE DIVISION  USING WDK7-1-PCB WDK7-2-PCB WDB6-PCB.                
024200     ENTRY 'DLITCBL' USING WDK7-1-PCB WDK7-2-PCB WDB6-PCB.                
024300                                                                          
024400     PERFORM A-INIT                                                       
024500     PERFORM IMS-GET-WDK7-1                                               
024600     PERFORM UNTIL SEGMENT-SLUT                                           
024700        EVALUATE WDK7-1-SEG-NAME-FB                                       
024800           WHEN 'WDK701  '                                                
024900* WS-FIRST FLAG IS USED TO AVOID WRITING OUTPUT RECORD FOR A              
025000* NEW PART NBR WHEN WDK701 IS READ. HERE WE WRITE THE OUTPUT              
025100* RECORD FOR LAST WDK711 AND WDK722 OF PREVIOUS PART NUMBER               
025200* ALSO THE FLAG IS RESET FOR EVERY NEW PART NBR ON WDK701                 
025300              IF WS-FIRST = 'N'                                           
025400                PERFORM S02-WRITE-W01184                                  
025500                PERFORM S04-INITIALIZE-WDK711                             
025600                PERFORM S05-INITIALIZE-WDK722                             
025700                MOVE 'Y' TO WS-FIRST                                      
025800              END-IF                                                      
025900              MOVE SART-IDARTNR TO W-IDARTNR                              
026000              MOVE SPACE        TO WS-PREV-IDLAND                         
026100           WHEN 'WDK711  '                                                
026200* WS-FIRST FLAG IS USED TO AVOID WRITING OUTPUT RECORD FOR A              
026300* NEW PART FIRST TIME WHEN WDK711 IS READ, SO THAT WE CAN READ            
026400* WDK722 UNTIL WE HAVE ALL FIELDS FOR OUTPUT RECORD                       
026500              IF WS-FIRST = 'N'                                           
026600                PERFORM S02-WRITE-W01184                                  
026700                PERFORM S04-INITIALIZE-WDK711                             
026800                PERFORM S05-INITIALIZE-WDK722                             
026900              END-IF                                                      
027000              PERFORM B-MOVE-WDK711                                       
027100              PERFORM S01-SEARCH-IDLAND                                   
027200              MOVE W-IDLAND  TO SLAG-IDLANDX2 OF SLAG-W01184              
027300              IF NDC                                                      
027400                PERFORM C-CHECK-MOVE-WDK712                               
027500              ELSE                                                        
027600                PERFORM S03-INITIALIZE-WDK712                             
027700              END-IF                                                      
027800              MOVE 'N' TO WS-FIRST                                        
027900           WHEN 'WDK722  '                                                
028000              PERFORM D-MOVE-WRITE-WDK722                                 
028100           WHEN 'WDK723  '                                                
028200              PERFORM E-MOVE-WRITE-WDK723                                 
028300           WHEN 'WDK724  '                                                
028400              PERFORM F-MOVE-WRITE-WDK724                                 
028500           WHEN 'WDK727  '                                                
028600              PERFORM G-CHECK-MOVE-WDK727                                 
028700           WHEN 'WDK728  '                                                
028800              PERFORM H-MOVE-WRITE-WDK728                                 
028900        END-EVALUATE                                                      
029000        PERFORM IMS-GET-WDK7-1                                            
029100     END-PERFORM                                                          
029200     PERFORM S02-WRITE-W01184                                             
029300     PERFORM Z-FINIT                                                      
029400     MOVE ZERO TO RETURN-CODE                                             
029500     GOBACK                                                               
029600     .                                                                    
029700     EJECT                                                                
029800                                                                          
029900 A-INIT SECTION.                                                          
030000     OPEN OUTPUT LDC-W01184                                               
030100                 SDC-W01184                                               
030200                 NDC-W01184                                               
030300                 NDC-W01183                                               
030400                 NDC-W01185                                               
030500                 NDC-W01188                                               
030600                 NDC-W01182                                               
030700                                                                          
030800     ACCEPT DAGENS-DATUM  FROM DATE                                       
030900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
031000     INITIALIZE SLAG-W01184                                               
031100                                                                          
031200* LOAD WDB601 TO A TABLE FOR SEARCH OF IDLANDX2 WITH IDDC                 
031300     INITIALIZE WS-TABELL                                                 
031400     MOVE +1 TO WS-B6-IX                                                  
031500                MAX-B6-IX                                                 
031600     PERFORM IMS-GN-WDB601                                                
031700     PERFORM UNTIL SEGMENT-SLUT                                           
031800        MOVE DCS-IDDC         TO TAB-IDDC(WS-B6-IX)                       
031900        MOVE DCS-IDLANDX2     TO TAB-IDLANDX2(WS-B6-IX)                   
032000        ADD +1 TO WS-B6-IX                                                
032100                  MAX-B6-IX                                               
032200        IF WS-B6-IX > 200                                                 
032300           MOVE 'DC-TABELLEN FULL' TO FELTEXT                             
032400           CALL FELLOG                                                    
032500        END-IF                                                            
032600        PERFORM IMS-GN-WDB601                                             
032700     END-PERFORM                                                          
032800     .                                                                    
032900     EJECT                                                                
033000                                                                          
033100 B-MOVE-WDK711     SECTION.                                               
033200     MOVE W-IDARTNR           TO SLAG-IDARTNR                             
033300     IF SLAG-KVEFRS IN WDK711 NOT NUMERIC                                 
033400       DISPLAY 'ARTIKEL ' W-IDARTNR                                       
033500       MOVE +0 TO SLAG-KVEFRS IN WDK711                                   
033600     END-IF                                                               
033700                                                                          
033800     MOVE CORR SLAG-WDK711    TO SLAG-W01184                              
033900                                                                          
034000     MOVE 1 TO IX                                                         
034100     PERFORM UNTIL IX > 12                                                
034200        MOVE SLAG-RESEASON IN SLAG-WDK711(IX) TO                          
034300             SLAG-RESEASON IN SLAG-W01184(IX)                             
034400        ADD 1 TO IX                                                       
034500     END-PERFORM                                                          
034600                                                                          
034700     MOVE SLAG-IDDC IN SLAG-WDK711     TO WS-IDDC                         
034800                                          W-IDDC                          
034900     .                                                                    
035000     EJECT                                                                
035100 C-CHECK-MOVE-WDK712    SECTION.                                          
035200     IF W-IDLAND NOT = WS-PREV-IDLAND                                     
035300       PERFORM IMS-GU-WDK712                                              
035400       IF SEGMENT-FINNS                                                   
035500         MOVE LART-BEFT           TO SLAG-BEFT OF SLAG-W01184             
035600         MOVE LART-IDARTNR-EMBQ0  TO SLAG-IDARTNR-EMBQ0                   
035700                                               OF SLAG-W01184             
035800         MOVE LART-IDARTNR-EMBQ1  TO SLAG-IDARTNR-EMBQ1                   
035900                                               OF SLAG-W01184             
036000         MOVE LART-IDARTNR-EMBQ2  TO SLAG-IDARTNR-EMBQ2                   
036100                                               OF SLAG-W01184             
036200         MOVE LART-IDLANDX2       TO SLAG-IDLANDX2                        
036300                                               OF SLAG-W01184             
036400                                     WS-PREV-IDLAND                       
036500         MOVE LART-KVDAGAR-INLEV  TO SLAG-KVDAGAR-INLEV                   
036600                                               OF SLAG-W01184             
036700         MOVE LART-PRARTSJK       TO SLAG-PRARTSJK                        
036800                                               OF SLAG-W01184             
036900         MOVE LART-PRMATRL        TO SLAG-PRMATRL                         
037000         MOVE LART-TIERSDAT-VIPS  TO SLAG-TIERSDAT-VIPS                   
037100         MOVE LART-VKART          TO SLAG-VKART                           
037200                                               OF SLAG-W01184             
037300         MOVE LART-VLARTNTO       TO SLAG-VLARTNTO                        
037400                                               OF SLAG-W01184             
037500         MOVE LART-KDARTURS       TO SLAG-KDARTURS                        
037600                                               OF SLAG-W01184             
037700         MOVE LART-KDMATRPR       TO SLAG-KDMATRPR                        
037800                                               OF SLAG-W01184             
037900         MOVE LART-DAPUBL         TO SLAG-DAPUBL                          
038000                                               OF SLAG-W01184             
038100         MOVE LART-IDPSN-DC       TO SLAG-IDPSN-DC                        
038200                                               OF SLAG-W01184             
038300         MOVE LART-TIUPPDAT-EMB TO SLAG-TIUPPDAT-EMB                      
038400                                               OF SLAG-W01184             
038500       ELSE                                                               
038600         PERFORM S03-INITIALIZE-WDK712                                    
038700       END-IF                                                             
038800     END-IF                                                               
038900     .                                                                    
039000     EJECT                                                                
039100 D-MOVE-WRITE-WDK722    SECTION.                                          
039200     MOVE XLAG-DAPBPLAN       TO SLAG-DAPBPLAN                            
039300     MOVE XLAG-DASEASON       TO SLAG-DASEASON                            
039400     MOVE XLAG-FLJIT          TO SLAG-FLJIT                               
039500     MOVE XLAG-IDINK          TO SLAG-IDINK                               
039600     MOVE XLAG-IDPLANGR-AG    TO SLAG-IDPLANGR-AG                         
039700     MOVE XLAG-KDAVT          TO SLAG-KDAVT                               
039800     MOVE XLAG-KDLEVPLF       TO SLAG-KDLEVPLF                            
039900     MOVE XLAG-IDANSK         TO SLAG-IDANSK                              
040000     MOVE XLAG-IDLEVNR-SHIP   TO SLAG-IDLEVNR-SHIP                        
040100     MOVE XLAG-KDLPSP         TO SLAG-KDLPSP                              
040200     MOVE XLAG-KDOPPLAN       TO SLAG-KDOPPLAN                            
040300     MOVE XLAG-KVDAGAR-FFH    TO SLAG-KVDAGAR-FFH                         
040400     MOVE XLAG-KVEOQ          TO SLAG-KVEOQ                               
040500     MOVE XLAG-KVPALL         TO SLAG-KVPALL                              
040600     MOVE XLAG-KVPB-PLAN      TO SLAG-KVPB-PLAN                           
040700     MOVE XLAG-KVPB-TREND     TO                                          
040800                              SLAG-KVPB-TREND                             
040900     MOVE XLAG-KVSLAGER       TO SLAG-KVSLAGER                            
041000     MOVE XLAG-KVSLUTKP       TO SLAG-KVSLUTKP                            
041100     MOVE XLAG-KVSPANT        TO SLAG-KVSPANT                             
041200     MOVE XLAG-KVULOAD        TO SLAG-KVULOAD                             
041300     MOVE XLAG-KVVECKOR-FT    TO SLAG-KVVECKOR-FT                         
041400     MOVE XLAG-KVVECKOR-LT    TO SLAG-KVVECKOR-LT                         
041500     MOVE XLAG-KVVECKOR-TREND TO SLAG-KVVECKOR-TREND                      
041600     MOVE XLAG-KVPB-JUST1     TO SLAG-KVPB-JUST1                          
041700     MOVE XLAG-KVPB-JUST2     TO SLAG-KVPB-JUST2                          
041800     MOVE XLAG-TIPBJUST-1     TO SLAG-TIPBJUST-1                          
041900     MOVE XLAG-TIPBJUST-2     TO SLAG-TIPBJUST-2                          
042000     MOVE XLAG-TIMANLED       TO SLAG-TIMANLED                            
042100     MOVE XLAG-TILEVDAT       TO SLAG-TILEVDAT                            
042200     MOVE XLAG-IDLEVNR-FRAM   TO SLAG-IDLEVNR-FRAM                        
042300     MOVE XLAG-IDLEVNR-SHIP-FRAM TO                                       
042400                          SLAG-IDLEVNR-SHIP-FRAM                          
042500     MOVE XLAG-FLLARM-BUF     TO SLAG-FLLARM-BUF                          
042600     MOVE 1 TO IX                                                         
042700     PERFORM UNTIL IX > 12                                                
042800        MOVE XLAG-RESEASON-PLAN(IX) TO                                    
042900             SLAG-RESEASON-PLAN(IX)                                       
043000        ADD 1 TO IX                                                       
043100     END-PERFORM                                                          
043200*                                                                         
043300     MOVE XLAG-TIDATUM-TREND TO                                           
043400                             SLAG-TIDATUM-TREND                           
043500*                                                                         
043600     MOVE 1 TO IX                                                         
043700     PERFORM UNTIL IX > 5                                                 
043800        MOVE XLAG-TILEVDAG(IX)  TO                                        
043900             SLAG-TILEVDAG(IX)                                            
044000        ADD 1 TO IX                                                       
044100     END-PERFORM                                                          
044200*                                                                         
044300     MOVE XLAG-TILPSP         TO SLAG-TILPSP                              
044400     MOVE XLAG-TIMANSEC       TO SLAG-TIMANSEC                            
044500     MOVE XLAG-TIOMSPEC       TO SLAG-TIOMSPEC                            
044600     MOVE XLAG-TIREFSTO-LOC   TO SLAG-TIREFSTO-LOC                        
044700     MOVE XLAG-TISLUTKP       TO SLAG-TISLUTKP                            
044800*WRITE WDK722                                                             
044900     MOVE W-IDARTNR           TO W01182-XLAG-IDARTNR                      
045000     MOVE W-IDDC              TO W01182-XLAG-IDDC                         
045100     MOVE XLAG-WDK722         TO W01182-XLAG-WDK722                       
045200     PERFORM S17-SKRIV-NDC-W01182                                         
045300*                                                                         
045400     .                                                                    
045500     EJECT                                                                
045600                                                                          
045700 E-MOVE-WRITE-WDK723    SECTION.                                          
045800     MOVE W-IDARTNR           TO SAVT-IDARTNR                             
045900     MOVE W-IDDC              TO SAVT-IDDC                                
046000     MOVE CORR SAVT-WDK723    TO SAVT-W01183                              
046100     PERFORM S14-SKRIV-NDC-W01183                                         
046200     .                                                                    
046300     EJECT                                                                
046400                                                                          
046500 F-MOVE-WRITE-WDK724    SECTION.                                          
046600     MOVE W-IDARTNR      TO SPRL-IDARTNR                                  
046700     MOVE W-IDDC         TO SPRL-IDDC                                     
046800     MOVE CORR WDK724    TO SPRL-W01185                                   
046900     PERFORM S15-SKRIV-NDC-W01185                                         
047000     .                                                                    
047100     EJECT                                                                
047200                                                                          
047300 G-CHECK-MOVE-WDK727    SECTION.                                          
047400     MOVE PROG-KVPB-JUST(1)   TO SLAG-KVPB-JUST1                          
047500     MOVE PROG-KVPB-JUST(2)   TO SLAG-KVPB-JUST2                          
047600     MOVE PROG-TIPBJUST(1)    TO SLAG-TIPBJUST-1                          
047700     MOVE PROG-TIPBJUST(2)    TO SLAG-TIPBJUST-2                          
047800     .                                                                    
047900     EJECT                                                                
048000                                                                          
048100 H-MOVE-WRITE-WDK728    SECTION.                                          
048200     MOVE W-IDARTNR      TO TRCK-IDARTNR                                  
048300     MOVE W-IDDC         TO TRCK-IDDC                                     
048400     MOVE CORR WDK728    TO TRCK-W01188                                   
048500     PERFORM S16-SKRIV-NDC-W01188                                         
048600     .                                                                    
048700     EJECT                                                                
048800                                                                          
048900 Z-FINIT SECTION.                                                         
049000     CLOSE LDC-W01184                                                     
049100           SDC-W01184                                                     
049200           NDC-W01184                                                     
049300           NDC-W01183                                                     
049400           NDC-W01185                                                     
049500           NDC-W01188                                                     
049600           NDC-W01182                                                     
049700     SKIP2                                                                
049800     MOVE 'S' TO POSTSUM-OPKOD                                            
049900     CALL POSTSUM USING POSTSUM-PARM                                      
050000     .                                                                    
050100     EJECT                                                                
050200                                                                          
050300 S01-SEARCH-IDLAND SECTION.                                               
050400     SET TAB-IX TO 1                                                      
050500     SEARCH WS-IDLAND-TAB                                                 
050600       AT END                                                             
050700         MOVE 'NO MATCH FOUND IN WDB601' TO FELTEXT                       
050800         CALL FELLOG                                                      
050900       WHEN TAB-IDDC (TAB-IX) = W-IDDC                                    
051000         MOVE TAB-IDLANDX2(TAB-IX) TO W-IDLAND                            
051100     END-SEARCH                                                           
051200     .                                                                    
051300     EJECT                                                                
051400                                                                          
051500 S02-WRITE-W01184 SECTION.                                                
051600     EVALUATE TRUE                                                        
051700        WHEN LDC                                                          
051800           PERFORM S11-SKRIV-LDC-W01184                                   
051900        WHEN SDC                                                          
052000           PERFORM S12-SKRIV-SDC-W01184                                   
052100        WHEN NDC                                                          
052200           PERFORM S13-SKRIV-NDC-W01184                                   
052300     END-EVALUATE                                                         
052400     .                                                                    
052500     EJECT                                                                
052600 S03-INITIALIZE-WDK712    SECTION.                                        
052700     MOVE ZERO                TO SLAG-BEFT OF SLAG-W01184                 
052800                                 SLAG-IDARTNR-EMBQ0                       
052900                                           OF SLAG-W01184                 
053000                                 SLAG-IDARTNR-EMBQ1                       
053100                                           OF SLAG-W01184                 
053200                                 SLAG-IDARTNR-EMBQ2                       
053300                                           OF SLAG-W01184                 
053400                                 SLAG-KVDAGAR-INLEV                       
053500                                           OF SLAG-W01184                 
053600                                 SLAG-PRARTSJK                            
053700                                           OF SLAG-W01184                 
053800                                 SLAG-IDPSN-DC                            
053900                                           OF SLAG-W01184                 
054000                                 SLAG-PRMATRL                             
054100                                           OF SLAG-W01184                 
054200                                 SLAG-TIERSDAT-VIPS                       
054300                                           OF SLAG-W01184                 
054400                                 SLAG-TIUPPDAT-EMB                        
054500                                           OF SLAG-W01184                 
054600                                 SLAG-VKART                               
054700                                           OF SLAG-W01184                 
054800                                 SLAG-VLARTNTO                            
054900                                           OF SLAG-W01184                 
055000                                 SLAG-KDMATRPR                            
055100                                           OF SLAG-W01184                 
055200                                 SLAG-DAPUBL                              
055300                                           OF SLAG-W01184                 
055400     MOVE SPACE               TO SLAG-KDARTURS                            
055500                                           OF SLAG-W01184                 
055600     .                                                                    
055700     EJECT                                                                
055800                                                                          
055900 S04-INITIALIZE-WDK711    SECTION.                                        
056000     INITIALIZE  SLAG-ADART         OF SLAG-W01184                        
056100                 SLAG-ADLAGOMR-CD   OF SLAG-W01184                        
056200                 SLAG-DASPSEA       OF SLAG-W01184                        
056300                 SLAG-FLBUYUPD      OF SLAG-W01184                        
056400                 SLAG-FLCDREL       OF SLAG-W01184                        
056500                 SLAG-FLFLYG        OF SLAG-W01184                        
056600                 SLAG-FLORDSP       OF SLAG-W01184                        
056700                 SLAG-FLORDSP-EJRO  OF SLAG-W01184                        
056800                 SLAG-FLREFBEO      OF SLAG-W01184                        
056900                 SLAG-FLREFLARM     OF SLAG-W01184                        
057000                 SLAG-FLSKROT-BEORD OF SLAG-W01184                        
057100                 SLAG-FLSPBULK      OF SLAG-W01184                        
057200                 SLAG-FLTABUPD      OF SLAG-W01184                        
057300                 SLAG-FLWILSON      OF SLAG-W01184                        
057400                 SLAG-IDARTNR       OF SLAG-W01184                        
057500                 SLAG-IDDC          OF SLAG-W01184                        
057600                 SLAG-IDDC-REF      OF SLAG-W01184                        
057700                 SLAG-IDLEVNR       OF SLAG-W01184                        
057800                 SLAG-IDPERSON-BUY  OF SLAG-W01184                        
057900                 SLAG-IDREFTAB      OF SLAG-W01184                        
058000                 SLAG-IDUSER-SPKVAL OF SLAG-W01184                        
058100                 SLAG-KDLEVSP       OF SLAG-W01184                        
058200                 SLAG-KDOPPLAN      OF SLAG-W01184                        
058300                 SLAG-KDREFSTA      OF SLAG-W01184                        
058400                 SLAG-KVAKS-PAV     OF SLAG-W01184                        
058500                 SLAG-KVAKS-SDC     OF SLAG-W01184                        
058600                 SLAG-KVBEART       OF SLAG-W01184                        
058700                 SLAG-KVDAGAR-CDBEH OF SLAG-W01184                        
058800                 SLAG-KVDAGAR-MANLT OF SLAG-W01184                        
058900                 SLAG-KVEFRS        OF SLAG-W01184                        
059000                 SLAG-KVINVS        OF SLAG-W01184                        
059100                 SLAG-KVLS          OF SLAG-W01184                        
059200                 SLAG-KVOKS-BULK    OF SLAG-W01184                        
059300                 SLAG-KVOKS-DAG     OF SLAG-W01184                        
059400                 SLAG-KVPB-REF      OF SLAG-W01184                        
059500                 SLAG-KVPBREOI      OF SLAG-W01184                        
059600                 SLAG-KVREFBER      OF SLAG-W01184                        
059700                 SLAG-KVREFOVL      OF SLAG-W01184                        
059800                 SLAG-KVREFPKT      OF SLAG-W01184                        
059900                 SLAG-KVRESS        OF SLAG-W01184                        
060000                 SLAG-KVRETUR-BEORD OF SLAG-W01184                        
060100                 SLAG-KVROS-BULK    OF SLAG-W01184                        
060200                 SLAG-KVROS-DAG     OF SLAG-W01184                        
060300                 SLAG-KVSKROT       OF SLAG-W01184                        
060400                 SLAG-KVSPARR-KVAL  OF SLAG-W01184                        
060500                 SLAG-KVUTRS        OF SLAG-W01184                        
060600                 SLAG-PRAVCOST      OF SLAG-W01184                        
060700                 SLAG-RETREND       OF SLAG-W01184                        
060800                 SLAG-TEKVAL        OF SLAG-W01184                        
060900                 SLAG-TIAVCOST      OF SLAG-W01184                        
061000                 SLAG-TIINVDAT      OF SLAG-W01184                        
061100                 SLAG-TIMANSEA      OF SLAG-W01184                        
061200                 SLAG-TIORDREG      OF SLAG-W01184                        
061300                 SLAG-TIPBREOI      OF SLAG-W01184                        
061400                 SLAG-TIREFMPB      OF SLAG-W01184                        
061500                 SLAG-TIREFPAF      OF SLAG-W01184                        
061600                 SLAG-TIREFPKT      OF SLAG-W01184                        
061700                 SLAG-TIREFSTA      OF SLAG-W01184                        
061800                 SLAG-TIREFSTO      OF SLAG-W01184                        
061900                 SLAG-TIRETUR-BEORD OF SLAG-W01184                        
062000                 SLAG-TISKROT       OF SLAG-W01184                        
062100                 SLAG-TISKROT-BEORD OF SLAG-W01184                        
062200                 SLAG-TISKROT-AUTO  OF SLAG-W01184                        
062300                 SLAG-TISPARR-KVAL  OF SLAG-W01184                        
062400                 SLAG-TISTODAT-LARM OF SLAG-W01184                        
062500                 SLAG-FLREFILL      OF SLAG-W01184                        
062600                 SLAG-FLREFNYO      OF SLAG-W01184                        
062700                 SLAG-FLPB-FLYTT    OF SLAG-W01184                        
062800     MOVE 1 TO IX                                                         
062900     PERFORM UNTIL IX > 12                                                
063000        INITIALIZE SLAG-RESEASON IN SLAG-W01184(IX)                       
063100        ADD 1 TO IX                                                       
063200     END-PERFORM                                                          
063300                                                                          
063400     .                                                                    
063500     EJECT                                                                
063600                                                                          
063700 S05-INITIALIZE-WDK722    SECTION.                                        
063800     INITIALIZE   SLAG-DAPBPLAN     OF SLAG-W01184                        
063900                  SLAG-DASEASON     OF SLAG-W01184                        
064000                  SLAG-FLJIT        OF SLAG-W01184                        
064100                  SLAG-IDINK        OF SLAG-W01184                        
064200                  SLAG-IDPLANGR-AG  OF SLAG-W01184                        
064300                  SLAG-KDAVT        OF SLAG-W01184                        
064400                  SLAG-KDLEVPLF     OF SLAG-W01184                        
064500                  SLAG-IDANSK       OF SLAG-W01184                        
064600                  SLAG-IDLEVNR-SHIP OF SLAG-W01184                        
064700                  SLAG-KDLPSP       OF SLAG-W01184                        
064800                  SLAG-KDOPPLAN     OF SLAG-W01184                        
064900                  SLAG-KVDAGAR-FFH  OF SLAG-W01184                        
065000                  SLAG-KVEOQ        OF SLAG-W01184                        
065100                  SLAG-KVPALL       OF SLAG-W01184                        
065200                  SLAG-KVPB-PLAN    OF SLAG-W01184                        
065300                  SLAG-KVPB-TREND   OF SLAG-W01184                        
065400                  SLAG-KVSLAGER     OF SLAG-W01184                        
065500                  SLAG-KVSLUTKP     OF SLAG-W01184                        
065600                  SLAG-KVSPANT      OF SLAG-W01184                        
065700                  SLAG-KVULOAD      OF SLAG-W01184                        
065800                  SLAG-KVVECKOR-FT  OF SLAG-W01184                        
065900                  SLAG-KVVECKOR-LT  OF SLAG-W01184                        
066000                  SLAG-KVVECKOR-TREND OF SLAG-W01184                      
066100                  SLAG-KVPB-JUST1     OF SLAG-W01184                      
066200                  SLAG-KVPB-JUST2     OF SLAG-W01184                      
066300                  SLAG-TIPBJUST-1     OF SLAG-W01184                      
066400                  SLAG-TIPBJUST-2     OF SLAG-W01184                      
066500                  SLAG-TIMANLED       OF SLAG-W01184                      
066600                  SLAG-TILEVDAT       OF SLAG-W01184                      
066700                  SLAG-IDLEVNR-FRAM   OF SLAG-W01184                      
066800                  SLAG-IDLEVNR-SHIP-FRAM OF SLAG-W01184                   
066900                  SLAG-TIDATUM-TREND     OF SLAG-W01184                   
067000                  SLAG-TILPSP            OF SLAG-W01184                   
067100                  SLAG-TIMANSEC          OF SLAG-W01184                   
067200                  SLAG-TIOMSPEC          OF SLAG-W01184                   
067300                  SLAG-TIREFSTO-LOC      OF SLAG-W01184                   
067400                  SLAG-TISLUTKP          OF SLAG-W01184                   
067500                  SLAG-FLLARM-BUF        OF SLAG-W01184                   
067600*                                                                         
067700     MOVE 1 TO IX                                                         
067800     PERFORM UNTIL IX > 12                                                
067900        INITIALIZE SLAG-RESEASON-PLAN(IX)                                 
068000        ADD 1 TO IX                                                       
068100     END-PERFORM                                                          
068200*                                                                         
068300     MOVE 1 TO IX                                                         
068400     PERFORM UNTIL IX > 5                                                 
068500        INITIALIZE SLAG-TILEVDAG(IX)                                      
068600        ADD 1 TO IX                                                       
068700     END-PERFORM                                                          
068800     .                                                                    
068900     EJECT                                                                
069000 S11-SKRIV-LDC-W01184 SECTION.                                            
069100     WRITE LDC-W01184-POST FROM W01184-AREA                               
069200                                                                          
069300     MOVE 'W01184' TO POSTSUM-FDNAMN                                      
069400     MOVE 'W01184D1' TO POSTSUM-DDNAMN2                                   
069500     CALL POSTSUM USING POSTSUM-PARM                                      
069600     .                                                                    
069700     EJECT                                                                
069800                                                                          
069900                                                                          
070000 S12-SKRIV-SDC-W01184 SECTION.                                            
070100     WRITE SDC-W01184-POST FROM W01184-AREA                               
070200                                                                          
070300     MOVE 'W01184' TO POSTSUM-FDNAMN                                      
070400     MOVE 'W01184D2' TO POSTSUM-DDNAMN2                                   
070500     CALL POSTSUM USING POSTSUM-PARM                                      
070600     .                                                                    
070700     EJECT                                                                
070800                                                                          
070900 S13-SKRIV-NDC-W01184 SECTION.                                            
071000     WRITE NDC-W01184-POST FROM W01184-AREA                               
071100                                                                          
071200     MOVE 'W01184' TO POSTSUM-FDNAMN                                      
071300     MOVE 'W01184D3' TO POSTSUM-DDNAMN2                                   
071400     CALL POSTSUM USING POSTSUM-PARM                                      
071500     .                                                                    
071600     EJECT                                                                
071700 S14-SKRIV-NDC-W01183 SECTION.                                            
071800     WRITE NDC-W01183-POST FROM W01183-AREA                               
071900                                                                          
072000     MOVE 'W01184' TO POSTSUM-FDNAMN                                      
072100     MOVE 'W01184D4' TO POSTSUM-DDNAMN2                                   
072200     CALL POSTSUM USING POSTSUM-PARM                                      
072300     .                                                                    
072400     EJECT                                                                
072500 S15-SKRIV-NDC-W01185 SECTION.                                            
072600     WRITE NDC-W01185-POST FROM W01185-AREA                               
072700                                                                          
072800     MOVE 'W01184' TO POSTSUM-FDNAMN                                      
072900     MOVE 'W01184D5' TO POSTSUM-DDNAMN2                                   
073000     CALL POSTSUM USING POSTSUM-PARM                                      
073100     .                                                                    
073200     EJECT                                                                
073300                                                                          
073400 S16-SKRIV-NDC-W01188 SECTION.                                            
073500     WRITE NDC-W01188-POST FROM W01188-AREA                               
073600                                                                          
073700     MOVE 'W01188' TO POSTSUM-FDNAMN                                      
073800     MOVE 'W01184D6' TO POSTSUM-DDNAMN2                                   
073900     CALL POSTSUM USING POSTSUM-PARM                                      
074000     .                                                                    
074100     EJECT                                                                
074200                                                                          
074300 S17-SKRIV-NDC-W01182 SECTION.                                            
074400     WRITE NDC-W01182-POST FROM W01182-AREA                               
074500                                                                          
074600     MOVE 'W01182' TO POSTSUM-FDNAMN                                      
074700     MOVE 'W01184D7' TO POSTSUM-DDNAMN2                                   
074800     CALL POSTSUM USING POSTSUM-PARM                                      
074900     .                                                                    
075000     EJECT                                                                
075100                                                                          
075200* --- IMS SEKTIONER ---                                                   
075300     SKIP3                                                                
075400     EJECT                                                                
075500*PCB - WDK7-1                                                             
075600 IMS-GET-WDK7-1 SECTION.                                                  
075700     CALL CBLTDLI USING GN WDK7-1-PCB DLI-IO-AREA                         
075800     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
075900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
076000     PERFORM IMS-STATUSKONTROLL                                           
076100     .                                                                    
076200     SKIP3                                                                
076300*PCB - WDK7-2                                                             
076400 IMS-GU-WDK712 SECTION.                                                   
076500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
076600          DELIMITED BY SIZE INTO SSA1                                     
076700     STRING 'WDK712  (IDLAND  = ' W-IDLAND-X ')'                          
076800          DELIMITED BY SIZE INTO SSA2                                     
076900     MOVE '  GE' TO GODK-STATUSKODER                                      
077000     CALL CBLTDLI USING GU WDK7-2-PCB DLI-IO-WDK712 SSA1 SSA2             
077100     MOVE WDK7-2-STATUS-CODE TO STATUS-WS                                 
077200     PERFORM IMS-STATUSKONTROLL                                           
077300     .                                                                    
077400     SKIP3                                                                
077500 IMS-GN-WDB601    SECTION.                                                
077600     MOVE 'WDB601  ' TO SSA1                                              
077700     MOVE '  GB'     TO GODK-STATUSKODER                                  
077800     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1                 
077900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
078000     PERFORM IMS-STATUSKONTROLL                                           
078100     .                                                                    
078200     EJECT                                                                
078300 IMS-STATUSKONTROLL SECTION.                                              
078400                                                                          
078500     SET STATUS-IX TO 1                                                   
078600     SEARCH GODK-STATUS                                                   
078700       AT END                                                             
078800         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
078900         DISPLAY FELTEXT                                                  
079000         CALL FELLOG                                                      
079100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
079200         CONTINUE                                                         
079300     END-SEARCH                                                           
079400     .                                                                    
