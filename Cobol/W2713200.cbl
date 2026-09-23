000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2713200.                                                
000400*AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
000500*DATE-WRITTEN.   JAN 1997.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET BERÄKNAR SÄSONGSINDEX FÖR ALLA AKTIVA                 
001100*        ARTIKLAR PÅ ALLA NDC                                             
001200*                                                                         
001300*        PROGRAMMET GÅR VID KVARTALSKIFTE                                 
001400*        KÖRS FÖR ETT DC I TAGET                                          
001500*                                                                         
001600*        PROGRAMMET LÄSER      WDK7                                       
001700*                              WDL7 + WDL4                                
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*         ---AKTUELL NDC                                                  
003200     SELECT W271DC        ASSIGN W27132D1.                                
003300                                                                          
003400     SELECT W27132                     ASSIGN TO W27132D2.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000                                                                          
004100                                                                          
004200 FD  W271DC                                                               
004300     LABEL RECORD STANDARD                                                
004400     RECORDING F                                                          
004500     BLOCK CONTAINS 0.                                                    
004600                                                                          
004700 01  FILLER                  PIC X(80).                                   
004800                                                                          
004900                                                                          
005000 FD  W27132                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  POST -COPY W27132  -PRE UT-    -L.                                   
005500                                                                          
005600     EJECT                                                                
005700 WORKING-STORAGE SECTION.                                                 
005800     SKIP2                                                                
005900                                                                          
006000*    -- CHECKED BY WY2000                                                 
006100 77  IDPGM                       PIC X(8)    VALUE 'W2713200'.            
006200 77  JA                          PIC X       VALUE 'J'.                   
006300 77  NEJ                         PIC X       VALUE 'N'.                   
006400 77  AKTIV                       PIC X       VALUE 'A'.                   
006500                                                                          
006600*    --- INDEX SAMT MAX-INDEX                                             
006700 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
006800 77  INDX                        PIC 9(2)    VALUE ZERO.                  
006900 77  IX                          PIC 9(3)    VALUE ZERO.                  
007000 77  DC-INDX                     PIC 9(2)    VALUE ZERO.                  
007100 77  MAX-FSGFAKT                 PIC 9(2)    VALUE 10.                    
007200                                                                          
007300 77  VECKO-INDX                  PIC 9(2)    VALUE ZERO.                  
007400                                                                          
007500 01  TAB-RADIX                   PIC 9(2)    VALUE ZERO.                  
007600 77  MAX-TAB-RADIX               PIC 9(2)    VALUE 12.                    
007700                                                                          
007800 77  VV-INDX                     PIC 9(2)    VALUE ZERO.                  
007900 77  MAX-VV-INDX                 PIC 9(2)    VALUE 53.                    
008000                                                                          
008100*    --- SWITCHAR                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
008300 01  TREND-SW                    PIC X(5)    VALUE SPACE.                 
008400     88  INGEN-TREND                         VALUE 'INGEN'.               
008500     88  SVAG-TREND                          VALUE 'SVAG '.               
008600     88  STARK-TREND                         VALUE 'STARK'.               
008700                                                                          
008800 01  INDX-SW                     PIC X       VALUE 'N'.                   
008900     88  INDX-HITTAT                         VALUE 'J'.                   
009000                                                                          
009100 01  ARTIKEL-SW                  PIC X       VALUE 'N'.                   
009200     88  ARTIKEL-SKALL-FORAENDRAS            VALUE 'J'.                   
009300                                                                          
009400 01  MANUELL-PROGNOS-SW          PIC X       VALUE 'N'.                   
009500     88  MANUELL-PROGNOS-SATT                VALUE 'J'.                   
009600                                                                          
009700*    --- ARBETSFÄLT                                                       
009800 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
009900 01  ARBETSFAELT.                                                         
010000     03  PERIODTABELL            OCCURS 12.                               
010100         05 TABELL-TIAARP        PIC  9(4)    VALUE ZERO.                 
010200         05 TABELL-FORSTA-TIAAVV PIC  9(2)    VALUE ZERO.                 
010300         05 TABELL-SISTA-TIAAVV  PIC  9(2)    VALUE ZERO.                 
010400         05 TABELL-KVOI        PIC S9(9)V9(1)  VALUE ZERO COMP-3.         
010500                                                                          
010600     03  SLUT-VV                 PIC 9(2)    VALUE ZERO.                  
010700     03  START-VV                PIC 9(2)    VALUE ZERO.                  
010800                                                                          
010900     03  WS-IDARTNR              PIC 9(9)    VALUE ZERO.                  
011000     03  IDDC-WS                 PIC X(2)    VALUE SPACE.                 
011100                                                                          
011200     03  WS-CURRENT-DATE.                                                 
011300         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
011400         05  FILLER              PIC 9(4)   VALUE ZERO.                   
011500         05  FILLER              PIC 9(6)   VALUE ZERO.                   
011600                                                                          
011700     03  FILLER REDEFINES WS-CURRENT-DATE.                                
011800*-----   INKLUSIVE SEKEL                                                  
011900         05  WS-DAGENS-DATUM     PIC 9(8).                                
012000         05  WS-DAGENS-TID.                                               
012100             07 WS-DAGENS-TIMME  PIC 9(2).                                
012200             07 WS-DAGENS-MINUT  PIC 9(2).                                
012300             07 WS-DAGENS-SEKUND PIC 9(2).                                
012400                                                                          
012500     03  WS-TIAAVV               PIC 9(4)    VALUE ZERO.                  
012600     03  FILLER REDEFINES WS-TIAAVV.                                      
012700         05 WS-TIAA              PIC 9(2).                                
012800         05 WS-TIVV              PIC 9(2).                                
012900                                                                          
013000     03  FOREG-TIAARP            PIC  9(4)   VALUE ZERO.                  
013100     03  FILLER REDEFINES FOREG-TIAARP.                                   
013200         05 FOREG-TIAA           PIC  9(2).                               
013300         05 FOREG-TIRP           PIC  9(2).                               
013400                                                                          
013500     03  DAGENS-TIAARP           PIC  9(4)   VALUE ZERO.                  
013600     03  FILLER REDEFINES DAGENS-TIAARP.                                  
013700         05 DAGENS-TIAA          PIC  9(2).                               
013800         05 DAGENS-TIRP          PIC  9(2).                               
013900                                                                          
014000     03  NAESTA-TIAARP           PIC  9(4)   VALUE ZERO.                  
014100     03  FILLER REDEFINES NAESTA-TIAARP.                                  
014200         05 NAESTA-TIAA          PIC  9(2).                               
014300         05 NAESTA-TIRP          PIC  9(2).                               
014400                                                                          
014500     03  SEASON-TIAARP           PIC  9(4)   VALUE ZERO.                  
014600     03  FILLER REDEFINES SEASON-TIAARP.                                  
014700         05 SEASON-TIAA          PIC  9(2).                               
014800         05 SEASON-TIRP          PIC  9(2).                               
014900                                                                          
015000     03  DAGENS-TIAAVVD          PIC  9(5)   VALUE ZERO.                  
015100     03  FILLER REDEFINES DAGENS-TIAAVVD.                                 
015200         05 DAGENS-TIAAVVD-AA    PIC  9(2).                               
015300         05 DAGENS-TIAAVVD-VV    PIC  9(2).                               
015400         05 DAGENS-TIAAVVD-D     PIC  9(1).                               
015500                                                                          
015600     03  DAGENS-TIAAVV-GRP       PIC  9(4)   VALUE ZERO.                  
015700                                                                          
015800                                                                          
015900     03  DAGENS-TIAAVVD-LAST-YEAR        PIC  9(5) VALUE ZERO.            
016000     03  FILLER REDEFINES DAGENS-TIAAVVD-LAST-YEAR.                       
016100         05 DAGENS-TIAAVVD-LAST-YEAR-AA  PIC 9(2).                        
016200         05 DAGENS-TIAAVVD-LAST-YEAR-VV  PIC 9(2).                        
016300         05 DAGENS-TIAAVVD-LAST-YEAR-D   PIC 9(1).                        
016400                                                                          
016500     03  DAGENS-TIVV             PIC  9(2)   VALUE ZERO.                  
016600                                                                          
016700     03  WS-ANTAL-VECKOR         PIC  9(2)      VALUE ZERO.               
016800     03  WS-VECKO-IO             PIC S9(9)V9 VALUE ZERO COMP-3.           
016900     03  WS-TEST-TIREFMPB        PIC S9(7)   VALUE ZERO COMP-3.           
017000     03  WS-TIREFMPB-TIAARP      PIC  9(4)      VALUE ZERO.               
017100     03  WS-FORSTA-TIAAVV        PIC  9(4)      VALUE ZERO.               
017200     03  WS-SISTA-TIAAVV         PIC  9(4)      VALUE ZERO.               
017300     03  WS-DAT-TIAAVV           PIC  9(4)      VALUE ZERO.               
017400     03  WS-ONORM-OI-GRAENS-PB   PIC S9(9)V9(1) VALUE ZERO COMP-3.        
017500     03  WS-KVOI-SEASON          PIC S9(9)V9(1) VALUE ZERO COMP-3.        
017600     03  WS-KVOI-TOT             PIC S9(11)     VALUE ZERO COMP-3.        
017700     03  WS-NY-KVPB-REF          PIC S9(6)V9(2) VALUE ZERO COMP-3.        
017800     03  NY-KVPB-REF             PIC S9(6)V9(1) VALUE ZERO COMP-3.        
017900     03  WS-PREL-KVPB-REF        PIC S9(6)V9(2) VALUE ZERO COMP-3.        
018000     03  WS-MEDEL-KVPB-REF       PIC S9(6)V9(2) VALUE ZERO COMP-3.        
018100     03  WS-ANTAL-FAKTORER-STOERRE-NOLL PIC S9(7)          COMP-3.        
018200     03  WS-KVOTEN               PIC S9(5)V9(2) VALUE ZERO COMP-3.        
018300                                                                          
018400       EJECT                                                              
018500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
018600 01  FILLER REDEFINES DAGENS-DATUM.                                       
018700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
018800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
018900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
019000     EJECT                                                                
019100                                                                          
019200 01  DC-POST.                                                             
019300     03  DC-PARAMETER        PIC X(4).                                    
019400         88 SAMTLIGA-DC      VALUE 'ALLA'.                                
019500         88 SAMTLIGA-SDC     VALUE 'EURO'.                                
019600         88 SAMTLIGA-NDC     VALUE 'NDC '.                                
019700         88 ENSTAKA-DC       VALUE 'DC21' 'DC22' 'DC23'                   
019800                                   'DC24' 'DC25' 'DC26'                   
019900                                   'DC3A'                                 
020000                                   'DC41' 'DC42' 'DC43' 'DC51'            
020100                                   'DC61' 'DC6A' 'DC62'.                  
020200                                                                          
020300     03  FILLER               PIC X(76).                                  
020400                                                                          
020500 01  DYNAMISKA-SUBPROGRAM.                                                
020600*                                                                         
020700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
020800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
020900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
021100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
021200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
021300     03  W271SEAS                PIC X(8)    VALUE 'W271SEAS'.            
021400     SKIP2                                                                
021500*    --- PARAMETRAR TILL ABEND                                            
021600                                                                          
021700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
021800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
021900     SKIP2                                                                
022000 01  FELTEXT.                                                             
022100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
022200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
022300     EJECT                                                                
022400*    --- PARAMETRAR TILL DATKORT                                          
022500*                                                                         
022600 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27132'.              
022700     SKIP2                                                                
022800 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
022900     SKIP2                                                                
023000*01  -COPY WDATKORT                                                       
023100     EJECT                                                                
023200*    --- PARAMETRAR TILL POSTSUM                                          
023300*                                                                         
023400*01  -COPY W0005   -PRE  POSTSUM-                                         
023500     EJECT                                                                
023600*    --- PARAMETRAR TILL WDATKONV                                         
023700*                                                                         
023800*01  -COPY WDATAREA                                                       
023900     EJECT                                                                
024000*    --- PARAMETRAR TILL SUBPROGRAM W271SEAS                              
024100*                                                                         
024200 01  FILLER                      PIC X(16)   VALUE 'W271SEAS'.            
024300     SKIP3                                                                
024400*01 -COPY W271SEAS                                                        
024500     EJECT                                                                
024600 01  UT-AREA-START              PIC X(24)   VALUE                         
024700                                 'UT-AREA-START  '.                       
024800     SKIP2                                                                
024900                                                                          
025000*01  AREA -COPY W27132     -PRE UT-                                       
025100     EJECT                                                                
025200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025300                                                                          
025400     SKIP3                                                                
025500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025600     SKIP3                                                                
025700 01  NYCKLAR-TILL-DLI.                                                    
025800     03  W-IDARTNR-X.                                                     
025900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
026000                                                                          
026100     03  W-IDDC-X.                                                        
026200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
026300                                                                          
026400     03  W-IDDC-B6-X.                                                     
026500         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
026600                                                                          
026700     03  W-KDSEGKEY-X.                                                    
026800         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
026900                                                                          
027000*                                                                         
027100     SKIP2                                                                
027200*    --- STATUS-KOD FRÅN IMS                                              
027300 01  STATUS-WS                   PIC XX.                                  
027400     88  SEGMENT-FINNS                       VALUE '  '.                  
027500     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
027600     SKIP2                                                                
027700 01  GODK-STATUSKODER.                                                    
027800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027900     SKIP3                                                                
028000 01  SSA1                        PIC X(64).                               
028100 01  SSA2                        PIC X(64).                               
028200     EJECT                                                                
028300*    --- IMS FUNKTIONSKODER                                               
028400*01  -COPY W0003                                                          
028500     EJECT                                                                
028600*    ---  DLI INPUT-OUTPUT AREA                                           
028700 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-K6'.        
028800     SKIP3                                                                
028900 01  DLI-IO-AREA-K6.                                                      
029000     03  IO-AREA-K6              PIC X(900)  VALUE SPACE.                 
029100     SKIP3                                                                
029200     03  WDK601 REDEFINES IO-AREA-K6.                                     
029300*        05  -COPY WDK601  -PRE WDK601-                                   
029400     SKIP3                                                                
029500     03  WDK611 REDEFINES IO-AREA-K6.                                     
029600*        05  -COPY WDK611  -PRE WDK611-                                   
029700     EJECT                                                                
029800 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-K7'.        
029900     SKIP3                                                                
030000 01  DLI-IO-AREA-K7.                                                      
030100     03  IO-AREA-K7              PIC X(300)  VALUE SPACE.                 
030200     SKIP3                                                                
030300     03  WDK701 REDEFINES IO-AREA-K7.                                     
030400*        05  -COPY WDK701  -PRE WDK701-                                   
030500     SKIP3                                                                
030600     03  WDK711 REDEFINES IO-AREA-K7.                                     
030700*        05  -COPY WDK711  -PRE WDK711-                                   
030800     EJECT                                                                
030900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
031000 01   DLI-IO-AREA-B601.                                                   
031100*     03  -COPY WDB601                                                    
031200                                                                          
031300     EJECT                                                                
031400 LINKAGE SECTION.                                                         
031500                                                                          
031600     EJECT                                                                
031700*01  -COPY W0008  -PRE WDK7-                                              
031800     05  WDK7-KEY-FB-AREA-IDARTNR       PIC S9(9) COMP-3.                 
031900     EJECT                                                                
032000*01  -COPY W0008  -PRE WDK6-                                              
032100     05  FILLER                  PIC X.                                   
032200     EJECT                                                                
032300*01  -COPY W0008  -PRE WDK7X-                                             
032400     05  FILLER                  PIC X.                                   
032500     EJECT                                                                
032600*01  -COPY W0008  -PRE WDL7-                                              
032700     05  FILLER                  PIC X.                                   
032800     EJECT                                                                
032900*01  -COPY W0008  -PRE WDL4-                                              
033000     05  FILLER                  PIC X.                                   
033100     EJECT                                                                
033200*01  -COPY W0008      -PRE WDB6-                                          
033300     05  FILLER                  PIC X.                                   
033400     EJECT                                                                
033500 PROCEDURE DIVISION  USING WDK7-PCB WDK6-PCB WDK7X-PCB WDL7-PCB           
033600                           WDL4-PCB WDB6-PCB.                             
033700     ENTRY 'DLITCBL' USING WDK7-PCB WDK6-PCB WDK7X-PCB WDL7-PCB           
033800                           WDL4-PCB WDB6-PCB.                             
033900                                                                          
034000     PERFORM A-INIT                                                       
034100     PERFORM IMS-GN-WDK7                                                  
034200     PERFORM UNTIL SEGMENT-SAKNAS                                         
034300       EVALUATE WDK7-SEG-NAME-FB                                          
034400         WHEN 'WDK701  '                                                  
034500           MOVE WDK7-KEY-FB-AREA-IDARTNR TO W-IDARTNR                     
034600         WHEN 'WDK711  '                                                  
034700            MOVE WDK711-SLAG-IDDC   TO W-IDDC-B6                          
034800            PERFORM IMS-GU-WDB601                                         
034900                                                                          
035000            IF  SAMTLIGA-DC                                               
035100            AND WDK711-SLAG-KDREFSTA = AKTIV                              
035200            AND WDK711-SLAG-DASPSEA < WS-DAGENS-DATUM                     
035300               PERFORM IMS-GU-WDK601                                      
035400               IF WDK601-ART-KDERS-UTG = 0                                
035500                  PERFORM IMS-GNP-WDK611                                  
036120                  IF (                                                    
036121                      WDK711-SLAG-IDDC-REF = SPACE AND                    
036130                      (                                                   
036131                        WDK711-SLAG-IDDC(1:1) = '7' OR                    
036132                        WDK711-SLAG-IDDC(1:1) = '4'                       
036133                      )                                                   
036134                     )                                                    
036140                  OR (                                                    
036150                      WDK711-SLAG-IDDC-REF > SPACE AND                    
036160                      WDK711-SLAG-IDDC-REF NOT = '11'                     
036170                     )                                                    
036200***OM LOKALT ANSKAFFAD ELLER INTERNREFILL GÖR EJ                          
036300***KONTROLL MOT WDK611-CLAG-FLREFILL = JA                                 
036400                    IF DCS-FLSEASBER = JA                                 
036500                       PERFORM C-BEHANDLA-ARTIKEL                         
036600                    END-IF                                                
036700                  ELSE                                                    
036800                    IF WDK611-CLAG-FLREFILL = JA                          
036900*                                                                         
037000*                      IF NOT LDC                                         
037100*                      HAR ERSATTS AV   IF DCS-FLSEASBER = JA             
037200*                                                                         
037300                       IF DCS-FLSEASBER = JA                              
037400                          PERFORM C-BEHANDLA-ARTIKEL                      
037500                       END-IF                                             
037600                    END-IF                                                
037700                  END-IF                                                  
037800               END-IF                                                     
037900            END-IF                                                        
038000       END-EVALUATE                                                       
038100       PERFORM IMS-GN-WDK7                                                
038200     END-PERFORM                                                          
038300                                                                          
038400     PERFORM Z-FINIT                                                      
038500                                                                          
038600     MOVE ZERO TO RETURN-CODE                                             
038700     GOBACK                                                               
038800     .                                                                    
038900     EJECT                                                                
039000 A-INIT SECTION.                                                          
039100                                                                          
039200     OPEN INPUT  W271DC                                                   
039300     OPEN OUTPUT W27132                                                   
039400                                                                          
039500     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE                        
039600                                                                          
039700     PERFORM S11-LAES-W271DC                                              
039800     .                                                                    
039900     EJECT                                                                
040000                                                                          
040100                                                                          
040200 C-BEHANDLA-ARTIKEL SECTION.                                              
040300                                                                          
040400     MOVE NEJ TO ARTIKEL-SW                                               
040500                                                                          
040600     MOVE WDK711-SLAG-IDDC      TO W-IDDC                                 
040700        IF WDK711-SLAG-FLREFILL = JA                                      
040800                                                                          
040900           MOVE W-IDARTNR    TO SEAS-IDARTNR                              
041000                                WS-IDARTNR                                
041100           MOVE W-IDDC       TO SEAS-IDDC                                 
041200           MOVE SPACE        TO SEAS-KDSVAR                               
041300           MOVE JA           TO SEAS-FLKVARTAL                            
041400                                                                          
041500           CALL W271SEAS USING SEAS-W271SEAS WDK7X-PCB WDL7-PCB           
041600                                    WDL4-PCB WDK6-PCB WDB6-PCB            
041700                                                                          
041800           IF SEAS-KDSVAR = SPACE                                         
041900             IF  SEAS-SEASON-ARTIKEL = JA                                 
042000*----- SÄSONG GÄLLER                                                      
042100*-----                                                                    
042200*----- SÅ SMÅNINGOM BÖR NOG SÄSONGSARTIKLAR MED ETT                       
042300*----- ÅRS HISTORIK SKRIVAS PÅ EN EGEN FIL SÅ ATT                         
042400*----- MAN KAN TA UT EN LISTA FÖR MANUELL BEDÖMNING                       
042500*----- AV DESSA                                                           
042600                                                                          
042700               MOVE 1            TO IX                                    
042800               PERFORM UNTIL IX > 12                                      
042900                 DIVIDE SEAS-RESEASON (IX)                                
043000                                       BY 100                             
043100                                       GIVING UT-RESEASON (IX)            
043200                 ADD 1           TO IX                                    
043300               END-PERFORM                                                
043400             ELSE                                                         
043500*----- FÖR KORT HISTORIK ELLER SÅ ÄR                                      
043600*----- OSÄKERHETEN ÄR FÖR STOR,                                           
043700*----- SÄSONG GÄLLER EJ                                                   
043800                                                                          
043900               MOVE 1            TO IX                                    
044000               PERFORM UNTIL IX > 12                                      
044100                 MOVE 1.00       TO UT-RESEASON (IX)                      
044200                 ADD 1           TO IX                                    
044300               END-PERFORM                                                
044400                                                                          
044500             END-IF                                                       
044600                                                                          
044700*----- SKRIV POST PÅ ARTIKELFIL                                           
044800             MOVE SEAS-IDARTNR         TO UT-IDARTNR                      
044900             MOVE SEAS-IDDC            TO UT-IDDC                         
045000             MOVE SEAS-OSAKERHET       TO UT-OSAKERHET                    
045100             MOVE SEAS-ANT-HIST-AR     TO UT-ANT-HIST-AR                  
045200             MOVE SEAS-ANT-HIST-MAN    TO UT-ANT-HIST-MAN                 
045300             MOVE SEAS-SEASON-ARTIKEL  TO UT-SEASON-ARTIKEL               
045400             PERFORM S12-SKRIV-W27132                                     
045500           END-IF                                                         
045600                                                                          
045700        END-IF                                                            
045800     .                                                                    
045900     EJECT                                                                
046000 Z-FINIT SECTION.                                                         
046100                                                                          
046200     CLOSE W271DC                                                         
046300           W27132                                                         
046400                                                                          
046500     MOVE 'S' TO POSTSUM-OPKOD                                            
046600     CALL POSTSUM USING POSTSUM-PARM                                      
046700     .                                                                    
046800     EJECT                                                                
046900 S11-LAES-W271DC SECTION.                                                 
047000                                                                          
047100      READ W271DC              INTO DC-POST                               
047200     .                                                                    
047300     SKIP3                                                                
047400 S12-SKRIV-W27132 SECTION.                                                
047500                                                                          
047600     WRITE UT-POST FROM UT-AREA                                           
047700                                                                          
047800     MOVE 'W27132'   TO POSTSUM-FDNAMN                                    
047900     MOVE 'W27132D1' TO POSTSUM-DDNAMN2                                   
048000     CALL POSTSUM USING POSTSUM-PARM                                      
048100     .                                                                    
048200     EJECT                                                                
048300* --- IMS SEKTIONER ---                                                   
048400     SKIP3                                                                
048500     EJECT                                                                
048600 IMS-GU-WDK601 SECTION.                                                   
048700                                                                          
048800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
048900          DELIMITED BY SIZE INTO SSA1                                     
049000     MOVE '  ' TO GODK-STATUSKODER                                        
049100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-K6 SSA1                   
049200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
049300     PERFORM IMS-STATUSKONTROLL                                           
049400     .                                                                    
049500     EJECT                                                                
049600 IMS-GNP-WDK611 SECTION.                                                  
049700                                                                          
049800     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
049900          DELIMITED BY SIZE INTO SSA1                                     
050000     MOVE '  ' TO GODK-STATUSKODER                                        
050100     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-K6 SSA1                  
050200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
050300     PERFORM IMS-STATUSKONTROLL                                           
050400     .                                                                    
050500     EJECT                                                                
050600 IMS-GN-WDK7 SECTION.                                                     
050700                                                                          
050800     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA-K7                        
050900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
051000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
051100     PERFORM IMS-STATUSKONTROLL                                           
051200     .                                                                    
051300     EJECT                                                                
051400                                                                          
051500 IMS-GU-WDB601    SECTION.                                                
051600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
051700          DELIMITED BY SIZE INTO SSA1                                     
051800     MOVE '  ' TO GODK-STATUSKODER                                        
051900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
052000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
052100     PERFORM IMS-STATUSKONTROLL                                           
052200     .                                                                    
052300     EJECT                                                                
052400 IMS-STATUSKONTROLL SECTION.                                              
052500                                                                          
052600     SET STATUS-IX TO 1                                                   
052700     SEARCH GODK-STATUS                                                   
052800       AT END                                                             
052900         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
053000           DELIMITED BY SIZE INTO FELTEXT-STR                             
053100         DISPLAY FELTEXT                                                  
053200         CALL FELLOG                                                      
053300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
053400         CONTINUE                                                         
053500     END-SEARCH                                                           
053600     .                                                                    
