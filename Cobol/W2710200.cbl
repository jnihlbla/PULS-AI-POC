000100*********************************************                             
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2710200.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   97/03/01.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER FIL MED DET SENASTE DYGNETS ORDERINGÅNG TILL               
001000*        SDC OCH NDC.                                                     
001100*                                                                         
001200*!!!!!!!!  OM DET ÄR ETT KINA DC FLYTTAR MAN MATERIALPRIS                 
001300*!!!!!!!!  TILL STDPRIS PÅ UTFILERNA                                      
001400*                                                                         
001500*        SDC: MEMO TILL LAGERSTYRARE SKAPAS VID:                          
001600*                            HÖG ORDERINGÅNG,                             
001700*                            FÖRSTA ORDERINGÅNG                           
001800*                            ORDERINGÅNG > Q1-KVANT                       
001900*                                                                         
002000*        NDC: MEMO TILL LAGERSTYRARE SKAPAS VID:                          
002100*                            HÖG ORDERINGÅNG,                             
002200*                                                                         
002300*             REFILLFÖRSLAG SKAPAS VID:                                   
002400*                            EXTRA HÖG ORDERINGÅNG                        
002500*                            FÖRSTA ORDERINGÅNG                           
002600*                            FÖRSTA ORDERINGÅNG PÅ ETT ÅR                 
002700*                                                                         
002800*        LDC: MEMO TILL LAGERSTYRARE SKAPAS VID:                          
002900*                            FÖRSTA ORDERINGÅNG                           
003000*                                                                         
003100*        PROGRAMMET LÄSER      WDK6                                       
003200*        PROGRAMMET LÄSER      WDK7                                       
003300*                                                                         
003400*    ABENDKODER:                                                          
003500*        U0016 -  . . . .                                                 
003600*        U1000 -  . . . .                                                 
003700*                                                                         
003800*                                                                         
003900                                                                          
004000     SKIP3                                                                
004100 ENVIRONMENT DIVISION.                                                    
004200     SKIP2                                                                
004300 INPUT-OUTPUT SECTION.                                                    
004400                                                                          
004500 FILE-CONTROL.                                                            
004600     SKIP2                                                                
004700*          --- ARTIKLAR MED ORDERINGÅNG SENASTE DYGNET                    
004800     SELECT W27101                     ASSIGN TO W27102D1.                
004900     SKIP2                                                                
005000*          --- ARTIKLAR MED FÖRSTA ORDERINGÅNG ELLER                      
005100*          --- EXTREMT HÖG ORDERINGÅNG, NDC                               
005200*              POSTER FÖR REFILLFÖRSLAG                                   
005300     SELECT W27102                     ASSIGN TO W27102D2.                
005400     SKIP2                                                                
005500*          --- ARTIKLAR MED FÖRSTA ORDERINGÅNG ELLER                      
005600*              HÖG ORDERINGÅNG, SDC. POSTER FÖR MEMON                     
005700     SELECT W27103                     ASSIGN TO W27102D3.                
005800*          --- ARTIKLAR MED HÖG ORDERINGÅNG, NDC                          
005900*              POSTER FÖR MEMON                                           
006000     SKIP2                                                                
006100     SELECT W27104                     ASSIGN TO W27102D4.                
006200 DATA DIVISION.                                                           
006300     SKIP2                                                                
006400 FILE SECTION.                                                            
006500     SKIP3                                                                
006600 FD  W27101                                                               
006700     RECORDING       F                                                    
006800     BLOCK CONTAINS  0.                                                   
006900                                                                          
007000*01  -COPY W27101      -L.                                                
007100                                                                          
007200                                                                          
007300 FD  W27102                                                               
007400     RECORDING       F                                                    
007500     BLOCK CONTAINS  0.                                                   
007600                                                                          
007700*01  POST -COPY W27111  -PRE  W27102-  -L.                                
007800                                                                          
007900                                                                          
008000 FD  W27103                                                               
008100     RECORDING       F                                                    
008200     BLOCK CONTAINS  0.                                                   
008300                                                                          
008400*01  POST -COPY W27103 -PRE  W27103-  -L.                                 
008500                                                                          
008600                                                                          
008700 FD  W27104                                                               
008800     RECORDING       F                                                    
008900     BLOCK CONTAINS  0.                                                   
009000                                                                          
009100*01  POST -COPY W27104 -PRE  W27104-  -L.                                 
009200     EJECT                                                                
009300 WORKING-STORAGE SECTION.                                                 
009400                                                                          
009500*    -COPY WY2000W1                                                       
009600*    -COPY WY2000W2                                                       
009700     SKIP3                                                                
009800                                                                          
009900*    -- CHECKED BY WY2000                                                 
010000 77  IDPGM                       PIC X(8)    VALUE 'W2710200'.            
010100 77  JA                          PIC X       VALUE 'J'.                   
010200 77  NEJ                         PIC X       VALUE 'N'.                   
010300                                                                          
010400*01  -COPY WWDCKONS                                                       
010500                                                                          
010600 77  IX                          PIC 9(2)    VALUE ZERO.                  
010700 77  IX2                         PIC 9(3)    VALUE ZERO.                  
010800 77  IX3                         PIC 9(3)    VALUE ZERO.                  
010900                                                                          
011000 77  W27101-EOF-SW               PIC X       VALUE 'N'.                   
011100     88  END-OF-W27101                       VALUE 'J'.                   
011200                                                                          
011300 77  LARM-SW                     PIC X       VALUE 'N'.                   
011400     88  LARM                                VALUE 'J'.                   
011500                                                                          
011600 77  SW-TRAEFF-LAND              PIC X       VALUE 'J'.                   
011700     88  LAND-FINNS                          VALUE 'J'.                   
011800     88  LAND-SAKNAS                         VALUE 'N'.                   
011900     EJECT                                                                
012000                                                                          
012100 01  ARBETSAREOR.                                                         
012200     03  WS-LARMGRANS            PIC 9(7)    VALUE ZERO.                  
012300     03  WS-LARMORSAK            PIC X(2)    VALUE SPACE.                 
012400     03  WS-KDREFTXT             PIC 9(2)    VALUE ZERO.                  
012500     03  WS-IDDISTR              PIC 9(5)    VALUE ZERO.                  
012600     03  WS-NEXT-WORKDAY         PIC 9(6)    VALUE ZERO.                  
012700     03  WS-KVPB-REF         PIC S9(6)V9(2) VALUE ZERO COMP-3.            
012800     03  WS-KVPB-REF-WEEK    PIC S9(6)V9(2) VALUE ZERO COMP-3.            
012900     03  WS-KVPB-REF-SEAS    PIC S9(6)V9(2) VALUE ZERO COMP-3.            
013000     03  WS-SUPERWEEK        PIC S9(6)V9(2) VALUE ZERO COMP-3.            
013100     03  WS-PRIS                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
013200     03  WS-PRIS2                PIC S9(7)V9(2) VALUE ZERO COMP-3.        
013300                                                                          
013400     03 WS-GRANS-TEST        PIC  9(7)V9(1) VALUE ZERO.                   
013500     03 WS-GRANS-TEST-DELAR    REDEFINES WS-GRANS-TEST.                   
013600        05 WS-GRANS-TEST-HELTAL PIC 9(7).                                 
013700        05 WS-GRANS-TEST-DECTAL PIC 9(1).                                 
013800                                                                          
013900     03 WS-ADART-NUM.                                                     
014000        05 WS-ADLAGOMR       PIC 9(2)       VALUE ZERO.                   
014100        05 WS-ADGANG         PIC 9(3)       VALUE ZERO.                   
014200        05 WS-ADPLATS        PIC 9(5)       VALUE ZERO.                   
014300                                                                          
014400                                                                          
014500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
014600 01  FILLER REDEFINES DAGENS-DATUM.                                       
014700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
014800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
014900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
015000                                                                          
015100 01  BLANDADE-DATUMFALT.                                                  
015200     03 DAGENS-TIAAMMDD-LAST-YEAR  PIC 9(6)    VALUE ZERO.                
015300     03 DAGENS-TIAAVVD-LAST-YEAR   PIC 9(5)    VALUE ZERO.                
015400                                                                          
015500     03  DAGENS-TIAAVVD              PIC 9(6).                            
015600     03  DAGENS-TIAAVVD-GRP REDEFINES DAGENS-TIAAVVD.                     
015700         05 DAGENS-TIAAVV-GRP.                                            
015800            07 DAGENS-TIAA-VECKA     PIC 9(2).                            
015900            07 DAGENS-TIVV           PIC 9(2).                            
016000         05 DAGENS-TID               PIC 9(1).                            
016100                                                                          
016200     03 WS-INNEV-TIAARP          PIC 9(4)       VALUE ZERO.               
016300     03 WS-INNEV-TIAARP-DELAR    REDEFINES WS-INNEV-TIAARP.               
016400        05 WS-INNEV-TIAA         PIC 9(2).                                
016500        05 WS-INNEV-TIRP         PIC 9(2).                                
016600                                                                          
016700     EJECT                                                                
016800*01  -COPY WWBYT03                                                        
016900     EJECT                                                                
017000                                                                          
017100*01  -COPY WWPRODSL                                                       
017200                                                                          
017300 01  DYNAMISKA-SUBPROGRAM.                                                
017400*                                                                         
017500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
017600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
017900     03  WORKDAY                  PIC X(8)   VALUE 'WORKDAY'.             
018000     03  WDATKONV                 PIC X(8)   VALUE 'WDATKONV'.            
018100     03  WDAGKONV                 PIC X(8)   VALUE 'WDAGKONV'.            
018200*    --- PARAMETRAR TILL ABEND                                            
018300                                                                          
018400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
018500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
018600     SKIP2                                                                
018700                                                                          
018800*    --- PARAMETRAR TILL POSTSUM                                          
018900*                                                                         
019000*01  -COPY W0005   -PRE  POSTSUM-                                         
019100     EJECT                                                                
019200                                                                          
019300*    --- PARAMETRAR TILL WORKDAY                                          
019400*                                                                         
019500*01  -COPY WORKAREA                                                       
019600     EJECT                                                                
019700*    --- PARAMETRAR TILL DATKONV                                          
019800*                                                                         
019900*01  -COPY WDATAREA                                                       
020000     EJECT                                                                
020100                                                                          
020200*    --- PARAMETRAR TILL DAGKONV                                          
020300*                                                                         
020400*01  -COPY WDAGAREA                                                       
020500     EJECT                                                                
020600                                                                          
020700 01  FELTEXT.                                                             
020800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
020900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
021000     EJECT                                                                
021100                                                                          
021200 01  W27101-AREA-START           PIC X(24)   VALUE                        
021300                                 'W27101-AREA-START  '.                   
021400*01  AREA -COPY W27101     -PRE W27101-                                   
021500     EJECT                                                                
021600                                                                          
021700                                                                          
021800 01  W27102-AREA-START           PIC X(24)   VALUE                        
021900                                 'W27102-AREA-START  '.                   
022000                                                                          
022100*01  AREA -COPY W27111     -PRE W27102-                                   
022200     EJECT                                                                
022300                                                                          
022400                                                                          
022500                                                                          
022600 01  W27103-AREA-START           PIC X(24)   VALUE                        
022700                                 'W27103-AREA-START  '.                   
022800                                                                          
022900*01  AREA -COPY W27103     -PRE W27103-                                   
023000     EJECT                                                                
023100                                                                          
023200                                                                          
023300 01  W27104-AREA-START           PIC X(24)   VALUE                        
023400                                 'W27104-AREA-START  '.                   
023500                                                                          
023600*01  AREA -COPY W27104     -PRE W27104-                                   
023700     EJECT                                                                
023800                                                                          
023900                                                                          
024000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024100*                                                                         
024200     EJECT                                                                
024300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024400     SKIP3                                                                
024500 01  NYCKLAR-TILL-DLI.                                                    
024600     03  W-IDARTNR-X.                                                     
024700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
024800     03  W-KDSEGKEY-X.                                                    
024900         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
025000     03  W-IDDC-X.                                                        
025100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
025200     03  W-IDDC-B6-X.                                                     
025300         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
025400     03  W-IDLAND-X.                                                      
025500         05  W-IDLAND        PIC X(2)   VALUE SPACE.                      
025600     SKIP2                                                                
025700*    --- STATUS-KOD FRÅN IMS                                              
025800 01  STATUS-WS                   PIC XX.                                  
025900     88  SEGMENT-FINNS                       VALUE '  '.                  
026000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026200     SKIP2                                                                
026300 01  GODK-STATUSKODER.                                                    
026400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026500     SKIP3                                                                
026600 01  SSA1                        PIC X(64).                               
026700 01  SSA2                        PIC X(64).                               
026800     EJECT                                                                
026900*    --- IMS FUNKTIONSKODER                                               
027000*01  -COPY W0003                                                          
027100     EJECT                                                                
027200*    ---  DLI INPUT-OUTPUT AREA                                           
027300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
027400 01  DLI-IO-WDK601.                                                       
027500*    03  -COPY WDK601                                                     
027600     EJECT                                                                
027700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
027800 01  DLI-IO-WDK611.                                                       
027900*    03  -COPY WDK611                                                     
028000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
028100 01  DLI-IO-WDK701.                                                       
028200*    03  -COPY WDK701                                                     
028300     EJECT                                                                
028400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
028500 01  DLI-IO-WDK711.                                                       
028600*    03  -COPY WDK711                                                     
028700     EJECT                                                                
028800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
028900 01  DLI-IO-WDK712.                                                       
029000*    03  -COPY WDK712                                                     
029100     EJECT                                                                
029200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
029300 01   DLI-IO-AREA-B601.                                                   
029400*     03  -COPY WDB601                                                    
029500     EJECT                                                                
029600 LINKAGE SECTION.                                                         
029700                                                                          
029800     EJECT                                                                
029900*01  -COPY W0008  -PRE WDK6-                                              
030000     05  FILLER                  PIC X.                                   
030100     EJECT                                                                
030200*01  -COPY W0008  -PRE WDK7-                                              
030300     05  FILLER                  PIC X.                                   
030400     EJECT                                                                
030500*01  -COPY W0008      -PRE WDB6-                                          
030600     05  FILLER                  PIC X.                                   
030700     EJECT                                                                
030800 PROCEDURE DIVISION  USING WDK6-PCB WDK7-PCB                              
030900                           WDB6-PCB.                                      
031000 MAIN SECTION.                                                            
031100     ENTRY 'DLITCBL' USING WDK6-PCB WDK7-PCB                              
031200                           WDB6-PCB.                                      
031300                                                                          
031400                                                                          
031500     PERFORM A-INIT                                                       
031600     PERFORM S01-LAES-W27101                                              
031700     PERFORM UNTIL END-OF-W27101                                          
031800        MOVE W27101-IDARTNR  TO W-IDARTNR                                 
031900                                BYT03-IDARTNR                             
032000        MOVE W27101-IDDC     TO W-IDDC                                    
032100                                W-IDDC-B6                                 
032200        PERFORM IMS-GU-WDB601                                             
032300                                                                          
032400        IF BYT03-OBJEKT                                                   
032500           CONTINUE                                                       
032600        ELSE                                                              
032700           PERFORM B-NOLLSTALL                                            
032800           PERFORM C-KOLLA-ORDERINGANG                                    
032900        END-IF                                                            
033000        PERFORM S01-LAES-W27101                                           
033100     END-PERFORM                                                          
033200                                                                          
033300                                                                          
033400     PERFORM Z-FINIT                                                      
033500                                                                          
033600     MOVE ZERO TO RETURN-CODE                                             
033700     GOBACK                                                               
033800     .                                                                    
033900     EJECT                                                                
034000 A-INIT SECTION.                                                          
034100                                                                          
034200     OPEN INPUT  W27101                                                   
034300                                                                          
034400     OPEN OUTPUT W27102                                                   
034500                 W27103                                                   
034600                 W27104                                                   
034700                                                                          
034800     ACCEPT DAGENS-DATUM  FROM DATE                                       
034900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
035000                                                                          
035100                                                                          
035200*    HÄMTA INNEVARANDE R-PERIOD                                           
035300*                                                                         
035400*                                                                         
035500     MOVE 'IDAG'         TO DAT-KDDATFORM                                 
035600     CALL WDATKONV USING DAT-KDDATFORM                                    
035700                         DAT-I-TIDATUM                                    
035800                         DAT-O-TIDATUM                                    
035900                         DAT-KDSVAR                                       
036000                                                                          
036100     IF DAT-KDSVAR-OK                                                     
036200        MOVE DAT-TIAARP  TO WS-INNEV-TIAARP                               
036300     ELSE                                                                 
036400        MOVE 'FEL FRÅN WDATKONV 1  I A-INIT SECTION I W27110' TO          
036500                                    FELTEXT-STR                           
036600        DISPLAY FELTEXT                                                   
036700        PERFORM S99-ABEND                                                 
036800     END-IF                                                               
036900                                                                          
037000*    BERÄKNA DAGENS AAMMDD MINUS ETT ÅR                                   
037100                                                                          
037200     MOVE DAGENS-DATUM       TO DAG-TIAAMMDD-TOM                          
037300     MOVE 365                TO DAG-KVKALDAG                              
037400     MOVE 003                TO DAG-KDCALL                                
037500     CALL WDAGKONV USING DAG-KDCALL                                       
037600                         DAG-DATUM-AREA                                   
037700                         DAG-KDSVAR                                       
037800                                                                          
037900     IF DAG-KDSVAR = SPACE                                                
038000        MOVE DAG-TIAAMMDD-FOM     TO  DAGENS-TIAAMMDD-LAST-YEAR           
038100     ELSE                                                                 
038200        MOVE 'FEL FRÅN WDAGKONV 1, I A-SECTION I W27102'                  
038300                                 TO   FELTEXT-STR                         
038400        DISPLAY FELTEXT                                                   
038500        PERFORM S99-ABEND                                                 
038600     END-IF                                                               
038700                                                                          
038800                                                                          
038900*    RÄKNA OM TIAAMMDD-LAST-YEAR TILL                                     
039000*             TIAAVVD-LAST-YEAR                                           
039100                                                                          
039200     MOVE DAGENS-TIAAMMDD-LAST-YEAR TO DAT-I-TIDATUM                      
039300     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
039400     CALL WDATKONV        USING   DAT-KDDATFORM                           
039500                                  DAT-I-TIDATUM                           
039600                                  DAT-O-TIDATUM                           
039700                                  DAT-KDSVAR                              
039800                                                                          
039900     IF DAG-KDSVAR = SPACE                                                
040000        MOVE DAT-TIAAVVD       TO  DAGENS-TIAAVVD-LAST-YEAR               
040100     ELSE                                                                 
040200        MOVE 'FEL FRÅN WDATKONV 1, I A-SECTION I W27102'                  
040300                                 TO   FELTEXT-STR                         
040400        DISPLAY FELTEXT                                                   
040500        PERFORM S99-ABEND                                                 
040600     END-IF                                                               
040700                                                                          
040800                                                                          
040900                                                                          
041000                                                                          
041100*    BERÄKNA NÄSTA ARBETSDAG                                              
041200                                                                          
041300     MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-FOM                        
041400     MOVE DAGENS-DATUM        TO WORK-TIAAMMDD-TOM                        
041500     MOVE 003                 TO WORK-KDCALL                              
041600     MOVE 0                   TO WORK-KVWORKD                             
041700     MOVE WC-CDC-SE           TO WORK-IDDC                                
041800     CALL WORKDAY             USING WORK-KDCALL                           
041900                                    WORK-DATE-AREA                        
042000                                    WORK-KDSVAR                           
042100     IF WORK-KDSVAR-OK                                                    
042200        MOVE WORK-TIAAMMDD-NEXT-WORKDAY TO WS-NEXT-WORKDAY                
042300     ELSE                                                                 
042400        MOVE DAGENS-DATUM               TO WS-NEXT-WORKDAY                
042500     END-IF                                                               
042600                                                                          
042700     .                                                                    
042800     EJECT                                                                
042900                                                                          
043000                                                                          
043100 B-NOLLSTALL SECTION.                                                     
043200                                                                          
043300     MOVE ZERO       TO WS-LARMGRANS                                      
043400                        WS-KDREFTXT                                       
043500                        WS-SUPERWEEK                                      
043600                        WS-KVPB-REF-WEEK                                  
043700                        WS-KVPB-REF-SEAS                                  
043800     MOVE NEJ        TO LARM-SW                                           
043900     MOVE SPACE      TO WS-LARMORSAK                                      
044000     .                                                                    
044100     EJECT                                                                
044200                                                                          
044300                                                                          
044400 C-KOLLA-ORDERINGANG SECTION.                                             
044500                                                                          
044600     PERFORM IMS-GU-WDK601                                                
044700     IF SEGMENT-FINNS                                                     
044800        IF ART-KDERS-UTG = 0                                              
044900           PERFORM IMS-GNP-WDK611                                         
045000           MOVE JA    TO SW-TRAEFF-LAND                                   
045100           IF DCS-CHINA                                                   
045200           OR DCS-NDC-NA                                                  
045300             MOVE DCS-IDLANDX2 TO W-IDLAND                                
045400             PERFORM IMS-GU-WDK712                                        
045500             IF SEGMENT-FINNS                                             
045600               MOVE LART-PRMATRL TO WS-PRIS                               
045700                                    WS-PRIS2                              
045800             ELSE                                                         
045900               MOVE NEJ TO SW-TRAEFF-LAND                                 
046000               DISPLAY 'ARTNR: ' W-IDARTNR                                
046100               DISPLAY 'DC:    ' W-IDDC                                   
046200             END-IF                                                       
046300           ELSE                                                           
046400             MOVE CLAG-PRARTSTD    TO WS-PRIS                             
046500           END-IF                                                         
046600           IF LAND-FINNS                                                  
046700             IF (DCS-SDC AND NOT DCS-CHINA)                               
046800             OR (DCS-SDC AND NOT DCS-USA)                                 
046900                PERFORM CA-KOLLA-LARM-SDC                                 
047000             END-IF                                                       
047310             IF DCS-NDC                                                   
047400                IF CLAG-KDERS < 20                                        
047500                   PERFORM CB-KOLLA-LARM-NDC                              
047600                END-IF                                                    
047700             END-IF                                                       
047800           END-IF                                                         
047900        END-IF                                                            
048000     END-IF                                                               
048100     .                                                                    
048200     EJECT                                                                
048300                                                                          
048400                                                                          
048500 CA-KOLLA-LARM-SDC SECTION.                                               
048600                                                                          
048700                                                                          
048800* LARM 1:A ORDERINGÅNG                                                    
048900                                                                          
049000     IF W27101-TIREFEFT-OLD = ZERO AND                                    
049100        W27101-TIREFEFT-NEW > ZERO                                        
049200        MOVE ART-TIFINLV                TO TMP1-YYWWD                     
049300        MOVE DAGENS-TIAAVVD-LAST-YEAR   TO TMP2-YYWWD                     
049400        PERFORM WY2000P2                                                  
049500        IF TMP1-YYWWD > TMP2-YYWWD                                        
049600           MOVE JA TO LARM-SW                                             
049700           MOVE '1A' TO WS-LARMORSAK                                      
049800           MOVE ZERO TO WS-KVPB-REF                                       
049900        END-IF                                                            
050000     END-IF                                                               
050100                                                                          
050200     IF LARM                                                              
050300        PERFORM IMS-GU-WDK701                                             
050400        IF SEGMENT-SAKNAS                                                 
050500           MOVE ZERO TO SLAG-KVLS                                         
050600        ELSE                                                              
050700           PERFORM IMS-GNP-WDK711                                         
050800           IF SEGMENT-SAKNAS                                              
050900              MOVE ZERO TO SLAG-KVLS                                      
051000           END-IF                                                         
051100        END-IF                                                            
051200     ELSE                                                                 
051300        IF DCS-IDDC = WC-SDC-NL                                           
051400           MOVE ART-KDPRODSL     TO TEST-KDPRODSL                         
051500           IF KDPRODSL-VOLVO-EMB                                          
051600              CONTINUE                                                    
051700           ELSE                                                           
051800              PERFORM CA1-KOLLA-PB-LARM-SDC                               
051900           END-IF                                                         
052000        ELSE                                                              
052100           PERFORM CA1-KOLLA-PB-LARM-SDC                                  
052200        END-IF                                                            
052300     END-IF                                                               
052400                                                                          
052500     IF LARM                                                              
052600        PERFORM S42-SKAPA-MEMOLARM-SDC                                    
052700        PERFORM S13-SKRIV-W27103                                          
052800     END-IF                                                               
052900     .                                                                    
053000     EJECT                                                                
053100                                                                          
053200                                                                          
053300 CA1-KOLLA-PB-LARM-SDC SECTION.                                           
053400                                                                          
053500     PERFORM IMS-GU-WDK701                                                
053600     IF SEGMENT-FINNS                                                     
053700        PERFORM IMS-GNP-WDK711                                            
053800        IF SEGMENT-FINNS                                                  
053900           COMPUTE WS-KVPB-REF-SEAS ROUNDED =                             
054000                   SLAG-KVPB-REF * SLAG-RESEASON(WS-INNEV-TIRP)           
054100*          IF WS-KVPB-REF-SEAS > ZERO                                     
054200           IF WS-KVPB-REF-SEAS > 0.1                                      
054300                                                                          
054400              PERFORM S50-SUPERWEEK                                       
054500              IF WS-SUPERWEEK < 20                                        
054600                 IF WS-KVPB-REF-SEAS < 1.0                                
054700                    MOVE 3 TO WS-LARMGRANS                                
054800                    IF W27101-KVOI-LAST-DAY >= WS-LARMGRANS               
054900                       MOVE JA TO LARM-SW                                 
055000                       MOVE 'PB' TO WS-LARMORSAK                          
055100                       MOVE WS-KVPB-REF-SEAS TO WS-KVPB-REF               
055200                    END-IF                                                
055300                 ELSE                                                     
055400                    IF WS-KVPB-REF-SEAS < 5.0                             
055500                       MOVE 4 TO WS-LARMGRANS                             
055600                       IF W27101-KVOI-LAST-DAY >= WS-LARMGRANS            
055700                          MOVE JA TO LARM-SW                              
055800                          MOVE 'PB' TO WS-LARMORSAK                       
055900                          MOVE WS-KVPB-REF-SEAS TO WS-KVPB-REF            
056000                       END-IF                                             
056100                    ELSE                                                  
056200                       IF WS-KVPB-REF-SEAS <= 10.0                        
056300                          MOVE 5 TO WS-LARMGRANS                          
056400                          IF W27101-KVOI-LAST-DAY >= WS-LARMGRANS         
056500                             MOVE JA TO LARM-SW                           
056600                             MOVE 'PB' TO WS-LARMORSAK                    
056700                             MOVE WS-KVPB-REF-SEAS TO WS-KVPB-REF         
056800                          END-IF                                          
056900                       ELSE                                               
057000                          COMPUTE WS-GRANS-TEST =                         
057100                                        WS-KVPB-REF-SEAS / 3              
057200                          IF WS-GRANS-TEST-DECTAL > 0                     
057300                              COMPUTE WS-GRANS-TEST-HELTAL =              
057400                                      WS-GRANS-TEST-HELTAL + 1            
057500                          END-IF                                          
057600                          MOVE WS-GRANS-TEST-HELTAL TO                    
057700                                           WS-LARMGRANS                   
057800                          IF W27101-KVOI-LAST-DAY >= WS-LARMGRANS         
057900                             MOVE JA TO LARM-SW                           
058000                             MOVE 'PB' TO WS-LARMORSAK                    
058100                             MOVE WS-KVPB-REF-SEAS TO WS-KVPB-REF         
058200                          END-IF                                          
058300                       END-IF                                             
058400                    END-IF                                                
058500                 END-IF                                                   
058600              END-IF                                                      
058700           END-IF                                                         
058800        END-IF                                                            
058900     END-IF                                                               
059000     .                                                                    
059100     EJECT                                                                
059200                                                                          
059300                                                                          
059400 CB-KOLLA-LARM-NDC SECTION.                                               
059500                                                                          
059600     PERFORM IMS-GU-WDK701                                                
059700     IF SEGMENT-FINNS                                                     
059800        PERFORM IMS-GNP-WDK711                                            
059900        IF SEGMENT-FINNS                                                  
060000           IF (DCS-CHINA AND SLAG-IDDC-REF = SPACES)                      
060100           OR (DCS-USA AND SLAG-IDDC-REF = SPACES)                        
060200             CONTINUE                                                     
060300           ELSE                                                           
060400             COMPUTE WS-KVPB-REF-SEAS ROUNDED =                           
060500                     SLAG-KVPB-REF * SLAG-RESEASON(WS-INNEV-TIRP)         
060600             PERFORM CBA-KOLLA-PB-MEMOLARM-NDC                            
060700*MEMOLARM   HÖG OI                                                        
060800             IF LARM                                                      
060900                PERFORM S41-SKAPA-MEMOLARM-NDC                            
061000                PERFORM S14-SKRIV-W27104                                  
061100             END-IF                                                       
061200                                                                          
061300             MOVE NEJ        TO LARM-SW                                   
061400                                                                          
061500                IF W27101-TIREFEFT-OLD = ZERO AND                         
061600                   W27101-TIREFEFT-NEW > ZERO                             
061700                   MOVE 40 TO WS-KDREFTXT                                 
061800                   MOVE JA TO LARM-SW                                     
061900                END-IF                                                    
062000                IF LARM                                                   
062100                   CONTINUE                                               
062200                ELSE                                                      
062300*                                                                         
062400                   MOVE W27101-TIREFEFT-OLD      TO TMP1-YYMMDD           
062500                   MOVE DAGENS-TIAAMMDD-LAST-YEAR TO TMP2-YYMMDD          
062600                   PERFORM WY2000P1                                       
062700                   IF TMP1-YYMMDD < TMP2-YYMMDD                           
062800                   AND NOT DCS-NDC-NA                                     
062900                      MOVE 45 TO WS-KDREFTXT                              
063000                      MOVE JA TO LARM-SW                                  
063100                   END-IF                                                 
063200                END-IF                                                    
063300             IF LARM                                                      
063400                  PERFORM S40-SKAPA-FORSLAG-NDC                           
063500                  PERFORM S12-SKRIV-W27102                                
063600             END-IF                                                       
063700           END-IF                                                         
063800        END-IF                                                            
063900     END-IF                                                               
064000     .                                                                    
064100     EJECT                                                                
064200                                                                          
064300                                                                          
064400 CBA-KOLLA-PB-MEMOLARM-NDC SECTION.                                       
064500                                                                          
064600     PERFORM S50-SUPERWEEK                                                
064700                                                                          
064800*    IF   WS-KVPB-REF-SEAS > ZERO                                         
064900     IF   WS-KVPB-REF-SEAS > 1.0                                          
065000     AND (WS-PRIS > 7.99                                                  
065100     OR  (WS-PRIS < 8.00                                                  
065200     AND  WS-SUPERWEEK < 20))                                             
065300     AND  W27101-KVOI-LAST-DAY > CLAG-KVQPACK-1                           
065400                                                                          
065500        IF WS-KVPB-REF-SEAS < 3.0                                         
065600           MOVE 3 TO WS-LARMGRANS                                         
065700           IF W27101-KVOI-LAST-DAY >= WS-LARMGRANS                        
065800              MOVE JA TO LARM-SW                                          
065900              MOVE 'PB' TO WS-LARMORSAK                                   
066000              MOVE WS-KVPB-REF-SEAS TO WS-KVPB-REF                        
066100           END-IF                                                         
066200        ELSE                                                              
066300           IF WS-KVPB-REF-SEAS <= 10.0                                    
066400              COMPUTE WS-LARMGRANS ROUNDED =                              
066500                   WS-KVPB-REF-SEAS * 1                                   
066600              IF W27101-KVOI-LAST-DAY >= WS-LARMGRANS                     
066700                 MOVE JA TO LARM-SW                                       
066800                 MOVE 'PB' TO WS-LARMORSAK                                
066900                 MOVE WS-KVPB-REF-SEAS TO WS-KVPB-REF                     
067000              END-IF                                                      
067100           ELSE                                                           
067200              IF WS-KVPB-REF-SEAS <= 30.0                                 
067300                 COMPUTE WS-LARMGRANS ROUNDED =                           
067400                                    (WS-KVPB-REF-SEAS * 0.8)              
067500                 IF W27101-KVOI-LAST-DAY >=                               
067600                                WS-LARMGRANS                              
067700                    MOVE JA TO LARM-SW                                    
067800                    MOVE 'PB' TO WS-LARMORSAK                             
067900                    MOVE WS-KVPB-REF-SEAS TO WS-KVPB-REF                  
068000                 END-IF                                                   
068100              ELSE                                                        
068200                 IF WS-KVPB-REF-SEAS <= 50.0                              
068300                   COMPUTE WS-LARMGRANS ROUNDED =                         
068400                                    (WS-KVPB-REF-SEAS * 0.6)              
068500                   IF W27101-KVOI-LAST-DAY >=                             
068600                                    WS-LARMGRANS                          
068700                      MOVE JA TO LARM-SW                                  
068800                      MOVE 'PB' TO WS-LARMORSAK                           
068900                      MOVE WS-KVPB-REF-SEAS TO WS-KVPB-REF                
069000                   END-IF                                                 
069100                 ELSE                                                     
069200                   COMPUTE WS-LARMGRANS ROUNDED =                         
069300                                    (WS-KVPB-REF-SEAS * 0.4)              
069400                   IF W27101-KVOI-LAST-DAY >=                             
069500                                    WS-LARMGRANS                          
069600                      MOVE JA TO LARM-SW                                  
069700                      MOVE 'PB' TO WS-LARMORSAK                           
069800                      MOVE WS-KVPB-REF-SEAS TO WS-KVPB-REF                
069900                   END-IF                                                 
070000                 END-IF                                                   
070100              END-IF                                                      
070200           END-IF                                                         
070300        END-IF                                                            
070400     END-IF                                                               
070500     .                                                                    
070600     EJECT                                                                
070700                                                                          
070800                                                                          
070900                                                                          
071000 Z-FINIT SECTION.                                                         
071100     CLOSE W27101                                                         
071200           W27102                                                         
071300           W27103                                                         
071400           W27104                                                         
071500     SKIP2                                                                
071600     MOVE 'S' TO POSTSUM-OPKOD                                            
071700     CALL POSTSUM USING POSTSUM-PARM                                      
071800     .                                                                    
071900     EJECT                                                                
072000 S01-LAES-W27101  SECTION.                                                
072100     READ W27101 INTO W27101-AREA                                         
072200     AT END                                                               
072300        SET END-OF-W27101 TO TRUE                                         
072400                                                                          
072500     NOT AT END                                                           
072600        MOVE 'W27101' TO POSTSUM-FDNAMN                                   
072700        MOVE 'W27102D1' TO POSTSUM-DDNAMN2                                
072800        CALL POSTSUM USING POSTSUM-PARM                                   
072900     END-READ                                                             
073000     .                                                                    
073100     EJECT                                                                
073200 S12-SKRIV-W27102 SECTION.                                                
073300                                                                          
073400     WRITE W27102-POST FROM W27102-AREA                                   
073500                                                                          
073600     MOVE 'W27102' TO POSTSUM-FDNAMN                                      
073700     MOVE 'W27102D2' TO POSTSUM-DDNAMN2                                   
073800     CALL POSTSUM USING POSTSUM-PARM                                      
073900     .                                                                    
074000     EJECT                                                                
074100                                                                          
074200                                                                          
074300 S13-SKRIV-W27103 SECTION.                                                
074400                                                                          
074500     WRITE W27103-POST FROM W27103-AREA                                   
074600                                                                          
074700     MOVE 'W27103' TO POSTSUM-FDNAMN                                      
074800     MOVE 'W27102D3' TO POSTSUM-DDNAMN2                                   
074900     CALL POSTSUM USING POSTSUM-PARM                                      
075000     .                                                                    
075100     EJECT                                                                
075200                                                                          
075300                                                                          
075400 S14-SKRIV-W27104 SECTION.                                                
075500                                                                          
075600     WRITE W27104-POST FROM W27104-AREA                                   
075700                                                                          
075800     MOVE 'W27104' TO POSTSUM-FDNAMN                                      
075900     MOVE 'W27102D4' TO POSTSUM-DDNAMN2                                   
076000     CALL POSTSUM USING POSTSUM-PARM                                      
076100     .                                                                    
076200     EJECT                                                                
076300                                                                          
076400                                                                          
076500 S40-SKAPA-FORSLAG-NDC SECTION.                                           
076600                                                                          
076700     MOVE SLAG-IDPERSON-BUY      TO W27102-IDPERSON-BUY                   
076800     MOVE W27101-IDARTNR         TO W27102-IDARTNR                        
076900     MOVE W27101-IDDC            TO W27102-IDDC                           
077000     IF SLAG-FLFLYG = JA                                                  
077100        MOVE 'A'                 TO W27102-KDREFTYP                       
077200     ELSE                                                                 
077300*      IF SLAG-IDDC-REF = '11' OR '71' OR '72' OR '73'                    
077400       IF SLAG-IDDC-REF NOT = SPACE                                       
077500          MOVE 'B'               TO W27102-KDREFTYP                       
077600       ELSE                                                               
077700          MOVE 'L'               TO W27102-KDREFTYP                       
077800       END-IF                                                             
077900     END-IF                                                               
078000     MOVE CLAG-ADLAGOMR          TO W27102-ADLAGOMR-CDC                   
078100     MOVE CLAG-ADGANG            TO W27102-ADGANG-CDC                     
078200     MOVE CLAG-ADPLATS           TO W27102-ADPLATS-CDC                    
078300     MOVE SLAG-ADLAGOMR          TO W27102-ADLAGOMR-SDC                   
078400     MOVE SLAG-ADGANG            TO W27102-ADGANG-SDC                     
078500     MOVE SLAG-ADPLATS           TO W27102-ADPLATS-SDC                    
078600     MOVE ZEROES                 TO W27102-IDDISTR                        
078700     MOVE ZEROES                 TO W27102-IDKUNDNR                       
078800     MOVE ZEROES                 TO W27102-KVBEART                        
078900     MOVE ZEROES                 TO W27102-KDFRAKT                        
079000     MOVE 'P'                    TO W27102-KDREFORS                       
079100     MOVE SLAG-IDLEVNR           TO W27102-IDLEVNR                        
079200     MOVE WS-KDREFTXT            TO W27102-KDREFTXT                       
079300     MOVE ZEROES                 TO W27102-KVBEART-CD                     
079400     MOVE ZEROES                 TO W27102-ADLAGOMR-CD                    
079500     MOVE ZEROES                 TO W27102-ADGANG-CD                      
079600     MOVE ZEROES                 TO W27102-ADPLATS-CD                     
079700     MOVE SLAG-IDDC-REF          TO W27102-IDDC-REF                       
079800     .                                                                    
079900     EJECT                                                                
080000                                                                          
080100                                                                          
080200 S41-SKAPA-MEMOLARM-NDC SECTION.                                          
080300                                                                          
080400     MOVE SLAG-IDPERSON-BUY      TO W27104-IDPERSON-BUY                   
080500     MOVE W27101-IDARTNR         TO W27104-IDARTNR                        
080600     MOVE W27101-IDDC            TO W27104-IDDC                           
080700     MOVE WS-LARMGRANS           TO W27104-LARMGRANS                      
080800     MOVE W27101-KVOI-LAST-DAY   TO W27104-KVOI                           
080900     MOVE WS-KVPB-REF            TO W27104-KVPB-REF                       
081000     MOVE SLAG-KVLS              TO W27104-KVLS                           
081100     MOVE CLAG-PRARTSTD          TO W27104-PRARTSTD                       
081200     MOVE WS-PRIS2               TO W27104-PRMATRL                        
081300     MOVE WS-LARMORSAK           TO W27104-LARMORSAK                      
081400     IF SLAG-FLFLYG = JA                                                  
081500        MOVE 'A'                 TO W27104-KDREFTYP                       
081600     ELSE                                                                 
081700*      IF SLAG-IDDC-REF = '11' OR '71' OR '72' OR '73'                    
081800       IF SLAG-IDDC-REF NOT = SPACE                                       
081900          MOVE 'B'               TO W27104-KDREFTYP                       
082000       ELSE                                                               
082100          MOVE 'L'               TO W27104-KDREFTYP                       
082200       END-IF                                                             
082300     END-IF                                                               
082400     MOVE 'DAY  '                TO W27104-LARMTYP                        
082500     .                                                                    
082600     EJECT                                                                
082700                                                                          
082800                                                                          
082900 S42-SKAPA-MEMOLARM-SDC SECTION.                                          
083000                                                                          
083100     MOVE SPACE            TO W27103-AREA                                 
083200                                                                          
083300     MOVE W27101-IDARTNR   TO W27103-IDARTNR                              
083400     MOVE W27101-IDDC      TO W27103-IDDC                                 
083500     MOVE WS-LARMGRANS     TO W27103-LARMGRANS                            
083600     MOVE W27101-KVOI-LAST-DAY TO W27103-KVOI                             
083700     MOVE WS-KVPB-REF      TO W27103-KVPB-REF                             
083800     MOVE SLAG-KVLS        TO W27103-KVLS                                 
083900     MOVE CLAG-PRARTSTD    TO W27103-PRARTSTD                             
084000     MOVE WS-PRIS2         TO W27103-PRMATRL                              
084100     MOVE WS-LARMORSAK     TO W27103-LARMORSAK                            
084200     .                                                                    
084300     EJECT                                                                
084400                                                                          
084500                                                                          
084600 S50-SUPERWEEK SECTION.                                                   
084700                                                                          
084800     COMPUTE WS-KVPB-REF-WEEK =                                           
084900             WS-KVPB-REF-SEAS / 4.33                                      
085000                                                                          
085100     IF WS-KVPB-REF-WEEK = ZERO                                           
085200        MOVE +1 TO WS-KVPB-REF-WEEK                                       
085300     END-IF                                                               
085400                                                                          
085500     COMPUTE WS-SUPERWEEK =                                               
085600      (((SLAG-KVLS + SLAG-KVBEART +                                       
085700       SLAG-KVAKS-PAV + SLAG-KVAKS-SDC)                                   
085800        -                                                                 
085900      (SLAG-KVOKS-DAG + SLAG-KVOKS-BULK +                                 
086000       SLAG-KVROS-DAG + SLAG-KVROS-BULK))                                 
086100          /                                                               
086200          WS-KVPB-REF-WEEK)                                               
086300                                                                          
086400     EJECT                                                                
086500     .                                                                    
086600                                                                          
086700                                                                          
086800 S99-ABEND SECTION.                                                       
086900                                                                          
087000     SKIP2                                                                
087100     MOVE 'S' TO POSTSUM-OPKOD                                            
087200     CALL POSTSUM USING POSTSUM-PARM                                      
087300     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
087400     .                                                                    
087500     EJECT                                                                
087600* --- IMS SEKTIONER ---                                                   
087700     SKIP3                                                                
087800     EJECT                                                                
087900 IMS-GU-WDK601 SECTION.                                                   
088000                                                                          
088100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
088200          DELIMITED BY SIZE INTO SSA1                                     
088300     MOVE '  GE' TO GODK-STATUSKODER                                      
088400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
088500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
088600     PERFORM IMS-STATUSKONTROLL                                           
088700     .                                                                    
088800     EJECT                                                                
088900 IMS-GNP-WDK611 SECTION.                                                  
089000                                                                          
089100     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
089200          DELIMITED BY SIZE INTO SSA1                                     
089300     MOVE '  GE' TO GODK-STATUSKODER                                      
089400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
089500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
089600     PERFORM IMS-STATUSKONTROLL                                           
089700     .                                                                    
089800     EJECT                                                                
089900 IMS-GU-WDK701 SECTION.                                                   
090000                                                                          
090100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
090200          DELIMITED BY SIZE INTO SSA1                                     
090300     MOVE '  GE' TO GODK-STATUSKODER                                      
090400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
090500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
090600     PERFORM IMS-STATUSKONTROLL                                           
090700     .                                                                    
090800     EJECT                                                                
090900 IMS-GNP-WDK711 SECTION.                                                  
091000                                                                          
091100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
091200          DELIMITED BY SIZE INTO SSA1                                     
091300     MOVE '  GE' TO GODK-STATUSKODER                                      
091400     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
091500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
091600     PERFORM IMS-STATUSKONTROLL                                           
091700     .                                                                    
091800     EJECT                                                                
091900                                                                          
092000 IMS-GU-WDB601    SECTION.                                                
092100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
092200          DELIMITED BY SIZE INTO SSA1                                     
092300     MOVE '  ' TO GODK-STATUSKODER                                        
092400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
092500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
092600     PERFORM IMS-STATUSKONTROLL                                           
092700     .                                                                    
092800     EJECT                                                                
092900 IMS-GU-WDK712      SECTION.                                              
093000                                                                          
093100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
093200          DELIMITED BY SIZE  INTO SSA1                                    
093300     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
093400          DELIMITED BY SIZE  INTO SSA2                                    
093500     MOVE '  GE'               TO GODK-STATUSKODER                        
093600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
093700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
093800     PERFORM IMS-STATUSKONTROLL                                           
093900     .                                                                    
094000     EJECT                                                                
094100 IMS-STATUSKONTROLL SECTION.                                              
094200                                                                          
094300     SET STATUS-IX TO 1                                                   
094400     SEARCH GODK-STATUS                                                   
094500       AT END                                                             
094600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
094700           DELIMITED BY SIZE INTO FELTEXT                                 
094800         DISPLAY FELTEXT                                                  
094900         CALL FELLOG                                                      
095000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
095100         CONTINUE                                                         
095200     END-SEARCH                                                           
095300     .                                                                    
095400*    -COPY WY2000P1                                                       
095500*    -COPY WY2000P2                                                       
