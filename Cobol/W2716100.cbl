000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2716100.                                                
000400*AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
000500*DATE-WRITTEN.   MAJ 1997.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET LÄSER RESTORDER FÖR FRAMTAGANDE AV LISTA              
001100*        'TOPP 100 RESTORDER' PER DC                                      
001200*                                                                         
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001500*                              WLARTC (WDK6)                              
001600*                              WLARTS (WDK7)                              
001700*                              WLORD  (WDA5)                              
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
002100*              -  "ÖVERSÄTTNING" AV LAGER TILL DC-INDX SAKNAS             
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
003200                                                                          
003300     SELECT W271UT                     ASSIGN TO W27161D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900                                                                          
004000                                                                          
004100 FD  W271UT                                                               
004200     RECORD CONTAINS 76 CHARACTERS                                        
004300     LABEL RECORD STANDARD                                                
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600 01  UT-POST                 PIC X(76).                                   
004700                                                                          
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000     SKIP2                                                                
005100                                                                          
005200*    -- CHECKED BY WY2000                                                 
005300 77  IDPGM                       PIC X(8)    VALUE 'W2716100'.            
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600 77  AKTIV                       PIC X       VALUE 'A'.                   
005700                                                                          
005800*    --- INDEX SAMT MAX-INDEX                                             
005900 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
006000 77  INDX                        PIC 9(2)    VALUE ZERO.                  
006100 77  IX                          PIC 9(3)    VALUE ZERO.                  
006200 77  DC-INDX                     PIC 9(2)    VALUE ZERO.                  
006300 77  MAX-FSGFAKT                 PIC 9(2)    VALUE 10.                    
006400                                                                          
006500 77  VECKO-INDX                  PIC 9(2)    VALUE ZERO.                  
006600                                                                          
006700 01  TAB-RADIX                   PIC 9(2)    VALUE ZERO.                  
006800 77  MAX-TAB-RADIX               PIC 9(2)    VALUE 12.                    
006900                                                                          
007000 77  VV-INDX                     PIC 9(2)    VALUE ZERO.                  
007100 77  MAX-VV-INDX                 PIC 9(2)    VALUE 53.                    
007200                                                                          
007300*    --- SWITCHAR                                                         
007400 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
007500 01  TREND-SW                    PIC X(5)    VALUE SPACE.                 
007600     88  INGEN-TREND                         VALUE 'INGEN'.               
007700     88  SVAG-TREND                          VALUE 'SVAG '.               
007800     88  STARK-TREND                         VALUE 'STARK'.               
007900                                                                          
008000 01  INDX-SW                     PIC X       VALUE 'N'.                   
008100     88  INDX-HITTAT                         VALUE 'J'.                   
008200                                                                          
008300 01  ARTIKEL-SW                  PIC X       VALUE 'N'.                   
008400     88  ARTIKEL-SKALL-FORAENDRAS            VALUE 'J'.                   
008500                                                                          
008600 01  MANUELL-PROGNOS-SW          PIC X       VALUE 'N'.                   
008700     88  MANUELL-PROGNOS-SATT                VALUE 'J'.                   
008800                                                                          
008900*    --- ARBETSFÄLT                                                       
009000 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
009100 01  ARBETSFAELT.                                                         
009200     03  PERIODTABELL            OCCURS 12.                               
009300         05 TABELL-TIAARP        PIC  9(4)    VALUE ZERO.                 
009400         05 TABELL-FORSTA-TIAAVV PIC  9(2)    VALUE ZERO.                 
009500         05 TABELL-SISTA-TIAAVV  PIC  9(2)    VALUE ZERO.                 
009600         05 TABELL-KVOI        PIC S9(9)V9(1)  VALUE ZERO COMP-3.         
009700                                                                          
009800     03  SLUT-VV                 PIC 9(2)    VALUE ZERO.                  
009900     03  START-VV                PIC 9(2)    VALUE ZERO.                  
010000                                                                          
010100     03  WS-IDARTNR              PIC 9(9)    VALUE ZERO.                  
010200                                                                          
010300     03  WS-CURRENT-DATE.                                                 
010400         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
010500         05  FILLER              PIC 9(4)   VALUE ZERO.                   
010600         05  FILLER              PIC 9(6)   VALUE ZERO.                   
010700                                                                          
010800     03  FILLER REDEFINES WS-CURRENT-DATE.                                
010900*-----   INKLUSIVE SEKEL                                                  
011000         05  WS-DAGENS-DATUM     PIC 9(8).                                
011100         05  WS-DAGENS-TID.                                               
011200             07 WS-DAGENS-TIMME  PIC 9(2).                                
011300             07 WS-DAGENS-MINUT  PIC 9(2).                                
011400             07 WS-DAGENS-SEKUND PIC 9(2).                                
011500                                                                          
011600     03  WS-TIAAVV               PIC 9(4)    VALUE ZERO.                  
011700     03  FILLER REDEFINES WS-TIAAVV.                                      
011800         05 WS-TIAA              PIC 9(2).                                
011900         05 WS-TIVV              PIC 9(2).                                
012000                                                                          
012100     03  FOREG-TIAARP            PIC  9(4)   VALUE ZERO.                  
012200     03  FILLER REDEFINES FOREG-TIAARP.                                   
012300         05 FOREG-TIAA           PIC  9(2).                               
012400         05 FOREG-TIRP           PIC  9(2).                               
012500                                                                          
012600     03  DAGENS-TIAARP           PIC  9(4)   VALUE ZERO.                  
012700     03  FILLER REDEFINES DAGENS-TIAARP.                                  
012800         05 DAGENS-TIAA          PIC  9(2).                               
012900         05 DAGENS-TIRP          PIC  9(2).                               
013000                                                                          
013100     03  NAESTA-TIAARP           PIC  9(4)   VALUE ZERO.                  
013200     03  FILLER REDEFINES NAESTA-TIAARP.                                  
013300         05 NAESTA-TIAA          PIC  9(2).                               
013400         05 NAESTA-TIRP          PIC  9(2).                               
013500                                                                          
013600     03  SEASON-TIAARP           PIC  9(4)   VALUE ZERO.                  
013700     03  FILLER REDEFINES SEASON-TIAARP.                                  
013800         05 SEASON-TIAA          PIC  9(2).                               
013900         05 SEASON-TIRP          PIC  9(2).                               
014000                                                                          
014100     03  DAGENS-TIAAVVD          PIC  9(5)   VALUE ZERO.                  
014200     03  FILLER REDEFINES DAGENS-TIAAVVD.                                 
014300         05 DAGENS-TIAAVVD-AA    PIC  9(2).                               
014400         05 DAGENS-TIAAVVD-VV    PIC  9(2).                               
014500         05 DAGENS-TIAAVVD-D     PIC  9(1).                               
014600                                                                          
014700     03  DAGENS-TIAAVV-GRP       PIC  9(4)   VALUE ZERO.                  
014800                                                                          
014900                                                                          
015000     03  DAGENS-TIAAVVD-LAST-YEAR        PIC  9(5) VALUE ZERO.            
015100     03  FILLER REDEFINES DAGENS-TIAAVVD-LAST-YEAR.                       
015200         05 DAGENS-TIAAVVD-LAST-YEAR-AA  PIC 9(2).                        
015300         05 DAGENS-TIAAVVD-LAST-YEAR-VV  PIC 9(2).                        
015400         05 DAGENS-TIAAVVD-LAST-YEAR-D   PIC 9(1).                        
015500                                                                          
015600     03  DAGENS-TIVV             PIC  9(2)   VALUE ZERO.                  
015700                                                                          
015800     03  WS-ANTAL-VECKOR         PIC  9(2)      VALUE ZERO.               
015900     03  WS-VECKO-IO             PIC S9(9)V9 VALUE ZERO COMP-3.           
016000     03  WS-TEST-TIREFMPB        PIC S9(7)   VALUE ZERO COMP-3.           
016100     03  WS-TIREFMPB-TIAARP      PIC  9(4)      VALUE ZERO.               
016200     03  WS-FORSTA-TIAAVV        PIC  9(4)      VALUE ZERO.               
016300     03  WS-SISTA-TIAAVV         PIC  9(4)      VALUE ZERO.               
016400     03  WS-DAT-TIAAVV           PIC  9(4)      VALUE ZERO.               
016500     03  WS-ONORM-OI-GRAENS-PB   PIC S9(9)V9(1) VALUE ZERO COMP-3.        
016600     03  WS-KVOI-SEASON          PIC S9(9)V9(1) VALUE ZERO COMP-3.        
016700     03  WS-KVOI-TOT             PIC S9(11)     VALUE ZERO COMP-3.        
016800     03  WS-NY-KVPB-REF          PIC S9(6)V9(2) VALUE ZERO COMP-3.        
016900     03  NY-KVPB-REF             PIC S9(6)V9(1) VALUE ZERO COMP-3.        
017000     03  WS-PREL-KVPB-REF        PIC S9(6)V9(2) VALUE ZERO COMP-3.        
017100     03  WS-MEDEL-KVPB-REF       PIC S9(6)V9(2) VALUE ZERO COMP-3.        
017200     03  WS-ANTAL-FAKTORER-STOERRE-NOLL PIC S9(7)          COMP-3.        
017300     03  WS-KVOTEN               PIC S9(5)V9(2) VALUE ZERO COMP-3.        
017400     03  WS-TIFINLV              PIC S9(5)V     VALUE ZERO COMP-3.        
017500     03  WS-DARODAT              PIC  9(8)      VALUE ZERO.               
017600     03  WS-KVOKS-TOT            PIC S9(7)   VALUE ZERO COMP-3.           
017700     03  WS-ANTAL-POSTER         PIC  9(7)      VALUE ZERO.               
017800     03  WS-ANTAL-DC11           PIC  9(7)      VALUE ZERO.               
017900     03  WS-ANTAL-DC2X           PIC  9(7)      VALUE ZERO.               
018000     03  WS-ANTAL-DC5X           PIC  9(7)      VALUE ZERO.               
018100     03  WS-ANTAL-RAD1           PIC  9(7)      VALUE ZERO.               
018200     03  WS-ANTAL-RAD2           PIC  9(7)      VALUE ZERO.               
018300     03  WS-ANTAL-RAD3           PIC  9(7)      VALUE ZERO.               
018400     03  WS-ANTAL-RAD4           PIC  9(7)      VALUE ZERO.               
018500     03  WS-ANTAL-RAD5           PIC  9(7)      VALUE ZERO.               
018600                                                                          
018700     EJECT                                                                
018800*      --- VALID IDDC CODES                                               
018900*                                                                         
019000*01    -COPY WWDC99                                                       
019100*                                                                         
019200 01  W-IDLAND                PIC X(2)    VALUE SPACE.                     
019300                                                                          
019400*01    -COPY WWDCLAND                                                     
019500       EJECT                                                              
019600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
019700 01  FILLER REDEFINES DAGENS-DATUM.                                       
019800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
019900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
020000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
020100     EJECT                                                                
020200                                                                          
020300 01  DYNAMISKA-SUBPROGRAM.                                                
020400*                                                                         
020500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
020600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
020700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
020900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
021000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
021100     SKIP2                                                                
021200*    --- PARAMETRAR TILL ABEND                                            
021300                                                                          
021400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
021500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
021600     SKIP2                                                                
021700 01  FELTEXT.                                                             
021800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
021900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
022000     EJECT                                                                
022100*    --- PARAMETRAR TILL DATKORT                                          
022200*                                                                         
022300 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27161'.              
022400     SKIP2                                                                
022500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
022600     SKIP2                                                                
022700*01  -COPY WDATKORT                                                       
022800     EJECT                                                                
022900*    --- PARAMETRAR TILL POSTSUM                                          
023000*                                                                         
023100*01  -COPY W0005   -PRE  POSTSUM-                                         
023200     EJECT                                                                
023300*    --- PARAMETRAR TILL WDATKONV                                         
023400*                                                                         
023500*01  -COPY WDATAREA                                                       
023600     EJECT                                                                
023700 01  UT-AREA-START              PIC X(24)   VALUE                         
023800                                 'UT-AREA-START  '.                       
023900     SKIP2                                                                
024000                                                                          
024100*01  AREA -COPY W27161     -PRE UT-                                       
024200     EJECT                                                                
024300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024400                                                                          
024500     SKIP3                                                                
024600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024700     SKIP3                                                                
024800 01  NYCKLAR-TILL-DLI.                                                    
024900     03  W-IDARTNR-X.                                                     
025000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
025100                                                                          
025200     03  W-IDDC-X.                                                        
025300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
025400                                                                          
025500     03  W-KDSEGKEY-X.                                                    
025600         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
025700                                                                          
025800     03  W-IDSKYLT-X.                                                     
025900         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
026000                                                                          
026100*                                                                         
026200     SKIP2                                                                
026300*    --- STATUS-KOD FRÅN IMS                                              
026400 01  STATUS-WS                   PIC XX.                                  
026500     88  SEGMENT-FINNS                       VALUE '  '.                  
026600     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
026700     SKIP2                                                                
026800 01  GODK-STATUSKODER.                                                    
026900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027000     SKIP3                                                                
027100 01  SSA1                        PIC X(64).                               
027200 01  SSA2                        PIC X(64).                               
027300     EJECT                                                                
027400*    --- IMS FUNKTIONSKODER                                               
027500*01  -COPY W0003                                                          
027600     EJECT                                                                
027700*    ---  DLI INPUT-OUTPUT AREA                                           
027800 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ARTC01'.           
027900     SKIP3                                                                
028000 01  DLI-IO-AREA-ARTC01.                                                  
028100*    03  -COPY WDK601                                                     
028200     SKIP3                                                                
028300 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ARTC11'.           
028400     SKIP3                                                                
028500 01  DLI-IO-AREA-ARTC11.                                                  
028600*    03  -COPY WDK611                                                     
028700     EJECT                                                                
028800 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ARTS01'.           
028900     SKIP3                                                                
029000 01  DLI-IO-AREA-ARTS01.                                                  
029100*    03  -COPY WDK701                                                     
029200     SKIP3                                                                
029300 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ARTS11'.           
029400 01  DLI-IO-AREA-ARTS11.                                                  
029500*    03  -COPY WDK711                                                     
029600     EJECT                                                                
029700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-BENA01'.           
029800     SKIP3                                                                
029900 01  DLI-IO-AREA-BENA01.                                                  
030000*    03  -COPY WDD301                                                     
030100     EJECT                                                                
030200 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-BENA11'.           
030300     SKIP3                                                                
030400 01  DLI-IO-AREA-BENA11.                                                  
030500*    03  -COPY WDD311                                                     
030600     EJECT                                                                
030700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORDP01'.           
030800 01  DLI-IO-AREA-ORDP01.                                                  
030900*  03    -COPY WDA501                                                     
031000     EJECT                                                                
031100 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-ARTM01'.            
031200     SKIP3                                                                
031300 01  DLI-IO-AREA-ARTM01.                                                  
031400*  03    -COPY WDK901                                                     
031500     EJECT                                                                
031600 LINKAGE SECTION.                                                         
031700                                                                          
031800     EJECT                                                                
031900*01  -COPY W0008  -PRE WDA5-                                              
032000     05  FILLER                  PIC X.                                   
032100     EJECT                                                                
032200*01  -COPY W0008  -PRE ARTC-                                              
032300     05  FILLER                  PIC X.                                   
032400     EJECT                                                                
032500*01  -COPY W0008  -PRE ARTS-                                              
032600     05  FILLER                  PIC X.                                   
032700     EJECT                                                                
032800*01  -COPY W0008  -PRE BENA-                                              
032900     05  FILLER                  PIC X.                                   
033000     EJECT                                                                
033100*01  -COPY W0008  -PRE ARTM-                                              
033200     05  FILLER                  PIC X.                                   
033300     EJECT                                                                
033400 PROCEDURE DIVISION  USING WDA5-PCB ARTC-PCB ARTS-PCB BENA-PCB            
033500                           ARTM-PCB.                                      
033600     ENTRY 'DLITCBL' USING WDA5-PCB ARTC-PCB ARTS-PCB BENA-PCB            
033700                           ARTM-PCB.                                      
033800                                                                          
033900     PERFORM A-INIT                                                       
034000     PERFORM IMS-GN-WDA5                                                  
034100     MOVE RAD-IDDC           TO WS-IDDC                                   
034200                                W-IDDC                                    
034300     PERFORM UNTIL SEGMENT-SAKNAS                                         
034400       IF RAD-KDSTARAD = '2'                                              
034500       AND NDC                                                            
034600         PERFORM B-BEHANDLA-ARTIKEL                                       
034700       END-IF                                                             
034800       PERFORM IMS-GN-WDA5                                                
034900       MOVE RAD-IDDC         TO WS-IDDC                                   
035000                                W-IDDC                                    
035100     END-PERFORM                                                          
035200                                                                          
035300     PERFORM Z-FINIT                                                      
035400                                                                          
035500     MOVE ZERO TO RETURN-CODE                                             
035600     GOBACK                                                               
035700     .                                                                    
035800     EJECT                                                                
035900 A-INIT SECTION.                                                          
036000                                                                          
036100     OPEN OUTPUT W271UT                                                   
036200                                                                          
036300     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE                        
036400     .                                                                    
036500     EJECT                                                                
036600                                                                          
036700                                                                          
036800 B-BEHANDLA-ARTIKEL SECTION.                                              
036900                                                                          
037000     MOVE SPACE              TO UT-AREA                                   
037100     MOVE RAD-IDDC           TO UT-IDDC                                   
037200     PERFORM S10-SOK-IDLAND                                               
037300     MOVE W-IDLAND           TO UT-IDLANDX2                               
037400     MOVE RAD-IDARTNR        TO UT-IDARTNR                                
037500                                 W-IDARTNR                                
037600     MOVE RAD-DARODAT        TO WS-DARODAT                                
037700     MOVE WS-DARODAT         TO UT-DARODAT                                
037800     MOVE RAD-KVBEART-Q      TO UT-KVBEART-Q                              
037900                                                                          
038000     PERFORM IMS-GU-WDK9-ARTM01                                           
038100     IF SEGMENT-FINNS                                                     
038200       COMPUTE WS-KVOKS-TOT = ART-KVOKS-BULK +                            
038300                              ART-KVOKS-DAG +                             
038400                              ART-KVOKS-VOR                               
038500     ELSE                                                                 
038600       MOVE ZERO            TO WS-KVOKS-TOT                               
038700     END-IF                                                               
038800                                                                          
038900     PERFORM IMS-GU-WDK6-ARTC01                                           
039000     MOVE ART-IDFKNGRP       TO UT-IDFKNGRP                               
039100                                                                          
039200     PERFORM IMS-GNP-WDK6-ARTC11                                          
039300     MOVE CLAG-KDERS         TO UT-KDERS                                  
039400     COMPUTE UT-AVAIL-CDC =                                               
039500               CLAG-KVLS - CLAG-KVRESS - WS-KVOKS-TOT                     
039600                         - CLAG-KVSPARR-KVAL                              
039700                                                                          
039800     PERFORM IMS-GU-WDK7-ARTS11                                           
039900       MOVE SLAG-IDPERSON-BUY                                             
040000                             TO UT-IDPERSON-BUY                           
040100     MOVE SLAG-PRAVCOST      TO UT-PRAVCOST                               
040200     MOVE SLAG-KVPB-REF      TO UT-KVPB-REF                               
040300     COMPUTE UT-ONHAND = SLAG-KVLS - SLAG-KVRESS                          
040400     MOVE SLAG-KVBEART       TO UT-KVBEART                                
040500     MOVE SLAG-KVAKS-SDC     TO UT-KVAKS-SDC                              
040600     IF SLAG-FLORDSP = JA                                                 
040700       MOVE 'F'              TO UT-FREEZECODE                             
040800     ELSE                                                                 
040900       IF SLAG-FLSPBULK = JA                                              
041000         MOVE 'P'            TO UT-FREEZECODE                             
041100       END-IF                                                             
041200     END-IF                                                               
041300                                                                          
041400     PERFORM IMS-GU-WDD3-BENA01-BSEQ                                      
041500     IF SEGMENT-FINNS                                                     
041600       MOVE 'USA' TO W-IDSKYLT                                            
041700       PERFORM IMS-GNP-WDD3-BENA11                                        
041800       IF SEGMENT-FINNS                                                   
041900         MOVE TEXT-BEART     TO UT-BEART                                  
042000       END-IF                                                             
042100     END-IF                                                               
042200     PERFORM S12-SKRIV-W271UT                                             
042300     .                                                                    
042400     EJECT                                                                
042500 Z-FINIT SECTION.                                                         
042600                                                                          
042700     CLOSE W271UT                                                         
042800                                                                          
042900     MOVE 'S' TO POSTSUM-OPKOD                                            
043000     CALL POSTSUM USING POSTSUM-PARM                                      
043100     .                                                                    
043200     EJECT                                                                
043300 S10-SOK-IDLAND SECTION.                                                  
043400                                                                          
043500     SEARCH ALL DC-LAND                                                   
043600        AT END                                                            
043700           MOVE SPACE           TO W-IDLAND                               
043800        WHEN DCLAND-IDDC (DCLAND-IX) = RAD-IDDC                           
043900           MOVE DCLAND-IDLANDX2 (DCLAND-IX)                               
044000                                TO W-IDLAND                               
044100     END-SEARCH                                                           
044200     .                                                                    
044300     EJECT                                                                
044400 S12-SKRIV-W271UT SECTION.                                                
044500                                                                          
044600     WRITE UT-POST FROM UT-AREA                                           
044700                                                                          
044800     MOVE 'W27161'   TO POSTSUM-FDNAMN                                    
044900     MOVE 'W27161D1' TO POSTSUM-DDNAMN2                                   
045000     CALL POSTSUM USING POSTSUM-PARM                                      
045100     .                                                                    
045200     EJECT                                                                
045300 S99-ABEND SECTION.                                                       
045400                                                                          
045500     MOVE 'S' TO POSTSUM-OPKOD                                            
045600     CALL POSTSUM USING POSTSUM-PARM                                      
045700     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
045800     .                                                                    
045900     EJECT                                                                
046000* --- IMS SEKTIONER ---                                                   
046100     SKIP3                                                                
046200     EJECT                                                                
046300 IMS-GU-WDK6-ARTC01 SECTION.                                              
046400                                                                          
046500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
046600          DELIMITED BY SIZE INTO SSA1                                     
046700     MOVE '  ' TO GODK-STATUSKODER                                        
046800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-ARTC01 SSA1               
046900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
047000     PERFORM IMS-STATUSKONTROLL                                           
047100     .                                                                    
047200     EJECT                                                                
047300 IMS-GNP-WDK6-ARTC11 SECTION.                                             
047400                                                                          
047500     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
047600          DELIMITED BY SIZE INTO SSA1                                     
047700     MOVE '  ' TO GODK-STATUSKODER                                        
047800     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-ARTC11 SSA1              
047900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
048000     PERFORM IMS-STATUSKONTROLL                                           
048100     .                                                                    
048200     EJECT                                                                
048300 IMS-GU-WDK7-ARTS11 SECTION.                                              
048400                                                                          
048500     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
048600          DELIMITED BY SIZE INTO SSA1                                     
048700     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
048800          DELIMITED BY SIZE INTO SSA2                                     
048900     MOVE '  ' TO GODK-STATUSKODER                                        
049000     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA-ARTS11 SSA1 SSA2          
049100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
049200     PERFORM IMS-STATUSKONTROLL                                           
049300     .                                                                    
049400     EJECT                                                                
049500 IMS-GU-WDD3-BENA01-BSEQ SECTION.                                         
049600                                                                          
049700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
049800          DELIMITED BY SIZE INTO SSA1                                     
049900     MOVE '  GE' TO GODK-STATUSKODER                                      
050000     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA01 SSA1               
050100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
050200     PERFORM IMS-STATUSKONTROLL                                           
050300     .                                                                    
050400     SKIP3                                                                
050500 IMS-GNP-WDD3-BENA11 SECTION.                                             
050600                                                                          
050700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
050800          DELIMITED BY SIZE INTO SSA1                                     
050900     MOVE '  GE' TO GODK-STATUSKODER                                      
051000     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA-BENA11 SSA1              
051100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
051200     PERFORM IMS-STATUSKONTROLL                                           
051300     .                                                                    
051400     EJECT                                                                
051500 IMS-GN-WDA5 SECTION.                                                     
051600                                                                          
051700     CALL CBLTDLI USING GN WDA5-PCB DLI-IO-AREA-ORDP01                    
051800     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
051900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
052000     PERFORM IMS-STATUSKONTROLL                                           
052100     .                                                                    
052200     EJECT                                                                
052300 IMS-GU-WDK9-ARTM01         SECTION.                                      
052400     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
052500            DELIMITED BY SIZE INTO SSA1                                   
052600     MOVE '  GE' TO GODK-STATUSKODER                                      
052700     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA-ARTM01 SSA1               
052800     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
052900     PERFORM IMS-STATUSKONTROLL                                           
053000     .                                                                    
053100     EJECT                                                                
053200 IMS-STATUSKONTROLL SECTION.                                              
053300                                                                          
053400     SET STATUS-IX TO 1                                                   
053500     SEARCH GODK-STATUS                                                   
053600       AT END                                                             
053700         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
053800           DELIMITED BY SIZE INTO FELTEXT-STR                             
053900         DISPLAY FELTEXT                                                  
054000         CALL FELLOG                                                      
054100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
054200         CONTINUE                                                         
054300     END-SEARCH                                                           
054400     .                                                                    
