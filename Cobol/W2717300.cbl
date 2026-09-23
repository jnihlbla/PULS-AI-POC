000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2717300.                                                
000400*AUTHOR.         STEFAN ANDREASSON.                                       
000500*DATE-WRITTEN.   MARS 1998.                                               
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAM FÖR ATT TA FRAM MANUELLT SATTA SÄSONGER                  
001100*        DÄR TOM DATUM KOMMER ATT UPPHÖRA INNAN NÄSTA                     
001200*        SÄSONGSBERÄKNING (EN GÅNG PER KVARTAL)                           
001300*        DETTA PROGRAM KÖRS TVÅ VECKOR INNAN SÄSONGSBERÄKNINGEN           
001400*        OCH LIGGER TILL GRUND FÖR EN LISTA                               
001500*                                                                         
001600*        PROGRAMMET LÄSER      WDK7   MHA SB                              
001700*                              WDK6                                       
001800*                              WDD3                                       
001900*                              WDL7                                       
002000*                              WDB6                                       
002100*                                                                         
002200*    SUBPROGRAM.                                                          
002300*            W009VADD    ADD AV VECKOR TILL DATUM                         
002400*                                                                         
002500*    ABENDKODER:                                                          
002600*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
002700*        U1000 -  . . . .                                                 
002800*                                                                         
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003600     SKIP2                                                                
003700     SELECT W271UT                     ASSIGN TO W27173D1.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W271UT                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  POST  -COPY W27173    -PRE UT-    -L.                                
004800                                                                          
004900                                                                          
005000                                                                          
005100     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300     SKIP2                                                                
005400                                                                          
005500*    -- CHECKED BY WY2000                                                 
005600 77  IDPGM                       PIC X(8)    VALUE 'W2717300'.            
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900 77  AKTIV                       PIC X       VALUE 'A'.                   
006000 01  CHKP-VAR.                                                            
006100   03 CHKP-MSG-IO-AREA-LENGTH    PIC S9(9)   VALUE +32 COMP SYNC.         
006200   03 CHKP-MSG-IO-AREA           PIC X(32)   VALUE SPACE.                 
006300   03 CHKP-AREA-LENGTH           PIC S9(9)   VALUE +32 COMP SYNC.         
006400   03 CHKP-AREA                  PIC X(32)   VALUE SPACE.                 
006500   03 CHKP-ANT                   PIC S9(3)   VALUE +0.                    
006600   03 CHKP-MAX                   PIC S9(3)   VALUE +500.                  
006700                                                                          
006800*    --- INDEX SAMT MAX-INDEX                                             
006900 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
007000 77  INDX                        PIC 9(2)    VALUE ZERO.                  
007100 77  IX                          PIC 9(3)    VALUE ZERO.                  
007200 77  DC-INDX                     PIC 9(2)    VALUE ZERO.                  
007300 77  MAX-FSGFAKT                 PIC 9(2)    VALUE 10.                    
007400                                                                          
007500 77  VECKO-INDX                  PIC 9(2)    VALUE ZERO.                  
007600                                                                          
007700 01  TAB-RADIX                   PIC 9(2)    VALUE ZERO.                  
007800 77  MAX-TAB-RADIX               PIC 9(2)    VALUE 12.                    
007900                                                                          
008000 77  VV-INDX                     PIC 9(2)    VALUE ZERO.                  
008100 77  MAX-VV-INDX                 PIC 9(2)    VALUE 53.                    
008200                                                                          
008300*    --- SWITCHAR                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
008500                                                                          
008600 01  TREND-SW                    PIC X(5)    VALUE SPACE.                 
008700     88  INGEN-TREND                         VALUE 'INGEN'.               
008800     88  SVAG-TREND                          VALUE 'SVAG '.               
008900     88  STARK-TREND                         VALUE 'STARK'.               
009000                                                                          
009100 01  INDX-SW                     PIC X       VALUE 'N'.                   
009200     88  INDX-HITTAT                         VALUE 'J'.                   
009300                                                                          
009400 01  ARTIKEL-SW                  PIC X       VALUE 'N'.                   
009500     88  ARTIKEL-SKALL-FORAENDRAS            VALUE 'J'.                   
009600                                                                          
009700 01  MANUELL-PROGNOS-SW          PIC X       VALUE 'N'.                   
009800     88  MANUELL-PROGNOS-SATT                VALUE 'J'.                   
009900                                                                          
010000*    --- ARBETSFÄLT                                                       
010100 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
010200 01  ARBETSFAELT.                                                         
010300     03  PERIODTABELL            OCCURS 12.                               
010400         05 TABELL-TIAARP        PIC  9(4)    VALUE ZERO.                 
010500         05 TABELL-FORSTA-TIAAVV PIC  9(2)    VALUE ZERO.                 
010600         05 TABELL-SISTA-TIAAVV  PIC  9(2)    VALUE ZERO.                 
010700         05 TABELL-KVOI        PIC S9(9)V9(1)  VALUE ZERO COMP-3.         
010800                                                                          
010900     03  SLUT-VV                 PIC 9(2)    VALUE ZERO.                  
011000     03  START-VV                PIC 9(2)    VALUE ZERO.                  
011100                                                                          
011200     03  WS-IDARTNR              PIC 9(9)    VALUE ZERO.                  
011300     03  WS-ANTAL-TRAEFF         PIC 9(9)   VALUE ZERO.                   
011400     03  WS-ANTAL-WDK711         PIC 9(9)   VALUE ZERO.                   
011500     03  WS-ANTAL-JA             PIC 9(9)   VALUE ZERO.                   
011600     03  WS-ANTAL-NEJ            PIC 9(9)   VALUE ZERO.                   
011700     03  WS-DAKVARTAL            PIC 9(8)   VALUE ZERO.                   
011800                                                                          
011900     03  WS-CURRENT-DATE.                                                 
012000         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
012100         05  FILLER              PIC 9(4)   VALUE ZERO.                   
012200         05  FILLER              PIC 9(6)   VALUE ZERO.                   
012300                                                                          
012400     03  FILLER REDEFINES WS-CURRENT-DATE.                                
012500*-----   INKLUSIVE SEKEL                                                  
012600         05  WS-DAGENS-DATUM     PIC 9(8).                                
012700         05  WS-DAGENS-TID.                                               
012800             07 WS-DAGENS-TIMME  PIC 9(2).                                
012900             07 WS-DAGENS-MINUT  PIC 9(2).                                
013000             07 WS-DAGENS-SEKUND PIC 9(2).                                
013100                                                                          
013200     03  WS-TIAAVV               PIC 9(4)    VALUE ZERO.                  
013300     03  FILLER REDEFINES WS-TIAAVV.                                      
013400         05 WS-TIAA              PIC 9(2).                                
013500         05 WS-TIVV              PIC 9(2).                                
013600                                                                          
013700     03  FOREG-TIAARP            PIC  9(4)   VALUE ZERO.                  
013800     03  FILLER REDEFINES FOREG-TIAARP.                                   
013900         05 FOREG-TIAA           PIC  9(2).                               
014000         05 FOREG-TIRP           PIC  9(2).                               
014100                                                                          
014200     03  DAGENS-TIAARP           PIC  9(4)   VALUE ZERO.                  
014300     03  FILLER REDEFINES DAGENS-TIAARP.                                  
014400         05 DAGENS-TIAA          PIC  9(2).                               
014500         05 DAGENS-TIRP          PIC  9(2).                               
014600                                                                          
014700     03  NAESTA-TIAARP           PIC  9(4)   VALUE ZERO.                  
014800     03  FILLER REDEFINES NAESTA-TIAARP.                                  
014900         05 NAESTA-TIAA          PIC  9(2).                               
015000         05 NAESTA-TIRP          PIC  9(2).                               
015100                                                                          
015200     03  SEASON-TIAARP           PIC  9(4)   VALUE ZERO.                  
015300     03  FILLER REDEFINES SEASON-TIAARP.                                  
015400         05 SEASON-TIAA          PIC  9(2).                               
015500         05 SEASON-TIRP          PIC  9(2).                               
015600                                                                          
015700     03  DAGENS-TIAAVVD          PIC  9(5)   VALUE ZERO.                  
015800     03  FILLER REDEFINES DAGENS-TIAAVVD.                                 
015900         05 DAGENS-TIAAVVD-AA    PIC  9(2).                               
016000         05 DAGENS-TIAAVVD-VV    PIC  9(2).                               
016100         05 DAGENS-TIAAVVD-D     PIC  9(1).                               
016200                                                                          
016300     03  DAGENS-TIAAVV-GRP       PIC  9(4)   VALUE ZERO.                  
016400                                                                          
016500                                                                          
016600     03  DAGENS-TIAAVVD-LAST-YEAR        PIC  9(5) VALUE ZERO.            
016700     03  FILLER REDEFINES DAGENS-TIAAVVD-LAST-YEAR.                       
016800         05 DAGENS-TIAAVVD-LAST-YEAR-AA  PIC 9(2).                        
016900         05 DAGENS-TIAAVVD-LAST-YEAR-VV  PIC 9(2).                        
017000         05 DAGENS-TIAAVVD-LAST-YEAR-D   PIC 9(1).                        
017100                                                                          
017200     03  DAGENS-TIVV             PIC  9(2)   VALUE ZERO.                  
017300                                                                          
017400     03  WS-ANTAL-VECKOR         PIC  9(2)      VALUE ZERO.               
017500     03  WS-VECKO-IO             PIC S9(9)V9 VALUE ZERO COMP-3.           
017600     03  WS-TEST-TIREFMPB        PIC S9(7)   VALUE ZERO COMP-3.           
017700     03  WS-TIREFMPB-TIAARP      PIC  9(4)      VALUE ZERO.               
017800     03  WS-FORSTA-TIAAVV        PIC  9(4)      VALUE ZERO.               
017900     03  WS-SISTA-TIAAVV         PIC  9(4)      VALUE ZERO.               
018000     03  WS-DAT-TIAAVV           PIC  9(4)      VALUE ZERO.               
018100     03  WS-ONORM-OI-GRAENS-PB   PIC S9(9)V9(1) VALUE ZERO COMP-3.        
018200     03  WS-KVOI-SEASON          PIC S9(9)V9(1) VALUE ZERO COMP-3.        
018300     03  WS-KVOI-TOT             PIC S9(11)     VALUE ZERO COMP-3.        
018400     03  WS-NY-KVPB-REF          PIC S9(6)V9(2) VALUE ZERO COMP-3.        
018500     03  NY-KVPB-REF             PIC S9(6)V9(1) VALUE ZERO COMP-3.        
018600     03  WS-PREL-KVPB-REF        PIC S9(6)V9(2) VALUE ZERO COMP-3.        
018700     03  WS-MEDEL-KVPB-REF       PIC S9(6)V9(2) VALUE ZERO COMP-3.        
018800     03  WS-ANTAL-FAKTORER-STOERRE-NOLL PIC S9(7)          COMP-3.        
018900     03  WS-KVOTEN               PIC S9(5)V9(2) VALUE ZERO COMP-3.        
019000     03  WS-TIFINLV              PIC S9(5)V     VALUE ZERO COMP-3.        
019100                                                                          
019200       EJECT                                                              
019300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
019400 01  FILLER REDEFINES DAGENS-DATUM.                                       
019500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
019600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
019700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
019800     EJECT                                                                
019900                                                                          
020000 01  DYNAMISKA-SUBPROGRAM.                                                
020100*                                                                         
020200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
020300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
020400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020500     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
020600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
020700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
020800     03  W271SEAS                PIC X(8)    VALUE 'W271SEAS'.            
020900     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
021000     SKIP2                                                                
021100*    --- PARAMETRAR TILL ABEND                                            
021200                                                                          
021300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
021400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
021500     SKIP2                                                                
021600 01  FELTEXT.                                                             
021700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
021800     03  FELTEXT-STATUS          PIC X(2)    VALUE SPACE.                 
021900     03  FILLER                  PIC X       VALUE SPACE.                 
022000     03  FELTEXT-TEXT            PIC X(69)   VALUE SPACE.                 
022100     EJECT                                                                
022200*    --- PARAMETRAR TILL DATKORT                                          
022300*                                                                         
022400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27132'.              
022500     SKIP2                                                                
022600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
022700     SKIP2                                                                
022800*01  -COPY WDATKORT                                                       
022900     EJECT                                                                
023000*    --- PARAMETRAR TILL POSTSUM                                          
023100*                                                                         
023200*01  -COPY W0005   -PRE  POSTSUM-                                         
023300     EJECT                                                                
023400*    --- PARAMETRAR TILL WDATKONV                                         
023500*                                                                         
023600*01  -COPY WDATAREA                                                       
023700     EJECT                                                                
023800*    --- PARAMETRAR TILL W009VADD                                         
023900 01  W009VADDW.                                                           
024000     03  W009VADDW-AAVV      PIC S9(5)               COMP-3.              
024100     03  W009VADDW-ANTAL     PIC S9(3)               COMP-3.              
024200     SKIP3                                                                
024300     EJECT                                                                
024400 01  UT-AREA-START              PIC X(24)   VALUE                         
024500                                 'UT-AREA-START  '.                       
024600     SKIP2                                                                
024700                                                                          
024800*01  AREA -COPY W27173     -PRE UT-                                       
024900     EJECT                                                                
025000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025100                                                                          
025200     SKIP3                                                                
025300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025400     SKIP3                                                                
025500 01  NYCKLAR-TILL-DLI.                                                    
025600     03  W-IDARTNR-X.                                                     
025700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
025800                                                                          
025900     03  W-IDDC-X.                                                        
026000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
026100                                                                          
026200     03  W-KDSEGKEY-X.                                                    
026300         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
026400                                                                          
026500     03  W-IDSKYLT-X.                                                     
026600         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
026700                                                                          
026800     03  W-IDDC-B6-X.                                                     
026900         05 W-IDDC-B6                  PIC X(2).                          
027000                                                                          
027100*                                                                         
027200     SKIP2                                                                
027300*    --- STATUS-KOD FRÅN IMS                                              
027400 01  STATUS-WS                   PIC XX.                                  
027500     88  SEGMENT-FINNS                       VALUE '  '.                  
027600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
027700     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
027800                                                   'GB'.                  
027900     88  IMS-EJ-OK                           VALUE 'XD'.                  
028000     SKIP2                                                                
028100 01  GODK-STATUSKODER.                                                    
028200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028300     SKIP3                                                                
028400 01  SSA1                        PIC X(64).                               
028500 01  SSA2                        PIC X(64).                               
028600     EJECT                                                                
028700*    --- IMS FUNKTIONSKODER                                               
028800*01  -COPY W0003                                                          
028900     EJECT                                                                
029000*    ---  DLI INPUT-OUTPUT AREA                                           
029100 01  FILLER                    PIC X(16)  VALUE 'DLI-WDK701'.             
029200     SKIP3                                                                
029300 01  DLI-IO-AREA-K7.                                                      
029400     03  IO-AREA-K7              PIC X(300)  VALUE SPACE.                 
029500     SKIP3                                                                
029600     03  WLARTS01 REDEFINES IO-AREA-K7.                                   
029700*        05  -COPY WDK701                                                 
029800     SKIP3                                                                
029900     03  WLARTS11 REDEFINES IO-AREA-K7.                                   
030000*        05  -COPY WDK711                                                 
030100     EJECT                                                                
030200 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC01'.                      
030300 01  DLI-IO-AREA-ARTC01.                                                  
030400*    03  -COPY WDK601                                                     
030500     EJECT                                                                
030600                                                                          
030700 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC11'.                      
030800 01  DLI-IO-AREA-ARTC11.                                                  
030900*    03  -COPY WDK611                                                     
031000     EJECT                                                                
031100                                                                          
031200 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-BENA01'.           
031300     SKIP3                                                                
031400 01  DLI-IO-AREA-BENA01.                                                  
031500*    03  -COPY WDD301                                                     
031600     EJECT                                                                
031700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-BENA11'.           
031800     SKIP3                                                                
031900 01  DLI-IO-AREA-BENA11.                                                  
032000*    03  -COPY WDD311                                                     
032100     EJECT                                                                
032200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-OIGA01'.             
032300     SKIP3                                                                
032400 01  DLI-IO-AREA-OIGA01.                                                  
032500*        05  -COPY WDL701                                                 
032600     EJECT                                                                
032700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-OIGA11'.             
032800     SKIP3                                                                
032900 01  DLI-IO-AREA-OIGA11.                                                  
033000*        05  -COPY WDL711                                                 
033100                                                                          
033200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
033300 01   DLI-IO-AREA-B601.                                                   
033400*     03  -COPY WDB601                                                    
033500                                                                          
033600     EJECT                                                                
033700 LINKAGE SECTION.                                                         
033800                                                                          
033900*01  -COPY W0009   -PRE MSG-                                              
034000     EJECT                                                                
034100*01  -COPY W0008  -PRE WDK7-                                              
034200     05  WDK7-KEY-FB-AREA-IDARTNR       PIC S9(9) COMP-3.                 
034300     EJECT                                                                
034400*01  -COPY W0008  -PRE ARTC-                                              
034500     05  FILLER                  PIC X.                                   
034600     EJECT                                                                
034700*01  -COPY W0008  -PRE BENA-                                              
034800     05  FILLER                  PIC X.                                   
034900     EJECT                                                                
035000*01  -COPY W0008 -PRE OIGA-                                               
035100     05  FILLER                  PIC X.                                   
035200     EJECT                                                                
035300*01  -COPY W0008 -PRE WDB6-                                               
035400     05  FILLER                  PIC X.                                   
035500     EJECT                                                                
035600 PROCEDURE DIVISION  USING WDK7-PCB ARTC-PCB BENA-PCB OIGA-PCB            
035700                           WDB6-PCB.                                      
035800     ENTRY 'DLITCBL' USING WDK7-PCB ARTC-PCB BENA-PCB OIGA-PCB            
035900                           WDB6-PCB.                                      
036000                                                                          
036100     PERFORM A-INIT                                                       
036200     PERFORM IMS-GN-WDK7                                                  
036300     PERFORM UNTIL SEGMENT-SAKNAS                                         
036400       EVALUATE WDK7-SEG-NAME-FB                                          
036500         WHEN 'WDK701  '                                                  
036600           MOVE WDK7-KEY-FB-AREA-IDARTNR                                  
036700                             TO W-IDARTNR                                 
036800         WHEN 'WDK711  '                                                  
036900            ADD 1            TO WS-ANTAL-WDK711                           
037000            IF  SLAG-KDREFSTA = AKTIV                                     
037100            AND SLAG-DASPSEA > WS-DAGENS-DATUM                            
037200            AND SLAG-DASPSEA < WS-DAKVARTAL                               
037300              MOVE SLAG-IDDC TO W-IDDC                                    
037400              PERFORM IMS-GU-WDL7-OIGA11                                  
037500*                                                                         
037600*  KONTROLL GÖRS ATT ARTIKELN FINNS PÅ L7                                 
037700*                                                                         
037800              IF SEGMENT-FINNS                                            
037900                 MOVE W-IDARTNR                                           
038000                             TO UT-IDARTNR                                
038100                 MOVE SLAG-IDDC                                           
038200                             TO UT-IDDC                                   
038300                 MOVE SLAG-TIMANSEA                                       
038400                             TO UT-TIMANSEA                               
038500                 MOVE SLAG-DASPSEA                                        
038600                             TO UT-DASPSEA                                
038700                 MOVE ZERO   TO UT-IDPERSON-BUY                           
038800                 IF DCS-IDDC NOT = SLAG-IDDC                              
038900                    MOVE SLAG-IDDC TO W-IDDC-B6                           
039000                    PERFORM IMS-GU-WDB601                                 
039100                 END-IF                                                   
039200                 MOVE SLAG-IDPERSON-BUY                                   
039300                             TO UT-IDPERSON-BUY                           
039400                 PERFORM IMS-GU-WDD3-BENA01-BSEQ                          
039500                 IF SEGMENT-FINNS                                         
039600                   MOVE 'USA' TO W-IDSKYLT                                
039700                   PERFORM IMS-GNP-WDD3-BENA11                            
039800                   IF SEGMENT-FINNS                                       
039900                     MOVE TEXT-BEART                                      
040000                             TO UT-BEART                                  
040100                   ELSE                                                   
040200                     MOVE SPACE                                           
040300                             TO UT-BEART                                  
040400                   END-IF                                                 
040500                 END-IF                                                   
040600                                                                          
040700                 PERFORM S01-SKRIV-W271UT                                 
040800                 ADD 1       TO WS-ANTAL-TRAEFF                           
040900              END-IF                                                      
041000            END-IF                                                        
041100       END-EVALUATE                                                       
041200       PERFORM IMS-GN-WDK7                                                
041300     END-PERFORM                                                          
041400                                                                          
041500     PERFORM Z-FINIT                                                      
041600                                                                          
041700     MOVE ZERO TO RETURN-CODE                                             
041800     GOBACK                                                               
041900     .                                                                    
042000     EJECT                                                                
042100 A-INIT SECTION.                                                          
042200                                                                          
042300     OPEN OUTPUT W271UT                                                   
042400                                                                          
042500     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE                        
042600                                                                          
042700     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
042800     MOVE WS-CURRENT-DATE (3:6)                                           
042900                             TO DAT-I-TIDATUM                             
043000                                                                          
043100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
043200                     DAT-O-TIDATUM DAT-KDSVAR                             
043300                                                                          
043400     IF DAT-KDSVAR-OK                                                     
043500*******                                                                   
043600******* TA FRAM DEN FÖRSTA I NÄSTA MÅNAD FÖR SENARE JÄMFÖRELSE            
043700*******                                                                   
043800       MOVE DAT-TIAAVVD (1:4)                                             
043900                             TO W009VADDW-AAVV                            
044000       MOVE 5                TO W009VADDW-ANTAL                           
044100       CALL W009VADD USING W009VADDW-AAVV W009VADDW-ANTAL                 
044200       MOVE W009VADDW-AAVV   TO DAT-I-TIDATUM                             
044300       MOVE 'AAVV'           TO DAT-KDDATFORM                             
044400                                                                          
044500       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
044600                       DAT-O-TIDATUM DAT-KDSVAR                           
044700                                                                          
044800       IF DAT-KDSVAR-OK                                                   
044900*******                                                                   
045000******* TA FRAM DEN FÖRSTA I NÄSTA MÅNAD FÖR SENARE JÄMFÖRELSE            
045100*******                                                                   
045200          MOVE DAT-TISEKEL   TO WS-DAKVARTAL (1:2)                        
045300          MOVE DAT-TIAA      TO WS-DAKVARTAL (3:2)                        
045400          MOVE DAT-TIMM      TO WS-DAKVARTAL (5:2)                        
045500          MOVE 01            TO WS-DAKVARTAL (7:2)                        
045600       ELSE                                                               
045700           STRING ' FEL FRÅN DATUMRUTIN WDATKONV2 '                       
045800           DELIMITED BY SIZE INTO FELTEXT                                 
045900           CALL FELLOG                                                    
046000       END-IF                                                             
046100                                                                          
046200     ELSE                                                                 
046300         STRING ' FEL FRÅN DATUMRUTIN WDATKONV1 '                         
046400         DELIMITED BY SIZE INTO FELTEXT                                   
046500         CALL FELLOG                                                      
046600     END-IF                                                               
046700                                                                          
046800     .                                                                    
046900     EJECT                                                                
047000                                                                          
047100 Z-FINIT SECTION.                                                         
047200                                                                          
047300     CLOSE W271UT                                                         
047400     DISPLAY 'ANTAL TRÄFF  : ' WS-ANTAL-TRAEFF                            
047500     DISPLAY 'ANTAL WDK711 : ' WS-ANTAL-WDK711                            
047600     DISPLAY 'WS-DAKVARTAL : ' WS-DAKVARTAL                               
047700     .                                                                    
047800     EJECT                                                                
047900 S01-SKRIV-W271UT SECTION.                                                
048000                                                                          
048100     WRITE UT-POST FROM UT-AREA                                           
048200     .                                                                    
048300     EJECT                                                                
048400* --- IMS SEKTIONER ---                                                   
048500     SKIP3                                                                
048600 IMS-GN-WDK7 SECTION.                                                     
048700                                                                          
048800     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA-K7                        
048900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
049000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
049100     PERFORM IMS-STATUSKONTROLL                                           
049200     .                                                                    
049300     EJECT                                                                
049400 IMS-GU-WDD3-BENA01-BSEQ SECTION.                                         
049500                                                                          
049600     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
049700          DELIMITED BY SIZE INTO SSA1                                     
049800     MOVE '  GE' TO GODK-STATUSKODER                                      
049900     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA01 SSA1               
050000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
050100     PERFORM IMS-STATUSKONTROLL                                           
050200     .                                                                    
050300     SKIP3                                                                
050400 IMS-GNP-WDD3-BENA11 SECTION.                                             
050500                                                                          
050600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
050700          DELIMITED BY SIZE INTO SSA1                                     
050800     MOVE '  GE' TO GODK-STATUSKODER                                      
050900     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA-BENA11 SSA1              
051000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
051100     PERFORM IMS-STATUSKONTROLL                                           
051200     .                                                                    
051300     EJECT                                                                
051400 IMS-GU-WDK6-ARTC11 SECTION.                                              
051500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
051600          DELIMITED BY SIZE INTO SSA1                                     
051700     MOVE 'WLARTC11'       TO SSA2                                        
051800     MOVE '  GE' TO GODK-STATUSKODER                                      
051900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-ARTC11 SSA1 SSA2          
052000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
052100     PERFORM IMS-STATUSKONTROLL                                           
052200     .                                                                    
052300     EJECT                                                                
052400                                                                          
052500 IMS-GU-WDL7-OIGA11 SECTION.                                              
052600                                                                          
052700     STRING 'WLOIGA01(IDARTNR  =' W-IDARTNR-X ')'                         
052800          DELIMITED BY SIZE INTO SSA1                                     
052900     STRING 'WLOIGA11(IDDC     =' W-IDDC-X ')'                            
053000          DELIMITED BY SIZE INTO SSA2                                     
053100     MOVE '  GE' TO GODK-STATUSKODER                                      
053200     CALL CBLTDLI USING GU OIGA-PCB DLI-IO-AREA-OIGA11 SSA1 SSA2          
053300     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
053400     PERFORM IMS-STATUSKONTROLL                                           
053500     .                                                                    
053600     EJECT                                                                
053700 IMS-GU-WDB601    SECTION.                                                
053800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
053900          DELIMITED BY SIZE INTO SSA1                                     
054000     MOVE '  GE' TO GODK-STATUSKODER                                      
054100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
054200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
054300     PERFORM IMS-STATUSKONTROLL                                           
054400     IF SEGMENT-SAKNAS                                                    
054500         MOVE SPACE TO DCS-KDDC                                           
054600     END-IF                                                               
054700     .                                                                    
054800     EJECT                                                                
054900 IMS-STATUSKONTROLL SECTION.                                              
055000                                                                          
055100     SET STATUS-IX TO 1                                                   
055200     SEARCH GODK-STATUS                                                   
055300       AT END                                                             
055400         MOVE 'OTILLÅTEN RETURKOD FRÅN IMS: '                             
055500                             TO FELTEXT-TEXT                              
055600         DISPLAY FELTEXT                                                  
055700         CALL FELLOG                                                      
055800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
055900         CONTINUE                                                         
056000     END-SEARCH                                                           
056100     .                                                                    
