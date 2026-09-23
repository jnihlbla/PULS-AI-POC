000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9104200.                                                
000400*AUTHOR.         STEFAN KIHLBERG.                                         
000500*DATE-WRITTEN.   94/10/28.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET LÄSER FLERA GRUNDFILER MED UPPGIFTER FRÅN             
001000*        OLIKA DATABASER.                                                 
001100*        SKAPAR FILEN W91042 MED UPPGIFTER FÖR CDC OCH SUMMERADE          
001200*        UPPGIFTER FÖR SDC'ER, NDC'ER I USA. NDC'T I KANADA SAMT          
001300*        LDC'ER, NDC'ER I KINA.                                           
001400*                                                                         
001500*                                                                         
001600*    ABENDKODER:                                                          
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
002800*          --- SAMTLIGA DATAELEMENT FRÅN WDK601 OCH WDK611                
002900     SELECT W01160                     ASSIGN TO W91042D1.                
003000     SKIP2                                                                
003100*          --- UTVALDA  DATAELEMENT FRÅN WDK621, PRIS                     
003200     SELECT W01161                     ASSIGN TO W91042D2.                
003300     SKIP2                                                                
003400*          --- SAMTLIGA DATAELEMENT FRÅN WDK625,NOTERINGAR                
003500     SELECT W01162                     ASSIGN TO W91042D3.                
003600     SKIP2                                                                
003700*          --- SAMTLIGA DATAELEMENT FRÅN WDD201                           
003800     SELECT W01166                     ASSIGN TO W91042D4.                
003900     SKIP2                                                                
004000*          --- UTVALDA DATAELEMENT FRÅN WDK901                            
004100     SELECT W01168                     ASSIGN TO W91042D5.                
004200     SKIP2                                                                
004300*          --- SAMTLIGA BENÄMNINGAR PER ARTIKEL FRÅN WDD3                 
004400     SELECT W01174                     ASSIGN TO W91042D7.                
004500     SKIP2                                                                
004600*          --- SAMTLIGA DATAELEMENT FRÅN WDK711, SAMTLIGA DC              
004700     SELECT W01184                     ASSIGN TO W91042D8.                
004800     SKIP2                                                                
004900*          --- SAMTLIGA DATAELEMENT FRÅN WDF701, SAMTLIGA FORD            
005000     SELECT W01169                     ASSIGN TO W91042DA.                
005100     SKIP2                                                                
005200*          --- FIL MEN UPPGIFTER FRÅN CDC OCH SUMMERADE SDC               
005300     SELECT W91042                     ASSIGN TO W91042D9.                
005400     EJECT                                                                
005500 DATA DIVISION.                                                           
005600     SKIP3                                                                
005700 FILE SECTION.                                                            
005800     SKIP3                                                                
005900 FD  W01160                                                               
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006300*01  -COPY W01160      -L.                                                
006400                                                                          
006500                                                                          
006600                                                                          
006700 FD  W01161                                                               
006800     RECORDING       F                                                    
006900     BLOCK CONTAINS  0.                                                   
007000                                                                          
007100*01  -COPY W01161      -L.                                                
007200                                                                          
007300                                                                          
007400                                                                          
007500 FD  W01162                                                               
007600     RECORDING       F                                                    
007700     BLOCK CONTAINS  0.                                                   
007800                                                                          
007900*01  -COPY W01162      -L.                                                
008000                                                                          
008100                                                                          
008200                                                                          
008300 FD  W01166                                                               
008400     RECORDING       F                                                    
008500     BLOCK CONTAINS  0.                                                   
008600                                                                          
008700*01  -COPY W01166      -L.                                                
008800                                                                          
008900                                                                          
009000                                                                          
009100 FD  W01168                                                               
009200     RECORDING       F                                                    
009300     BLOCK CONTAINS  0.                                                   
009400                                                                          
009500*01  -COPY W01168      -L.                                                
009600                                                                          
009700                                                                          
009800                                                                          
009900                                                                          
010000 FD  W01174                                                               
010100     RECORDING       F                                                    
010200     BLOCK CONTAINS  0.                                                   
010300                                                                          
010400*01  -COPY W01174      -L.                                                
010500                                                                          
010600                                                                          
010700                                                                          
010800 FD  W01184                                                               
010900     RECORDING       F                                                    
011000     BLOCK CONTAINS  0.                                                   
011100                                                                          
011200*01  -COPY W01184      -L.                                                
011300                                                                          
011400                                                                          
011500                                                                          
011600 FD  W01169                                                               
011700     RECORDING       F                                                    
011800     BLOCK CONTAINS  0.                                                   
011900                                                                          
012000*01  -COPY W01169      -L.                                                
012100                                                                          
012200                                                                          
012300                                                                          
012400 FD  W91042                                                               
012500     RECORDING       F                                                    
012600     BLOCK CONTAINS  0.                                                   
012700                                                                          
012800*01  POST -COPY W91042  -PRE 42-                                          
012900     EJECT                                                                
013000 WORKING-STORAGE SECTION.                                                 
013100                                                                          
013200                                                                          
013300*    -- CHECKED BY WY2000                                                 
013400 77  IDPGM                       PIC X(8)    VALUE 'W9104200'.            
013500 77  JA                          PIC X       VALUE 'J'.                   
013600 77  NEJ                         PIC X       VALUE 'N'.                   
013700 77  IDAO-IX                     PIC S9(1)   VALUE ZERO COMP-3.           
013800                                                                          
013900 01  SWITCHAR.                                                            
014000     03  SW-IDAO-FLYTTAD         PIC X        VALUE 'N'.                  
014100       88  IDAO-FLYTTAD                       VALUE 'J'.                  
014200                                                                          
014300 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
014400     88  END-OF-W01160                       VALUE 'J'.                   
014500                                                                          
014600 77  W01161-EOF-SW               PIC X       VALUE 'N'.                   
014700     88  END-OF-W01161                       VALUE 'J'.                   
014800                                                                          
014900 77  W01162-EOF-SW               PIC X       VALUE 'N'.                   
015000     88  END-OF-W01162                       VALUE 'J'.                   
015100                                                                          
015200 77  W01166-EOF-SW               PIC X       VALUE 'N'.                   
015300     88  END-OF-W01166                       VALUE 'J'.                   
015400                                                                          
015500 77  W01168-EOF-SW               PIC X       VALUE 'N'.                   
015600     88  END-OF-W01168                       VALUE 'J'.                   
015700                                                                          
015800 77  W01174-EOF-SW               PIC X       VALUE 'N'.                   
015900     88  END-OF-W01174                       VALUE 'J'.                   
016000                                                                          
016100 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
016200     88  END-OF-W01184                       VALUE 'J'.                   
016300                                                                          
016400 77  W01169-EOF-SW               PIC X       VALUE 'N'.                   
016500     88  END-OF-W01169                       VALUE 'J'.                   
016600                                                                          
016700     EJECT                                                                
016800*      --- VALID IDDC                                                     
016900*                                                                         
017000 01    WS-IDDC-TABELL.                                                    
017100    03 WS-VALID-IDDC  OCCURS 100 INDEXED BY WS-IDDC-IX.                   
017200       05 WS-IDDC             PIC X(2).                                   
017300       05 WS-KDDC             PIC X(2).                                   
017600          88 WS-SDC           VALUE 'S '.                                 
017700          88 WS-NDC-NA        VALUE 'NA'.                                 
017800          88 WS-NDC-PF        VALUE 'NP'.                                 
017900          88 WS-NDC-CN        VALUE 'NC'.                                 
018100       05 WS-IDLANDX2         PIC X(2).                                   
018200       EJECT                                                              
018300 01  ARBETSAREOR.                                                         
018400     03  WS-SPAR-IDARTNR      PIC S9(9)      VALUE ZERO COMP-3.           
018500     03  IX                   PIC S9(9)      VALUE ZERO COMP-3.           
018600     03  DC-IX                PIC 9(2)       VALUE ZERO.                  
018700                                                                          
018800     03  DC-GRP-TABELL OCCURS 5.                                          
018900                                                                          
019000         05 TAB-SUM-KVAKS-DCGRP    PIC S9(7)  VALUE ZERO COMP-3.          
019100         05 TAB-SUM-KVEFRS-DCGRP   PIC S9(7)  VALUE ZERO COMP-3.          
019200         05 TAB-SUM-KVLS-DCGRP     PIC S9(7)  VALUE ZERO COMP-3.          
019300         05 TAB-SUM-KVOKS-DCGRP    PIC S9(7)  VALUE ZERO COMP-3.          
019400         05 TAB-SUM-KVRESS-DCGRP   PIC S9(7)  VALUE ZERO COMP-3.          
019500         05 TAB-SUM-KVROS-DCGRP    PIC S9(7)  VALUE ZERO COMP-3.          
019600         05 TAB-SUM-KVPB-REF-DCGRP PIC S9(6)V9(1)                         
019700                                        VALUE ZERO COMP-3.                
019800                                                                          
019900                                                                          
020000 01  W-DAPRLIST               PIC 9(8).                                   
020100 01  FILLER REDEFINES W-DAPRLIST.                                         
020200     03  FILLER               PIC 99.                                     
020300     03  W-TIPRLIST           PIC 9(6).                                   
020400     SKIP2                                                                
020500 01  DAGENS-DATUM             PIC 9(6)       VALUE ZERO.                  
020600 01  FILLER REDEFINES DAGENS-DATUM.                                       
020700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
020800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
020900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
021000     EJECT                                                                
021100 01  DYNAMISKA-SUBPROGRAM.                                                
021200*                                                                         
021300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
021400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
021500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021700     SKIP2                                                                
021800*    --- PARAMETRAR TILL ABEND                                            
021900                                                                          
022000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
022100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
022200     SKIP2                                                                
022300 01  FELTEXT.                                                             
022400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
022500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
022600     EJECT                                                                
022700*    --- PARAMETRAR TILL POSTSUM                                          
022800*                                                                         
022900*01  -COPY W0005   -PRE  POSTSUM-                                         
023000     EJECT                                                                
023100 01  W01160-AREA-START           PIC X(24)   VALUE                        
023200                                 'W01160-AREA-START  '.                   
023300     SKIP2                                                                
023400                                                                          
023500*01  AREA -COPY W01160     -PRE W01160-                                   
023600     EJECT                                                                
023700 01  W01161-AREA-START           PIC X(24)   VALUE                        
023800                                 'W01161-AREA-START  '.                   
023900     SKIP2                                                                
024000                                                                          
024100*01  AREA -COPY W01161     -PRE W01161-                                   
024200     EJECT                                                                
024300 01  W01162-AREA-START           PIC X(24)   VALUE                        
024400                                 'W01162-AREA-START  '.                   
024500     SKIP2                                                                
024600                                                                          
024700*01  AREA -COPY W01162     -PRE W01162-                                   
024800     EJECT                                                                
024900 01  W01166-AREA-START           PIC X(24)   VALUE                        
025000                                 'W01166-AREA-START  '.                   
025100     SKIP2                                                                
025200                                                                          
025300*01  AREA -COPY W01166     -PRE W01166-                                   
025400     EJECT                                                                
025500 01  W01168-AREA-START           PIC X(24)   VALUE                        
025600                                 'W01168-AREA-START  '.                   
025700     SKIP2                                                                
025800                                                                          
025900*01  AREA -COPY W01168     -PRE W01168-                                   
026000     EJECT                                                                
026100 01  W01174-AREA-START           PIC X(24)   VALUE                        
026200                                 'W01174-AREA-START  '.                   
026300     SKIP2                                                                
026400                                                                          
026500*01  AREA -COPY W01174     -PRE W01174-                                   
026600     EJECT                                                                
026700 01  W01184-AREA-START           PIC X(24)   VALUE                        
026800                                 'W01184-AREA-START  '.                   
026900     SKIP2                                                                
027000                                                                          
027100*01  AREA -COPY W01184     -PRE W01184-                                   
027200     EJECT                                                                
027300 01  W01184-AREA-START           PIC X(24)   VALUE                        
027400                                 'W01184-AREA-START  '.                   
027500     SKIP2                                                                
027600                                                                          
027700*01  AREA -COPY W01169     -PRE W01169-                                   
027800     EJECT                                                                
027900 01  W01169-AREA-START           PIC X(24)   VALUE                        
028000                                 'W01169-AREA-START  '.                   
028100     SKIP2                                                                
028200                                                                          
028300*01  AREA -COPY W91042     -PRE W91042-                                   
028400     EJECT                                                                
028500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
028600*                                                                         
028700 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
028800                                                                          
028900*    --- STATUS-KOD FRÅN IMS                                              
029000 01  STATUS-WS                   PIC XX.                                  
029100     88  SEGMENT-FINNS                       VALUE '  '.                  
029200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
029300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
029400     88  BASEN-SLUT                          VALUE 'GB'.                  
029500     SKIP2                                                                
029600 01  GODK-STATUSKODER.                                                    
029700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029800     SKIP3                                                                
029900 01  SSA1                        PIC X(160).                              
030000                                                                          
030100*    --- IMS FUNKTIONSKODER                                               
030200*01  -COPY W0003                                                          
030300                                                                          
030400 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
030500 01   DLI-IO-AREA-B601.                                                   
030600*     03  -COPY WDB601                                                    
030700 LINKAGE SECTION.                                                         
030800                                                                          
030900*01  -COPY W0008      -PRE WDB6-                                          
031000     05  FILLER                  PIC X.                                   
031100 PROCEDURE DIVISION USING  WDB6-PCB.                                      
031200     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
031300     SKIP2                                                                
031400                                                                          
031500     PERFORM A-INIT                                                       
031600     PERFORM B-LAES-INFILER                                               
031700     PERFORM UNTIL END-OF-W01160                                          
031800        MOVE W01160-CLAG-IDARTNR TO WS-SPAR-IDARTNR                       
031900        PERFORM C-NOLLSTALL                                               
032000        PERFORM D-BEHANDLA-W01160                                         
032100        PERFORM E-BEHANDLA-W01161                                         
032200        PERFORM F-BEHANDLA-W01162                                         
032300        PERFORM G-BEHANDLA-W01166                                         
032400        PERFORM H-BEHANDLA-W01168                                         
032500        PERFORM J-BEHANDLA-W01174                                         
032600        PERFORM K-BEHANDLA-W01184                                         
032700        PERFORM L-BEHANDLA-W01169                                         
032800        PERFORM S09-SKRIV-W91042                                          
032900        PERFORM S01-LAES-W01160                                           
033000     END-PERFORM                                                          
033100     PERFORM Z-FINIT                                                      
033200                                                                          
033300     MOVE ZERO TO RETURN-CODE                                             
033400     GOBACK                                                               
033500     .                                                                    
033600     EJECT                                                                
033700                                                                          
033800                                                                          
033900 A-INIT SECTION.                                                          
034000                                                                          
034100     OPEN INPUT  W01160                                                   
034200                 W01161                                                   
034300                 W01162                                                   
034400                 W01166                                                   
034500                 W01168                                                   
034600                 W01174                                                   
034700                 W01184                                                   
034800                 W01169                                                   
034900                                                                          
035000     OPEN OUTPUT W91042                                                   
035100     SKIP2                                                                
035200     ACCEPT DAGENS-DATUM  FROM DATE                                       
035300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
035400                                                                          
035500     PERFORM AA-LADDA-DC-TABELL                                           
035600     .                                                                    
035700     EJECT                                                                
035800                                                                          
035900                                                                          
036000 AA-LADDA-DC-TABELL SECTION.                                              
036100                                                                          
036200     INITIALIZE WS-IDDC-TABELL                                            
036300     SET WS-IDDC-IX TO +1                                                 
036400     PERFORM IMS-GN-WDB601                                                
036500     PERFORM UNTIL BASEN-SLUT                                             
036600        MOVE DCS-IDDC     TO WS-IDDC(WS-IDDC-IX)                          
036700        MOVE DCS-KDDC     TO WS-KDDC(WS-IDDC-IX)                          
036800        MOVE DCS-IDLANDX2 TO WS-IDLANDX2(WS-IDDC-IX)                      
036900        PERFORM IMS-GN-WDB601                                             
037000        SET WS-IDDC-IX UP BY +1                                           
037100        IF WS-IDDC-IX > 100                                               
037200           MOVE 'DC-TABELLEN FULL' TO FELTEXT                             
037300           CALL FELLOG                                                    
037400        END-IF                                                            
037500     END-PERFORM                                                          
037600     .                                                                    
037700     EJECT                                                                
037800                                                                          
037900                                                                          
038000 B-LAES-INFILER SECTION.                                                  
038100                                                                          
038200     PERFORM S01-LAES-W01160                                              
038300     PERFORM S02-LAES-W01161                                              
038400     PERFORM S03-LAES-W01162                                              
038500     PERFORM S04-LAES-W01166                                              
038600     PERFORM S05-LAES-W01168                                              
038700     PERFORM S07-LAES-W01174                                              
038800     PERFORM S08-LAES-W01184                                              
038900     PERFORM S09-LAES-W01169                                              
039000     .                                                                    
039100     EJECT                                                                
039200                                                                          
039300                                                                          
039400 C-NOLLSTALL SECTION.                                                     
039500                                                                          
039600     INITIALIZE W91042-AREA                                               
039700                                                                          
039800     MOVE 1 TO DC-IX                                                      
039900     PERFORM UNTIL DC-IX > 5                                              
040000        MOVE ZERO TO TAB-SUM-KVAKS-DCGRP(DC-IX)                           
040100                     TAB-SUM-KVEFRS-DCGRP(DC-IX)                          
040200                     TAB-SUM-KVLS-DCGRP(DC-IX)                            
040300                     TAB-SUM-KVOKS-DCGRP(DC-IX)                           
040400                     TAB-SUM-KVRESS-DCGRP(DC-IX)                          
040500                     TAB-SUM-KVROS-DCGRP(DC-IX)                           
040600                     TAB-SUM-KVPB-REF-DCGRP(DC-IX)                        
040700        ADD 1 TO DC-IX                                                    
040800     END-PERFORM                                                          
040900     .                                                                    
041000     EJECT                                                                
041100                                                                          
041200                                                                          
041300 D-BEHANDLA-W01160 SECTION.                                               
041400                                                                          
041500     MOVE '042'                    TO W91042-IDPTYP                       
041600                                                                          
041700     MOVE W01160-CLAG-IDARTNR      TO W91042-IDARTNR                      
041800                                                                          
041900     PERFORM DA-FLYTTA-ROTSEGMENT                                         
042000     PERFORM DB-FLYTTA-ARTIKEL-INFO                                       
042100     PERFORM DC-FLYTTA-CDC-INFO                                           
042200     PERFORM DD-FLYTTA-C1-INFO                                            
042300     .                                                                    
042400     EJECT                                                                
042500                                                                          
042600                                                                          
042700 DA-FLYTTA-ROTSEGMENT.                                                    
042800                                                                          
042900     MOVE W01160-CLAG-KDERS-UTG    TO W91042-KDERS-UTG                    
043000     MOVE W01160-CLAG-FLERS        TO W91042-FLERS                        
043100     MOVE W01160-CLAG-FLIART       TO W91042-FLIART                       
043200     MOVE W01160-CLAG-IDAO(1)      TO W91042-IDAO(1)                      
043300     MOVE +2              TO IDAO-IX                                      
043400     MOVE NEJ             TO SW-IDAO-FLYTTAD                              
043500     PERFORM UNTIL IDAO-IX > 5 OR IDAO-FLYTTAD                            
043600        IF W01160-CLAG-IDAO(IDAO-IX) = SPACE                              
043700           COMPUTE IDAO-IX = IDAO-IX - 1                                  
043800           IF IDAO-IX = 1                                                 
043900              MOVE SPACE TO W91042-IDAO(2)                                
044000           ELSE                                                           
044100              MOVE W01160-CLAG-IDAO(IDAO-IX) TO W91042-IDAO(2)            
044200           END-IF                                                         
044300           MOVE JA TO SW-IDAO-FLYTTAD                                     
044400        END-IF                                                            
044500        ADD +1 TO IDAO-IX                                                 
044600     END-PERFORM                                                          
044700     MOVE W01160-CLAG-IDFKNGRP     TO W91042-IDFKNGRP                     
044800     MOVE W01160-CLAG-IDFTG        TO W91042-IDFTG                        
044900     MOVE W01160-CLAG-IDLEVNR      TO W91042-IDLEVNR                      
045000     MOVE W01160-CLAG-KDPRODSL     TO W91042-KDPRODSL                     
045100     MOVE W01160-CLAG-KDSORT       TO W91042-KDSORT                       
045200     MOVE W01160-CLAG-REKSIFFR     TO W91042-REKSIFFR                     
045300     MOVE W01160-CLAG-TIERSDAT     TO W91042-TIERSDAT                     
045400     MOVE W01160-CLAG-TIFINLV      TO W91042-TIFINLV                      
045500     MOVE W01160-CLAG-TIREGDAT     TO W91042-TIREGDAT                     
045600     .                                                                    
045700     EJECT                                                                
045800                                                                          
045900                                                                          
046000 DB-FLYTTA-ARTIKEL-INFO.                                                  
046100                                                                          
046200     MOVE W01160-CLAG-BEFT          TO W91042-BEFT                        
046300     MOVE W01160-CLAG-FLGEMART      TO W91042-FLGEMART                    
046400     MOVE W01160-CLAG-FLLSRDEL      TO W91042-FLLSRDEL                    
046500     MOVE W01160-CLAG-FLTPO1        TO W91042-FLTPO1                      
046600     MOVE W01160-CLAG-IDANSK        TO W91042-IDANSK                      
046700     MOVE W01160-CLAG-IDARTNR-EMBQ0 TO W91042-IDARTNR-EMBQ0               
046800     MOVE W01160-CLAG-IDARTNR-EMBQ1 TO W91042-IDARTNR-EMBQ1               
046900     MOVE W01160-CLAG-IDARTNR-EMBQ2 TO W91042-IDARTNR-EMBQ2               
047000     MOVE W01160-CLAG-IDARTNR-EMBQ3 TO W91042-IDARTNR-EMBQ3               
047100     MOVE W01160-CLAG-IDARTNR-EMBQ4 TO W91042-IDARTNR-EMBQ4               
047200     MOVE W01160-CLAG-IDBERED       TO W91042-IDBERED                     
047300     MOVE W01160-CLAG-IDINK         TO W91042-IDINK                       
047400     MOVE W01160-CLAG-IDLKTO        TO W91042-IDLKTO                      
047500     MOVE W01160-CLAG-IDPROENH(1)   TO W91042-IDPROENH(1)                 
047600     MOVE W01160-CLAG-IDPROENH(2)   TO W91042-IDPROENH(2)                 
047700     MOVE W01160-CLAG-IDPROENH(3)   TO W91042-IDPROENH(3)                 
047800     MOVE W01160-CLAG-IDPROJ        TO W91042-IDPROJ                      
047900     MOVE W01160-CLAG-IDPROJUP      TO W91042-IDPROJUP                    
048000     MOVE W01160-CLAG-IDRITN        TO W91042-IDRITN                      
048100     MOVE +1                        TO IX                                 
048200     PERFORM UNTIL IX > 6                                                 
048300        MOVE W01160-CLAG-IDSTATNR(IX) TO W91042-IDSTATNR(IX)              
048400        ADD +1 TO IX                                                      
048500     END-PERFORM                                                          
048600     MOVE W01160-CLAG-KDAGE         TO W91042-KDAGE                       
048700     MOVE W01160-CLAG-KDARTHNT      TO W91042-KDARTHNT                    
048800     MOVE W01160-CLAG-KDARTURS      TO W91042-KDARTURS                    
048900     MOVE W01160-CLAG-KDBPSR        TO W91042-KDBPSR                      
049000     MOVE W01160-CLAG-KDERS         TO W91042-KDERS                       
049100     MOVE W01160-CLAG-KDFARLIG      TO W91042-KDFARLIG                    
049200     MOVE W01160-CLAG-KDFREKKL      TO W91042-KDFREKKL                    
049300     MOVE W01160-CLAG-KDFORPPL      TO W91042-KDFORPPL                    
049400     MOVE W01160-CLAG-KDFORPGP      TO W91042-KDFORPGP                    
049500     MOVE W01160-CLAG-KDFORPUF      TO W91042-KDFORPUF                    
049600     MOVE W01160-CLAG-KDGK          TO W91042-KDGK                        
049700     MOVE W01160-CLAG-KDHF          TO W91042-KDHF                        
049800     MOVE W01160-CLAG-KDLTK         TO W91042-KDLTK                       
049900     MOVE W01160-CLAG-KDPRISKL      TO W91042-KDPRISKL                    
050000     MOVE W01160-CLAG-KDPSLLOC      TO W91042-KDPSLLOC                    
050100     MOVE W01160-CLAG-KDSRA         TO W91042-KDSRA                       
050200     MOVE W01160-CLAG-KDTIPPR       TO W91042-KDTIPPR                     
050300     MOVE W01160-CLAG-KDUART        TO W91042-KDUART                      
050400     MOVE W01160-CLAG-KDVVKL        TO W91042-KDVVKL                      
050500     MOVE W01160-CLAG-KDVTH         TO W91042-KDVTH                       
050600     MOVE W01160-CLAG-KDYTBEH       TO W91042-KDYTBEH                     
050700     MOVE W01160-CLAG-KVAVIS-SEN    TO W91042-KVAVIS-SEN                  
050800     MOVE W01160-CLAG-KVFRYSTI      TO W91042-KVFRYSTI                    
050900     MOVE W01160-CLAG-KVQPACK-0     TO W91042-KVQPACK-0                   
051000     MOVE W01160-CLAG-KVQPACK-1     TO W91042-KVQPACK-1                   
051100     MOVE W01160-CLAG-KVQPACK-2     TO W91042-KVQPACK-2                   
051200     MOVE W01160-CLAG-KVQPACK-3     TO W91042-KVQPACK-3                   
051300     MOVE W01160-CLAG-KVQPACK-4     TO W91042-KVQPACK-4                   
051400     MOVE W01160-CLAG-PRARTSJK      TO W91042-PRARTSJK                    
051500     MOVE W01160-CLAG-PRARTSTD      TO W91042-PRARTSTD                    
051600     MOVE W01160-CLAG-PRDIRLON      TO W91042-PRDIRLON                    
051700     MOVE W01160-CLAG-PRDMTRL       TO W91042-PRDMTRL                     
051800     MOVE W01160-CLAG-PRINK         TO W91042-PRINK                       
051900     MOVE W01160-CLAG-PRLFKST       TO W91042-PRLFKST                     
052000     MOVE W01160-CLAG-PROVRPAL      TO W91042-PROVRPAL                    
052100     MOVE W01160-CLAG-TIAVIDAT-SEN  TO W91042-TIAVIDAT-SEN                
052200     MOVE W01160-CLAG-TIURPROD      TO W91042-TIURPROD                    
052300     MOVE W01160-CLAG-VKART         TO W91042-VKART                       
052400     MOVE W01160-CLAG-VLARTNTO      TO W91042-VLARTNTO                    
052500     MOVE W01160-CLAG-ADINLOMR-BOA  TO W91042-ADINLOMR-BOA                
052600     MOVE ZERO                      TO W91042-FLSPECPR                    
052700     MOVE 1                         TO W91042-KDPRTILL                    
052800     MOVE W01160-CLAG-PRARTBTO-EXP  TO W91042-PRARTBTO-EXP                
052900     MOVE +1                        TO IX                                 
053000     PERFORM UNTIL IX > 3                                                 
053100       MOVE W01160-CLAG-IDKAT(IX)   TO W91042-IDKAT(IX)                   
053200       ADD +1 TO IX                                                       
053300     END-PERFORM                                                          
053400                                                                          
053500     .                                                                    
053600     EJECT                                                                
053700                                                                          
053800                                                                          
053900 DC-FLYTTA-CDC-INFO.                                                      
054000                                                                          
054100     MOVE W01160-CLAG-ADLAGOMR      TO W91042-ADLAGOMR                    
054200     MOVE W01160-CLAG-ADGANG        TO W91042-ADGANG                      
054300     MOVE W01160-CLAG-ADPLATS       TO W91042-ADPLATS                     
054400     MOVE W01160-CLAG-KDVSOP        TO W91042-KDVSOP                      
054500     MOVE W01160-CLAG-KVPB-SATS     TO W91042-KVPB-SATS                   
054600     MOVE W01160-CLAG-KVPB-TPO      TO W91042-KVPB-TPO                    
054700     .                                                                    
054800     EJECT                                                                
054900                                                                          
055000                                                                          
055100 DD-FLYTTA-C1-INFO.                                                       
055200                                                                          
055300     COMPUTE W91042-KVAKS(1) =  W01160-CLAG-KVAKS-CDC +                   
055400                                W01160-CLAG-KVAKS-PAV +                   
055500                                W01160-CLAG-KVAKS-T                       
055600     MOVE W01160-CLAG-KVLS          TO W91042-KVLS(1)                     
055700     MOVE W01160-CLAG-KVEFRS        TO W91042-KVEFRS(1)                   
055800     MOVE W01160-CLAG-KVPB-SEP      TO W91042-KVPB-SEP(1)                 
055900     MOVE W01160-CLAG-KVRESS        TO W91042-KVRESS(1)                   
056000     MOVE W01160-CLAG-KVROS         TO W91042-KVROS(1)                    
056100     .                                                                    
056200     EJECT                                                                
056300                                                                          
056400 E-BEHANDLA-W01161 SECTION.                                               
056500                                                                          
056600     IF W01161-PRL-IDARTNR = WS-SPAR-IDARTNR                              
056700        IF W01161-PRL-IDLEVNR = W91042-IDLEVNR                            
056800           PERFORM EA-FLYTTA-W01161                                       
056900        END-IF                                                            
057000        PERFORM UNTIL W01161-PRL-IDARTNR > WS-SPAR-IDARTNR                
057100           PERFORM S02-LAES-W01161                                        
057200        END-PERFORM                                                       
057300     ELSE                                                                 
057400        IF W01161-PRL-IDARTNR > WS-SPAR-IDARTNR                           
057500           CONTINUE                                                       
057600        ELSE                                                              
057700           MOVE 'ARTIKEL PÅ FIL W01161 FRÅN WDK621 FINNS EJ               
057800-          'PÅ W01160' TO FELTEXT-STR                                     
057900           DISPLAY W01161-PRL-IDARTNR                                     
058000           DISPLAY FELTEXT                                                
058100           PERFORM S99-ABEND                                              
058200        END-IF                                                            
058300     END-IF                                                               
058400     .                                                                    
058500     EJECT                                                                
058600                                                                          
058700                                                                          
058800 EA-FLYTTA-W01161 SECTION.                                                
058900                                                                          
059000*    MOVE W01161-PRL-TIPRLIST      TO W91042-TIPRLIST                     
059100     COMPUTE W-DAPRLIST = 99999999 - W01161-PRL-DAPRLIST-9KOMPL           
059200     MOVE W-TIPRLIST               TO W91042-TIPRLIST                     
059300     MOVE W01161-PRL-KDSTATUS-PR   TO W91042-KDSTATUS-PR                  
059400     MOVE W01161-PRL-PRARTBES-PR   TO W91042-PRARTBES-PR                  
059500     .                                                                    
059600     EJECT                                                                
059700                                                                          
059800                                                                          
059900 F-BEHANDLA-W01162 SECTION.                                               
060000                                                                          
060100     IF W01162-NOT-IDARTNR = WS-SPAR-IDARTNR                              
060200        PERFORM UNTIL W01162-NOT-IDARTNR > WS-SPAR-IDARTNR                
060300           IF W01162-NOT-KDNOTTYP = 3                                     
060400              PERFORM FA-FLYTTA-W01162                                    
060500           END-IF                                                         
060600           PERFORM S03-LAES-W01162                                        
060700        END-PERFORM                                                       
060800     ELSE                                                                 
060900        IF W01162-NOT-IDARTNR > WS-SPAR-IDARTNR                           
061000           CONTINUE                                                       
061100        ELSE                                                              
061200           MOVE 'ARTIKEL PÅ FIL W01162 FRÅN WDK625 FINNS EJ               
061300-          'PÅ W01160' TO FELTEXT-STR                                     
061400           DISPLAY W01162-NOT-IDARTNR                                     
061500           DISPLAY FELTEXT                                                
061600           PERFORM S99-ABEND                                              
061700        END-IF                                                            
061800     END-IF                                                               
061900     .                                                                    
062000     EJECT                                                                
062100                                                                          
062200                                                                          
062300 FA-FLYTTA-W01162 SECTION.                                                
062400                                                                          
062500     MOVE W01162-NOT-KDNOTTYP      TO W91042-KDNOTTYP                     
062600     MOVE W01162-NOT-TEARTNOT      TO W91042-TEARTNOT                     
062700     .                                                                    
062800     EJECT                                                                
062900                                                                          
063000                                                                          
063100 G-BEHANDLA-W01166 SECTION.                                               
063200                                                                          
063300     IF W01166-ART-IDARTNR = WS-SPAR-IDARTNR                              
063400        PERFORM GA-FLYTTA-W01166                                          
063500        PERFORM S04-LAES-W01166                                           
063600     ELSE                                                                 
063700        IF W01166-ART-IDARTNR > WS-SPAR-IDARTNR                           
063800           CONTINUE                                                       
063900        ELSE                                                              
064000                                                                          
064100*PÅ WDD2 FÅR DET FINNAS ARTIKLAR SOM                                      
064200*EJ FINNS PÅ ARTIKELREGISTRET                                             
064300                                                                          
064400           PERFORM UNTIL W01166-ART-IDARTNR > WS-SPAR-IDARTNR             
064500              IF W01166-ART-IDARTNR = WS-SPAR-IDARTNR                     
064600                 PERFORM GA-FLYTTA-W01166                                 
064700              END-IF                                                      
064800              PERFORM S04-LAES-W01166                                     
064900            END-PERFORM                                                   
065000        END-IF                                                            
065100     END-IF                                                               
065200     .                                                                    
065300     EJECT                                                                
065400                                                                          
065500                                                                          
065600 GA-FLYTTA-W01166 SECTION.                                                
065700                                                                          
065800     MOVE W01166-ART-IDARTNR-MOTSV TO W91042-IDARTNR-MOTSV                
065900     MOVE W01166-ART-TEORSAK       TO W91042-TEORSAK                      
066000     .                                                                    
066100     EJECT                                                                
066200                                                                          
066300                                                                          
066400 H-BEHANDLA-W01168 SECTION.                                               
066500                                                                          
066600     IF W01168-ART-IDARTNR = WS-SPAR-IDARTNR                              
066700        PERFORM HA-FLYTTA-W01168                                          
066800        PERFORM S05-LAES-W01168                                           
066900     ELSE                                                                 
067000        IF W01168-ART-IDARTNR > WS-SPAR-IDARTNR                           
067100           CONTINUE                                                       
067200        ELSE                                                              
067300           MOVE 'ARTIKEL PÅ FIL W01168 FRÅN WDK9   FINNS EJ               
067400-          'PÅ W01160' TO FELTEXT-STR                                     
067500           DISPLAY W01168-ART-IDARTNR                                     
067600           DISPLAY FELTEXT                                                
067700           PERFORM S99-ABEND                                              
067800        END-IF                                                            
067900     END-IF                                                               
068000     .                                                                    
068100     EJECT                                                                
068200                                                                          
068300                                                                          
068400 HA-FLYTTA-W01168 SECTION.                                                
068500                                                                          
068600     COMPUTE W91042-KVOKS(1) = W01168-ART-KVOKS-BULK +                    
068700                               W01168-ART-KVOKS-DAG +                     
068800                               W01168-ART-KVOKS-VOR                       
068900     .                                                                    
069000     EJECT                                                                
069100                                                                          
069200                                                                          
069300                                                                          
069400 J-BEHANDLA-W01174 SECTION.                                               
069500                                                                          
069600     IF W01174-IDARTNR = WS-SPAR-IDARTNR                                  
069700        PERFORM JA-FLYTTA-W01174                                          
069800        PERFORM S07-LAES-W01174                                           
069900     ELSE                                                                 
070000        IF W01174-IDARTNR > WS-SPAR-IDARTNR                               
070100           CONTINUE                                                       
070200        ELSE                                                              
070300           MOVE 'ARTIKEL PÅ FIL W01174 FRÅN WDD3   FINNS EJ               
070400-          'PÅ W01160' TO FELTEXT-STR                                     
070500           DISPLAY W01174-IDARTNR                                         
070600           DISPLAY FELTEXT                                                
070700           PERFORM S07-LAES-W01174                                        
070800*          PERFORM S99-ABEND                                              
070900*****  FIX FÖR ATT KLARA FELKONVERTERAD WDD3 ****                         
071000**         PERFORM UNTIL W01174-IDARTNR NOT <  WS-SPAR-IDARTNR            
071100**             PERFORM S07-LAES-W01174                                    
071200**         END-PERFORM                                                    
071300**         IF W01174-IDARTNR NOT < WS-SPAR-IDARTNR                        
071400**             PERFORM JA-FLYTTA-W01174                                   
071500**             PERFORM S07-LAES-W01174                                    
071600**         END-IF                                                         
071700        END-IF                                                            
071800     END-IF                                                               
071900     .                                                                    
072000     EJECT                                                                
072100                                                                          
072200                                                                          
072300 JA-FLYTTA-W01174 SECTION.                                                
072400                                                                          
072500     MOVE +1                       TO IX                                  
072600     PERFORM UNTIL IX > 10                                                
072700        MOVE W01174-IDSKYLT(IX)    TO W91042-IDSKYLT(IX)                  
072800        MOVE W01174-BEART(IX)      TO W91042-BEART(IX)                    
072900        ADD +1                     TO IX                                  
073000     END-PERFORM                                                          
073100     .                                                                    
073200     EJECT                                                                
073300                                                                          
073400                                                                          
073500 K-BEHANDLA-W01184 SECTION.                                               
073600                                                                          
073700     IF W01184-SLAG-IDARTNR = WS-SPAR-IDARTNR                             
073800        PERFORM UNTIL W01184-SLAG-IDARTNR NOT = WS-SPAR-IDARTNR           
073900                                                                          
074000           PERFORM KA-KOLLA-DC                                            
074100           IF WS-SDC (WS-IDDC-IX)                                         
074200              MOVE 1 TO DC-IX                                             
074300           END-IF                                                         
074400                                                                          
074500           IF WS-NDC-NA (WS-IDDC-IX)                                      
074600              IF WS-IDLANDX2 (WS-IDDC-IX) = 'CA'                          
074700                 MOVE 3 TO DC-IX                                          
074800              ELSE                                                        
074900                 MOVE 2 TO DC-IX                                          
075000              END-IF                                                      
075100           END-IF                                                         
075200                                                                          
075300           IF WS-NDC-PF (WS-IDDC-IX)                                      
075400                 MOVE 4 TO DC-IX                                          
075500           END-IF                                                         
075600                                                                          
075700           IF WS-NDC-CN (WS-IDDC-IX)                                      
075800           OR (WS-IDLANDX2 (WS-IDDC-IX) = 'CN'                            
075900              AND WS-SDC (WS-IDDC-IX))                                    
076000                 MOVE 5 TO DC-IX                                          
076100           END-IF                                                         
076200                                                                          
076210           IF DC-IX > 0 AND DC-IX < 6                                     
076300              COMPUTE TAB-SUM-KVAKS-DCGRP(DC-IX) =                        
076400                             TAB-SUM-KVAKS-DCGRP(DC-IX)  +                
076500                             W01184-SLAG-KVAKS-PAV       +                
076600                             W01184-SLAG-KVAKS-SDC                        
076700                                                                          
076800              COMPUTE TAB-SUM-KVEFRS-DCGRP(DC-IX) =                       
076900                             TAB-SUM-KVEFRS-DCGRP(DC-IX) +                
077000                             W01184-SLAG-KVEFRS                           
077100                                                                          
077200              COMPUTE TAB-SUM-KVLS-DCGRP(DC-IX) =                         
077300                             TAB-SUM-KVLS-DCGRP(DC-IX)   +                
077400                             W01184-SLAG-KVLS                             
077500                                                                          
077600              COMPUTE TAB-SUM-KVOKS-DCGRP(DC-IX) =                        
077700                             TAB-SUM-KVOKS-DCGRP(DC-IX)  +                
077800                             W01184-SLAG-KVOKS-BULK      +                
077900                             W01184-SLAG-KVOKS-DAG                        
078000                                                                          
078100              COMPUTE TAB-SUM-KVRESS-DCGRP(DC-IX) =                       
078200                             TAB-SUM-KVRESS-DCGRP(DC-IX) +                
078300                             W01184-SLAG-KVRESS                           
078400                                                                          
078500              COMPUTE TAB-SUM-KVROS-DCGRP(DC-IX) =                        
078600                             TAB-SUM-KVROS-DCGRP(DC-IX) +                 
078700                             W01184-SLAG-KVROS-BULK     +                 
078800                             W01184-SLAG-KVROS-DAG                        
078900                                                                          
079000              COMPUTE TAB-SUM-KVPB-REF-DCGRP(DC-IX) =                     
079100                             TAB-SUM-KVPB-REF-DCGRP(DC-IX) +              
079200                             W01184-SLAG-KVPB-REF                         
079210           END-IF                                                         
079300                                                                          
079400           PERFORM S08-LAES-W01184                                        
079500                                                                          
079600        END-PERFORM                                                       
079700                                                                          
079800        PERFORM KB-FLYTTA-W01184                                          
079900     ELSE                                                                 
080000        IF W01184-SLAG-IDARTNR > WS-SPAR-IDARTNR                          
080100           CONTINUE                                                       
080200        ELSE                                                              
080300           MOVE 'ARTIKEL PÅ FIL W01184 FRÅN WDK711                        
080400-          'FINNS EJ PÅ W01160' TO FELTEXT-STR                            
080500           DISPLAY FELTEXT                                                
080600           DISPLAY 'IDARTNR = ' W01184-SLAG-IDARTNR                       
080700           DISPLAY 'IDDC    = ' W01184-SLAG-IDDC                          
080800           PERFORM S99-ABEND                                              
080900        END-IF                                                            
081000     END-IF                                                               
081100     .                                                                    
081200     EJECT                                                                
081300                                                                          
081400                                                                          
081500 KA-KOLLA-DC      SECTION.                                                
081600                                                                          
081700     IF NOT WS-IDDC (WS-IDDC-IX) = W01184-SLAG-IDDC                       
081800        SET WS-IDDC-IX TO 1                                               
081900        SEARCH WS-VALID-IDDC                                              
082000          AT END                                                          
082100         STRING 'DC SAKNAS PÅ WDB6  ' W01184-SLAG-IDDC                    
082200           DELIMITED BY SIZE INTO FELTEXT                                 
082300         CALL FELLOG                                                      
082400          WHEN WS-IDDC (WS-IDDC-IX) = W01184-SLAG-IDDC CONTINUE           
082500        END-SEARCH                                                        
082600     END-IF                                                               
082700     .                                                                    
082800     EJECT                                                                
082900                                                                          
083000                                                                          
083100 KB-FLYTTA-W01184 SECTION.                                                
083200                                                                          
083300     MOVE 1 TO DC-IX                                                      
083400     PERFORM UNTIL DC-IX > 5                                              
083500        MOVE TAB-SUM-KVAKS-DCGRP(DC-IX)  TO                               
083600                                   W91042-KVAKS(DC-IX + 1)                
083700        MOVE TAB-SUM-KVEFRS-DCGRP(DC-IX) TO                               
083800                                   W91042-KVEFRS(DC-IX + 1)               
083900        MOVE TAB-SUM-KVLS-DCGRP(DC-IX)   TO                               
084000                                   W91042-KVLS(DC-IX + 1)                 
084100        MOVE TAB-SUM-KVOKS-DCGRP(DC-IX)  TO                               
084200                                   W91042-KVOKS(DC-IX + 1)                
084300        MOVE TAB-SUM-KVRESS-DCGRP(DC-IX)   TO                             
084400                                   W91042-KVRESS(DC-IX + 1)               
084500        MOVE TAB-SUM-KVROS-DCGRP(DC-IX)   TO                              
084600                                   W91042-KVROS(DC-IX + 1)                
084700        MOVE TAB-SUM-KVPB-REF-DCGRP(DC-IX)   TO                           
084800                                   W91042-KVPB-SEP(DC-IX + 1)             
084900        ADD 1 TO DC-IX                                                    
085000     END-PERFORM                                                          
085100     .                                                                    
085200     EJECT                                                                
085300                                                                          
085400                                                                          
085500 L-BEHANDLA-W01169 SECTION.                                               
085600                                                                          
085700     IF W01169-MPNR-IDARTNR = WS-SPAR-IDARTNR                             
085800        PERFORM LA-FLYTTA-W01169                                          
085900        PERFORM S09-LAES-W01169                                           
086000     ELSE                                                                 
086100        IF W01169-MPNR-IDARTNR > WS-SPAR-IDARTNR                          
086200           CONTINUE                                                       
086300        ELSE                                                              
086400                                                                          
086500*PÅ WDF7 FÅR DET FINNAS ARTIKLAR SOM                                      
086600*EJ FINNS PÅ ARTIKELREGISTRET                                             
086700                                                                          
086800           PERFORM UNTIL W01169-MPNR-IDARTNR > WS-SPAR-IDARTNR            
086900              IF W01169-MPNR-IDARTNR = WS-SPAR-IDARTNR                    
087000                 PERFORM LA-FLYTTA-W01169                                 
087100              END-IF                                                      
087200              PERFORM S09-LAES-W01169                                     
087300            END-PERFORM                                                   
087400        END-IF                                                            
087500     END-IF                                                               
087600     .                                                                    
087700     EJECT                                                                
087800                                                                          
087900                                                                          
088000 LA-FLYTTA-W01169 SECTION.                                                
088100                                                                          
088200     MOVE W01169-MPNR-FLGEMFMC     TO W91042-FLGEMFMC                     
088300     .                                                                    
088400     EJECT                                                                
088500                                                                          
088600                                                                          
088700 Z-FINIT SECTION.                                                         
088800     CLOSE W01160                                                         
088900           W01161                                                         
089000           W01162                                                         
089100           W01166                                                         
089200           W01168                                                         
089300           W01174                                                         
089400           W01184                                                         
089500           W01169                                                         
089600           W91042                                                         
089700                                                                          
089800     MOVE 'S' TO POSTSUM-OPKOD                                            
089900     CALL POSTSUM USING POSTSUM-PARM                                      
090000     .                                                                    
090100     EJECT                                                                
090200                                                                          
090300                                                                          
090400 S01-LAES-W01160  SECTION.                                                
090500     READ W01160 INTO W01160-AREA                                         
090600     AT END                                                               
090700        SET END-OF-W01160 TO TRUE                                         
090800                                                                          
090900     NOT AT END                                                           
091000        MOVE 'W01160' TO POSTSUM-FDNAMN                                   
091100        MOVE 'W91042D1' TO POSTSUM-DDNAMN2                                
091200        CALL POSTSUM USING POSTSUM-PARM                                   
091300     END-READ                                                             
091400     .                                                                    
091500     EJECT                                                                
091600                                                                          
091700                                                                          
091800 S02-LAES-W01161  SECTION.                                                
091900     READ W01161 INTO W01161-AREA                                         
092000     AT END                                                               
092100        MOVE +999999999 TO W01161-PRL-IDARTNR                             
092200        SET END-OF-W01161 TO TRUE                                         
092300                                                                          
092400     NOT AT END                                                           
092500        MOVE 'W01161' TO POSTSUM-FDNAMN                                   
092600        MOVE 'W91042D2' TO POSTSUM-DDNAMN2                                
092700        CALL POSTSUM USING POSTSUM-PARM                                   
092800     END-READ                                                             
092900     .                                                                    
093000     EJECT                                                                
093100                                                                          
093200                                                                          
093300 S03-LAES-W01162  SECTION.                                                
093400     READ W01162 INTO W01162-AREA                                         
093500     AT END                                                               
093600        MOVE +999999999 TO W01162-NOT-IDARTNR                             
093700        SET END-OF-W01162 TO TRUE                                         
093800                                                                          
093900     NOT AT END                                                           
094000        MOVE 'W01162' TO POSTSUM-FDNAMN                                   
094100        MOVE 'W91042D3' TO POSTSUM-DDNAMN2                                
094200        CALL POSTSUM USING POSTSUM-PARM                                   
094300     END-READ                                                             
094400     .                                                                    
094500     EJECT                                                                
094600                                                                          
094700                                                                          
094800 S04-LAES-W01166  SECTION.                                                
094900     READ W01166 INTO W01166-AREA                                         
095000     AT END                                                               
095100        MOVE +999999999 TO W01166-ART-IDARTNR                             
095200                                                                          
095300     NOT AT END                                                           
095400        MOVE 'W01166' TO POSTSUM-FDNAMN                                   
095500        MOVE 'W91042D4' TO POSTSUM-DDNAMN2                                
095600        CALL POSTSUM USING POSTSUM-PARM                                   
095700     END-READ                                                             
095800     .                                                                    
095900     EJECT                                                                
096000                                                                          
096100                                                                          
096200 S05-LAES-W01168  SECTION.                                                
096300     READ W01168 INTO W01168-AREA                                         
096400     AT END                                                               
096500        MOVE +999999999 TO W01168-ART-IDARTNR                             
096600        SET END-OF-W01168 TO TRUE                                         
096700                                                                          
096800     NOT AT END                                                           
096900        MOVE 'W01168' TO POSTSUM-FDNAMN                                   
097000        MOVE 'W91042D5' TO POSTSUM-DDNAMN2                                
097100        CALL POSTSUM USING POSTSUM-PARM                                   
097200     END-READ                                                             
097300     .                                                                    
097400     EJECT                                                                
097500                                                                          
097600                                                                          
097700 S07-LAES-W01174  SECTION.                                                
097800     READ W01174 INTO W01174-AREA                                         
097900     AT END                                                               
098000        MOVE +999999999 TO W01174-IDARTNR                                 
098100        SET END-OF-W01174 TO TRUE                                         
098200                                                                          
098300     NOT AT END                                                           
098400        MOVE 'W01174' TO POSTSUM-FDNAMN                                   
098500        MOVE 'W91042D7' TO POSTSUM-DDNAMN2                                
098600        CALL POSTSUM USING POSTSUM-PARM                                   
098700     END-READ                                                             
098800     .                                                                    
098900     EJECT                                                                
099000                                                                          
099100                                                                          
099200 S08-LAES-W01184 SECTION.                                                 
099300     READ W01184 INTO W01184-AREA                                         
099400     AT END                                                               
099500        MOVE +999999999 TO W01184-SLAG-IDARTNR                            
099600        SET END-OF-W01184 TO TRUE                                         
099700                                                                          
099800     NOT AT END                                                           
099900        MOVE 'W01184' TO POSTSUM-FDNAMN                                   
100000        MOVE 'W91042D8' TO POSTSUM-DDNAMN2                                
100100        CALL POSTSUM USING POSTSUM-PARM                                   
100200     END-READ                                                             
100300     .                                                                    
100400     EJECT                                                                
100500                                                                          
100600                                                                          
100700 S09-LAES-W01169 SECTION.                                                 
100800     READ W01169 INTO W01169-AREA                                         
100900     AT END                                                               
101000        MOVE +999999999 TO W01169-MPNR-IDARTNR                            
101100        SET END-OF-W01169 TO TRUE                                         
101200                                                                          
101300     NOT AT END                                                           
101400        MOVE 'W01169' TO POSTSUM-FDNAMN                                   
101500        MOVE 'W91042DA' TO POSTSUM-DDNAMN2                                
101600        CALL POSTSUM USING POSTSUM-PARM                                   
101700     END-READ                                                             
101800     .                                                                    
101900     EJECT                                                                
102000                                                                          
102100                                                                          
102200 S09-SKRIV-W91042 SECTION.                                                
102300                                                                          
102400     WRITE 42-POST FROM W91042-AREA                                       
102500                                                                          
102600     MOVE 'W91042' TO POSTSUM-FDNAMN                                      
102700     MOVE 'W91042D9' TO POSTSUM-DDNAMN2                                   
102800     CALL POSTSUM USING POSTSUM-PARM                                      
102900     .                                                                    
103000     EJECT                                                                
103100                                                                          
103200                                                                          
103300 S99-ABEND SECTION.                                                       
103400                                                                          
103500     SKIP2                                                                
103600     MOVE 'S' TO POSTSUM-OPKOD                                            
103700     CALL POSTSUM USING POSTSUM-PARM                                      
103800     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
103900     .                                                                    
104000                                                                          
104100 IMS-GN-WDB601    SECTION.                                                
104200     MOVE 'WDB601  ' TO SSA1                                              
104300     MOVE '  GB'     TO GODK-STATUSKODER                                  
104400     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1                 
104500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
104600     PERFORM IMS-STATUSKONTROLL                                           
104700     .                                                                    
104800     EJECT                                                                
104900 IMS-STATUSKONTROLL SECTION.                                              
105000                                                                          
105100     SET STATUS-IX TO 1                                                   
105200     SEARCH GODK-STATUS                                                   
105300       AT END                                                             
105400         STRING 'STATUSKOD FROM IMS ' STATUS-WS                           
105500           DELIMITED BY SIZE INTO FELTEXT                                 
105600         CALL FELLOG                                                      
105700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
105800     END-SEARCH                                                           
105900     .                                                                    
