000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2713800.                                                
000400*AUTHOR.         STEFAN ANDREASSON.                                       
000500*DATE-WRITTEN.   FEB 1998.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET LÄSER EN FIL MED LARM OM ONORMAL                      
001100*        PROGNOSFÖRÄNDRING OCH DÄR SEDAN W2713900 SKAPAR                  
001200*        ETT MEMO                                                         
001300*                                                                         
001400*        SAME PROGRAM CAN BE EXECUTED USING INPUT FILE                    
001500*        CONTAING SLOW MOVING PARTS TO GET THE PARTS DESCRIPTION          
001600*                                                                         
001700*                                                                         
001800*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001900*                                                                         
002000*    ABENDKODER:                                                          
002100*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
002200*              -  "ÖVERSÄTTNING" AV LAGER TILL DC-INDX SAKNAS             
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300                                                                          
003400     SELECT W271IN                     ASSIGN TO W27138D1.                
003500                                                                          
003600     SELECT W271UT                     ASSIGN TO W27138D2.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200                                                                          
004300 FD  W271IN                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  POST  -COPY W27138    -PRE IN-    -L.                                
004800                                                                          
004900     SKIP3                                                                
005000                                                                          
005100 FD  W271UT                                                               
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400                                                                          
005500*01  POST  -COPY W27138    -PRE UT-    -L.                                
005600                                                                          
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900     SKIP2                                                                
006000                                                                          
006100*    -- CHECKED BY WY2000                                                 
006200 77  IDPGM                       PIC X(8)    VALUE 'W2713800'.            
006300 77  JA                          PIC X       VALUE 'J'.                   
006400 77  NEJ                         PIC X       VALUE 'N'.                   
006500 77  AKTIV                       PIC X       VALUE 'A'.                   
006600                                                                          
006700*    --- INDEX SAMT MAX-INDEX                                             
006800 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
006900 77  INDX                        PIC 9(2)    VALUE ZERO.                  
007000 77  IX                          PIC 9(3)    VALUE ZERO.                  
007100 77  DC-INDX                     PIC 9(2)    VALUE ZERO.                  
007200 77  MAX-FSGFAKT                 PIC 9(2)    VALUE 10.                    
007300                                                                          
007400 77  VECKO-INDX                  PIC 9(2)    VALUE ZERO.                  
007500                                                                          
007600 01  TAB-RADIX                   PIC 9(2)    VALUE ZERO.                  
007700 77  MAX-TAB-RADIX               PIC 9(2)    VALUE 12.                    
007800                                                                          
007900 77  VV-INDX                     PIC 9(2)    VALUE ZERO.                  
008000 77  MAX-VV-INDX                 PIC 9(2)    VALUE 53.                    
008100                                                                          
008200*    --- SWITCHAR                                                         
008300 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
008400                                                                          
008500 01  TREND-SW                    PIC X(5)    VALUE SPACE.                 
008600     88  INGEN-TREND                         VALUE 'INGEN'.               
008700     88  SVAG-TREND                          VALUE 'SVAG '.               
008800     88  STARK-TREND                         VALUE 'STARK'.               
008900                                                                          
009000 01  INDX-SW                     PIC X       VALUE 'N'.                   
009100     88  INDX-HITTAT                         VALUE 'J'.                   
009200                                                                          
009300 01  ARTIKEL-SW                  PIC X       VALUE 'N'.                   
009400     88  ARTIKEL-SKALL-FORAENDRAS            VALUE 'J'.                   
009500                                                                          
009600 01  MANUELL-PROGNOS-SW          PIC X       VALUE 'N'.                   
009700     88  MANUELL-PROGNOS-SATT                VALUE 'J'.                   
009800                                                                          
009900*    --- ARBETSFÄLT                                                       
010000 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
010100 01  ARBETSFAELT.                                                         
010200     03  PERIODTABELL            OCCURS 12.                               
010300         05 TABELL-TIAARP        PIC  9(4)    VALUE ZERO.                 
010400         05 TABELL-FORSTA-TIAAVV PIC  9(2)    VALUE ZERO.                 
010500         05 TABELL-SISTA-TIAAVV  PIC  9(2)    VALUE ZERO.                 
010600         05 TABELL-KVOI        PIC S9(9)V9(1)  VALUE ZERO COMP-3.         
010700                                                                          
010800     03  SLUT-VV                 PIC 9(2)    VALUE ZERO.                  
010900     03  START-VV                PIC 9(2)    VALUE ZERO.                  
011000                                                                          
011100     03  WS-IDARTNR              PIC 9(9)    VALUE ZERO.                  
011200                                                                          
011300     03  WS-CURRENT-DATE.                                                 
011400         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
011500         05  FILLER              PIC 9(4)   VALUE ZERO.                   
011600         05  FILLER              PIC 9(6)   VALUE ZERO.                   
011700                                                                          
011800     03  FILLER REDEFINES WS-CURRENT-DATE.                                
011900*-----   INKLUSIVE SEKEL                                                  
012000         05  WS-DAGENS-DATUM     PIC 9(8).                                
012100         05  WS-DAGENS-TID.                                               
012200             07 WS-DAGENS-TIMME  PIC 9(2).                                
012300             07 WS-DAGENS-MINUT  PIC 9(2).                                
012400             07 WS-DAGENS-SEKUND PIC 9(2).                                
012500                                                                          
012600     03  WS-TIAAVV               PIC 9(4)    VALUE ZERO.                  
012700     03  FILLER REDEFINES WS-TIAAVV.                                      
012800         05 WS-TIAA              PIC 9(2).                                
012900         05 WS-TIVV              PIC 9(2).                                
013000                                                                          
013100     03  FOREG-TIAARP            PIC  9(4)   VALUE ZERO.                  
013200     03  FILLER REDEFINES FOREG-TIAARP.                                   
013300         05 FOREG-TIAA           PIC  9(2).                               
013400         05 FOREG-TIRP           PIC  9(2).                               
013500                                                                          
013600     03  DAGENS-TIAARP           PIC  9(4)   VALUE ZERO.                  
013700     03  FILLER REDEFINES DAGENS-TIAARP.                                  
013800         05 DAGENS-TIAA          PIC  9(2).                               
013900         05 DAGENS-TIRP          PIC  9(2).                               
014000                                                                          
014100     03  NAESTA-TIAARP           PIC  9(4)   VALUE ZERO.                  
014200     03  FILLER REDEFINES NAESTA-TIAARP.                                  
014300         05 NAESTA-TIAA          PIC  9(2).                               
014400         05 NAESTA-TIRP          PIC  9(2).                               
014500                                                                          
014600     03  SEASON-TIAARP           PIC  9(4)   VALUE ZERO.                  
014700     03  FILLER REDEFINES SEASON-TIAARP.                                  
014800         05 SEASON-TIAA          PIC  9(2).                               
014900         05 SEASON-TIRP          PIC  9(2).                               
015000                                                                          
015100     03  DAGENS-TIAAVVD          PIC  9(5)   VALUE ZERO.                  
015200     03  FILLER REDEFINES DAGENS-TIAAVVD.                                 
015300         05 DAGENS-TIAAVVD-AA    PIC  9(2).                               
015400         05 DAGENS-TIAAVVD-VV    PIC  9(2).                               
015500         05 DAGENS-TIAAVVD-D     PIC  9(1).                               
015600                                                                          
015700     03  DAGENS-TIAAVV-GRP       PIC  9(4)   VALUE ZERO.                  
015800                                                                          
015900                                                                          
016000     03  DAGENS-TIAAVVD-LAST-YEAR        PIC  9(5) VALUE ZERO.            
016100     03  FILLER REDEFINES DAGENS-TIAAVVD-LAST-YEAR.                       
016200         05 DAGENS-TIAAVVD-LAST-YEAR-AA  PIC 9(2).                        
016300         05 DAGENS-TIAAVVD-LAST-YEAR-VV  PIC 9(2).                        
016400         05 DAGENS-TIAAVVD-LAST-YEAR-D   PIC 9(1).                        
016500                                                                          
016600     03  DAGENS-TIVV             PIC  9(2)   VALUE ZERO.                  
016700                                                                          
016800     03  WS-ANTAL-VECKOR         PIC  9(2)      VALUE ZERO.               
016900     03  WS-VECKO-IO             PIC S9(9)V9 VALUE ZERO COMP-3.           
017000     03  WS-TEST-TIREFMPB        PIC S9(7)   VALUE ZERO COMP-3.           
017100     03  WS-TIREFMPB-TIAARP      PIC  9(4)      VALUE ZERO.               
017200     03  WS-FORSTA-TIAAVV        PIC  9(4)      VALUE ZERO.               
017300     03  WS-SISTA-TIAAVV         PIC  9(4)      VALUE ZERO.               
017400     03  WS-DAT-TIAAVV           PIC  9(4)      VALUE ZERO.               
017500     03  WS-ONORM-OI-GRAENS-PB   PIC S9(9)V9(1) VALUE ZERO COMP-3.        
017600     03  WS-KVOI-SEASON          PIC S9(9)V9(1) VALUE ZERO COMP-3.        
017700     03  WS-KVOI-TOT             PIC S9(11)     VALUE ZERO COMP-3.        
017800     03  WS-NY-KVPB-REF          PIC S9(6)V9(2) VALUE ZERO COMP-3.        
017900     03  NY-KVPB-REF             PIC S9(6)V9(1) VALUE ZERO COMP-3.        
018000     03  WS-PREL-KVPB-REF        PIC S9(6)V9(2) VALUE ZERO COMP-3.        
018100     03  WS-MEDEL-KVPB-REF       PIC S9(6)V9(2) VALUE ZERO COMP-3.        
018200     03  WS-ANTAL-FAKTORER-STOERRE-NOLL PIC S9(7)          COMP-3.        
018300     03  WS-KVOTEN               PIC S9(5)V9(2) VALUE ZERO COMP-3.        
018400     03  WS-TIFINLV              PIC S9(5)V     VALUE ZERO COMP-3.        
018500     03  WS-DARODAT              PIC  9(8)      VALUE ZERO.               
018600     03  WS-KVOKS-TOT            PIC S9(7)   VALUE ZERO COMP-3.           
018700     03  WS-ANTAL-POSTER         PIC  9(7)      VALUE ZERO.               
018800     03  WS-ANTAL-DC11           PIC  9(7)      VALUE ZERO.               
018900     03  WS-ANTAL-DC2X           PIC  9(7)      VALUE ZERO.               
019000     03  WS-ANTAL-DC5X           PIC  9(7)      VALUE ZERO.               
019100     03  WS-ANTAL-RAD1           PIC  9(7)      VALUE ZERO.               
019200     03  WS-ANTAL-RAD2           PIC  9(7)      VALUE ZERO.               
019300     03  WS-ANTAL-RAD3           PIC  9(7)      VALUE ZERO.               
019400     03  WS-ANTAL-RAD4           PIC  9(7)      VALUE ZERO.               
019500     03  WS-ANTAL-RAD5           PIC  9(7)      VALUE ZERO.               
019600                                                                          
019700     EJECT                                                                
019800                                                                          
019900 77  W271IN-EOF-SW               PIC X       VALUE 'N'.                   
020000     88  END-OF-W271IN                       VALUE 'J'.                   
020100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
020200 01  FILLER REDEFINES DAGENS-DATUM.                                       
020300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
020400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
020500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
020600     EJECT                                                                
020700                                                                          
020800 01  DYNAMISKA-SUBPROGRAM.                                                
020900*                                                                         
021000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
021100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
021400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
021500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
021600     SKIP2                                                                
021700*    --- PARAMETRAR TILL ABEND                                            
021800                                                                          
021900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
022000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
022100     SKIP2                                                                
022200 01  FELTEXT.                                                             
022300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
022400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
022500     EJECT                                                                
022600*    --- PARAMETRAR TILL DATKORT                                          
022700*                                                                         
022800 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27138'.              
022900     SKIP2                                                                
023000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
023100     SKIP2                                                                
023200*01  -COPY WDATKORT                                                       
023300     EJECT                                                                
023400*    --- PARAMETRAR TILL POSTSUM                                          
023500*                                                                         
023600*01  -COPY W0005   -PRE  POSTSUM-                                         
023700     EJECT                                                                
023800*    --- PARAMETRAR TILL WDATKONV                                         
023900*                                                                         
024000*01  -COPY WDATAREA                                                       
024100     EJECT                                                                
024200 01  IN-AREA-START              PIC X(24)   VALUE                         
024300                                 'IN-AREA-START  '.                       
024400     SKIP2                                                                
024500                                                                          
024600*01  AREA -COPY W27138     -PRE IN-                                       
024700     EJECT                                                                
024800 01  UT-AREA-START              PIC X(24)   VALUE                         
024900                                 'UT-AREA-START  '.                       
025000     SKIP2                                                                
025100                                                                          
025200*01  AREA -COPY W27138     -PRE UT-                                       
025300     EJECT                                                                
025400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025500                                                                          
025600     SKIP3                                                                
025700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025800     SKIP3                                                                
025900 01  NYCKLAR-TILL-DLI.                                                    
026000     03  W-IDARTNR-X.                                                     
026100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
026200                                                                          
026300     03  W-IDDC-X.                                                        
026400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
026500                                                                          
026600     03  W-KDSEGKEY-X.                                                    
026700         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
026800                                                                          
026900     03  W-IDSKYLT-X.                                                     
027000         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
027100                                                                          
027200*                                                                         
027300     SKIP2                                                                
027400*    --- STATUS-KOD FRÅN IMS                                              
027500 01  STATUS-WS                   PIC XX.                                  
027600     88  SEGMENT-FINNS                       VALUE '  '.                  
027700     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
027800     SKIP2                                                                
027900 01  GODK-STATUSKODER.                                                    
028000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028100     SKIP3                                                                
028200 01  SSA1                        PIC X(64).                               
028300 01  SSA2                        PIC X(64).                               
028400     EJECT                                                                
028500*    --- IMS FUNKTIONSKODER                                               
028600*01  -COPY W0003                                                          
028700     EJECT                                                                
028800*    ---  DLI INPUT-OUTPUT AREA                                           
028900 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-BENA01'.           
029000     SKIP3                                                                
029100 01  DLI-IO-AREA-BENA01.                                                  
029200*    03  -COPY WDD301                                                     
029300     EJECT                                                                
029400 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-BENA11'.           
029500     SKIP3                                                                
029600 01  DLI-IO-AREA-BENA11.                                                  
029700*    03  -COPY WDD311                                                     
029800     EJECT                                                                
029900 LINKAGE SECTION.                                                         
030000                                                                          
030100     EJECT                                                                
030200*01  -COPY W0008  -PRE BENA-                                              
030300     05  FILLER                  PIC X.                                   
030400     EJECT                                                                
030500 PROCEDURE DIVISION  USING BENA-PCB.                                      
030600     ENTRY 'DLITCBL' USING BENA-PCB.                                      
030700                                                                          
030800     PERFORM A-INIT                                                       
030900                                                                          
031000     PERFORM S01-LAES-W271IN                                              
031100                                                                          
031200     PERFORM UNTIL END-OF-W271IN                                          
031300                                                                          
031400       MOVE IN-AREA          TO UT-AREA                                   
031500       MOVE IN-IDARTNR       TO W-IDARTNR                                 
031600       PERFORM IMS-GU-WDD3-BENA01-BSEQ                                    
031700       IF SEGMENT-FINNS                                                   
031800         MOVE 'USA'          TO W-IDSKYLT                                 
031900         PERFORM IMS-GNP-WDD3-BENA11                                      
032000         IF SEGMENT-FINNS                                                 
032100           MOVE TEXT-BEART   TO UT-BEART                                  
032200         ELSE                                                             
032300           MOVE SPACE        TO UT-BEART                                  
032400         END-IF                                                           
032500       END-IF                                                             
032600                                                                          
032700       PERFORM S02-SKRIV-UTFIL                                            
032800                                                                          
032900       PERFORM S01-LAES-W271IN                                            
033000                                                                          
033100     END-PERFORM                                                          
033200                                                                          
033300     PERFORM Z-FINIT                                                      
033400                                                                          
033500     MOVE ZERO TO RETURN-CODE                                             
033600     GOBACK                                                               
033700     .                                                                    
033800     EJECT                                                                
033900 A-INIT SECTION.                                                          
034000                                                                          
034100     OPEN INPUT  W271IN                                                   
034200     OPEN OUTPUT W271UT                                                   
034300                                                                          
034400     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE                        
034500     .                                                                    
034600     EJECT                                                                
034700 Z-FINIT SECTION.                                                         
034800                                                                          
034900     CLOSE W271IN                                                         
035000           W271UT                                                         
035100                                                                          
035200     MOVE 'S' TO POSTSUM-OPKOD                                            
035300     CALL POSTSUM USING POSTSUM-PARM                                      
035400     .                                                                    
035500     EJECT                                                                
035600 S01-LAES-W271IN SECTION.                                                 
035700     SKIP2                                                                
035800     READ W271IN             INTO IN-AREA                                 
035900     AT END                                                               
036000        MOVE JA TO W271IN-EOF-SW                                          
036100                                                                          
036200     END-READ                                                             
036300     .                                                                    
036400     EJECT                                                                
036500 S02-SKRIV-UTFIL  SECTION.                                                
036600                                                                          
036700     WRITE UT-POST     FROM UT-AREA                                       
036800                                                                          
036900     MOVE 'W27138'   TO POSTSUM-FDNAMN                                    
037000     MOVE 'W27138D1' TO POSTSUM-DDNAMN2                                   
037100     CALL POSTSUM USING POSTSUM-PARM                                      
037200     .                                                                    
037300     EJECT                                                                
037400* --- IMS SEKTIONER ---                                                   
037500     SKIP3                                                                
037600 IMS-GU-WDD3-BENA01-BSEQ SECTION.                                         
037700                                                                          
037800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
037900          DELIMITED BY SIZE INTO SSA1                                     
038000     MOVE '  GE' TO GODK-STATUSKODER                                      
038100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA01 SSA1               
038200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
038300     PERFORM IMS-STATUSKONTROLL                                           
038400     .                                                                    
038500     SKIP3                                                                
038600 IMS-GNP-WDD3-BENA11 SECTION.                                             
038700                                                                          
038800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
038900          DELIMITED BY SIZE INTO SSA1                                     
039000     MOVE '  GE' TO GODK-STATUSKODER                                      
039100     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA-BENA11 SSA1              
039200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
039300     PERFORM IMS-STATUSKONTROLL                                           
039400     .                                                                    
039500     EJECT                                                                
039600 IMS-STATUSKONTROLL SECTION.                                              
039700                                                                          
039800     SET STATUS-IX TO 1                                                   
039900     SEARCH GODK-STATUS                                                   
040000       AT END                                                             
040100         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
040200           DELIMITED BY SIZE INTO FELTEXT-STR                             
040300         DISPLAY FELTEXT                                                  
040400         CALL FELLOG                                                      
040500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
040600         CONTINUE                                                         
040700     END-SEARCH                                                           
040800     .                                                                    
