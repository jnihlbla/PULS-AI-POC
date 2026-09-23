000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2713A00.                                                
000400*AUTHOR.         STEFAN ANDREASSON.                                       
000500*DATE-WRITTEN.   FEB 1998.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000810*    FUNKTION:                                                            
000820*        PROGRAMMET LÄSER EN FIL MED LARM OM ONORMAL                      
000830*        PROGNOSFÖRÄNDRING OCH DÄR SEDAN W2713B00 SKAPAR                  
000831*        ETT MEMO                                                         
000840*                                                                         
001200*                                                                         
001240*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
001800*              -  "ÖVERSÄTTNING" AV LAGER TILL DC-INDX SAKNAS             
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002610 FILE-CONTROL.                                                            
002620     SKIP2                                                                
002650                                                                          
002680     SELECT W271IN                     ASSIGN TO W2713AD1.                
002681                                                                          
002682     SELECT W271UT                     ASSIGN TO W2713AD2.                
002690     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003501                                                                          
003502 FD  W271IN                                                               
003503     RECORDING       F                                                    
003504     BLOCK CONTAINS  0.                                                   
003505                                                                          
003506*01  POST  -COPY W2713A    -PRE IN-    -L.                                
003507                                                                          
003508     SKIP3                                                                
003509                                                                          
003510 FD  W271UT                                                               
003511     RECORDING       F                                                    
003512     BLOCK CONTAINS  0.                                                   
003513                                                                          
003514*01  POST  -COPY W2713A    -PRE UT-    -L.                                
003515                                                                          
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W2713A00'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  AKTIV                       PIC X       VALUE 'A'.                   
004900                                                                          
005000*    --- INDEX SAMT MAX-INDEX                                             
005100 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
005200 77  INDX                        PIC 9(2)    VALUE ZERO.                  
005210 77  IX                          PIC 9(3)    VALUE ZERO.                  
005300 77  DC-INDX                     PIC 9(2)    VALUE ZERO.                  
005400 77  MAX-FSGFAKT                 PIC 9(2)    VALUE 10.                    
005500                                                                          
005600 77  VECKO-INDX                  PIC 9(2)    VALUE ZERO.                  
005700                                                                          
005800 01  TAB-RADIX                   PIC 9(2)    VALUE ZERO.                  
005900 77  MAX-TAB-RADIX               PIC 9(2)    VALUE 12.                    
006000                                                                          
006100 77  VV-INDX                     PIC 9(2)    VALUE ZERO.                  
006200 77  MAX-VV-INDX                 PIC 9(2)    VALUE 53.                    
006300                                                                          
006400*    --- SWITCHAR                                                         
006500 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
006600                                                                          
006700 01  TREND-SW                    PIC X(5)    VALUE SPACE.                 
006800     88  INGEN-TREND                         VALUE 'INGEN'.               
006900     88  SVAG-TREND                          VALUE 'SVAG '.               
007000     88  STARK-TREND                         VALUE 'STARK'.               
007100                                                                          
007200 01  INDX-SW                     PIC X       VALUE 'N'.                   
007300     88  INDX-HITTAT                         VALUE 'J'.                   
007400                                                                          
007500 01  ARTIKEL-SW                  PIC X       VALUE 'N'.                   
007600     88  ARTIKEL-SKALL-FORAENDRAS            VALUE 'J'.                   
007700                                                                          
007800 01  MANUELL-PROGNOS-SW          PIC X       VALUE 'N'.                   
007900     88  MANUELL-PROGNOS-SATT                VALUE 'J'.                   
008000                                                                          
008100*    --- ARBETSFÄLT                                                       
008200 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
008300 01  ARBETSFAELT.                                                         
008400     03  PERIODTABELL            OCCURS 12.                               
008500         05 TABELL-TIAARP        PIC  9(4)    VALUE ZERO.                 
008600         05 TABELL-FORSTA-TIAAVV PIC  9(2)    VALUE ZERO.                 
008700         05 TABELL-SISTA-TIAAVV  PIC  9(2)    VALUE ZERO.                 
008800         05 TABELL-KVOI        PIC S9(9)V9(1)  VALUE ZERO COMP-3.         
008900                                                                          
009000     03  SLUT-VV                 PIC 9(2)    VALUE ZERO.                  
009100     03  START-VV                PIC 9(2)    VALUE ZERO.                  
009101                                                                          
009112     03  WS-IDARTNR              PIC 9(9)    VALUE ZERO.                  
009121                                                                          
009130     03  WS-CURRENT-DATE.                                                 
009140         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
009150         05  FILLER              PIC 9(4)   VALUE ZERO.                   
009160         05  FILLER              PIC 9(6)   VALUE ZERO.                   
009170                                                                          
009180     03  FILLER REDEFINES WS-CURRENT-DATE.                                
009190*-----   INKLUSIVE SEKEL                                                  
009191         05  WS-DAGENS-DATUM     PIC 9(8).                                
009192         05  WS-DAGENS-TID.                                               
009193             07 WS-DAGENS-TIMME  PIC 9(2).                                
009194             07 WS-DAGENS-MINUT  PIC 9(2).                                
009195             07 WS-DAGENS-SEKUND PIC 9(2).                                
009200                                                                          
009300     03  WS-TIAAVV               PIC 9(4)    VALUE ZERO.                  
009400     03  FILLER REDEFINES WS-TIAAVV.                                      
009500         05 WS-TIAA              PIC 9(2).                                
009600         05 WS-TIVV              PIC 9(2).                                
009700                                                                          
009800     03  FOREG-TIAARP            PIC  9(4)   VALUE ZERO.                  
009900     03  FILLER REDEFINES FOREG-TIAARP.                                   
010000         05 FOREG-TIAA           PIC  9(2).                               
010100         05 FOREG-TIRP           PIC  9(2).                               
010200                                                                          
010300     03  DAGENS-TIAARP           PIC  9(4)   VALUE ZERO.                  
010400     03  FILLER REDEFINES DAGENS-TIAARP.                                  
010500         05 DAGENS-TIAA          PIC  9(2).                               
010600         05 DAGENS-TIRP          PIC  9(2).                               
010700                                                                          
010800     03  NAESTA-TIAARP           PIC  9(4)   VALUE ZERO.                  
010900     03  FILLER REDEFINES NAESTA-TIAARP.                                  
011000         05 NAESTA-TIAA          PIC  9(2).                               
011100         05 NAESTA-TIRP          PIC  9(2).                               
011200                                                                          
011201     03  SEASON-TIAARP           PIC  9(4)   VALUE ZERO.                  
011202     03  FILLER REDEFINES SEASON-TIAARP.                                  
011203         05 SEASON-TIAA          PIC  9(2).                               
011204         05 SEASON-TIRP          PIC  9(2).                               
011205                                                                          
011210     03  DAGENS-TIAAVVD          PIC  9(5)   VALUE ZERO.                  
011220     03  FILLER REDEFINES DAGENS-TIAAVVD.                                 
011230         05 DAGENS-TIAAVVD-AA    PIC  9(2).                               
011231         05 DAGENS-TIAAVVD-VV    PIC  9(2).                               
011232         05 DAGENS-TIAAVVD-D     PIC  9(1).                               
011250                                                                          
011251     03  DAGENS-TIAAVV-GRP       PIC  9(4)   VALUE ZERO.                  
011252                                                                          
011253                                                                          
011260     03  DAGENS-TIAAVVD-LAST-YEAR        PIC  9(5) VALUE ZERO.            
011270     03  FILLER REDEFINES DAGENS-TIAAVVD-LAST-YEAR.                       
011280         05 DAGENS-TIAAVVD-LAST-YEAR-AA  PIC 9(2).                        
011290         05 DAGENS-TIAAVVD-LAST-YEAR-VV  PIC 9(2).                        
011291         05 DAGENS-TIAAVVD-LAST-YEAR-D   PIC 9(1).                        
011292                                                                          
011300     03  DAGENS-TIVV             PIC  9(2)   VALUE ZERO.                  
011400                                                                          
011500     03  WS-ANTAL-VECKOR         PIC  9(2)      VALUE ZERO.               
011502     03  WS-VECKO-IO             PIC S9(9)V9 VALUE ZERO COMP-3.           
011510     03  WS-TEST-TIREFMPB        PIC S9(7)   VALUE ZERO COMP-3.           
011520     03  WS-TIREFMPB-TIAARP      PIC  9(4)      VALUE ZERO.               
011600     03  WS-FORSTA-TIAAVV        PIC  9(4)      VALUE ZERO.               
011700     03  WS-SISTA-TIAAVV         PIC  9(4)      VALUE ZERO.               
011800     03  WS-DAT-TIAAVV           PIC  9(4)      VALUE ZERO.               
011900     03  WS-ONORM-OI-GRAENS-PB   PIC S9(9)V9(1) VALUE ZERO COMP-3.        
012000     03  WS-KVOI-SEASON          PIC S9(9)V9(1) VALUE ZERO COMP-3.        
012010     03  WS-KVOI-TOT             PIC S9(11)     VALUE ZERO COMP-3.        
012100     03  WS-NY-KVPB-REF          PIC S9(6)V9(2) VALUE ZERO COMP-3.        
012200     03  NY-KVPB-REF             PIC S9(6)V9(1) VALUE ZERO COMP-3.        
012300     03  WS-PREL-KVPB-REF        PIC S9(6)V9(2) VALUE ZERO COMP-3.        
012400     03  WS-MEDEL-KVPB-REF       PIC S9(6)V9(2) VALUE ZERO COMP-3.        
012500     03  WS-ANTAL-FAKTORER-STOERRE-NOLL PIC S9(7)          COMP-3.        
012600     03  WS-KVOTEN               PIC S9(5)V9(2) VALUE ZERO COMP-3.        
012610     03  WS-TIFINLV              PIC S9(5)V     VALUE ZERO COMP-3.        
012611     03  WS-DARODAT              PIC  9(8)      VALUE ZERO.               
012612     03  WS-KVOKS-TOT            PIC S9(7)   VALUE ZERO COMP-3.           
012660     03  WS-ANTAL-POSTER         PIC  9(7)      VALUE ZERO.               
012670     03  WS-ANTAL-DC11           PIC  9(7)      VALUE ZERO.               
012680     03  WS-ANTAL-DC2X           PIC  9(7)      VALUE ZERO.               
012690     03  WS-ANTAL-DC5X           PIC  9(7)      VALUE ZERO.               
012691     03  WS-ANTAL-RAD1           PIC  9(7)      VALUE ZERO.               
012692     03  WS-ANTAL-RAD2           PIC  9(7)      VALUE ZERO.               
012693     03  WS-ANTAL-RAD3           PIC  9(7)      VALUE ZERO.               
012694     03  WS-ANTAL-RAD4           PIC  9(7)      VALUE ZERO.               
012695     03  WS-ANTAL-RAD5           PIC  9(7)      VALUE ZERO.               
012700                                                                          
012800     EJECT                                                                
012810                                                                          
012820 77  W271IN-EOF-SW               PIC X       VALUE 'N'.                   
012830     88  END-OF-W271IN                       VALUE 'J'.                   
012900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013000 01  FILLER REDEFINES DAGENS-DATUM.                                       
013100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
013200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
013300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
013400     EJECT                                                                
013530                                                                          
013610 01  DYNAMISKA-SUBPROGRAM.                                                
013700*                                                                         
013800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
014300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014500     SKIP2                                                                
014600*    --- PARAMETRAR TILL ABEND                                            
014700                                                                          
014800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
014900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
015000     SKIP2                                                                
015100 01  FELTEXT.                                                             
015200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
015300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
015400     EJECT                                                                
015500*    --- PARAMETRAR TILL DATKORT                                          
015600*                                                                         
015700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W2713A'.              
015800     SKIP2                                                                
015900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
016000     SKIP2                                                                
016100*01  -COPY WDATKORT                                                       
016200     EJECT                                                                
016300*    --- PARAMETRAR TILL POSTSUM                                          
016400*                                                                         
016500*01  -COPY W0005   -PRE  POSTSUM-                                         
016600     EJECT                                                                
016700*    --- PARAMETRAR TILL WDATKONV                                         
016800*                                                                         
016900*01  -COPY WDATAREA                                                       
017000     EJECT                                                                
017900 01  IN-AREA-START              PIC X(24)   VALUE                         
018000                                 'IN-AREA-START  '.                       
018100     SKIP2                                                                
018200                                                                          
018311*01  AREA -COPY W2713A     -PRE IN-                                       
018312     EJECT                                                                
018313 01  UT-AREA-START              PIC X(24)   VALUE                         
018314                                 'UT-AREA-START  '.                       
018315     SKIP2                                                                
018316                                                                          
018317*01  AREA -COPY W2713A     -PRE UT-                                       
018400     EJECT                                                                
018500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018600                                                                          
018700     SKIP3                                                                
018800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018900     SKIP3                                                                
019000 01  NYCKLAR-TILL-DLI.                                                    
019100     03  W-IDARTNR-X.                                                     
019200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
019300                                                                          
019400     03  W-IDDC-X.                                                        
019500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
019600                                                                          
019700     03  W-KDSEGKEY-X.                                                    
019800         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
019801                                                                          
019810     03  W-IDSKYLT-X.                                                     
019820         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
019900                                                                          
020000*                                                                         
020100     SKIP2                                                                
020200*    --- STATUS-KOD FRÅN IMS                                              
020300 01  STATUS-WS                   PIC XX.                                  
020400     88  SEGMENT-FINNS                       VALUE '  '.                  
020500     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
020600     SKIP2                                                                
020700 01  GODK-STATUSKODER.                                                    
020800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020900     SKIP3                                                                
021000 01  SSA1                        PIC X(64).                               
021100 01  SSA2                        PIC X(64).                               
021200     EJECT                                                                
021300*    --- IMS FUNKTIONSKODER                                               
021400*01  -COPY W0003                                                          
021500     EJECT                                                                
021600*    ---  DLI INPUT-OUTPUT AREA                                           
024920 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-BENA01'.           
024930     SKIP3                                                                
024940 01  DLI-IO-AREA-BENA01.                                                  
024950*    03  -COPY WDD301                                                     
024960     EJECT                                                                
024970 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-BENA11'.           
024980     SKIP3                                                                
024990 01  DLI-IO-AREA-BENA11.                                                  
024991*    03  -COPY WDD311                                                     
024997     EJECT                                                                
025010 LINKAGE SECTION.                                                         
025100                                                                          
025200     EJECT                                                                
025900*01  -COPY W0008  -PRE BENA-                                              
026000     05  FILLER                  PIC X.                                   
026100     EJECT                                                                
026200 PROCEDURE DIVISION  USING BENA-PCB.                                      
026300     ENTRY 'DLITCBL' USING BENA-PCB.                                      
026310                                                                          
026320     PERFORM A-INIT                                                       
026330                                                                          
026340     PERFORM S01-LAES-W271IN                                              
026350                                                                          
026360     PERFORM UNTIL END-OF-W271IN                                          
026370                                                                          
026371       MOVE IN-AREA          TO UT-AREA                                   
026380       MOVE IN-IDARTNR       TO W-IDARTNR                                 
026390       PERFORM IMS-GU-WDD3-BENA01-BSEQ                                    
026400       IF SEGMENT-FINNS                                                   
026401         MOVE 'USA'          TO W-IDSKYLT                                 
026402         PERFORM IMS-GNP-WDD3-BENA11                                      
026403         IF SEGMENT-FINNS                                                 
026404           MOVE TEXT-BEART   TO UT-BEART                                  
026405         ELSE                                                             
026406           MOVE SPACE        TO UT-BEART                                  
026407         END-IF                                                           
026408       END-IF                                                             
026409                                                                          
026410       PERFORM S02-SKRIV-UTFIL                                            
026411                                                                          
026412       PERFORM S01-LAES-W271IN                                            
026413                                                                          
026414     END-PERFORM                                                          
026415                                                                          
026416     PERFORM Z-FINIT                                                      
026417                                                                          
026418     MOVE ZERO TO RETURN-CODE                                             
026419     GOBACK                                                               
026420     .                                                                    
026421     EJECT                                                                
028600 A-INIT SECTION.                                                          
028700                                                                          
028810     OPEN INPUT  W271IN                                                   
028811     OPEN OUTPUT W271UT                                                   
028812                                                                          
028820     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE                        
031500     .                                                                    
031600     EJECT                                                                
061300 Z-FINIT SECTION.                                                         
061400                                                                          
061510     CLOSE W271IN                                                         
061520           W271UT                                                         
061600                                                                          
061700     MOVE 'S' TO POSTSUM-OPKOD                                            
061800     CALL POSTSUM USING POSTSUM-PARM                                      
061900     .                                                                    
061910     EJECT                                                                
061920 S01-LAES-W271IN SECTION.                                                 
061930     SKIP2                                                                
061940     READ W271IN             INTO IN-AREA                                 
061950     AT END                                                               
061960        MOVE JA TO W271IN-EOF-SW                                          
061970                                                                          
061980     END-READ                                                             
061990     .                                                                    
062000     EJECT                                                                
092800 S02-SKRIV-UTFIL  SECTION.                                                
092900                                                                          
093000     WRITE UT-POST     FROM UT-AREA                                       
093100                                                                          
093200     MOVE 'W2713A'   TO POSTSUM-FDNAMN                                    
093300     MOVE 'W2713AD1' TO POSTSUM-DDNAMN2                                   
093400     CALL POSTSUM USING POSTSUM-PARM                                      
093500     .                                                                    
093600     EJECT                                                                
094400* --- IMS SEKTIONER ---                                                   
094500     SKIP3                                                                
095906 IMS-GU-WDD3-BENA01-BSEQ SECTION.                                         
095907                                                                          
095908     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
095909          DELIMITED BY SIZE INTO SSA1                                     
095910     MOVE '  GE' TO GODK-STATUSKODER                                      
095911     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA01 SSA1               
095912     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
095913     PERFORM IMS-STATUSKONTROLL                                           
095914     .                                                                    
095915     SKIP3                                                                
095916 IMS-GNP-WDD3-BENA11 SECTION.                                             
095917                                                                          
095918     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
095919          DELIMITED BY SIZE INTO SSA1                                     
095920     MOVE '  GE' TO GODK-STATUSKODER                                      
095921     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA-BENA11 SSA1              
095922     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
095923     PERFORM IMS-STATUSKONTROLL                                           
095924     .                                                                    
095925     EJECT                                                                
098000 IMS-STATUSKONTROLL SECTION.                                              
098100                                                                          
098200     SET STATUS-IX TO 1                                                   
098300     SEARCH GODK-STATUS                                                   
098400       AT END                                                             
098500         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
098600           DELIMITED BY SIZE INTO FELTEXT-STR                             
098700         DISPLAY FELTEXT                                                  
098800         CALL FELLOG                                                      
098900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
099000         CONTINUE                                                         
099100     END-SEARCH                                                           
099200     .                                                                    
