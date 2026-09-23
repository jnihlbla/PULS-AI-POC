000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6116000.                                                
000400*AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000500*DATE-WRITTEN.   MAJ 2004.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000810*    FUNKTION:                                                            
000820*        FIXPROGRAM FÖR ATT LÄSA WDK611 OCH SKRIVA PÅ EN FIL              
001200*                                                                         
001300*        PROGRAMMET LÄSER      WDK6   MHA SB                              
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
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
002630     SELECT W611UT                     ASSIGN TO W61160D1.                
002690     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003501 FD  W611UT                                                               
003502     RECORDING       F                                                    
003503     BLOCK CONTAINS  0.                                                   
003504 01  UT-POST                 PIC X(80).                                   
003505                                                                          
003506                                                                          
004100                                                                          
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W6116000'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  AKTIV                       PIC X       VALUE 'A'.                   
004900                                                                          
005000*    --- INDEX SAMT MAX-INDEX                                             
005100 77  FILLER                      PIC X(16)   VALUE 'INDEX'.               
005200 77  INDX                        PIC 9(2)    VALUE ZERO.                  
005210 77  IX                          PIC 9(3)    VALUE ZERO.                  
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
007910                                                                          
007920 01  BEHANDLA-SW                 PIC X       VALUE 'N'.                   
007930     88  BEHANDLA                            VALUE 'J'.                   
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
009121     03  WS-IDLEVNR              PIC X(5)    VALUE SPACE.                 
009122     03  WS-ART-KDERS-UTG        PIC 9(3)    VALUE ZERO.                  
009123     03  WS-CLAG-KDERS           PIC 9(3)    VALUE ZERO.                  
009124     03  WS-CLAG-IDANSK          PIC 9(3)    VALUE ZERO.                  
009126     03  WS-ANTAL-TRAEFF         PIC 9(9)   VALUE ZERO.                   
009127     03  WS-ANTAL-TRAEFF-KDFARLIG PIC 9(9)   VALUE ZERO.                  
009129     03  WS-ANTAL-WDK601         PIC 9(9)   VALUE ZERO.                   
009130     03  WS-ANTAL-WDK611         PIC 9(9)   VALUE ZERO.                   
009131     03  WS-ANTAL-WDK626         PIC 9(9)   VALUE ZERO.                   
009132     03  WS-ANTAL-EMB            PIC 9(9)   VALUE ZERO.                   
009133     03  WS-ANTAL-JA             PIC 9(9)   VALUE ZERO.                   
009134     03  WS-ANTAL-NEJ            PIC 9(9)   VALUE ZERO.                   
009135     03  WS-ANTAL-BAADA          PIC 9(9)   VALUE ZERO.                   
009136     03  WS-ANTAL-PBPLAN         PIC 9(9)   VALUE ZERO.                   
009137     03  WS-ANTAL-SEASON         PIC 9(9)   VALUE ZERO.                   
009138     03  WS-ANTAL-UT             PIC 9(9)   VALUE ZERO.                   
009139     03  WS-ANTAL-CD             PIC 9(9)   VALUE ZERO.                   
009140     03  WS-IDFKNGRP             PIC 9(5)   VALUE ZERO.                   
009141     03  WS-SPARA-LASN           PIC X(6)   VALUE SPACE.                  
009142     03  WS-KVVECKOR-LT          PIC 9(4)   VALUE ZERO.                   
009143     03  WS-TIREGDAT             PIC 9(6)   VALUE ZERO.                   
009144                                                                          
009145     03  WS-CURRENT-DATE.                                                 
009146         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
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
012620     03  WS-TIPBDAT              PIC  9(5)      VALUE ZERO.               
012700                                                                          
012800     EJECT                                                                
012900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013000 01  FILLER REDEFINES DAGENS-DATUM.                                       
013100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
013200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
013300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
013400     EJECT                                                                
013602                                                                          
013603 01  UT-AREA.                                                             
013604     03  UT-IDARTNR           PIC 9(8)       VALUE ZERO.                  
013616     03  FILLER               PIC X(72)      VALUE SPACE.                 
013617                                                                          
013620 01  DYNAMISKA-SUBPROGRAM.                                                
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
015310     SKIP2                                                                
015320 01  FELTEXT.                                                             
015330     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
015340     03  FELTEXT-STATUS          PIC X(2)    VALUE SPACE.                 
015350     03  FILLER                  PIC X       VALUE SPACE.                 
015360     03  FELTEXT-TEXT            PIC X(69)   VALUE SPACE.                 
015400     EJECT                                                                
015500*    --- PARAMETRAR TILL DATKORT                                          
015600*                                                                         
015700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61160'.              
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
016910     EJECT                                                                
018500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018600                                                                          
018700     SKIP3                                                                
018800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018900     SKIP3                                                                
019000 01  NYCKLAR-TILL-DLI.                                                    
019100     03  W-IDARTNR-X.                                                     
019200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
019300                                                                          
019700     03  W-KDSEGKEY-X.                                                    
019800         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
019801                                                                          
019810     03  W-IDSKYLT-X.                                                     
019820         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
019830                                                                          
019840     03  W-KDNOTTYP-X.                                                    
019850         05  W-KDNOTTYP          PIC  S9(01)  COMP-3 VALUE ZERO.          
019860                                                                          
019900                                                                          
020000*                                                                         
020100     SKIP2                                                                
020510*    --- STATUS-KOD FRÅN IMS                                              
020520 01  STATUS-WS                   PIC XX.                                  
020530     88  SEGMENT-FINNS                       VALUE '  '.                  
020540     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020550     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
020560                                                   'GB'.                  
020570     88  IMS-EJ-OK                           VALUE 'XD'.                  
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
021710 01  FILLER                    PIC X(16)  VALUE 'DLI-WDK601'.             
021800     SKIP3                                                                
021900 01  DLI-IO-AREA-K6.                                                      
022000     03  IO-AREA-K6              PIC X(900)  VALUE SPACE.                 
022100     SKIP3                                                                
022200     03  WLARTS01 REDEFINES IO-AREA-K6.                                   
022300*        05  -COPY WDK601                                                 
022400     SKIP3                                                                
022500     03  WLARTS11 REDEFINES IO-AREA-K6.                                   
022600*        05  -COPY WDK611                                                 
022700     SKIP3                                                                
022800     03  WLARTS26 REDEFINES IO-AREA-K6.                                   
022900*        05  -COPY WDK626                                                 
025050     EJECT                                                                
025060 LINKAGE SECTION.                                                         
025100                                                                          
025110*01  -COPY W0009   -PRE MSG-                                              
025200     EJECT                                                                
025300*01  -COPY W0008  -PRE WDK6-                                              
025400     05  WDK6-KEY-FB-AREA-IDARTNR       PIC S9(9) COMP-3.                 
026120     EJECT                                                                
026200 PROCEDURE DIVISION  USING WDK6-PCB.                                      
026300     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
026350                                                                          
026500     PERFORM A-INIT                                                       
026600     PERFORM IMS-GN-WDK6                                                  
026700     PERFORM UNTIL SEGMENT-SAKNAS                                         
026800       EVALUATE WDK6-SEG-NAME-FB                                          
026900         WHEN 'WDK601  '                                                  
026910           MOVE 'WDK601'     TO WS-SPARA-LASN                             
026920           ADD 1             TO WS-ANTAL-WDK601                           
027000           MOVE WDK6-KEY-FB-AREA-IDARTNR                                  
027100                             TO W-IDARTNR                                 
027101           MOVE ART-KDERS-UTG                                             
027102                             TO WS-ART-KDERS-UTG                          
027120         WHEN 'WDK611  '                                                  
027121           MOVE 'WDK611'     TO WS-SPARA-LASN                             
027122            ADD 1            TO WS-ANTAL-WDK611                           
027219            IF  CLAG-FLCDART             = JA                             
027220              ADD 1          TO WS-ANTAL-CD                               
027221              IF CLAG-KVQPACK-3 NOT = CLAG-KVPALL                         
027222                ADD 1        TO WS-ANTAL-TRAEFF                           
027223              MOVE W-IDARTNR TO UT-IDARTNR                                
027226              PERFORM S01-SKRIV-W611UT                                    
027227              END-IF                                                      
027228            END-IF                                                        
027345           MOVE 'WDK626'     TO WS-SPARA-LASN                             
027350            ADD 1            TO WS-ANTAL-WDK626                           
027400*                                                                         
027600       END-EVALUATE                                                       
027700       PERFORM IMS-GN-WDK6                                                
027800     END-PERFORM                                                          
027900                                                                          
028000     PERFORM Z-FINIT                                                      
028100                                                                          
028200     MOVE ZERO TO RETURN-CODE                                             
028300     GOBACK                                                               
028400     .                                                                    
028500     EJECT                                                                
028600 A-INIT SECTION.                                                          
028700                                                                          
028811     OPEN OUTPUT W611UT                                                   
028812                                                                          
028820     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE                        
031500     .                                                                    
031600     EJECT                                                                
043920                                                                          
061300 Z-FINIT SECTION.                                                         
061400                                                                          
061410     CLOSE W611UT                                                         
061500     DISPLAY 'ANTAL TRÄFF  : ' WS-ANTAL-TRAEFF                            
061501     DISPLAY 'ANTAL CD     : ' WS-ANTAL-CD                                
061502     DISPLAY 'ANT KDFARLIG : ' WS-ANTAL-TRAEFF-KDFARLIG                   
061510     DISPLAY 'ANTAL JA     : ' WS-ANTAL-JA                                
061520     DISPLAY 'ANTAL NEJ    : ' WS-ANTAL-NEJ                               
061600     DISPLAY 'ANTAL WDK601 : ' WS-ANTAL-WDK601                            
061700     DISPLAY 'ANTAL WDK611 : ' WS-ANTAL-WDK611                            
061710     DISPLAY 'ANTAL WDK626 : ' WS-ANTAL-WDK626                            
061800     DISPLAY 'ANTAL EMB    : ' WS-ANTAL-EMB                               
061801     DISPLAY 'ANTAL BAADA  : ' WS-ANTAL-BAADA                             
061802     DISPLAY 'ANTAL PBPLAN : ' WS-ANTAL-PBPLAN                            
061803     DISPLAY 'ANTAL SEASON : ' WS-ANTAL-SEASON                            
061804     DISPLAY 'WS-IDARTNR   : ' WS-IDARTNR                                 
061805     DISPLAY 'WS-KVVECKOR-LT : ' WS-KVVECKOR-LT                           
061900     .                                                                    
062000     EJECT                                                                
062100 S01-SKRIV-W611UT SECTION.                                                
062200                                                                          
062300     WRITE UT-POST FROM UT-AREA                                           
062800     .                                                                    
094390     EJECT                                                                
094400* --- IMS SEKTIONER ---                                                   
094500     SKIP3                                                                
095900 IMS-GN-WDK6 SECTION.                                                     
096000                                                                          
096100     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-AREA-K6                        
096200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
096300     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
096400     PERFORM IMS-STATUSKONTROLL                                           
096500     .                                                                    
096600     EJECT                                                                
098090 IMS-STATUSKONTROLL SECTION.                                              
098100                                                                          
098200     SET STATUS-IX TO 1                                                   
098300     SEARCH GODK-STATUS                                                   
098400       AT END                                                             
098500         MOVE 'OTILLÅTEN RETURKOD FRÅN IMS: '                             
098510                             TO FELTEXT-TEXT                              
098700         DISPLAY FELTEXT                                                  
098800         CALL FELLOG                                                      
098900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
099000         CONTINUE                                                         
099100     END-SEARCH                                                           
099200     .                                                                    
