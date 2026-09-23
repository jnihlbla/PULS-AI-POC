000100 ID  DIVISION.                                                            
000200                                                                          
000300 PROGRAM-ID.    W4883600.                                                 
000400*AUTHOR.        E RINGQVIST.                                              
000500*DATE-WRITTEN.  MAJ  1984                                                 
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*           PROGRAMMET LÄSER IN EN KORRIGERINGSFILE, W48835.              
001000                                                                          
001100*           SALDOBASEN ÄNDRAS ELLER SKRIVS, BEROENDE PÅ OM                
001200*           ARTIKELN OCH DC      FINNS.                                   
001300*                                                                         
001400*           LAGERPLATSHISTORIKBASEN UPPDATERAS (WDJ9)                     
001410*                                                                         
001500*    UPDATED BY KUMAR LOVISH. MERGED W4883610 IN THIS PROG.               
001600                                                                          
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900                                                                          
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300*-----------------------------------------------INREG KORRTRANSAR         
002400     SELECT W48835      ASSIGN TO UT-S-W48836D1.                          
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700                                                                          
002800 FILE SECTION.                                                            
002900                                                                          
003000 FD  W48835                                                               
003100     RECORDING F                                                          
003200     BLOCK CONTAINS 0.                                                    
003300*01  POST    -COPY W488031     -PRE W48835    -L.                         
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(8)    VALUE 'W4883600'.            
003900 77  JA                          PIC X(1)    VALUE 'J'.                   
004000 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004010 77  W-CHKP-RAKNARE              PIC S9(5)   VALUE +0    COMP-3.          
004020 77  W-CHKP-MAX                  PIC S9(5)   VALUE +800  COMP-3.          
004030 77  CHKP-ID                     PIC X(8)    VALUE 'W4883600'.            
004040 77  MSG-IO-AREA-LENGTH-1        PIC S9(9)   VALUE +32  COMP SYNC.        
004050 77  MSG-IO-AREA-1               PIC X(32)   VALUE SPACE.                 
004060 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
004070 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
004100*    --- ARBETSFÄLT FÖR UPPDATERING AV PLATSREGISTRET (WDJ9)              
004200 77  LOGG-DATUM              PIC S9(8)       VALUE ZERO.                  
004300 77  LOGG-TID                PIC S9(7)       VALUE ZERO.                  
004400 77  BUFFER-LOCATION         PIC X           VALUE 'B'.                   
004500*- - - - - - - - - - - - - -  EOF                                         
004600 77  W48835-EOF                  PIC X       VALUE 'N'.                   
004700*                            *** GENERELLA SUBRUTINER                     
004800 01  SUBPROGRAM.                                                          
004900     03  CBLTDLI             PIC X(8)        VALUE 'CBLTDLI '.            
005000     03  FELLOG              PIC X(8)        VALUE 'FELLOG  '.            
005200     03  POSTSUM             PIC X(8)        VALUE 'POSTSUM '.            
005300                                                                          
005800     EJECT                                                                
005900*   --- PARAMETRAR TILL SUBPROGRAM POSTSUM                                
006100*01    -COPY W0005      -PRE  POSTSUM-                                    
006200     EJECT                                                                
006300*                                                                         
006400 01  NYCKLAR-TILL-DLI.                                                    
006500     03  W-IDARTNR-X.                                                     
006600         05  W-IDARTNR       PIC S9(9)       COMP-3.                      
006700     03  W-IDDC-X.                                                        
006800         05  W-IDDC          PIC X(2)        VALUE ZERO.                  
006900     03  W-WDD811KY-X.                                                    
007000         05  W-IDDC-WDD8     PIC X(2).                                    
007100         05  W-ADBUFFOMR     PIC S9(3)       COMP-3.                      
007200         05  W-DABUFPAF      PIC  9(8).                                   
007300         05  W-ADBUFFGANG    PIC S9(3)       COMP-3.                      
007400         05  W-ADBUFFPL      PIC S9(5)       COMP-3.                      
007500     03  W-WDJ911KY-X.                                                    
007600         05  W-IDDC-WDJ9     PIC X(2)        VALUE ZERO.                  
007700         05  W-DASTADAT      PIC S9(9)       VALUE ZERO.                  
007800         05  W-TISTATID      PIC S9(7)       VALUE ZERO.                  
007900         05  W-ADLAGOMR      PIC 9(2)        VALUE ZERO COMP-3.           
008000         05  W-ADGANG        PIC 9(2)        VALUE ZERO COMP-3.           
008100         05  W-ADPLATS       PIC 9(5)        VALUE ZERO COMP-3.           
008110                                                                          
008200 01  FILLER                   PIC X(16) VALUE 'W48835'.                   
008300*01  AREA  -PRE IN1-   -COPY W488031                                      
008400     EJECT                                                                
008600                                                                          
008700*                            *** ARBETSAREOR TILL IMS                     
008800 01  IMS-WS.                                                              
008900     03  FILLER              PIC X(8)        VALUE 'IMS-WS  '.            
009000                                                                          
009100*                            *** STATUSKOD FRÅN IMS                       
009200     03 STATUS-WS            PIC XX.                                      
009300         88  SEGMENT-FINNS                   VALUE '  '.                  
009400         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
009500                                                                          
009600     03  GODK-STATUSKODER.                                                
009700         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.            
009800                                                                          
009900     03  SSA1                PIC X(96).                                   
010000     03  SSA2                PIC X(96).                                   
010100     EJECT                                                                
010200*01  -COPY W0003                                                          
010300     EJECT                                                                
010400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
010500                                                                          
010600 01  DLI-IO-AREA.                                                         
010700     03  IO-AREA            PIC X(100)  VALUE SPACE.                      
010800     SKIP3                                                                
010900*    03 WDD801 -PRE ARTD- -COPY WDD801  -RED IO-AREA                      
011000     EJECT                                                                
011100*    03 WDD811 -PRE ARTD-  -COPY WDD811  -RED IO-AREA                     
011200     EJECT                                                                
011300                                                                          
011400 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDJ901'.          
011500 01  DLI-IO-WDJ901.                                                       
011600*    03  -COPY WDJ901    -PRE LOCB-                                       
011700     EJECT                                                                
011800 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDJ911'.          
011900 01  DLI-IO-WDJ911.                                                       
012000*    03  -COPY WDJ911    -PRE LOCB-                                       
012100     SKIP3                                                                
012200                                                                          
012300     EJECT                                                                
012400 LINKAGE SECTION.                                                         
012500                                                                          
012600*01 -COPY W0009 -PRE  MSG-.                                               
012601     EJECT                                                                
012610*01 -COPY W0008 -PRE  WDD8-.                                              
012700     05 FILLER          PIC X.                                            
012800                                                                          
012900*    PCB FÖR PLATSREGISTRET                                               
013000*01 -COPY W0008 -PRE  WDJ9-                                               
013100     05 FILLER          PIC X.                                            
013200     EJECT                                                                
013300                                                                          
013400 PROCEDURE DIVISION USING MSG-PCB WDD8-PCB WDJ9-PCB.                      
013500     ENTRY 'DLITCBL' USING MSG-PCB WDD8-PCB WDJ9-PCB.                     
013600                                                                          
013700     PERFORM A-INIT                                                       
013800                                                                          
013900     PERFORM S01-LAES-W48835                                              
014000                                                                          
014100     PERFORM UNTIL                                                        
014200      NOT ( W48835-EOF = NEJ )                                            
014600       PERFORM A-UPPDATERA-WDD8-WDJ9                                      
014700        IF W-CHKP-RAKNARE           >  W-CHKP-MAX                         
014800           PERFORM IMS-CHECKPOINT                                         
014900           MOVE ZERO                TO W-CHKP-RAKNARE                     
015000        END-IF                                                            
015100       PERFORM S01-LAES-W48835                                            
015200     END-PERFORM                                                          
015300     PERFORM Z-FINIT                                                      
015400                                                                          
015500     MOVE ZERO TO RETURN-CODE                                             
015600     GOBACK                                                               
015700     .                                                                    
015800     EJECT                                                                
015900 A-INIT SECTION.                                                          
016000                                                                          
016010     PERFORM IMS-RESTART                                                  
016060                                                                          
016100     OPEN INPUT  W48835                                                   
016101                                                                          
016110     MOVE ZERO  TO W-CHKP-RAKNARE                                         
016200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016300     .                                                                    
016400     EJECT                                                                
019700 A-UPPDATERA-WDD8-WDJ9 SECTION.                                           
019710                                                                          
019720     MOVE IN1-IDARTNR        TO W-IDARTNR                                 
019730     MOVE IN1-IDDC           TO W-IDDC-WDD8                               
019740                                W-IDDC-X                                  
019750     MOVE 1                  TO W-ADBUFFOMR                               
019760     MOVE ZERO               TO W-ADBUFFGANG                              
019770                                W-ADBUFFPL                                
019780     MOVE ZERO               TO W-DABUFPAF                                
019790     PERFORM IMS-GHU-SALDOSEG                                             
019791                                                                          
019792     IF SEGMENT-FINNS                                                     
019793       MOVE 1                TO ARTD-SALDO-ADBUFFOMR                      
019794       MOVE ZERO             TO ARTD-SALDO-ADBUFFGANG                     
019795                                ARTD-SALDO-ADBUFFPL                       
019796                                ARTD-SALDO-DABUFPAF                       
019797       MOVE IN1-KVBUFF-F     TO ARTD-SALDO-KVBUFF-F                       
019798       MOVE IN1-KVBUFF-OF    TO ARTD-SALDO-KVBUFF-OF                      
019799       MOVE IN1-KVKOLLI-F    TO ARTD-SALDO-KVKOLLI-F                      
019800       MOVE IN1-KVKOLLI-OF   TO ARTD-SALDO-KVKOLLI-OF                     
019801       PERFORM IMS-REPL-WDD8                                              
019803                                                                          
019804* --- UPPDATERING AV LAGERPLATSHISTORIKBASEN (WDJ9)                       
019805       PERFORM IMS-GU-WDJ901                                              
019806       IF SEGMENT-SAKNAS                                                  
019807         MOVE W-IDARTNR TO LOCB-ART-IDARTNR                               
019808         PERFORM IMS-ISRT-WDJ901                                          
019809         PERFORM IMS-GU-WDJ901                                            
019810       END-IF                                                             
019811       IF SEGMENT-FINNS                                                   
019812         MOVE FUNCTION CURRENT-DATE(1:8)  TO LOGG-DATUM                   
019813         MOVE FUNCTION CURRENT-DATE(9:6)  TO LOGG-TID                     
019814         COMPUTE LOCB-HIST-DASTADAT-9KOMPL = 99999999 -                   
019815                                                    LOGG-DATUM            
019816         COMPUTE LOCB-HIST-TISTATID-9KOMPL = 999999 - LOGG-TID            
019817         MOVE IN1-IDDC           TO LOCB-HIST-IDDC                        
019818         MOVE 1                  TO LOCB-HIST-ADLAGOMR                    
019819         MOVE ZERO               TO LOCB-HIST-ADGANG                      
019820                                    LOCB-HIST-ADPLATS                     
019821         MOVE BUFFER-LOCATION    TO LOCB-HIST-KDLOC                       
019822         MOVE ' W48836'          TO LOCB-HIST-IDUSER                      
019823         MOVE SPACE              TO LOCB-HIST-IDUSER-STO                  
019824         MOVE ZERO               TO LOCB-HIST-DASTODAT                    
019825                                                                          
019826         PERFORM IMS-ISRT-WDJ911                                          
019827       END-IF                                                             
019830     END-IF                                                               
019831     .                                                                    
019832     EJECT                                                                
019833 S01-LAES-W48835 SECTION.                                                 
019834                                                                          
019835     READ W48835 INTO IN1-W488031                                         
019836     AT END                                                               
019837     MOVE JA TO W48835-EOF                                                
019838     END-READ                                                             
019839                                                                          
019840     IF W48835-EOF = NEJ                                                  
019841       MOVE 'W48835'   TO POSTSUM-FDNAMN                                  
019842       MOVE 'W48836D1' TO POSTSUM-DDNAMN2                                 
019843       MOVE IN1-IDPTYP TO POSTSUM-TRANSTYP                                
019844       CALL POSTSUM USING POSTSUM-PARM                                    
019845     END-IF                                                               
019846     .                                                                    
019847     EJECT                                                                
019850 Z-FINIT SECTION.                                                         
019900                                                                          
020000     CLOSE W48835                                                         
020100                                                                          
020200     MOVE 'S'      TO POSTSUM-OPKOD                                       
020300     CALL POSTSUM USING POSTSUM-PARM                                      
020400     .                                                                    
020410     EJECT                                                                
020500*****  IMS SECTIONER   ****                                               
020610 IMS-RESTART  SECTION.                                                    
020620                                                                          
020630     MOVE SPACE TO MSG-IO-AREA-1                                          
020640     MOVE '  ' TO GODK-STATUSKODER                                        
020650     CALL CBLTDLI USING XRST MSG-PCB                                      
020660                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
020670                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
020680     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020690     PERFORM IMS-STATUSKONTROLL                                           
020691     .                                                                    
020693     SKIP3                                                                
020694 IMS-CHECKPOINT  SECTION.                                                 
020695                                                                          
020696     MOVE CHKP-ID TO MSG-IO-AREA-1                                        
020697     MOVE '  XD' TO GODK-STATUSKODER                                      
020698     CALL CBLTDLI USING CHKP MSG-PCB                                      
020699                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
020700                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
020701     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020702     PERFORM IMS-STATUSKONTROLL                                           
020703     .                                                                    
020704     EJECT                                                                
020710 IMS-GHU-SALDOSEG    SECTION.                                             
020900     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
021000           DELIMITED BY SIZE INTO SSA1                                    
021100     STRING 'WDD811  (WDD811KY =' W-WDD811KY-X ')'                        
021200           DELIMITED BY SIZE INTO SSA2                                    
021300     MOVE '  GE'   TO GODK-STATUSKODER                                    
021400     CALL CBLTDLI USING GHU WDD8-PCB IO-AREA SSA1 SSA2                    
021500     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
021600     PERFORM IMS-STATUSKONTROLL                                           
021700     .                                                                    
021800     SKIP3                                                                
021900 IMS-REPL-WDD8   SECTION.                                                 
022100     MOVE '  '     TO GODK-STATUSKODER                                    
022200     CALL CBLTDLI USING REPL WDD8-PCB IO-AREA                             
022300     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
022400     PERFORM IMS-STATUSKONTROLL                                           
022410     ADD +1 TO W-CHKP-RAKNARE                                             
022500     .                                                                    
022600     SKIP3                                                                
022800 IMS-GU-WDJ901 SECTION.                                                   
023000     STRING 'WDJ901  (IDARTNR  =' W-IDARTNR-X ')'                         
023100          DELIMITED BY SIZE INTO SSA1                                     
023200     MOVE '  GE' TO GODK-STATUSKODER                                      
023300     CALL CBLTDLI USING GU WDJ9-PCB DLI-IO-WDJ901 SSA1                    
023400     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
023500     PERFORM IMS-STATUSKONTROLL                                           
023600     .                                                                    
023700     EJECT                                                                
023900 IMS-ISRT-WDJ901 SECTION.                                                 
024100     MOVE 'WDJ901   ' TO SSA1                                             
024200     MOVE '  ' TO GODK-STATUSKODER                                        
024300     CALL CBLTDLI USING ISRT WDJ9-PCB DLI-IO-WDJ901 SSA1                  
024400     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
024500     PERFORM IMS-STATUSKONTROLL                                           
024510     ADD +1 TO W-CHKP-RAKNARE                                             
024600     .                                                                    
024700     SKIP3                                                                
026100 IMS-REPL-WDJ911 SECTION.                                                 
026300     MOVE '  ' TO GODK-STATUSKODER                                        
026400     CALL CBLTDLI USING REPL WDJ9-PCB DLI-IO-WDJ911                       
026500     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
026600     PERFORM IMS-STATUSKONTROLL                                           
026610     ADD +1 TO W-CHKP-RAKNARE                                             
026700     .                                                                    
026900     SKIP3                                                                
027100 IMS-ISRT-WDJ911 SECTION.                                                 
027300     STRING 'WDJ901  (IDARTNR  =' W-IDARTNR-X ')'                         
027400          DELIMITED BY SIZE INTO SSA1                                     
027500     MOVE 'WDJ911  ' TO SSA2                                              
027600     MOVE '  II' TO GODK-STATUSKODER                                      
027700     CALL CBLTDLI USING ISRT WDJ9-PCB DLI-IO-WDJ911 SSA1 SSA2             
027800     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
027900     PERFORM IMS-STATUSKONTROLL                                           
027910     ADD +1 TO W-CHKP-RAKNARE                                             
028000     .                                                                    
028100     SKIP3                                                                
028300 IMS-STATUSKONTROLL SECTION.                                              
028500     SET STATUS-IX TO 1                                                   
028600     SEARCH GODK-STATUS AT END CALL FELLOG                                
028700     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
028800     CONTINUE                                                             
028900     END-SEARCH                                                           
029100     .                                                                    
