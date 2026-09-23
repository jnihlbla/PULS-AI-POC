000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4090200.                                                
000400 AUTHOR.         SVANTE BJÖRKBERG.                                        
000500     DATE-WRITTEN.   JULI 86.                                             
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*                                                                         
001100*        SALDOBILD:                                                       
001200*                                                                         
001300*           INDATA            : DISTR, KUND, DATUM                        
001400*                                                                         
001500*           BILDEN SVARAR MED : FYSISKA AVVIKELSER                        
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W4T902                                              
001900*                     W4T902 7                                            
002000*                     W4T902 8                                            
002100*        MID:         W4I90201                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W4O90201                                            
002500*    SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP3                                                                
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003010*    -- CHECKED BY WY2000                                                 
003100 77   PROGRAM-NAMN           VALUE 'W4090200'                             
003200                                 PIC X(8).                                
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500 77  NYCKLAR-OK                  PIC X.                                   
003600 77  SPRAK-IX                    PIC S9(9)   VALUE ZERO COMP SYNC.        
003700 77  DAG-IX                      PIC S9.                                  
004000 77  ACCRAD                      PIC S9(3)   COMP-3.                      
004100                                                                          
004200 77  DISTR-NUM                   PIC 9       VALUE 4.                     
004300 77  KUNDNR-NUM                  PIC 9       VALUE 2.                     
004400 77  REGDAT-NUM                  PIC 9       VALUE 1.                     
004500                                                                          
004600 77  GIVNA-NYCKLAR               PIC 9.                                   
004700   88  OTILLATEN-NYCKELKOMB                  VALUE 0 1 2 3.               
004800   88  DISTR-NYCKEL                          VALUE 4.                     
004900   88  DISTR-REGDAT-NYCKEL                   VALUE 5.                     
005000   88  DISTR-KUNDNR-NYCKEL                   VALUE 6.                     
005100   88  DISTR-KUNDNR-REGDAT-NYCKEL            VALUE 7.                     
005110*      --- VALID IDDC CODES                                               
005120*                                                                         
005130*01    -COPY WWDCKONS                                                     
005140       EJECT                                                              
005200                                                                          
005300 77  ARB-IDDISTR                 PIC 9(4).                                
005400 77  ARB-IDKUNDNR                PIC 9(6).                                
005500 77  ARB-TIREGDAT                PIC 9(6).                                
005600                                                                          
005700 77  IDDISTR-WS                  PIC X(4).                                
005800 77  IDKUNDNR-WS                 PIC X(6).                                
005900 77  TIREGDAT-WS                 PIC X(6).                                
006000     SKIP2                                                                
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
006500     SKIP2                                                                
006600 77  WS-IDTRANS                  PIC X(4).                                
006700   88  WS-GODKAEND-BILD          VALUE '4901' '4902'.                     
006800   88  WS-4901-BILD              VALUE '4901'.                            
006900                                                                          
007000 01  FILLER.                                                              
007100   03  HIST-TIREGDAT               PIC 9(6) OCCURS 3.                     
007200   03  FILLER                      PIC 9(6) VALUE ZERO.                   
007300     EJECT                                                                
007400                                                                          
007500 01    FILLER      PIC X(16)   VALUE 'NYCKLAR TILL DLI'.                  
007600                                                                          
007700 01    NYCKLAR-TILL-DLI.                                                  
007800   03    W-4319KEY-X.                                                     
007900     05    W-IDHTYP              PIC X(4)   VALUE '4319'.                 
008000     05    W-TIREGDAT            PIC S9(7)  COMP-3.                       
008100     05    FILLER                PIC X(22)  VALUE LOW-VALUE.              
008200                                                                          
008300   03    W-4320KEY-MIN-X.                                                 
008400     05    W-IDDISTR-MIN         PIC S9(5)  COMP-3.                       
008500     05    W-IDKUNDNR-MIN-X.                                              
008600       07    W-IDKUNDNR-MIN        PIC S9(7)  COMP-3.                     
008700     05    W-IDORDNR-MIN-X.                                               
008800       07    W-IDORDNR-MIN         PIC S9(5)  COMP-3.                     
008900     05    FILLER                PIC X(10)  VALUE LOW-VALUE.              
009000                                                                          
009100   03    W-4320KEY-MAX-X.                                                 
009200     05    W-IDDISTR-MAX         PIC S9(5)  COMP-3.                       
009300     05    W-IDKUNDNR-MAX-X.                                              
009400       07    W-IDKUNDNR-MAX        PIC S9(7)  COMP-3.                     
009500     05    W-IDORDNR-MAX-X.                                               
009600       07    W-IDORDNR-MAX         PIC S9(5)  COMP-3.                     
009700     05    FILLER                PIC X(10)  VALUE HIGH-VALUE.             
009800                                                                          
009900   03    W-IDARTNR-X.                                                     
010000     05    W-IDARTNR             PIC S9(9)  COMP-3.                       
010100     EJECT                                                                
010200 01    MEDDELANDE.                                                        
010300   03    FEL1.                                                            
010400     05    FILLER                PIC X(40)   VALUE                        
010500           'FYS. AVV. SAKNAS FÖR GIVNA NYCKLAR      '.                    
010600     05    FILLER                PIC X(40)   VALUE                        
010700           'PHYSICAL DEVIATION IS MISSING           '.                    
010800   03 FILLER                     REDEFINES FEL1.                          
010900     05    FEL-1                 PIC X(40)   OCCURS 2.                    
011000                                                                          
011100   03    FEL2.                                                            
011200     05    FILLER                PIC X(40)   VALUE                        
011300           'FELAKTIGA NYCKLAR                       '.                    
011400     05    FILLER                PIC X(40)   VALUE                        
011500           'KEYS NOT VALID                          '.                    
011600   03 FILLER                     REDEFINES FEL2.                          
011700     05    FEL-2                 PIC X(40)   OCCURS 2.                    
011800                                                                          
011900   03    FEL3.                                                            
012000     05    FILLER                PIC X(40)   VALUE                        
012100           'OTILLÅTET DISTRIKT                      '.                    
012200     05    FILLER                PIC X(40)   VALUE                        
012300           'NOT A VALID DISTRICT                    '.                    
012400   03 FILLER                     REDEFINES FEL3.                          
012500     05    FEL-3                 PIC X(40)   OCCURS 2.                    
012600                                                                          
012700   03    MED1.                                                            
012800     05    FILLER                PIC X(40)   VALUE                        
012900           'FLER, TRYCK PF8                         '.                    
013000     05    FILLER                PIC X(40)   VALUE                        
013100           'MORE, PRESS PF8                         '.                    
013200   03 FILLER                     REDEFINES MED1.                          
013300     05    MED-1                 PIC X(40)   OCCURS 2.                    
013400     EJECT                                                                
013500 01  TEST-IDDISTR         PIC S9(5) COMP-3.                               
013600                                                                          
014100*01  FILLER  -COPY WWDIST18      -RED TEST-IDDISTR                        
014300     EJECT                                                                
014400                                                                          
014500*01  FILLER  -COPY WWDIST20      -RED TEST-IDDISTR                        
014700     EJECT                                                                
014800                                                                          
014900*01  -COPY WORKAREA                                                       
015100     EJECT                                                                
015200                                                                          
015300******************************************************************        
015400*                                                                         
015500*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
015600*                                                                         
015700 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
015800     SKIP3                                                                
015900*01  MID -COPY W4I90201.                                                  
016100     EJECT                                                                
016200*01    -COPY WMSGAREA                                                     
016400     EJECT                                                                
016500*  03  MOD -COPY W4O90201 -RED MSG-AREA.                                  
016700     EJECT                                                                
016800*01    -COPY WMFSAREA                                                     
017000     EJECT                                                                
017100******************************************************************        
017200*                                                                         
017300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017400*                                                                         
017500******************************************************************        
017600 01    IMS-WS.                                                            
017700   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
017800     SKIP3                                                                
017900*                        **** STATUS-KOD FRÅN IMS                         
018000   03    STATUS-WS               PIC XX.                                  
018100     88    SEGMENT-FINNS                     VALUE '  '.                  
018200     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
018300     SKIP3                                                                
018400   03    GODK-STATUSKODER.                                                
018500     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
018600     SKIP3                                                                
018700 01    SSA1                      PIC X(96).                               
018800 01    SSA2                      PIC X(64).                               
018900 01    SSA3                      PIC X(64).                               
019000     EJECT                                                                
019100*                            IMS FUNKTIONSKODER                           
019200*01    -COPY W0003                                                        
019400     EJECT                                                                
019500*                            DLI INPUT-OUTPUT AREA                        
019600 01    DLI-IO-AREA.                                                       
019700   03    IO-AREA                 PIC X(200)  VALUE SPACE.                 
019800     SKIP3                                                                
019900*03  4319-AREA  -COPY WDGX4319 -RED IO-AREA.                              
020100     EJECT                                                                
020200*03  4320-AREA  -COPY WDGX4320 -RED IO-AREA.                              
020400     EJECT                                                                
020500 LINKAGE SECTION.                                                         
020600*01  -COPY W0009     -PRE MSG-                                            
020800     EJECT                                                                
020900*01  -COPY W0008     -PRE XXJD-                                           
021100     05  FILLER                  PIC X.                                   
021200     EJECT                                                                
021300 PROCEDURE DIVISION USING MSG-PCB XXJD-PCB.                               
021400                                                                          
021500     ENTRY 'DLITCBL' USING MSG-PCB XXJD-PCB.                              
021600                                                                          
021700     PERFORM IMS-GET-MSG                                                  
021800                                                                          
021900     IF SEGMENT-FINNS                                                     
022000       PERFORM A-INIT-SPARA-INPUT                                         
022100                                                                          
022200       IF (IDDISTR-WS   NUMERIC)    AND                                   
022300          (IDKUNDNR-WS  NUMERIC)    AND                                   
022400          (TIREGDAT-WS  NUMERIC)                                          
022500                                                                          
022600         MOVE IDDISTR-WS      TO TEST-IDDISTR                             
022700                                                                          
022800         IF  NOT DIST18-SKROT          AND                                
023000             NOT DIST20-EMBALLAGE                                         
023200           PERFORM B-VILKA-NYCKLAR-ANVANDS                                
023300                                                                          
023400           IF DISTR-NYCKEL            OR                                  
023500              DISTR-KUNDNR-NYCKEL                                         
023600             PERFORM E-BERAKNA-DATUM                                      
023700           END-IF                                                         
023800                                                                          
023900           PERFORM C-LAS-FORSTA-RADEN                                     
024000                                                                          
024100           EVALUATE TRUE                                                  
024200           WHEN NYCKLAR-OK = JA                                           
024300             PERFORM D-LAGG-UT-RADER                                      
024400           WHEN OTILLATEN-NYCKELKOMB                                      
024500             MOVE FEL-2 (SPRAK-IX)   TO MOD-TEMFSFEL                      
024600           WHEN OTHER                                                     
024700             MOVE FEL-1 (SPRAK-IX)   TO MOD-TEMFSFEL                      
024800           END-EVALUATE                                                   
024900         ELSE                                                             
025000           MOVE FEL-3 (SPRAK-IX)   TO MOD-TEMFSFEL                        
025100         END-IF                                                           
025200       ELSE                                                               
025300         MOVE FEL-2 (SPRAK-IX)   TO MOD-TEMFSFEL                          
025400       END-IF                                                             
025500                                                                          
025600       PERFORM IMS-INSERT-MSG                                             
025700     END-IF                                                               
025800                                                                          
025900     MOVE ZERO TO RETURN-CODE                                             
026000     GOBACK                                                               
026100     .                                                                    
026200     EJECT                                                                
026300 A-INIT-SPARA-INPUT SECTION.                                              
026400                                                                          
026500     IF MSG-DUBBLA-TRANSKODER                                             
026600       MOVE MSG-INDATA-MINUS-2-TRANSKODER  TO MID-W4I90201                
026700       MOVE MSG-IDTRANS-2                  TO MFS-IDTRANS                 
026800       MOVE MSG-KDMFSFOR-2                 TO MFS-KDMFSFOR                
026900       MOVE MSG-KDTRTYP                    TO MFS-KDTRTYP                 
027000     ELSE                                                                 
027100       MOVE MSG-INDATA-MINUS-1-TRANSKOD    TO MID-W4I90201                
027200       MOVE MSG-IDTRANS-1                  TO MFS-IDTRANS                 
027300       MOVE MSG-KDMFSFOR-1                 TO MFS-KDMFSFOR                
027400       MOVE ' '                            TO MFS-KDTRTYP                 
027500     END-IF                                                               
027600                                                                          
027700     MOVE MSG-IDPFK                        TO MFS-IDPFK                   
027800     MOVE MFS-IDTRANS                      TO WS-IDTRANS                  
027900                                                                          
028000     IF SWEDISH-TEXT                                                      
028100       MOVE +1                             TO SPRAK-IX                    
028200     ELSE                                                                 
028300       MOVE +2                             TO SPRAK-IX                    
028400     END-IF                                                               
028500                                                                          
028600     MOVE LOW-VALUE                        TO MSG-AREA                    
028700     MOVE 'W4O90201'                       TO MFS-IDMOD                   
028800     MOVE '4902'                           TO MOD-IDTRANS                 
028810     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O90201 + 4                        
029000     MOVE NEJ                              TO NYCKLAR-OK                  
029100                                                                          
029200     PERFORM AA-SPARA-NYCKLAR                                             
029300     PERFORM AB-RENSA-MOD                                                 
029400     EJECT                                                                
029500                                                                          
029600     .                                                                    
029700 AA-SPARA-NYCKLAR        SECTION.                                         
029800                                                                          
029900     IF MID-IDDISTR-IN = ALL '+'                                          
030000       MOVE MID-IDDISTR-UT TO IDDISTR-WS                                  
030100       INSPECT IDDISTR-WS REPLACING ALL SPACE BY ZERO                     
030200     ELSE                                                                 
030300       MOVE MID-IDDISTR-IN TO IDDISTR-WS                                  
030400       MOVE '000000'       TO MID-TIREGDAT-UT                             
030500     END-IF                                                               
030600     MOVE IDDISTR-WS     TO MOD-IDDISTR-UT                                
030700     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
030800     SKIP3                                                                
030900                                                                          
031000     IF MID-IDKUNDNR-IN = ALL '+'                                         
031100       MOVE MID-IDKUNDNR-UT TO IDKUNDNR-WS                                
031200       INSPECT IDKUNDNR-WS REPLACING ALL SPACE BY ZERO                    
031300     ELSE                                                                 
031400       MOVE MID-IDKUNDNR-IN TO IDKUNDNR-WS                                
031500       MOVE '000000'       TO MID-TIREGDAT-UT                             
031600     END-IF                                                               
031700     MOVE IDKUNDNR-WS     TO MOD-IDKUNDNR-UT                              
031800     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
031900     SKIP3                                                                
032000                                                                          
032100     IF MID-TIREGDAT-IN = ALL '+'                                         
032200       MOVE MID-TIREGDAT-UT TO TIREGDAT-WS                                
032300       INSPECT TIREGDAT-WS REPLACING ALL SPACE BY ZERO                    
032400     ELSE                                                                 
032500       MOVE MID-TIREGDAT-IN TO TIREGDAT-WS                                
032600     END-IF                                                               
032700     MOVE TIREGDAT-WS    TO MOD-TIREGDAT-UT                               
032800     INSPECT MOD-TIREGDAT-UT REPLACING LEADING ZERO BY SPACE              
032900     EJECT                                                                
033000                                                                          
033100     .                                                                    
033200 AB-RENSA-MOD        SECTION.                                             
033300                                                                          
033400     MOVE MFS-RENSA-FAELT       TO MOD-TEMFSFEL                           
033500                                   MOD-IDDISTR-IN                         
033600                                   MOD-IDKUNDNR-IN                        
033700                                   MOD-TIREGDAT-IN                        
033800                                   MOD-SPAR-IDDISTR                       
033900                                   MOD-SPAR-IDKUNDNR                      
034000                                   MOD-SPAR-IDORDNR                       
034100                                   MOD-SPAR-IDARTNR                       
034200                                   MOD-TEMFSINF                           
034300                                                                          
034400     IF NOT WS-GODKAEND-BILD                                              
034500       MOVE MFS-RENSA-FAELT     TO MOD-IDDISTR-UT                         
034600                                   MOD-IDKUNDNR-UT                        
034700                                   MOD-TIREGDAT-UT                        
034800     END-IF                                                               
034900                                                                          
035000     IF WS-4901-BILD                                                      
035100       MOVE MFS-RENSA-FAELT     TO MOD-TIREGDAT-UT                        
035200       MOVE '000000'            TO TIREGDAT-WS                            
035300     END-IF                                                               
035400     EJECT                                                                
035500                                                                          
035600     .                                                                    
035700 B-VILKA-NYCKLAR-ANVANDS SECTION.                                         
035800                                                                          
035900     MOVE ZERO            TO GIVNA-NYCKLAR                                
036000     MOVE IDDISTR-WS      TO ARB-IDDISTR                                  
036100     MOVE IDKUNDNR-WS     TO ARB-IDKUNDNR                                 
036200     MOVE TIREGDAT-WS     TO ARB-TIREGDAT                                 
036300                                                                          
036400     IF ARB-IDDISTR > 0                                                   
036500       ADD DISTR-NUM      TO GIVNA-NYCKLAR                                
036600     END-IF                                                               
036700                                                                          
036800     IF ARB-IDKUNDNR > 0                                                  
036900       ADD KUNDNR-NUM     TO GIVNA-NYCKLAR                                
037000     END-IF                                                               
037100                                                                          
037200     IF ARB-TIREGDAT > 0                                                  
037300       ADD REGDAT-NUM     TO GIVNA-NYCKLAR                                
037400     END-IF                                                               
037500     EJECT                                                                
037600                                                                          
037700     .                                                                    
037800 C-LAS-FORSTA-RADEN            SECTION.                                   
037900                                                                          
038000     IF MFS-IDPFK = '8'                                                   
038100       PERFORM CA-PF8-TRANS                                               
038200     END-IF                                                               
038300                                                                          
038400     IF MFS-IDPFK = ' '                                                   
038500       PERFORM CB-ENTER-TRANS                                             
038600     END-IF                                                               
038700     EJECT                                                                
038800                                                                          
038900     .                                                                    
039000 CA-PF8-TRANS      SECTION.                                               
039100                                                                          
039200     IF (MID-IDDISTR-IN        = ALL '+')   AND                           
039300        (MID-IDKUNDNR-IN       = ALL '+')   AND                           
039400        (MID-TIREGDAT-IN       = ALL '+')   AND                           
039500        (MID-SPAR-IDDISTR  NOT = ALL '0')   AND                           
039600        (MID-SPAR-IDORDNR  NOT = ALL '0')   AND                           
039700        (MID-SPAR-IDARTNR  NOT = ALL '0')                                 
039800                                                                          
039900       MOVE MID-SPAR-TIREGDAT    TO W-TIREGDAT                            
040000       MOVE 1                    TO DAG-IX                                
040100                                                                          
040200       IF DISTR-NYCKEL           OR                                       
040300          DISTR-KUNDNR-NYCKEL                                             
040400                                                                          
040500         PERFORM UNTIL W-TIREGDAT  = HIST-TIREGDAT (DAG-IX)               
040600                 OR DAG-IX NOT < 4                                        
040700           ADD +1 TO DAG-IX                                               
040800         END-PERFORM                                                      
040900         MOVE HIST-TIREGDAT (DAG-IX)  TO W-TIREGDAT                       
041000       END-IF                                                             
041100                                                                          
041120       IF DAG-IX < 4                                                      
041300         PERFORM IMS-GET-XXJD01-MED-GU                                    
041400                                                                          
041500         IF SEGMENT-FINNS                                                 
041600           PERFORM CAA-LAS-XXJD11                                         
041700           MOVE JA          TO NYCKLAR-OK                                 
041800         END-IF                                                           
041900                                                                          
042000         IF SEGMENT-SAKNAS                                                
042100           MOVE ' '         TO MFS-IDPFK                                  
042200         END-IF                                                           
042300       ELSE                                                               
042400                                                                          
042500         MOVE ' '           TO MFS-IDPFK                                  
042600       END-IF                                                             
042700     ELSE                                                                 
042800                                                                          
042900       MOVE ' '             TO MFS-IDPFK                                  
043000     END-IF                                                               
043100     EJECT                                                                
043200                                                                          
043300     .                                                                    
043400 CAA-LAS-XXJD11                 SECTION.                                  
043500                                                                          
043600     MOVE MID-SPAR-IDDISTR  TO W-IDDISTR-MIN                              
043700                               W-IDDISTR-MAX                              
043800     MOVE MID-SPAR-IDKUNDNR TO W-IDKUNDNR-MIN                             
043900                               W-IDKUNDNR-MAX                             
044000     MOVE MID-SPAR-IDORDNR  TO W-IDORDNR-MIN                              
044100                               W-IDORDNR-MAX                              
044200     MOVE MID-SPAR-IDARTNR  TO W-IDARTNR                                  
044300     PERFORM IMS-GET-XXJD11-MED-GNP-1                                     
044400     EJECT                                                                
044500                                                                          
044600     .                                                                    
044700 CB-ENTER-TRANS                   SECTION.                                
044800                                                                          
044900     EVALUATE TRUE                                                        
045000     WHEN DISTR-NYCKEL                                                    
045100       PERFORM CBA-LAS-XXJD11                                             
045200                                                                          
045300     WHEN DISTR-KUNDNR-NYCKEL                                             
045400       PERFORM CBB-LAS-XXJD11                                             
045500                                                                          
045600     WHEN DISTR-REGDAT-NYCKEL                                             
045700       PERFORM CBC-LAS-XXJD11                                             
045800                                                                          
045900     WHEN DISTR-KUNDNR-REGDAT-NYCKEL                                      
046000       PERFORM CBD-LAS-XXJD11                                             
046100     END-EVALUATE                                                         
046200     EJECT                                                                
046300                                                                          
046400     .                                                                    
046500 CBA-LAS-XXJD11        SECTION.                                           
046600                                                                          
046700     MOVE IDDISTR-WS           TO W-IDDISTR-MIN                           
046800                                  W-IDDISTR-MAX                           
046900     MOVE LOW-VALUE            TO W-IDKUNDNR-MIN-X                        
047000     MOVE HIGH-VALUE           TO W-IDKUNDNR-MAX-X                        
047100     MOVE LOW-VALUE            TO W-IDORDNR-MIN-X                         
047200     MOVE HIGH-VALUE           TO W-IDORDNR-MAX-X                         
047300                                                                          
047400     MOVE 0                  TO DAG-IX                                    
047500     MOVE 'GE'               TO STATUS-WS                                 
047600                                                                          
047620     PERFORM UNTIL SEGMENT-FINNS OR DAG-IX NOT < 3                        
047800       PERFORM UNTIL SEGMENT-FINNS OR DAG-IX NOT < 3                      
047900         ADD +1 TO DAG-IX                                                 
048000         MOVE HIST-TIREGDAT (DAG-IX)  TO W-TIREGDAT                       
048100         PERFORM IMS-GET-XXJD01-MED-GU                                    
048200       END-PERFORM                                                        
048300                                                                          
048400       IF SEGMENT-FINNS                                                   
048500         PERFORM IMS-GET-XXJD11-MED-GNP-2                                 
048600       END-IF                                                             
048700     END-PERFORM                                                          
048800                                                                          
048900     IF SEGMENT-FINNS                                                     
049000       MOVE JA                 TO NYCKLAR-OK                              
049100     END-IF                                                               
049200     EJECT                                                                
049300                                                                          
049400     .                                                                    
049500 CBB-LAS-XXJD11                   SECTION.                                
049600                                                                          
049700     MOVE IDDISTR-WS           TO W-IDDISTR-MIN                           
049800                                  W-IDDISTR-MAX                           
049900     MOVE IDKUNDNR-WS          TO W-IDKUNDNR-MIN                          
050000                                  W-IDKUNDNR-MAX                          
050100     MOVE LOW-VALUE            TO W-IDORDNR-MIN-X                         
050200     MOVE HIGH-VALUE           TO W-IDORDNR-MAX-X                         
050300                                                                          
050400     MOVE 0                  TO DAG-IX                                    
050500     MOVE 'GE'               TO STATUS-WS                                 
050600                                                                          
050620     PERFORM UNTIL SEGMENT-FINNS OR DAG-IX NOT < 3                        
050800       PERFORM UNTIL SEGMENT-FINNS OR DAG-IX NOT < 3                      
050900         ADD +1 TO DAG-IX                                                 
051000         MOVE HIST-TIREGDAT (DAG-IX)  TO W-TIREGDAT                       
051100         PERFORM IMS-GET-XXJD01-MED-GU                                    
051200       END-PERFORM                                                        
051300                                                                          
051400       IF SEGMENT-FINNS                                                   
051500         PERFORM IMS-GET-XXJD11-MED-GNP-2                                 
051600       END-IF                                                             
051700     END-PERFORM                                                          
051800                                                                          
051900     IF SEGMENT-FINNS                                                     
052000       MOVE JA                 TO NYCKLAR-OK                              
052100     END-IF                                                               
052200     EJECT                                                                
052300                                                                          
052400     .                                                                    
052500 CBC-LAS-XXJD11                   SECTION.                                
052600                                                                          
052700     MOVE TIREGDAT-WS        TO W-TIREGDAT                                
052800     PERFORM IMS-GET-XXJD01-MED-GU                                        
052900                                                                          
053000     IF SEGMENT-FINNS                                                     
053100       MOVE IDDISTR-WS         TO W-IDDISTR-MIN                           
053200                                  W-IDDISTR-MAX                           
053300       MOVE LOW-VALUE          TO W-IDKUNDNR-MIN-X                        
053400       MOVE HIGH-VALUE         TO W-IDKUNDNR-MAX-X                        
053500       MOVE LOW-VALUE          TO W-IDORDNR-MIN-X                         
053600       MOVE HIGH-VALUE         TO W-IDORDNR-MAX-X                         
053700                                                                          
053800       PERFORM IMS-GET-XXJD11-MED-GNP-2                                   
053900                                                                          
054000       IF SEGMENT-FINNS                                                   
054100         MOVE JA                 TO NYCKLAR-OK                            
054200       END-IF                                                             
054300     END-IF                                                               
054400     SKIP3                                                                
054500                                                                          
054600     .                                                                    
054700 CBD-LAS-XXJD11                   SECTION.                                
054800                                                                          
054900     MOVE TIREGDAT-WS        TO W-TIREGDAT                                
055000     PERFORM IMS-GET-XXJD01-MED-GU                                        
055100                                                                          
055200     IF SEGMENT-FINNS                                                     
055300       MOVE IDDISTR-WS         TO W-IDDISTR-MIN                           
055400                                  W-IDDISTR-MAX                           
055500       MOVE IDKUNDNR-WS        TO W-IDKUNDNR-MIN-X                        
055600                                  W-IDKUNDNR-MAX-X                        
055700       MOVE LOW-VALUE          TO W-IDORDNR-MIN-X                         
055800       MOVE HIGH-VALUE         TO W-IDORDNR-MAX-X                         
055900                                                                          
056000       PERFORM IMS-GET-XXJD11-MED-GNP-2                                   
056100                                                                          
056200       IF SEGMENT-FINNS                                                   
056300         MOVE JA                 TO NYCKLAR-OK                            
056400       END-IF                                                             
056500     END-IF                                                               
056600     EJECT                                                                
056700                                                                          
056800     .                                                                    
056900 D-LAGG-UT-RADER           SECTION.                                       
057000                                                                          
057100     MOVE 1                      TO ACCRAD                                
057200                                                                          
057300     PERFORM UNTIL SEGMENT-SAKNAS OR ACCRAD NOT < 15                      
057400       PERFORM DA-FLYTTA-TILL-MOD                                         
057500       ADD +1 TO ACCRAD                                                   
057600                                                                          
057700       PERFORM IMS-GET-XXJD11-MED-GNP-2                                   
057720       IF (DISTR-NYCKEL OR DISTR-KUNDNR-NYCKEL)                           
057900                                                                          
058000         PERFORM UNTIL SEGMENT-FINNS OR DAG-IX NOT < 3                    
058100           PERFORM UNTIL SEGMENT-FINNS OR DAG-IX NOT < 3                  
058200             ADD +1 TO DAG-IX                                             
058300             MOVE HIST-TIREGDAT (DAG-IX)   TO W-TIREGDAT                  
058400             PERFORM IMS-GET-XXJD01-MED-GU                                
058500           END-PERFORM                                                    
058600                                                                          
058700           IF SEGMENT-FINNS                                               
058800             PERFORM IMS-GET-XXJD11-MED-GNP-2                             
058900           END-IF                                                         
059000         END-PERFORM                                                      
059100                                                                          
059200       END-IF                                                             
059300     END-PERFORM                                                          
059400                                                                          
059500     IF SEGMENT-FINNS                                                     
059600       IF DISTR-REGDAT-NYCKEL          OR                                 
059700          DISTR-KUNDNR-REGDAT-NYCKEL                                      
059800         MOVE TIREGDAT-WS        TO MOD-SPAR-TIREGDAT                     
059900       ELSE                                                               
060000         MOVE ZERO               TO MOD-SPAR-TIREGDAT                     
060100       END-IF                                                             
060200                                                                          
060300       MOVE W-TIREGDAT           TO MOD-SPAR-TIREGDAT                     
060400       MOVE 4320-IDDISTR         TO MOD-SPAR-IDDISTR                      
060500       MOVE 4320-IDKUNDNR        TO MOD-SPAR-IDKUNDNR                     
060600       MOVE 4320-IDORDNR         TO MOD-SPAR-IDORDNR                      
060700       MOVE 4320-IDARTNR         TO MOD-SPAR-IDARTNR                      
060800                                                                          
060900       MOVE MED-1 (SPRAK-IX)     TO MOD-TEMFSINF                          
061000     END-IF                                                               
061100     EJECT                                                                
061200                                                                          
061300     .                                                                    
061400 DA-FLYTTA-TILL-MOD          SECTION.                                     
061500                                                                          
061600     MOVE 4320-IDORDNR           TO MOD-IDORDNR   (ACCRAD)                
061700     MOVE 4320-IDKUNDNR          TO MOD-IDKUNDNR  (ACCRAD)                
061800     MOVE 4320-BEVARREF          TO MOD-BEVARREF  (ACCRAD)                
061900     MOVE 4320-IDARTNR           TO MOD-IDARTNR   (ACCRAD)                
062000     MOVE '-'                    TO MOD-STRECK    (ACCRAD)                
062100     MOVE 4320-REKSIFFR          TO MOD-REKSIFFR  (ACCRAD)                
062200     MOVE 4320-KVAVBART          TO MOD-KVAVBART  (ACCRAD)                
062300     MOVE 4320-KVLEVART          TO MOD-KVLEVART  (ACCRAD)                
062400     MOVE 4320-IDPRODNR          TO MOD-IDPRODNR  (ACCRAD)                
062500     MOVE 4320-KDORDKL           TO MOD-KDORDKL   (ACCRAD)                
062600     MOVE 4320-IDDC              TO MOD-IDDC      (ACCRAD)                
062700     MOVE 4320-BEART             TO MOD-BEART     (ACCRAD)                
062800     EJECT                                                                
062900                                                                          
063000     .                                                                    
063100 E-BERAKNA-DATUM             SECTION.                                     
063200                                                                          
063300     MOVE WC-CDC-SE              TO WORK-IDDC                             
063310     MOVE 003                    TO WORK-KDCALL                           
063400     ACCEPT WORK-TIAAMMDD-TOM    FROM DATE                                
063500     MOVE WORK-TIAAMMDD-TOM      TO HIST-TIREGDAT ( 1 )                   
063600     MOVE 2                      TO DAG-IX                                
063700                                                                          
063720     PERFORM UNTIL DAG-IX NOT < 4                                         
063900       MOVE DAG-IX               TO WORK-KVWORKD                          
064000                                                                          
064100       CALL WORKDAY  USING   WORK-KDCALL                                  
064200                             WORK-DATE-AREA                               
064300                             WORK-KDSVAR                                  
064400                                                                          
064500       IF WORK-KDSVAR-OK                                                  
064600         MOVE WORK-TIAAMMDD-FOM  TO HIST-TIREGDAT (DAG-IX)                
064700       ELSE                                                               
064800         CALL FELLOG                                                      
064900       END-IF                                                             
065000                                                                          
065100       ADD 1 TO DAG-IX                                                    
065200     END-PERFORM                                                          
065300     EJECT                                                                
065400                                                                          
065500******************************************************************        
065600*                                                                         
065700*                  I M S   -   S E K T I O N E R                          
065800*                                                                         
065900******************************************************************        
066000     SKIP3                                                                
066100     .                                                                    
066200 IMS-GET-MSG               SECTION.                                       
066300     MOVE '  QC' TO GODK-STATUSKODER                                      
066400     CALL CBLTDLI USING GU                                                
066500                          MSG-PCB                                         
066600                          MSG-IO-AREA                                     
066700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
066800     PERFORM IMS-STATUSKONTROLL                                           
066900     SKIP3                                                                
067000     .                                                                    
067100 IMS-INSERT-MSG SECTION.                                                  
067200     IF ENGLISH-TEXT                                                      
067300       MOVE 'N' TO MFS-KDHUVOMR                                           
067400     END-IF                                                               
067500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
067600     MOVE SPACE TO GODK-STATUSKODER                                       
067700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
067800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
067900     PERFORM IMS-STATUSKONTROLL                                           
068000     EJECT                                                                
068100                                                                          
068200     .                                                                    
068300 IMS-GET-XXJD01-MED-GU            SECTION.                                
068400                                                                          
068500     STRING 'WLXXJD01(WDGXKEY  =' W-4319KEY-X ')'                         
068600            DELIMITED BY SIZE INTO SSA1                                   
068700     MOVE '  GE' TO GODK-STATUSKODER                                      
068800     CALL CBLTDLI USING GU XXJD-PCB DLI-IO-AREA SSA1                      
068900     MOVE XXJD-STATUS-CODE TO STATUS-WS                                   
069000     PERFORM IMS-STATUSKONTROLL                                           
069100     SKIP3                                                                
069200                                                                          
069300     .                                                                    
069400 IMS-GET-XXJD11-MED-GNP-1         SECTION.                                
069500                                                                          
069600     STRING 'WLXXJD11(WDGXKEY >=' W-4320KEY-MIN-X                         
069700                    '&WDGXKEY <=' W-4320KEY-MAX-X                         
069800                    '&IDARTNR  =' W-IDARTNR-X ')'                         
069900            DELIMITED BY SIZE INTO SSA1                                   
070000     MOVE '  GE' TO GODK-STATUSKODER                                      
070100     CALL CBLTDLI USING GNP XXJD-PCB DLI-IO-AREA SSA1                     
070200     MOVE XXJD-STATUS-CODE TO STATUS-WS                                   
070300     PERFORM IMS-STATUSKONTROLL                                           
070400     SKIP3                                                                
070500                                                                          
070600     .                                                                    
070700 IMS-GET-XXJD11-MED-GNP-2         SECTION.                                
070800                                                                          
070900     STRING 'WLXXJD11(WDGXKEY >=' W-4320KEY-MIN-X                         
071000                    '&WDGXKEY <=' W-4320KEY-MAX-X ')'                     
071100            DELIMITED BY SIZE INTO SSA1                                   
071200     MOVE '  GE' TO GODK-STATUSKODER                                      
071300     CALL CBLTDLI USING GNP XXJD-PCB DLI-IO-AREA SSA1                     
071400     MOVE XXJD-STATUS-CODE TO STATUS-WS                                   
071500     PERFORM IMS-STATUSKONTROLL                                           
071600     EJECT                                                                
071700                                                                          
071800     .                                                                    
071900 IMS-STATUSKONTROLL SECTION.                                              
072000     SET STATUS-IX TO 1                                                   
072100     SEARCH GODK-STATUS AT END CALL FELLOG                                
072200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
072300     END-SEARCH                                                           
072400     .                                                                    
