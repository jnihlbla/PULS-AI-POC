001300 ID DIVISION.                                                             
001500 PROGRAM-ID.     W5011700.                                                
001600 AUTHOR.         GUN LÖFGREN.                                             
001700 DATE-WRITTEN.   96/05/06.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNKTION:                                                            
002100*        FRÅGA/ ÄNDRA KÖ FÖR STANDARDPRISUPPDATERING                      
002200*                                                                         
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W5T117                                              
002700*        MID:         W5I11701                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W5O11701                                            
003100                                                                          
003110                                                                          
003200                                                                          
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 DATA DIVISION.                                                           
003510     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003601                                                                          
003610*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W5011700'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400                                                                          
004600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004800                                                                          
005000                                                                          
005100 77  INDATA-OK                   PIC X       VALUE 'J'.                   
005310                                                                          
005320 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005330     88  NYCKLAR-OK                          VALUE 'J'.                   
005340     88  NYCKLAR-FEL                         VALUE 'N'.                   
005400                                                                          
005401 77  BYT-SW                      PIC X       VALUE 'N'.                   
005404                                                                          
005410 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005420     88  ALLT-OK                             VALUE 'J'.                   
005440                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '5117'.                
005700     88  GODK-MID                            VALUE '5111' '5112'          
005800                                                   '5113' '5114'          
005900                                                   '5115' '5116'          
006000                                                   '5117' '5118'          
006100                                                   '5119'.                
006200     88  HELP-MID                            VALUE '0551'.                
006301                                                                          
006310 01  DIVERSE.                                                             
006320     03  MAX-RAD-IX              PIC S9(9)   VALUE +13  COMP SYNC.        
006330     03  RAD-IX                  PIC S9(9)   VALUE ZERO COMP SYNC.        
006331     03  W-KDPRIBEH              PIC X       VALUE SPACE.                 
006335     03  WS-ANT-PLUS             PIC S9(3)   VALUE ZERO COMP-3.           
006340                                                                          
006342     EJECT                                                                
006400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERELLA-SUBPROGRAM.                                                
006600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007300*01 -COPY WMEDAREA                                                        
007400     SKIP3                                                                
007500 01  MESSAGE-CODES.                                                       
007600     03  ERR-DATA                PIC X(3)    VALUE '001'.                 
007801     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007803     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007804     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007805     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
007806     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007807     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007810     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008000     EJECT                                                                
008100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008200*                                                                         
008300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008400     SKIP3                                                                
008500*01 -COPY WMSGINIT                                                        
008600     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W5I11701                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W5O11701                                                 
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'PROG-PROG'.           
010001*                                                                         
010010 01  W-PROG-TO-PROG-SW.                                                   
010100                                                                          
010101     03  M-SW-KVLL               PIC S9(4)   VALUE +50  COMP SYNC.        
010102     03  FILLER                  PIC X(2)    VALUE LOW-VALUE.             
010103     03  FILLER                  PIC X(8)    VALUE 'W5T116  '.            
010104     03  FILLER                  PIC X(4)    VALUE '5117'.                
010105     03  FILLER                  PIC X       VALUE '1'.                   
010106*                                                                         
010107     03  MID  -COPY  W5I11601  -PRE 5116-                                 
010108*                                                                         
010109     EJECT                                                                
010110 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010710                                                                          
010720 01  SPAR-AREA.                                                           
010730   03  SPAR-IDTRANS              PIC X(4)    VALUE SPACE.                 
010740   03  SPAR-WDH801KY-ENTER       PIC X(17)   VALUE SPACE.                 
010741   03  SPAR-WDH801KY-NEXT        PIC X(17)   VALUE SPACE.                 
010742   03  SPAR-TIREGTID  OCCURS 13  PIC S9(7)   VALUE ZERO COMP-3.           
010800                                                                          
011200 01  NYCKLAR-TILL-DLI.                                                    
011399   03  W-WDH801KY-X.                                                      
011400     05  W-IDARTNR               PIC S9(9)   VALUE ZERO COMP-3.           
011401     05  W-DAREGDAT              PIC 9(8)    VALUE ZERO.                  
011402     05  W-TIREGTID-X.                                                    
011403       07  W-TIREGTID            PIC S9(7)   VALUE ZERO COMP-3.           
011409                                                                          
011410     03  W-WDH801KY-MIN.                                                  
011411         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
011412         05  FILLER              PIC X(12)   VALUE LOW-VALUE.             
011413     03  W-WDH801KY-MAX.                                                  
011414         05  W-IDARTNR-MAX       PIC S9(9)   VALUE 999999998              
011415                                                        COMP-3.           
011416         05  FILLER              PIC X(12)   VALUE HIGH-VALUE.            
011420     SKIP2                                                                
011500*    --- STATUS-KOD FRÅN IMS                                              
011600 01  STATUS-WS                   PIC XX.                                  
011700     88  SEGMENT-FINNS                       VALUE '  '.                  
011800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011910     88  BAS-SLUT                            VALUE 'GB'.                  
012000     SKIP2                                                                
012100 01  GODK-STATUSKODER.                                                    
012200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012300     SKIP3                                                                
012400 01  SSA1                        PIC X(96).                               
012600     EJECT                                                                
012700*    --- IMS FUNKTIONSKODER                                               
012800*01  -COPY W0003                                                          
013000     EJECT                                                                
013100*    ---  DLI INPUT-OUTPUT AREA                                           
013200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013300                                                                          
013600 01  DLI-IO-AREA.                                                         
013862*  03  -COPY WDH801                                                       
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014200*01  -COPY W0009   -PRE MSG-                                              
014201                                                                          
014210*01  -COPY W0009   -PRE ALT-                                              
014220     EJECT                                                                
014300*01  -COPY W0008   -PRE USEA-                                             
014400     05  FILLER                  PIC X.                                   
014500                                                                          
014505*01  -COPY W0008  -PRE PRIG-                                              
014510     05  FILLER                  PIC X.                                   
014600     EJECT                                                                
014701 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB PRIG-PCB.             
014702 MAIN SECTION.                                                            
014710     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB PRIG-PCB.             
014800                                                                          
015000     PERFORM IMS-GET-MSG                                                  
015100     IF SEGMENT-FINNS                                                     
015200       PERFORM A-INIT                                                     
015300       PERFORM B-KOLLA-NYCKLAR                                            
015400       IF NYCKLAR-OK                                                      
015500         IF MFS-UPDATE                                                    
015600           PERFORM G-KOLLA-INPUT                                          
015601           IF INDATA-OK = JA                                              
015602             PERFORM H-UPPDATERA                                          
015603           END-IF                                                         
015700         ELSE                                                             
015900           IF MFS-FIRST                                                   
015901             PERFORM C-FOERSTA-SIDA                                       
015902           ELSE                                                           
015903             IF MFS-NEXT                                                  
015904               PERFORM D-NAESTA-SIDA                                      
015905             ELSE                                                         
015906               PERFORM E-SAMMA-SIDA                                       
015907             END-IF                                                       
015908           END-IF                                                         
015912         END-IF                                                           
015920         IF ALLT-OK                                                       
015930           PERFORM F-LAES-VISA-INFO                                       
015940         END-IF                                                           
016000       END-IF                                                             
016010       IF BYT-SW = NEJ                                                    
016100         COMPUTE MSG-KVLL = LENGTH OF MOD-W5O11701 + 4                    
016200         PERFORM IMS-INSERT-MSG                                           
016210       END-IF                                                             
016300     END-IF                                                               
016500                                                                          
016600     MOVE ZERO TO RETURN-CODE                                             
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
017000 A-INIT SECTION.                                                          
017100                                                                          
017200     IF MSG-DUBBLA-TRANSKODER                                             
017300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I11701                 
017400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017600     ELSE                                                                 
017700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I11701                  
017800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018000     END-IF                                                               
018100                                                                          
018200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018500                                                                          
018600     MOVE LOW-VALUE TO MSG-AREA                                           
018700     MOVE 'W5O11701' TO MFS-IDMOD                                         
018800     MOVE '5117' TO MOD-IDTRANS                                           
018900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019000                                                                          
019500     IF EGEN-MID OR HELP-MID                                              
019600       CONTINUE                                                           
019700     ELSE                                                                 
019800       MOVE SPACE TO MFS-KDTRTYP                                          
019900       MOVE '7' TO MFS-IDPFK                                              
020000     END-IF                                                               
020001                                                                          
020010     IF MID-IDARTNR-IN NOT = ALL '+'                                      
020020       MOVE '7'         TO MFS-IDPFK                                      
020030       MOVE SPACE       TO MFS-KDTRTYP                                    
020040     END-IF                                                               
020100                                                                          
020300     .                                                                    
020400     EJECT                                                                
020500 B-KOLLA-NYCKLAR SECTION.                                                 
020510                                                                          
020513     MOVE JA                TO NYCKLAR-SW                                 
020515                                                                          
020520     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020530     MOVE '001'             TO MSGI-KDCALL                                
020540     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020541     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020542     MOVE '5117'            TO MSGI-IDTRANS                               
020550                                                                          
020560     IF EGEN-MID                                                          
020570       MOVE MID-IDARTNR-IN       TO MSGI-IDARTNR                          
020580     ELSE                                                                 
020590       IF MID-IDARTNR-IN  NUMERIC                                         
020592         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
020593       END-IF                                                             
020595     END-IF                                                               
020596                                                                          
020597     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020598     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
020599                                                                          
020600*    -- KONTROLL AV IDARTNR                                               
020602     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
020605                                                                          
020660     IF MSGI-IDARTNR NUMERIC                                              
020661       IF MSGI-IDARTNR > ZERO                                             
020670         MOVE MSGI-IDARTNR TO W-IDARTNR-MIN                               
020673                              W-IDARTNR-MAX                               
020680       ELSE                                                               
020681         MOVE ZERO         TO W-IDARTNR-MIN                               
020684         MOVE 99999998     TO W-IDARTNR-MAX                               
020685       END-IF                                                             
020691     ELSE                                                                 
020693       MOVE NEJ  TO NYCKLAR-SW                                            
020694     END-IF                                                               
020695                                                                          
020696     MOVE MFS-RENSA-FAELT TO MOD-KDPRIBEH-IN                              
020697                                                                          
020698     IF MID-KDPRIBEH-IN = '+'                                             
020699       MOVE MID-KDPRIBEH-UT TO W-KDPRIBEH                                 
020700     ELSE                                                                 
020701       MOVE MID-KDPRIBEH-IN TO W-KDPRIBEH                                 
020707     END-IF                                                               
020708                                                                          
020709     IF W-KDPRIBEH = ' ' OR 'N' OR 'V' OR 'J' OR 'B'                      
020710       CONTINUE                                                           
020711     ELSE                                                                 
020712       MOVE NEJ  TO NYCKLAR-SW                                            
020713     END-IF                                                               
020714                                                                          
020715     IF GODK-MID OR NYCKLAR-OK                                            
020716       MOVE MSGI-IDARTNR      TO MOD-IDARTNR-UT                           
020720       INSPECT MOD-IDARTNR-UT REPLACING                                   
020730                              LEADING ZERO BY SPACE                       
020731       MOVE W-KDPRIBEH          TO MOD-KDPRIBEH-UT                        
020740     ELSE                                                                 
020750       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
020751                               MOD-KDPRIBEH-UT                            
020760     END-IF                                                               
020761                                                                          
020780     IF NYCKLAR-FEL                                                       
020790       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
020791       CALL WMEDKONV USING MED-WMEDAREA                                   
020792       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
020793       MOVE +1 TO RAD-IX                                                  
020794       PERFORM UNTIL RAD-IX > MAX-RAD-IX                                  
020796         PERFORM MFS-RENSA-FAELT-IN                                       
020797         PERFORM MFS-RENSA-FAELT-UT                                       
020798         ADD +1 TO RAD-IX                                                 
020799       END-PERFORM                                                        
020800     END-IF                                                               
020801     .                                                                    
020802     EJECT                                                                
020810                                                                          
020900 C-FOERSTA-SIDA   SECTION.                                                
021000                                                                          
021100     MOVE INF-FIRST-PAGE      TO MED-IDMFSINF                             
021200     CALL WMEDKONV  USING  MED-WMEDAREA                                   
021300     MOVE MED-MFSINF          TO MOD-TEMFSFEL                             
021400     MOVE JA                  TO ALLT-SW                                  
021411     .                                                                    
021412     EJECT                                                                
021430 D-NAESTA-SIDA    SECTION.                                                
021440                                                                          
021445     IF SPAR-IDTRANS = '5117'                                             
021446       MOVE SPAR-WDH801KY-NEXT TO W-WDH801KY-MIN                          
021456     END-IF                                                               
021463                                                                          
021470     MOVE 999999998        TO W-IDARTNR-MAX                               
021480     MOVE JA               TO ALLT-SW                                     
021492     .                                                                    
021493     EJECT                                                                
021496 E-SAMMA-SIDA SECTION.                                                    
021497                                                                          
021499     PERFORM EA-KOLL-ENTER-UPDATE                                         
021500     IF ALLT-OK                                                           
021501       PERFORM EB-KOLL-OM-BYT-BILD                                        
021502       IF ALLT-OK                                                         
021503         IF SPAR-IDTRANS = '5117'                                         
021504           MOVE SPAR-WDH801KY-ENTER TO W-WDH801KY-MIN                     
021506         END-IF                                                           
021509         MOVE JA TO ALLT-SW                                               
021510       END-IF                                                             
021511     END-IF                                                               
021518     .                                                                    
021519     EJECT                                                                
021520 EA-KOLL-ENTER-UPDATE SECTION.                                            
021521                                                                          
021522     IF MID-KDPRIBEH (1)     = '+'                                        
021523       AND MID-KDPRIBEH (2)  = '+'                                        
021524       AND MID-KDPRIBEH (3)  = '+'                                        
021525       AND MID-KDPRIBEH (4)  = '+'                                        
021526       AND MID-KDPRIBEH (5)  = '+'                                        
021527       AND MID-KDPRIBEH (6)  = '+'                                        
021528       AND MID-KDPRIBEH (7)  = '+'                                        
021529       AND MID-KDPRIBEH (8)  = '+'                                        
021530       AND MID-KDPRIBEH (9)  = '+'                                        
021531       AND MID-KDPRIBEH (10) = '+'                                        
021532       AND MID-KDPRIBEH (11) = '+'                                        
021533       AND MID-KDPRIBEH (12) = '+'                                        
021534       AND MID-KDPRIBEH (13) = '+'                                        
021535         MOVE JA TO ALLT-SW                                               
021536     ELSE                                                                 
021559       MOVE +1 TO RAD-IX                                                  
021560       PERFORM UNTIL RAD-IX > MAX-RAD-IX                                  
021561         IF MID-IDARTNR (RAD-IX) NOT = ALL '+'                            
021562           MOVE MFS-OPEN-ALPHA-NOMOD TO                                   
021563                MOD-KDBEH-ATTR (RAD-IX)                                   
021564                MOD-KDPRIBEH-ATTR (RAD-IX)                                
021565         END-IF                                                           
021568         IF MID-KDPRIBEH (RAD-IX) NOT = '+'                               
021569           MOVE MFS-OPEN-ALPHA-FIELD-HI TO                                
021570                MOD-KDPRIBEH-ATTR (RAD-IX)                                
021573         END-IF                                                           
021574         PERFORM MFS-ROER-EJ-FAELT-IN                                     
021575         PERFORM MFS-ROER-EJ-FAELT-UT                                     
021576         ADD +1 TO RAD-IX                                                 
021578       END-PERFORM                                                        
021579                                                                          
021720       MOVE NEJ TO ALLT-SW                                                
021730       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
021740       CALL WMEDKONV USING MED-WMEDAREA                                   
021750       MOVE MED-MFSINF TO MOD-TEMFSFEL                                    
021794     END-IF                                                               
021795     .                                                                    
021796     EJECT                                                                
021800 EB-KOLL-OM-BYT-BILD SECTION.                                             
021801                                                                          
021802     MOVE +1 TO RAD-IX                                                    
021803     PERFORM UNTIL RAD-IX > MAX-RAD-IX                                    
021804       IF MID-KDBEH (RAD-IX) = 'S'                                        
021805         MOVE NEJ TO ALLT-SW                                              
021810         INSPECT MID-IDARTNR (RAD-IX)                                     
021811                 REPLACING LEADING SPACE BY ZERO                          
021813         MOVE MID-IDARTNR (RAD-IX) TO W-IDARTNR-MIN                       
021814                                      W-IDARTNR-MAX                       
021815         MOVE SPAR-TIREGTID (RAD-IX) TO W-TIREGTID                        
021816         PERFORM IMS-GET-PRIG01-HOLD                                      
021817         IF SEGMENT-FINNS                                                 
021818           MOVE LOW-VALUE TO 5116-MID-W5I11601                            
021819           MOVE MID-IDARTNR (RAD-IX) TO 5116-MID-IDARTNR-IN               
021820           MOVE ZERO                  TO 5116-MID-IDARTNR-UT              
021821           MOVE '+'                   TO 5116-MID-KDPRIBEH-IN             
021822           MOVE PRI-DAREGDAT          TO 5116-MID-DAREGDAT                
021823           MOVE PRI-TIREGTID          TO 5116-MID-TIREGTID                
021824           COMPUTE M-SW-KVLL = LENGTH OF 5116-MID-W5I11601 + 17           
021825           PERFORM IMS-INSERT-ALT-MSG                                     
021826           MOVE JA TO BYT-SW                                              
021827         ELSE                                                             
021828           MOVE NEJ TO BYT-SW                                             
021829         END-IF                                                           
021831         MOVE +98 TO RAD-IX                                               
021832       END-IF                                                             
021833       ADD +1 TO RAD-IX                                                   
021834     END-PERFORM                                                          
021840     .                                                                    
021850     EJECT                                                                
021900 F-LAES-VISA-INFO SECTION.                                                
022000                                                                          
023466     PERFORM IMS-GET-PRIG01                                               
023467     IF SEGMENT-FINNS                                                     
023469       MOVE PRI-IDARTNR  TO W-IDARTNR                                     
023470       MOVE PRI-DAREGDAT TO W-DAREGDAT                                    
023471       MOVE PRI-TIREGTID TO W-TIREGTID                                    
023472       MOVE W-WDH801KY-X TO SPAR-WDH801KY-ENTER                           
023473     ELSE                                                                 
023474       MOVE ERR-PART-MISSING TO MED-IDMFSFEL                              
023475       CALL WMEDKONV USING MED-WMEDAREA                                   
023476       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
023477       MOVE W-WDH801KY-MIN TO SPAR-WDH801KY-ENTER                         
023478     END-IF                                                               
023479                                                                          
023480     MOVE +1 TO RAD-IX                                                    
023481     PERFORM UNTIL RAD-IX > MAX-RAD-IX                                    
023482       IF SEGMENT-FINNS                                                   
023487         IF W-KDPRIBEH = SPACE                                            
023488             OR W-KDPRIBEH = PRI-KDPRIBEH                                 
023489           IF PRI-FLKLAR NOT = JA                                         
023490               OR PRI-N-PRARTSTD NOT = ZERO                               
023493             PERFORM FA-LAEGG-UT-RAD                                      
023495             ADD +1 TO RAD-IX                                             
023498           END-IF                                                         
023499         END-IF                                                           
023500         PERFORM IMS-GET-PRIG01-NEXT                                      
023508       ELSE                                                               
023509         PERFORM MFS-RENSA-FAELT-IN                                       
023510         PERFORM MFS-RENSA-FAELT-UT                                       
023511         ADD +1 TO RAD-IX                                                 
023512       END-IF                                                             
023514     END-PERFORM                                                          
023515                                                                          
023516     IF SEGMENT-FINNS                                                     
023517       MOVE PRI-IDARTNR  TO W-IDARTNR                                     
023518       MOVE PRI-DAREGDAT TO W-DAREGDAT                                    
023519       MOVE PRI-TIREGTID TO W-TIREGTID                                    
023520       MOVE W-WDH801KY-X TO SPAR-WDH801KY-NEXT                            
023521       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
023522       CALL WMEDKONV USING MED-WMEDAREA                                   
023523       MOVE MED-MFSINF        TO MOD-TEMFSINF                             
023524     ELSE                                                                 
023525       MOVE W-WDH801KY-X TO SPAR-WDH801KY-NEXT                            
023526     END-IF                                                               
023527                                                                          
023528     MOVE '002'  TO MSGI-KDCALL                                           
023529     MOVE '5117' TO SPAR-IDTRANS                                          
023537     MOVE SPAR-AREA TO MSGI-SPAR-AREA                                     
023541     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023543     .                                                                    
023544     EJECT                                                                
023545 FA-LAEGG-UT-RAD  SECTION.                                                
023546                                                                          
023547     MOVE PRI-IDARTNR         TO MOD-IDARTNR      (RAD-IX)                
023548     IF PRI-N-PRARTBEL-PR NOT = ZERO                                      
023549       MOVE PRI-N-PRARTBEL-PR TO MOD-PRARTBEL-PR  (RAD-IX)                
023550     ELSE                                                                 
023551       MOVE PRI-O-PRARTBEL-PR TO MOD-PRARTBEL-PR  (RAD-IX)                
023552     END-IF                                                               
023553     IF PRI-N-KDVALISO NOT = SPACE                                        
023554       MOVE PRI-N-KDVALISO    TO MOD-KDVALISO     (RAD-IX)                
023555     ELSE                                                                 
023556       MOVE PRI-O-KDVALISO    TO MOD-KDVALISO     (RAD-IX)                
023557     END-IF                                                               
023558     MOVE PRI-O-PRINK         TO MOD-PRINK-AKT    (RAD-IX)                
023559     MOVE PRI-N-PRINK         TO MOD-PRINK-KOM    (RAD-IX)                
023560     MOVE PRI-O-PRARTSTD      TO MOD-PRARTSTD-AKT (RAD-IX)                
023561     MOVE PRI-N-PRARTSTD      TO MOD-PRARTSTD-KOM (RAD-IX)                
023562     MOVE PRI-KDPRIBEH        TO MOD-KDPRIBEH (RAD-IX)                    
023563     MOVE SPACE               TO MOD-KDBEH        (RAD-IX)                
023564     MOVE MFS-OPEN-ALPHA-NOMOD TO MOD-KDBEH-ATTR (RAD-IX)                 
023565                                 MOD-KDPRIBEH-ATTR (RAD-IX)               
023570     MOVE PRI-TIREGTID        TO SPAR-TIREGTID    (RAD-IX)                
024300     .                                                                    
024400     EJECT                                                                
024500 G-KOLLA-INPUT     SECTION.                                               
024600                                                                          
024610     MOVE JA   TO INDATA-OK                                               
024700     MOVE +1   TO RAD-IX                                                  
024800     PERFORM UNTIL RAD-IX > MAX-RAD-IX                                    
024810       IF MID-IDARTNR (RAD-IX) NOT = ALL '+'                              
024820         MOVE MFS-OPEN-ALPHA-NOMOD TO                                     
024830              MOD-KDBEH-ATTR (RAD-IX)                                     
024840              MOD-KDPRIBEH-ATTR (RAD-IX)                                  
024850       END-IF                                                             
024900       IF MID-KDPRIBEH (RAD-IX) = '+'                                     
024910         ADD +1 TO WS-ANT-PLUS                                            
024911       ELSE                                                               
024920         IF MID-KDPRIBEH (RAD-IX) = 'N' OR 'B' OR 'V'                     
024930           MOVE MFS-ALFA-FAELT-RAETT TO                                   
024940                      MOD-KDPRIBEH-ATTR (RAD-IX)                          
024950         ELSE                                                             
024960           MOVE MFS-ALFA-FAELT-FEL   TO                                   
024970                      MOD-KDPRIBEH-ATTR (RAD-IX)                          
024990           MOVE NEJ TO INDATA-OK                                          
024991         END-IF                                                           
024992       END-IF                                                             
024993       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024994       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024995       ADD 1   TO RAD-IX                                                  
024996     END-PERFORM                                                          
025009                                                                          
025010     IF INDATA-OK = JA                                                    
025011       IF WS-ANT-PLUS = MAX-RAD-IX                                        
025012         MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                        
025013         CALL WMEDKONV USING MED-WMEDAREA                                 
025014         MOVE MED-MFSFEL    TO MOD-TEMFSFEL                               
025015       END-IF                                                             
025016     ELSE                                                                 
025017       MOVE ERR-DATA      TO MED-IDMFSFEL                                 
025018       CALL WMEDKONV USING MED-WMEDAREA                                   
025019       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
025020     END-IF                                                               
025021     MOVE NEJ TO ALLT-SW                                                  
025100     .                                                                    
025200     EJECT                                                                
025300 H-UPPDATERA       SECTION.                                               
025310                                                                          
025320     MOVE 1     TO RAD-IX                                                 
025330     PERFORM UNTIL RAD-IX > MAX-RAD-IX                                    
025340       IF MID-KDPRIBEH (RAD-IX) NOT = '+'                                 
025341         INSPECT MID-IDARTNR (RAD-IX) REPLACING                           
025342                 LEADING SPACE BY ZERO                                    
025343         MOVE SPAR-TIREGTID (RAD-IX) TO W-TIREGTID                        
025344         MOVE MID-IDARTNR   (RAD-IX) TO W-IDARTNR-MIN                     
025345                                        W-IDARTNR-MAX                     
025346         PERFORM IMS-GET-PRIG01-HOLD                                      
025347         IF SEGMENT-FINNS                                                 
025348           MOVE MID-KDPRIBEH (RAD-IX) TO PRI-KDPRIBEH                     
025350           PERFORM IMS-REPL-PRIG01                                        
025353           MOVE INF-UPDATE-DONE TO MED-IDMFSINF                           
025354           CALL WMEDKONV USING MED-WMEDAREA                               
025355           MOVE MED-MFSINF        TO MOD-TEMFSINF                         
025356         ELSE                                                             
025357           MOVE ERR-PART-MISSING  TO MED-IDMFSFEL                         
025358           CALL WMEDKONV USING MED-WMEDAREA                               
025359           MOVE MED-MFSFEL        TO MOD-TEMFSFEL                         
025369         END-IF                                                           
025370         MOVE MFS-OPEN-ALPHA-FIELD-HI TO                                  
025371              MOD-KDPRIBEH-ATTR (RAD-IX)                                  
025374       END-IF                                                             
025375       ADD 1                    TO RAD-IX                                 
025377     END-PERFORM                                                          
025378                                                                          
025380     .                                                                    
025585     EJECT                                                                
025590 MFS-RENSA-FAELT-UT SECTION.                                              
025600                                                                          
025900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR      (RAD-IX)                    
026000                             MOD-PRARTBEL-PR  (RAD-IX)                    
026100                             MOD-KDVALISO     (RAD-IX)                    
026110                             MOD-PRINK-AKT    (RAD-IX)                    
026120                             MOD-PRINK-KOM    (RAD-IX)                    
026130                             MOD-PRARTSTD-AKT (RAD-IX)                    
026131                             MOD-PRARTSTD-KOM (RAD-IX)                    
026140                             MOD-KDPRIBEH (RAD-IX)                        
026200     .                                                                    
026400     SKIP3                                                                
026500 MFS-RENSA-FAELT-IN SECTION.                                              
026600                                                                          
026730     MOVE MFS-RENSA-FAELT TO MOD-KDBEH        (RAD-IX)                    
026750                             MOD-KDPRIBEH (RAD-IX)                        
027000     .                                                                    
027100     EJECT                                                                
027200 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
027300                                                                          
027530     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR     (RAD-IX)                   
027540                               MOD-PRARTBEL-PR (RAD-IX)                   
027550                               MOD-KDVALISO    (RAD-IX)                   
027560                               MOD-PRINK-AKT   (RAD-IX)                   
027570                               MOD-PRINK-KOM   (RAD-IX)                   
027580                               MOD-PRARTSTD-AKT (RAD-IX)                  
027581                               MOD-PRARTSTD-KOM (RAD-IX)                  
027591                               MOD-KDPRIBEH (RAD-IX)                      
028000     .                                                                    
028100     SKIP3                                                                
028200 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
028300                                                                          
028440     MOVE MFS-ROER-EJ-FAELT TO MOD-KDBEH        (RAD-IX)                  
028451                               MOD-KDPRIBEH (RAD-IX)                      
028700     .                                                                    
030306     EJECT                                                                
030310* --- IMS SEKTIONER ---                                                   
030400                                                                          
030500 IMS-GET-MSG SECTION.                                                     
030600                                                                          
030700     MOVE '  QC' TO GODK-STATUSKODER                                      
030800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031000     PERFORM IMS-STATUSKONTROLL                                           
031100     .                                                                    
031200     SKIP3                                                                
031300 IMS-INSERT-MSG SECTION.                                                  
031400                                                                          
031500     IF ENGLISH-TEXT                                                      
031600       MOVE '0' TO MFS-KDHUVOMR                                           
031700     END-IF                                                               
031800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031900     MOVE SPACE TO GODK-STATUSKODER                                       
032000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032200     PERFORM IMS-STATUSKONTROLL                                           
032300     .                                                                    
032500     SKIP3                                                                
032600 IMS-INSERT-ALT-MSG  SECTION.                                             
032610                                                                          
032615     MOVE SPACE TO GODK-STATUSKODER                                       
032616     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
032617     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
032618     PERFORM IMS-STATUSKONTROLL                                           
032619     .                                                                    
032620     EJECT                                                                
032621 IMS-GET-PRIG01      SECTION.                                             
032622                                                                          
032623     STRING 'WLPRIG01(WDH801KY>=' W-WDH801KY-MIN                          
032624            '&WDH801KY<=' W-WDH801KY-MAX ')'                              
032625          DELIMITED BY SIZE INTO SSA1                                     
032626     MOVE '  GE' TO GODK-STATUSKODER                                      
032627     CALL CBLTDLI USING GU PRIG-PCB DLI-IO-AREA SSA1                      
032628     MOVE PRIG-STATUS-CODE TO STATUS-WS                                   
032629     PERFORM IMS-STATUSKONTROLL                                           
032630     .                                                                    
032631     SKIP3                                                                
032632 IMS-GET-PRIG01-NEXT SECTION.                                             
032633                                                                          
032635     STRING 'WLPRIG01(WDH801KY>=' W-WDH801KY-MIN                          
032636            '&WDH801KY<=' W-WDH801KY-MAX ')'                              
032637          DELIMITED BY SIZE INTO SSA1                                     
032638     MOVE '  GEGB' TO GODK-STATUSKODER                                    
032639     CALL CBLTDLI USING GN PRIG-PCB DLI-IO-AREA SSA1                      
032640     MOVE PRIG-STATUS-CODE TO STATUS-WS                                   
032641     PERFORM IMS-STATUSKONTROLL                                           
032642     .                                                                    
032643     EJECT                                                                
032652 IMS-GET-PRIG01-HOLD  SECTION.                                            
032653                                                                          
032655     STRING 'WLPRIG01(WDH801KY>=' W-WDH801KY-MIN                          
032656            '&WDH801KY<=' W-WDH801KY-MAX                                  
032657            '&TIREGTID =' W-TIREGTID-X ')'                                
032658          DELIMITED BY SIZE INTO SSA1                                     
032659     MOVE '  GE' TO GODK-STATUSKODER                                      
032660     CALL CBLTDLI USING GHU PRIG-PCB DLI-IO-AREA SSA1                     
032661     MOVE PRIG-STATUS-CODE TO STATUS-WS                                   
032662     PERFORM IMS-STATUSKONTROLL                                           
032663     .                                                                    
032664     SKIP3                                                                
032665 IMS-REPL-PRIG01   SECTION.                                               
032670                                                                          
032682     MOVE '    ' TO GODK-STATUSKODER                                      
032683     CALL CBLTDLI USING REPL PRIG-PCB DLI-IO-AREA                         
032684     MOVE PRIG-STATUS-CODE TO STATUS-WS                                   
032685     PERFORM IMS-STATUSKONTROLL                                           
032686     .                                                                    
032687     EJECT                                                                
032690 IMS-STATUSKONTROLL SECTION.                                              
032700                                                                          
032800     SET STATUS-IX TO 1                                                   
032900     SEARCH GODK-STATUS                                                   
033000       AT END                                                             
033100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033200         DELIMITED BY SIZE INTO FELTEXT                                   
033300         CALL FELLOG                                                      
033400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033500         CONTINUE                                                         
033600     END-SEARCH                                                           
033700     .                                                                    
