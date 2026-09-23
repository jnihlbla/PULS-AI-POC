001300 ID DIVISION.                                                             
001500 PROGRAM-ID.     W5028600.                                                
001600 AUTHOR.         GUN LÖFGREN.                                             
001700 DATE-WRITTEN.   97/08.                                                   
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNKTION:                                                            
002100*        RAPPORTERA IN RE COUNT-UPPGIFTER FÖR                             
002110*        NDC:S ÅRSINVENTERING.                                            
002200*                                                                         
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W5T286                                              
002700*        MID:         W5I28601                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W5O28601                                            
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
003700 77  IDPGM                       PIC X(08)   VALUE 'W5028600'.            
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
005410 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005420     88  ALLT-OK                             VALUE 'J'.                   
005440                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '5286'.                
005700     88  GODK-MID                            VALUE '5281' '5282'          
005800                                                   '5283' '5284'          
005900                                                   '5285' '5286'.         
006200     88  HELP-MID                            VALUE '0551'.                
006301                                                                          
006302     EJECT                                                                
006310 01  DIVERSE.                                                             
006320     03  MAX-RAD-IX              PIC S9(9)   VALUE +13  COMP SYNC.        
006330     03  RAD-IX                  PIC S9(9)   VALUE ZERO COMP SYNC.        
006331     03  W-KVLS                  PIC X(7)    VALUE SPACE.                 
006332     03  WS-SEKTION              PIC X(32)   VALUE SPACE.                 
006333     03  WS-TIREGDAT             PIC X(6).                                
006334     03  WS-TIREGDAT-N REDEFINES WS-TIREGDAT                              
006335                                 PIC 9(6).                                
006336     03  WS-TIREGTID             PIC X(6).                                
006337     03  WS-TIREGTID-N REDEFINES WS-TIREGTID                              
006338                                 PIC 9(6).                                
006339     03  DAGENS-DATUM            PIC 9(6)    VALUE ZERO.                  
006340     03  DAGENS-TID              PIC 9(8)    VALUE ZERO.                  
006341     03  FILLER REDEFINES DAGENS-TID.                                     
006342      05 DAGENS-TID-6            PIC 9(6).                                
006343      05 FILLER                  PIC 9(2).                                
006344                                                                          
006380     SKIP3                                                                
006400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERELLA-SUBPROGRAM.                                                
006600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007300*01 -COPY WMEDAREA                                                        
007400     EJECT                                                                
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
009100*01  MID -COPY W5I28601                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W5O28601                                                 
009900     EJECT                                                                
010110 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010710                                                                          
011200 01  NYCKLAR-TILL-DLI.                                                    
011300*                                                                         
011399   03  W-WDJ7CSEQ-X.                                                      
011400     05  W-IDDC                  PIC X(2)    VALUE SPACE.                 
011401     05  W-IDUSER                PIC X(8)    VALUE SPACE.                 
011402     05  W-TIREGDAT-X.                                                    
011403      07 W-TIREGDAT              PIC S9(7)   COMP-3 VALUE ZERO.           
011404     05  W-TIREGTID-X.                                                    
011405      07 W-TIREGTID              PIC S9(7)   COMP-3 VALUE ZERO.           
011409                                                                          
011410   03  W-WDJ701KY-X.                                                      
011411     05  W-IDDC-P                PIC X(2)    VALUE SPACE.                 
011412     05  W-IDARTNR-P             PIC S9(9)   COMP-3 VALUE ZERO.           
011437                                                                          
011440     EJECT                                                                
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
013862*  03  -COPY WDJ701                                                       
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014200*01  -COPY W0009   -PRE MSG-                                              
014201                                                                          
014300*01  -COPY W0008   -PRE USEA-                                             
014400     05  FILLER                  PIC X.                                   
014500     EJECT                                                                
014505*01  -COPY W0008  -PRE ACS-                                               
014510     05  FILLER                  PIC X.                                   
014600     EJECT                                                                
014610*01  -COPY W0008  -PRE ACS2-                                              
014620     05  FILLER                  PIC X.                                   
014630     EJECT                                                                
014700                                                                          
014704 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ACS-PCB ACS2-PCB.             
014706 MAIN SECTION.                                                            
014710     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ACS-PCB ACS2-PCB.             
014800                                                                          
014900     MOVE 'MAIN'     TO WS-SEKTION                                        
015000     PERFORM IMS-GET-MSG                                                  
015100     IF SEGMENT-FINNS                                                     
015200       PERFORM A-INIT                                                     
015300       PERFORM B-KOLLA-NYCKLAR                                            
015400       IF NYCKLAR-OK                                                      
015500         IF MFS-UPDATE                                                    
015600           PERFORM G-KOLLA-INPUT                                          
015601           IF INDATA-OK = JA                                              
015602             PERFORM H-UPPDATERA                                          
015603             PERFORM F-LAES-VISA-INFO                                     
015604           END-IF                                                         
015700         ELSE                                                             
015710           PERFORM E-SAMMA-SIDA                                           
015800           IF ALLT-OK                                                     
015900             PERFORM F-LAES-VISA-INFO                                     
015910           END-IF                                                         
015912         END-IF                                                           
016000       END-IF                                                             
016100       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O28601 + 4                      
016200       PERFORM IMS-INSERT-MSG                                             
016300     END-IF                                                               
016500                                                                          
016600     MOVE ZERO TO RETURN-CODE                                             
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
017000 A-INIT SECTION.                                                          
017100                                                                          
017110     MOVE 'A-INIT'         TO WS-SEKTION                                  
017200     IF MSG-DUBBLA-TRANSKODER                                             
017300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I28601                 
017400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017600     ELSE                                                                 
017700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I28601                  
017800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018000     END-IF                                                               
018100                                                                          
018200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018500                                                                          
018600     MOVE LOW-VALUE TO MSG-AREA                                           
018700     MOVE 'W5O286N1' TO MFS-IDMOD                                         
018800     MOVE '5286' TO MOD-IDTRANS                                           
018900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019000                                                                          
019500     IF EGEN-MID OR HELP-MID                                              
019600       CONTINUE                                                           
019700     ELSE                                                                 
019800       MOVE SPACE TO MFS-KDTRTYP                                          
019900       MOVE '7' TO MFS-IDPFK                                              
020000     END-IF                                                               
020001                                                                          
020010     ACCEPT DAGENS-DATUM  FROM DATE                                       
020011     ACCEPT DAGENS-TID    FROM TIME                                       
020100                                                                          
020300     .                                                                    
020400     EJECT                                                                
020500 B-KOLLA-NYCKLAR SECTION.                                                 
020510                                                                          
020511     MOVE 'B-KOLLA-NYCKLAR' TO WS-SEKTION                                 
020513     MOVE JA                TO NYCKLAR-SW                                 
020515                                                                          
020520     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020530     MOVE '001'             TO MSGI-KDCALL                                
020540     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020541     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020542     MOVE '5286'            TO MSGI-IDTRANS                               
020550                                                                          
020560     IF EGEN-MID                                                          
020570       MOVE MID-IDUSER-UT        TO MOD-IDUSER-UT                         
020571       MOVE MID-TIREGDAT-UT      TO MOD-TIREGDAT-UT                       
020572       MOVE MID-TIREGTID-UT      TO MOD-TIREGTID-UT                       
020590       IF MID-IDUSER-IN NOT = ALL '+'                                     
020592         MOVE MID-IDUSER-IN      TO MOD-IDUSER-UT                         
020593         MOVE MID-IDUSER-IN      TO W-IDUSER                              
020594       END-IF                                                             
020596       IF MID-TIREGDAT-IN NOT = ALL '+'                                   
020597         MOVE MID-TIREGDAT-IN    TO MOD-TIREGDAT-UT                       
020598       END-IF                                                             
020599       IF MID-TIREGTID-IN NOT = ALL '+'                                   
020600         MOVE MID-TIREGTID-IN    TO MOD-TIREGTID-UT                       
020602       END-IF                                                             
020603     END-IF                                                               
020604                                                                          
020606     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020607                                                                          
020609     MOVE MFS-RENSA-FAELT TO MOD-IDUSER-IN                                
020610     MOVE MFS-RENSA-FAELT TO MOD-TIREGDAT-IN                              
020611     MOVE MFS-RENSA-FAELT TO MOD-TIREGTID-IN                              
020612                                                                          
020613     MOVE MSGI-IDDC       TO W-IDDC                                       
020614     MOVE MOD-IDUSER-UT   TO W-IDUSER                                     
020620                                                                          
020697     IF MOD-TIREGDAT-UT   NUMERIC                                         
020698       IF MOD-TIREGDAT-UT   > ZERO                                        
020699         MOVE MOD-TIREGDAT-UT  TO WS-TIREGDAT                             
020700         MOVE WS-TIREGDAT-N    TO W-TIREGDAT                              
020701       ELSE                                                               
020702         MOVE NEJ          TO NYCKLAR-SW                                  
020703       END-IF                                                             
020704     ELSE                                                                 
020705       MOVE NEJ  TO NYCKLAR-SW                                            
020706     END-IF                                                               
020707                                                                          
020709     IF MOD-TIREGTID-UT   NUMERIC                                         
020710       IF MOD-TIREGTID-UT   > ZERO                                        
020711         MOVE MOD-TIREGTID-UT  TO WS-TIREGTID                             
020712         MOVE WS-TIREGTID-N    TO W-TIREGTID                              
020713       ELSE                                                               
020714         MOVE NEJ          TO NYCKLAR-SW                                  
020715       END-IF                                                             
020716     ELSE                                                                 
020717       MOVE NEJ  TO NYCKLAR-SW                                            
020718     END-IF                                                               
020719                                                                          
020732     IF GODK-MID OR NYCKLAR-OK                                            
020733       CONTINUE                                                           
020740     ELSE                                                                 
020750       MOVE MFS-RENSA-FAELT TO MOD-IDUSER-UT                              
020751                               MOD-TIREGDAT-UT                            
020752                               MOD-TIREGTID-UT                            
020760     END-IF                                                               
020761                                                                          
020780     IF NYCKLAR-FEL                                                       
020781       MOVE 'GB'          TO MED-IDSKYLT                                  
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
020804                                                                          
020805     .                                                                    
020806     EJECT                                                                
020810                                                                          
021496 E-SAMMA-SIDA SECTION.                                                    
021497                                                                          
021498     MOVE 'E-SAMMA-SIDA'      TO WS-SEKTION                               
021499                                                                          
021522     IF MID-KVLS-IN  (1)     = ALL '+'                                    
021523       AND MID-KVLS-IN  (2)  = ALL '+'                                    
021524       AND MID-KVLS-IN  (3)  = ALL '+'                                    
021525       AND MID-KVLS-IN  (4)  = ALL '+'                                    
021526       AND MID-KVLS-IN  (5)  = ALL '+'                                    
021527       AND MID-KVLS-IN  (6)  = ALL '+'                                    
021528       AND MID-KVLS-IN  (7)  = ALL '+'                                    
021529       AND MID-KVLS-IN  (8)  = ALL '+'                                    
021530       AND MID-KVLS-IN  (9)  = ALL '+'                                    
021531       AND MID-KVLS-IN  (10) = ALL '+'                                    
021532       AND MID-KVLS-IN  (11) = ALL '+'                                    
021533       AND MID-KVLS-IN  (12) = ALL '+'                                    
021534       AND MID-KVLS-IN  (13) = ALL '+'                                    
021535         MOVE JA TO ALLT-SW                                               
021536     ELSE                                                                 
021559       MOVE +1 TO RAD-IX                                                  
021560       PERFORM UNTIL (RAD-IX > MAX-RAD-IX)  OR                            
021561                     (MID-IDARTNR (RAD-IX) =  ALL '+')                    
021562         IF MID-KVLS-IN (RAD-IX) NOT = ALL '+'                            
021566           MOVE MID-KVLS-IN (RAD-IX) TO MOD-KVLS (RAD-IX)                 
021567         ELSE                                                             
021568           MOVE MFS-RENSA-FAELT      TO MOD-KVLS (RAD-IX)                 
021570         END-IF                                                           
021574         PERFORM MFS-ROER-EJ-FAELT-UT                                     
021576         ADD +1 TO RAD-IX                                                 
021578       END-PERFORM                                                        
021580       PERFORM UNTIL (RAD-IX > MAX-RAD-IX)                                
021584         MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR (RAD-IX)                
021585         MOVE MFS-RENSA-FAELT      TO MOD-KVLS (RAD-IX)                   
021587         ADD +1 TO RAD-IX                                                 
021588       END-PERFORM                                                        
021590                                                                          
021720       MOVE NEJ TO ALLT-SW                                                
021721       MOVE 'GB'           TO MED-IDSKYLT                                 
021730       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
021740       CALL WMEDKONV USING MED-WMEDAREA                                   
021750       MOVE MED-MFSINF TO MOD-TEMFSFEL                                    
021794     END-IF                                                               
021795                                                                          
021799     .                                                                    
021800     EJECT                                                                
021900 F-LAES-VISA-INFO SECTION.                                                
021901                                                                          
021910     MOVE 'F-LAES-VISA-INFO'  TO WS-SEKTION                               
022000                                                                          
023466     PERFORM IMS-GET-ACS-NEXT                                             
023467     IF SEGMENT-FINNS                                                     
023468       MOVE +1 TO RAD-IX                                                  
023469       PERFORM UNTIL RAD-IX > MAX-RAD-IX  OR SEGMENT-SAKNAS               
023470         IF ACS-IDUSER-RCOUNT-REG = SPACE                                 
023471           PERFORM FA-LAEGG-UT-RAD                                        
023472           ADD +1 TO RAD-IX                                               
023473         END-IF                                                           
023474         PERFORM IMS-GET-ACS-NEXT                                         
023475       END-PERFORM                                                        
023476       PERFORM UNTIL RAD-IX > MAX-RAD-IX                                  
023478         PERFORM MFS-RENSA-FAELT-UT                                       
023479         MOVE MFS-RENSA-FAELT  TO MOD-KVLS (RAD-IX)                       
023480         ADD +1  TO RAD-IX                                                
023481       END-PERFORM                                                        
023482     ELSE                                                                 
023483       MOVE +1 TO RAD-IX                                                  
023484       PERFORM UNTIL RAD-IX > MAX-RAD-IX                                  
023485         PERFORM MFS-RENSA-FAELT-UT                                       
023486         ADD +1  TO RAD-IX                                                
023487       END-PERFORM                                                        
023488       MOVE 'GB'             TO MED-IDSKYLT                               
023489       MOVE ERR-PART-MISSING TO MED-IDMFSFEL                              
023490       CALL WMEDKONV USING MED-WMEDAREA                                   
023491       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
023492     END-IF                                                               
023500                                                                          
023515                                                                          
023528     MOVE '002'  TO MSGI-KDCALL                                           
023541     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023542                                                                          
023547     .                                                                    
023548     EJECT                                                                
023549 FA-LAEGG-UT-RAD  SECTION.                                                
023550                                                                          
023551     MOVE 'FA-LAEGG-UT-RAD'   TO WS-SEKTION                               
023552                                                                          
023553     MOVE ACS-ADLAGOMR        TO MOD-ADLAGOMR   (RAD-IX)                  
023558     MOVE ACS-ADGANG          TO MOD-ADGANG     (RAD-IX)                  
023559     MOVE ACS-ADPLATS         TO MOD-ADPLATS    (RAD-IX)                  
023560     MOVE ACS-IDARTNR         TO MOD-IDARTNR    (RAD-IX)                  
023561     MOVE ACS-BEART           TO MOD-BEART      (RAD-IX)                  
023564     MOVE MFS-OPEN-ALPHA-NOMOD  TO MOD-KVLS-ATTR  (RAD-IX)                
023565     MOVE MFS-ADD-SAETT-CURSOR  TO MOD-KVLS-ATTR  (1)                     
023566     MOVE MFS-RENSA-FAELT     TO MOD-KVLS       (RAD-IX)                  
023567                                                                          
024300     .                                                                    
024400     EJECT                                                                
024500 G-KOLLA-INPUT     SECTION.                                               
024600                                                                          
024601     MOVE 'G-KOLLA-INPUT'     TO WS-SEKTION                               
024610     MOVE JA   TO INDATA-OK                                               
024700     MOVE +1   TO RAD-IX                                                  
024800     PERFORM UNTIL (RAD-IX > MAX-RAD-IX) OR                               
024801                   (MID-IDARTNR (RAD-IX) = ALL '+')                       
024810       IF MID-KVLS-IN (RAD-IX) = ALL '+'                                  
024960         MOVE MFS-NUM-FAELT-FEL   TO                                      
024970                      MOD-KVLS-ATTR (RAD-IX)                              
024980         MOVE MFS-RENSA-FAELT     TO MOD-KVLS (RAD-IX)                    
024990         MOVE NEJ TO INDATA-OK                                            
024991       ELSE                                                               
024992         INSPECT MID-KVLS-IN (RAD-IX) REPLACING LEADING                   
024993         SPACE BY ZERO                                                    
024994         IF MID-KVLS-IN (RAD-IX) NUMERIC                                  
024995           MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVLS-ATTR (RAD-IX)            
024996           MOVE MID-KVLS-IN (RAD-IX)  TO MOD-KVLS (RAD-IX)                
024997         ELSE                                                             
024998           MOVE MFS-NUM-FAELT-FEL   TO                                    
024999                        MOD-KVLS-ATTR (RAD-IX)                            
025000           MOVE MFS-ROER-EJ-FAELT   TO MOD-KVLS (RAD-IX)                  
025001           MOVE NEJ TO INDATA-OK                                          
025002         END-IF                                                           
025003       END-IF                                                             
025004       PERFORM MFS-ROER-EJ-FAELT-UT                                       
025005       ADD 1   TO RAD-IX                                                  
025006     END-PERFORM                                                          
025009                                                                          
025010     PERFORM UNTIL (RAD-IX > MAX-RAD-IX)                                  
025011       MOVE MFS-RENSA-FAELT     TO MOD-KVLS    (RAD-IX)                   
025012       MOVE MFS-RENSA-FAELT     TO MOD-IDARTNR (RAD-IX)                   
025013       ADD 1   TO RAD-IX                                                  
025014     END-PERFORM                                                          
025015                                                                          
025016     IF INDATA-OK = JA                                                    
025017       CONTINUE                                                           
025018     ELSE                                                                 
025019       MOVE 'GB'          TO MED-IDSKYLT                                  
025020       MOVE ERR-DATA      TO MED-IDMFSFEL                                 
025021       CALL WMEDKONV USING MED-WMEDAREA                                   
025022       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
025023     END-IF                                                               
025024     MOVE NEJ TO ALLT-SW                                                  
025100     .                                                                    
025200     EJECT                                                                
025300 H-UPPDATERA       SECTION.                                               
025310                                                                          
025311     MOVE 'H-UPPDATERA'       TO WS-SEKTION                               
025320     MOVE 1     TO RAD-IX                                                 
025330     PERFORM UNTIL RAD-IX > MAX-RAD-IX  OR                                
025340                   MID-IDARTNR (RAD-IX) = ALL '+'                         
025341       INSPECT MID-IDARTNR (RAD-IX) REPLACING                             
025342                 LEADING SPACE BY ZERO                                    
025344       MOVE MID-IDARTNR   (RAD-IX) TO W-IDARTNR-P                         
025345       MOVE MSGI-IDDC              TO W-IDDC-P                            
025346       PERFORM IMS-GET-ACS-HOLD                                           
025348       INSPECT MID-KVLS-IN (RAD-IX) REPLACING                             
025349                        LEADING SPACE BY ZERO                             
025350       MOVE MID-KVLS-IN (RAD-IX) TO W-KVLS                                
025351       MOVE W-KVLS               TO ACS-KVRCOUNT                          
025352       MOVE MSGI-IDUSER          TO ACS-IDUSER-RCOUNT-REG                 
025353       MOVE DAGENS-DATUM         TO ACS-TIREGDAT-RCOUNT-REG               
025354       MOVE DAGENS-TID-6         TO ACS-TIREGTID-RCOUNT-REG               
025355       PERFORM IMS-REPL-ACS                                               
025356       MOVE 'GB'                 TO MED-IDSKYLT                           
025357       MOVE INF-UPDATE-DONE      TO MED-IDMFSINF                          
025358       CALL WMEDKONV USING MED-WMEDAREA                                   
025359       MOVE MED-MFSINF           TO MOD-TEMFSINF                          
025375       ADD 1                     TO RAD-IX                                
025377     END-PERFORM                                                          
025378                                                                          
025380     .                                                                    
025585     EJECT                                                                
025590 MFS-RENSA-FAELT-UT SECTION.                                              
025600                                                                          
025900     MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR  (RAD-IX)                       
026000                             MOD-ADGANG    (RAD-IX)                       
026100                             MOD-ADPLATS   (RAD-IX)                       
026110                             MOD-IDARTNR   (RAD-IX)                       
026120                             MOD-KVLS      (RAD-IX)                       
026130                             MOD-BEART     (RAD-IX)                       
026200     .                                                                    
026400     SKIP3                                                                
026500 MFS-RENSA-FAELT-IN SECTION.                                              
026600                                                                          
026730     MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR (RAD-IX)                        
026750                             MOD-ADGANG   (RAD-IX)                        
026760                             MOD-ADPLATS  (RAD-IX)                        
026770                             MOD-IDARTNR  (RAD-IX)                        
026780                             MOD-KVLS     (RAD-IX)                        
027000     .                                                                    
027100     EJECT                                                                
027200 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
027300                                                                          
027530     MOVE MFS-ROER-EJ-FAELT TO MOD-ADLAGOMR  (RAD-IX)                     
027540                               MOD-ADGANG    (RAD-IX)                     
027550                               MOD-ADPLATS   (RAD-IX)                     
027560                               MOD-IDARTNR   (RAD-IX)                     
027580                               MOD-BEART     (RAD-IX)                     
028000     .                                                                    
028100     SKIP3                                                                
028200 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
028300                                                                          
028440     MOVE MFS-ROER-EJ-FAELT TO MOD-ADLAGOMR     (RAD-IX)                  
028451                               MOD-ADGANG   (RAD-IX)                      
028452                               MOD-ADPLATS  (RAD-IX)                      
028453                               MOD-IDARTNR  (RAD-IX)                      
028454                               MOD-KVLS     (RAD-IX)                      
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
031600       MOVE 'N' TO MFS-KDHUVOMR                                           
031700     END-IF                                                               
031800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031900     MOVE SPACE TO GODK-STATUSKODER                                       
032000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032200     PERFORM IMS-STATUSKONTROLL                                           
032300     .                                                                    
032500     SKIP3                                                                
032621 IMS-GET-ACS   SECTION.                                                   
032622                                                                          
032623     STRING 'WDJ701  (WDJ7CSEQ =' W-WDJ7CSEQ-X ')'                        
032625          DELIMITED BY SIZE INTO SSA1                                     
032626     MOVE '  GE' TO GODK-STATUSKODER                                      
032627     CALL CBLTDLI USING GU ACS-PCB DLI-IO-AREA SSA1                       
032628     MOVE ACS-STATUS-CODE TO STATUS-WS                                    
032629     PERFORM IMS-STATUSKONTROLL                                           
032630     .                                                                    
032631     SKIP3                                                                
032632 IMS-GET-ACS-NEXT SECTION.                                                
032633                                                                          
032635     STRING 'WDJ701  (WDJ7CSEQ =' W-WDJ7CSEQ-X ')'                        
032637          DELIMITED BY SIZE INTO SSA1                                     
032638     MOVE '  GEGB' TO GODK-STATUSKODER                                    
032639     CALL CBLTDLI USING GN ACS-PCB DLI-IO-AREA SSA1                       
032640     MOVE ACS-STATUS-CODE TO STATUS-WS                                    
032641     PERFORM IMS-STATUSKONTROLL                                           
032642     .                                                                    
032643     EJECT                                                                
032652 IMS-GET-ACS-HOLD  SECTION.                                               
032653                                                                          
032655     STRING 'WDJ701  (WDJ701KY =' W-WDJ701KY-X ')'                        
032658          DELIMITED BY SIZE INTO SSA1                                     
032659     MOVE '    ' TO GODK-STATUSKODER                                      
032660     CALL CBLTDLI USING GHU ACS2-PCB DLI-IO-AREA SSA1                     
032661     MOVE ACS2-STATUS-CODE TO STATUS-WS                                   
032662     PERFORM IMS-STATUSKONTROLL                                           
032663     .                                                                    
032664     SKIP3                                                                
032665 IMS-REPL-ACS      SECTION.                                               
032670                                                                          
032682     MOVE '    ' TO GODK-STATUSKODER                                      
032683     CALL CBLTDLI USING REPL ACS2-PCB DLI-IO-AREA                         
032684     MOVE ACS2-STATUS-CODE TO STATUS-WS                                   
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
