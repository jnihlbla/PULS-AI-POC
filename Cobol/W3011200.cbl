000100 PROCESS DYNAM                                                            
001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W3011200.                                                
001500 AUTHOR.         JONNY SANDSTEN.                                          
001600 DATE-WRITTEN.   98/02/27.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
001901*                                                                         
001910*        FRÅGE- OCH UPPDATERINGSBILD. GENOM ATT ANGE ARTIKELNR            
001920*        KAN MAN FÅ FRAM RADIOTYP OCH BESKRIVNING                         
001930*                                                                         
001940*        PROGRAMMET UPPDATERAR WL3111 (WDGX3112)                          
001950*                                                                         
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W3T112                                              
002600*        MID:         W3I11201                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W3O11201                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W3011200'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004700                                                                          
004801 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004802     88  INDATA-OK                           VALUE 'J'.                   
004810     88  INDATA-FEL                          VALUE 'N'.                   
004900                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005201                                                                          
005202 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005203     88  ALLT-OK                             VALUE 'J'.                   
005205                                                                          
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '3112'.                
005600     88  GODK-MID                            VALUE '3111' '3112'          
005700                                                   '3113' '3114'          
005800                                                   '3115' '3116'          
005900                                                   '3117' '3118'          
006000                                                   '3119'.                
006100     88  HELP-MID                            VALUE '0551'.                
006200     EJECT                                                                
006201                                                                          
006202 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
006203 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
006204 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
006205 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
006206 01  FILLER                  PIC X(16)   VALUE 'WS-DB2-SEKTION'.          
006207 01  WS-DB2-SEKTION              PIC X(30)   VALUE SPACE.                 
006208 01  FILLER                  PIC X(16)   VALUE 'WS-FIL-SEKTION'.          
006209 01  WS-FIL-SEKTION              PIC X(30)   VALUE SPACE.                 
006210                                                                          
006211*    ---GENERELLA ARBETSFÄLT                                              
006220 01  WS-IDARTNR                  PIC X(10)   VALUE SPACE.                 
006221 01  WS-IDARTNR-RED              PIC X(10)   VALUE SPACE.                 
006230     EJECT                                                                
006300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007501     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007503     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007504     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007510     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007520     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007800     03  ART-MISSING             PIC X(3)    VALUE '017'.                 
007820     03  INFO-SAKNAS             PIC X(3)    VALUE '413'.                 
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008500     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W3I11201                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W3O11201                                                 
009900     EJECT                                                                
009910*        ARBETS-AREOR TILL DB2- OCH IMS-SEKTIONERNA                       
009920*                                                                         
009930 01  FILLER                  PIC X(16)   VALUE 'DB2-WS     '.             
009940*01  -COPY WRADIOT -PRE RADIO-                                            
009960     EJECT                                                                
009970 01  FILLER                  PIC X(16) VALUE 'WRADIOT-AREA'.              
009980       EXEC SQL INCLUDE WRADIOT END-EXEC.                                 
009990     SKIP3                                                                
009991 01  FILLER                  PIC X(16) VALUE 'SQLCA-AREA'.                
009992       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009993*                        **** STATUS-KOD FRÅN DB2                         
009994 01  FILLER                     PIC X(16) VALUE 'SQLCODE-WS'.             
009995 01  DB2-WS.                                                              
009996   03  SQLCODE-WS                PIC 9(3) VALUE ZERO.                     
009997     88  CURSOR-OK                           VALUE 000.                   
009998     88  RADER-FINNS                         VALUE 000.                   
009999     88  RADER-SAKNAS                        VALUE 100.                   
010000     88  904-KOD                             VALUE 904.                   
010001     SKIP1                                                                
010002   03  GODK-SQLCODESKODER.                                                
010003     05  GODK-SQLCODE OCCURS 5                                            
010004         INDEXED BY SQLCODE-IX PIC 999.                                   
010005     EJECT                                                                
010006 01  FILLER                     PIC X(16) VALUE 'TEST-SQLCODE'.           
010007 01  TEST-SQLCODE               PIC 9(3) VALUE ZERO.                      
010010 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100     SKIP3                                                                
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010710 77  W-IDARTNR-RADIO             PIC S9(10) COMP-3  VALUE ZERO.           
010800     SKIP3                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011001     03  W-WDGXKEY-X.                                                     
011002         05  W-IDHTYP            PIC X(4)     VALUE '3111'.               
011003         05  W-WDGXKEY           PIC X(26)    VALUE LOW-VALUE.            
011004     03  W-IDARTNR-X.                                                     
011010         05  W-IDARTNR           PIC 9(10)     VALUE ZERO.                
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(64).                               
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900                                                                          
013001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL311101'.                    
013002 01  DLI-IO-WL311101.                                                     
013003*    03  -COPY WDGX3112 -PRE WL311101-                                    
013004     EJECT                                                                
013005 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL311111'.                    
013006 01  DLI-IO-WL311111.                                                     
013010*    03  -COPY WDGX3112 -PRE WL311111-                                    
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013802*01  -COPY W0008   -PRE USEA-                                             
013803     05  FILLER                  PIC X.                                   
013804                                                                          
013805*01  -COPY W0008  -PRE 3111-                                              
013810     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014001 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB 3111-PCB.                     
014002 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB 3111-PCB.                     
014100                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FINNS                                                     
014500       PERFORM A-INIT                                                     
014600       PERFORM B-KOLLA-NYCKLAR                                            
014700       IF NYCKLAR-OK                                                      
014801         IF MFS-UPDATE                                                    
014802           PERFORM G-KOLLA-INPUT                                          
014803           IF INDATA-OK                                                   
014804             PERFORM H-UPPDATERA                                          
014805           END-IF                                                         
014810         ELSE                                                             
015001           IF MFS-FIRST                                                   
015002             PERFORM C-FOERSTA-SIDA                                       
015003           ELSE                                                           
015004             PERFORM E-SAMMA-SIDA                                         
015010           END-IF                                                         
015110         END-IF                                                           
015130         IF INDATA-OK                                                     
015200            PERFORM F-LAES-VISA-INFO                                      
015210         END-IF                                                           
015300       END-IF                                                             
015600       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O11201 + 4                      
015700       PERFORM IMS-INSERT-MSG                                             
015800     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016510     MOVE 'A-INIT'  TO WS-SEKTION                                         
016520*    DISPLAY WS-SEKTION                                                   
016600                                                                          
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I11201                 
016900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I11201                  
017300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE TO MSG-AREA                                           
018200     MOVE 'W3O112N1' TO MFS-IDMOD                                         
018300     MOVE '3112' TO MOD-IDTRANS                                           
018400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018500                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE TO MFS-KDTRTYP                                          
019000       MOVE '7' TO MFS-IDPFK                                              
019100     END-IF                                                               
019110                                                                          
019120     INITIALIZE GODK-SQLCODESKODER                                        
019400     .                                                                    
019500     EJECT                                                                
019600 B-KOLLA-NYCKLAR SECTION.                                                 
019610     MOVE 'B-KOLLA-NYCKLAR'  TO WS-SEKTION                                
019620*    DISPLAY WS-SEKTION                                                   
019700                                                                          
019800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
019900     MOVE '001'             TO MSGI-KDCALL                                
020000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020200     MOVE '3112'            TO MSGI-IDTRANS                               
020300                                                                          
020750     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020751                                                                          
020752     IF ENGLISH-TEXT                                                      
020753       MOVE 'GB ' TO MED-IDSKYLT                                          
020754     ELSE                                                                 
020755       MOVE 'S  ' TO MED-IDSKYLT                                          
020756     END-IF                                                               
020760                                                                          
020800     MOVE JA TO NYCKLAR-SW                                                
020801     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
020803                                                                          
020804     IF GODK-MID                                                          
020806       IF MID-IDARTNR-IN = ALL '+'                                        
020807          MOVE MID-IDARTNR-UT TO WS-IDARTNR                               
020808          INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO              
020809       ELSE                                                               
020810         MOVE MID-IDARTNR-IN TO WS-IDARTNR                                
020811         INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO               
020812         MOVE SPACE TO MFS-KDTRTYP                                        
020813         MOVE '7' TO MFS-IDPFK                                            
020820       END-IF                                                             
020900                                                                          
021000       IF WS-IDARTNR NUMERIC AND WS-IDARTNR > 0                           
021100          MOVE WS-IDARTNR TO W-IDARTNR                                    
021110                             W-IDARTNR-RADIO                              
021200       ELSE                                                               
021210          MOVE NEJ TO NYCKLAR-SW                                          
021220       END-IF                                                             
021221     ELSE                                                                 
021222       MOVE NEJ TO NYCKLAR-SW                                             
021224     END-IF                                                               
021225                                                                          
021230                                                                          
021240     IF GODK-MID OR NYCKLAR-OK                                            
021250        MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                 
021251        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
021260     ELSE                                                                 
021270        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                            
021280     END-IF                                                               
021290                                                                          
021300     IF NYCKLAR-FEL                                                       
021310*---FÖR ATT INTE FÅ NYCKLAR FEL NÄR MAN KOMMER FRÅN EN ICKE               
021320*---GODKÄND BILD                                                          
021330       IF GODK-MID                                                        
021400         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
021500         CALL WMEDKONV USING MED-WMEDAREA                                 
021600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
021700         PERFORM MFS-RENSA-FAELT-IN                                       
021800         PERFORM MFS-RENSA-FAELT-UT                                       
021900       END-IF                                                             
021910     END-IF                                                               
022000     .                                                                    
022200     EJECT                                                                
022392 C-FOERSTA-SIDA SECTION.                                                  
022393     MOVE 'C-FOERSTA-SIDA'  TO WS-SEKTION                                 
022394*    DISPLAY WS-SEKTION                                                   
022395                                                                          
022396     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
022397     CALL WMEDKONV USING MED-WMEDAREA                                     
022398     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
022399                                                                          
022400     PERFORM MFS-RENSA-FAELT-IN                                           
022401     .                                                                    
022402     EJECT                                                                
022403 E-SAMMA-SIDA SECTION.                                                    
022404     MOVE 'E-SAMMA-SIDA'  TO WS-SEKTION                                   
022405*    DISPLAY WS-SEKTION                                                   
022406                                                                          
022407     IF MID-IDAPPTYP = ALL '+'                                            
022408     AND MID-BEAPPTYP = ALL '+'                                           
022409     AND MID-BELEVART = ALL '+'                                           
022410       PERFORM MFS-RENSA-FAELT-IN                                         
022411     ELSE                                                                 
022412       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
022413       CALL WMEDKONV USING MED-WMEDAREA                                   
022414       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
022417       PERFORM MFS-RENSA-FAELT-IN                                         
022419     END-IF                                                               
022420     .                                                                    
022430     EJECT                                                                
022440 F-LAES-VISA-INFO SECTION.                                                
022450     MOVE 'F-LAES-VISA-INFO'  TO WS-SEKTION                               
022460*    DISPLAY WS-SEKTION                                                   
022500                                                                          
022672     PERFORM DB2-SELECT-RADIOT-TAB                                        
022673     IF SQLCODE = ZERO                                                    
022674        MOVE RADIO-APPARATTYP           TO MOD-IDAPPTYP                   
022675        MOVE RADIO-BEAPTYP              TO MOD-BEAPPTYP                   
022676        MOVE RADIO-IDARTNR-LABEL        TO MOD-BELEVART                   
022678     ELSE                                                                 
022679        MOVE ART-MISSING TO MED-IDMFSINF                                  
022680        CALL WMEDKONV USING MED-WMEDAREA                                  
022681        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
022690     END-IF                                                               
023600     .                                                                    
023700     EJECT                                                                
024602 G-KOLLA-INPUT SECTION.                                                   
024603     MOVE 'G-KOLLA-INPUT'  TO WS-SEKTION                                  
024604*    DISPLAY WS-SEKTION                                                   
024605                                                                          
024606     MOVE JA  TO INDATA-SW                                                
024607     IF MID-IDAPPTYP = ALL '+'                                            
024608     AND MID-BEAPPTYP = ALL '+'                                           
024609     AND MID-BELEVART = ALL '+'                                           
024610       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
024611       CALL WMEDKONV USING MED-WMEDAREA                                   
024612       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024613       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024614       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024615       MOVE NEJ TO INDATA-SW                                              
024714     END-IF                                                               
024715     .                                                                    
024716     EJECT                                                                
024717 H-UPPDATERA SECTION.                                                     
024718     MOVE 'H-UPPDATERA'  TO WS-SEKTION                                    
024719*    DISPLAY WS-SEKTION                                                   
024720                                                                          
024722     PERFORM DB2-SELECT-RADIOT-TAB                                        
024723     IF SQLCODE = ZERO                                                    
024724                                                                          
024725******************************************************************        
024726**** FÖRÄNDRING AV RAD I WRADIOT             *********************        
024727******************************************************************        
024728                                                                          
024740       IF MID-IDAPPTYP = ALL '+'                                          
024751         MOVE RADIO-APPARATTYP TO MID-IDAPPTYP                            
024752         MOVE MFS-ALFA-FAELT-RAETT TO                                     
024753                                     MOD-IDAPPTYP-ATTR                    
024754       ELSE                                                               
024755         MOVE MFS-ALFA-FAELT-RAETT TO                                     
024756                                     MOD-IDAPPTYP-ATTR                    
024757       END-IF                                                             
024758                                                                          
024760       IF MID-BEAPPTYP = ALL '+'                                          
024761         MOVE RADIO-BEAPTYP  TO MID-BEAPPTYP                              
024762         MOVE MFS-ALFA-FAELT-RAETT TO                                     
024763                                   MOD-BEAPPTYP-ATTR                      
024764       ELSE                                                               
024765         MOVE MFS-ALFA-FAELT-RAETT TO                                     
024766                                   MOD-BEAPPTYP-ATTR                      
024767       END-IF                                                             
024768                                                                          
024770       IF MID-BELEVART NOT = ALL '+'                                      
024771          INSPECT MID-BELEVART REPLACING LEADING SPACE BY ZERO            
024773          MOVE MID-BELEVART    TO WS-IDARTNR-RED                          
024774                                  MOD-BELEVART                            
024777          IF WS-IDARTNR-RED NUMERIC AND WS-IDARTNR-RED > 0                
024778             MOVE WS-IDARTNR-RED     TO RADIO-IDARTNR-LABEL               
024779             MOVE MFS-ALFA-FAELT-RAETT TO                                 
024780                                     MOD-BELEVART-ATTR                    
024781          ELSE                                                            
024782             MOVE MFS-ALFA-FAELT-FEL TO                                   
024783                                     MOD-BELEVART-ATTR                    
024784             MOVE NEJ TO INDATA-SW                                        
024785          END-IF                                                          
024786       END-IF                                                             
024787                                                                          
024788       IF INDATA-OK                                                       
024789           MOVE MID-IDAPPTYP   TO RADIO-APPARATTYP                        
024790           MOVE MID-BEAPPTYP   TO RADIO-BEAPTYP                           
024791           PERFORM DB2-UPDATE-WRADIOT                                     
024792       END-IF                                                             
024793     ELSE                                                                 
024794                                                                          
024796******************************************************************        
024797**** FÖRSTA GÅNGEN RADEN LÄGGS UPP I WRADIOT *********************        
024798******************************************************************        
024799                                                                          
024800        MOVE WS-IDARTNR     TO RADIO-IDARTNR-RADIO                        
024801                                                                          
024802        IF  MID-BEAPPTYP NOT = ALL '+'                                    
024803            MOVE MID-BEAPPTYP   TO RADIO-BEAPTYP                          
024804            MOVE MFS-ALFA-FAELT-RAETT TO                                  
024805                                     MOD-BEAPPTYP-ATTR                    
024806        ELSE                                                              
024807            MOVE SPACE          TO RADIO-BEAPTYP                          
024808        END-IF                                                            
024809                                                                          
024810        IF MID-IDAPPTYP NOT = ALL '+'                                     
024811           MOVE MID-IDAPPTYP   TO RADIO-APPARATTYP                        
024812           MOVE MFS-ALFA-FAELT-RAETT TO                                   
024813                                     MOD-IDAPPTYP-ATTR                    
024814        ELSE                                                              
024815           MOVE NEJ TO INDATA-SW                                          
024816           MOVE MFS-ALFA-FAELT-FEL TO                                     
024817                                     MOD-IDAPPTYP-ATTR                    
024818        END-IF                                                            
024819                                                                          
024822        IF MID-BELEVART NOT = ALL '+'                                     
024823          INSPECT MID-BELEVART REPLACING LEADING SPACE BY ZERO            
024825          MOVE MID-BELEVART    TO WS-IDARTNR-RED                          
024826                                  MOD-BELEVART                            
024829          IF WS-IDARTNR-RED NUMERIC AND WS-IDARTNR-RED > 0                
024830             MOVE WS-IDARTNR-RED TO RADIO-IDARTNR-LABEL                   
024831             MOVE MFS-ALFA-FAELT-RAETT TO                                 
024832                                     MOD-BELEVART-ATTR                    
024833          ELSE                                                            
024835             MOVE MFS-ALFA-FAELT-FEL TO                                   
024836                                     MOD-BELEVART-ATTR                    
024837             MOVE NEJ TO INDATA-SW                                        
024838          END-IF                                                          
024839        ELSE                                                              
024840           MOVE NEJ TO INDATA-SW                                          
024841           MOVE MFS-ALFA-FAELT-FEL TO                                     
024842                                     MOD-BELEVART-ATTR                    
024843        END-IF                                                            
024844        IF INDATA-OK                                                      
024845           PERFORM DB2-ISRT-WRADIOT                                       
024846        END-IF                                                            
024847     END-IF                                                               
024848                                                                          
024849     IF INDATA-OK                                                         
024850        MOVE INF-UPDATE-DONE TO MED-IDMFSINF                              
024851        CALL WMEDKONV USING MED-WMEDAREA                                  
024852        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
024853        PERFORM MFS-FORM-ATTR                                             
024854        PERFORM MFS-RENSA-FAELT-IN                                        
024855     ELSE                                                                 
024856        MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                        
024857        CALL WMEDKONV USING MED-WMEDAREA                                  
024858        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
024859        PERFORM MFS-ROER-EJ-FAELT-IN                                      
024862     END-IF                                                               
024863     .                                                                    
024864     EJECT                                                                
024865 MFS-RENSA-FAELT-UT SECTION.                                              
024866     MOVE 'MFS-RENSA-FAELT-UT' TO WS-SEKTION                              
024870*    DISPLAY WS-SEKTION                                                   
024900                                                                          
025000*    --- ALLA UTDATA-FÄLT                                                 
025200     MOVE MFS-RENSA-FAELT TO MOD-IDAPPTYP                                 
025311                             MOD-BEAPPTYP                                 
025312                             MOD-BELEVART                                 
025321                             MOD-IDAPPTYP-ATTR                            
025322                             MOD-BEAPPTYP-ATTR                            
025323                             MOD-BELEVART-ATTR                            
025400     .                                                                    
025600     SKIP3                                                                
026310 MFS-RENSA-FAELT-IN SECTION.                                              
026320                                                                          
026330*    --- ALLA INDATA-FÄLT                                                 
026340     MOVE MFS-RENSA-FAELT TO    MOD-IDARTNR-IN                            
026370                                MOD-IDAPPTYP                              
026380                                MOD-BEAPPTYP                              
026390                                MOD-BELEVART                              
026392     .                                                                    
026393     EJECT                                                                
026400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026410     MOVE 'MFS-ROER-EJ-FAELT-UT' TO WS-SEKTION                            
026420*    DISPLAY WS-SEKTION                                                   
026500                                                                          
026600*    --- ALLA UTDATA-FÄLT                                                 
026800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                             
026803                               MOD-BEAPPTYP                               
026804                               MOD-BEAPPTYP-ATTR                          
026805                               MOD-IDAPPTYP                               
026806                               MOD-IDAPPTYP-ATTR                          
026820                               MOD-BELEVART                               
026830                               MOD-BELEVART-ATTR                          
027100     .                                                                    
027200     SKIP3                                                                
027300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027310     MOVE 'MFS-ROER-EJ-FAELT-IN' TO WS-SEKTION                            
027320*    DISPLAY WS-SEKTION                                                   
027400                                                                          
027500*    --- ALLA INDATA-FÄLT                                                 
027600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-IN                             
027740                               MOD-BEAPPTYP                               
027750                               MOD-IDAPPTYP                               
027760                               MOD-BELEVART                               
027800     .                                                                    
027900     EJECT                                                                
028000 MFS-FORM-ATTR SECTION.                                                   
028010     MOVE 'MFS-FORM-ATTR   ' TO WS-SEKTION                                
028020*    DISPLAY WS-SEKTION                                                   
028100                                                                          
028200*    --- ALLA INDATA-FÄLT                                                 
028400     MOVE MFS-FORMATETS-ATTR TO MOD-BEAPPTYP-ATTR                         
028410     MOVE MFS-FORMATETS-ATTR TO MOD-IDAPPTYP-ATTR                         
028420     MOVE MFS-FORMATETS-ATTR TO MOD-BELEVART-ATTR                         
028500     .                                                                    
028600     SKIP2                                                                
028700 MFS-LAES-IN-IGEN SECTION.                                                
028710     MOVE 'MFS-LAES-IN-IGEN' TO WS-SEKTION                                
028720*    DISPLAY WS-SEKTION                                                   
028800                                                                          
028900*    --- ALLA INDATA-FÄLT                                                 
029100     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEAPPTYP-ATTR                      
029110     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDAPPTYP-ATTR                      
029120     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BELEVART-ATTR                      
029200     .                                                                    
029300     EJECT                                                                
029400* --- IMS SEKTIONER ---                                                   
029500     SKIP3                                                                
029600 IMS-GET-MSG SECTION.                                                     
029610     MOVE 'IMS-GET-MSG' TO WS-IMS-SEKTION                                 
029620*    DISPLAY WS-IMS-SEKTION                                               
029700                                                                          
029800     MOVE '  QC' TO GODK-STATUSKODER                                      
029900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030100     PERFORM IMS-STATUSKONTROLL                                           
030200     .                                                                    
030300     SKIP3                                                                
030400 IMS-INSERT-MSG SECTION.                                                  
030410     MOVE 'IMS-INSERT-MSG' TO WS-IMS-SEKTION                              
030420*    DISPLAY WS-IMS-SEKTION                                               
030500                                                                          
030600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
030700       MOVE '0' TO MFS-KDHUVOMR                                           
030800     END-IF                                                               
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031501     EJECT                                                                
031502 IMS-GET-WL311101 SECTION.                                                
031503     MOVE 'IMS-GET-WL311101' TO WS-IMS-SEKTION                            
031504*    DISPLAY WS-IMS-SEKTION                                               
031505                                                                          
031506     STRING 'WL311101(WDGXKEY  =' W-WDGXKEY-X ')'                         
031507          DELIMITED BY SIZE INTO SSA1                                     
031508     MOVE '  GE' TO GODK-STATUSKODER                                      
031509     CALL CBLTDLI USING GU 3111-PCB DLI-IO-WL311101 SSA1                  
031510     MOVE 3111-STATUS-CODE TO STATUS-WS                                   
031511     PERFORM IMS-STATUSKONTROLL                                           
031512     .                                                                    
031513     EJECT                                                                
031522 IMS-GET-WL311111 SECTION.                                                
031523     MOVE 'IMS-GET-WL311111' TO WS-IMS-SEKTION                            
031524*    DISPLAY WS-IMS-SEKTION                                               
031525                                                                          
031526     STRING 'WL311111(IDARTNR  =' W-IDARTNR-X ')'                         
031527          DELIMITED BY SIZE INTO SSA1                                     
031528     MOVE '  GE' TO GODK-STATUSKODER                                      
031529     CALL CBLTDLI USING GHNP 3111-PCB DLI-IO-WL311111 SSA1                
031530     MOVE 3111-STATUS-CODE TO STATUS-WS                                   
031531     PERFORM IMS-STATUSKONTROLL                                           
031532     .                                                                    
031533     SKIP3                                                                
031542 IMS-ISRT-WL311111 SECTION.                                               
031543     MOVE 'IMS-ISRT-WL311111' TO WS-IMS-SEKTION                           
031544*    DISPLAY WS-IMS-SEKTION                                               
031545                                                                          
031546     STRING 'WL311101(WDGXKEY  =' W-WDGXKEY-X ')'                         
031547          DELIMITED BY SIZE INTO SSA1                                     
031548     MOVE 'WL311111 ' TO SSA2                                             
031549     MOVE '  II' TO GODK-STATUSKODER                                      
031550     CALL CBLTDLI USING ISRT 3111-PCB DLI-IO-WL311111 SSA1 SSA2           
031551     MOVE 3111-STATUS-CODE TO STATUS-WS                                   
031552     PERFORM IMS-STATUSKONTROLL                                           
031553     .                                                                    
031554     SKIP3                                                                
031555 IMS-REPL-WL311111 SECTION.                                               
031556     MOVE 'IMS-REPL-WL311111' TO WS-IMS-SEKTION                           
031557*    DISPLAY WS-IMS-SEKTION                                               
031558                                                                          
031559     MOVE '  ' TO GODK-STATUSKODER                                        
031560     CALL CBLTDLI USING REPL 3111-PCB DLI-IO-WL311111                     
031561     MOVE 3111-STATUS-CODE TO STATUS-WS                                   
031562     PERFORM IMS-STATUSKONTROLL                                           
031563     .                                                                    
031570     SKIP3                                                                
031700 IMS-STATUSKONTROLL SECTION.                                              
031800                                                                          
031900     SET STATUS-IX TO 1                                                   
032000     SEARCH GODK-STATUS                                                   
032100       AT END                                                             
032200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032300         DELIMITED BY SIZE INTO FELTEXT                                   
032400         CALL FELLOG                                                      
032500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032600         CONTINUE                                                         
032700     END-SEARCH                                                           
032800     .                                                                    
032900 DB2-SELECT-RADIOT-TAB  SECTION.                                          
033000     MOVE 'DB2-SELECT-RADIOT-TAB' TO WS-DB2-SEKTION                       
033010*    DISPLAY WS-DB2-SEKTION                                               
033100                                                                          
033200     MOVE 000100  TO GODK-SQLCODESKODER                                   
033300     EXEC SQL SELECT                                                      
033400                  IDARTNR_RADIO,                                          
033401                  APPARATTYP,                                             
033410                  BEAPTYP,                                                
033420                  IDARTNR_LABEL                                           
033500              INTO                                                        
033600                  :RADIO-IDARTNR-RADIO,                                   
033601                  :RADIO-APPARATTYP,                                      
033610                  :RADIO-BEAPTYP,                                         
033620                  :RADIO-IDARTNR-LABEL                                    
033700            FROM WRADIOT                                                  
033800            WHERE IDARTNR_RADIO = :W-IDARTNR-RADIO                        
034000     END-EXEC                                                             
034110                                                                          
034200     MOVE SQLCODE TO SQLCODE-WS                                           
034300                     TEST-SQLCODE                                         
034400     PERFORM DB2-STATUSKONTROLL                                           
034500     .                                                                    
034600     EJECT                                                                
034752 DB2-UPDATE-WRADIOT  SECTION.                                             
034753     MOVE 'DB2-UPDATE-WRADIOT' TO WS-DB2-SEKTION                          
034754*    DISPLAY WS-DB2-SEKTION                                               
034755     SKIP2                                                                
034756     MOVE 000               TO GODK-SQLCODESKODER                         
034757     EXEC SQL UPDATE WRADIOT                                              
034758        SET APPARATTYP  = :RADIO-APPARATTYP,                              
034759            BEAPTYP     = :RADIO-BEAPTYP,                                 
034760            IDARTNR_LABEL  = :RADIO-IDARTNR-LABEL                         
034766     WHERE IDARTNR_RADIO = :W-IDARTNR-RADIO                               
034767     END-EXEC                                                             
034768     MOVE SQLCODE           TO SQLCODE-WS                                 
034769     PERFORM DB2-STATUSKONTROLL                                           
034770     .                                                                    
034771     EJECT                                                                
034783 DB2-ISRT-WRADIOT  SECTION.                                               
034784     MOVE 'DB2-ISRT-WRADIOT' TO WS-DB2-SEKTION                            
034785*    DISPLAY WS-DB2-SEKTION                                               
034786     MOVE 000               TO GODK-SQLCODESKODER                         
034787     SKIP2                                                                
034788     EXEC SQL INSERT INTO WRADIOT                                         
034789           (IDARTNR_RADIO,                                                
034790            APPARATTYP,                                                   
034806            BEAPTYP,                                                      
034807            IDARTNR_LABEL)                                                
034808        VALUES (:RADIO-IDARTNR-RADIO,                                     
034809                :RADIO-APPARATTYP,                                        
034827                :RADIO-BEAPTYP,                                           
034828                :RADIO-IDARTNR-LABEL)                                     
034829     END-EXEC                                                             
034830     MOVE SQLCODE           TO SQLCODE-WS                                 
034831     PERFORM DB2-STATUSKONTROLL                                           
034832     .                                                                    
034833     EJECT                                                                
034834 DB2-STATUSKONTROLL  SECTION.                                             
034840                                                                          
034900     SET SQLCODE-IX TO 1                                                  
035000     SEARCH GODK-SQLCODE                                                  
035100       AT END CALL FELLOG                                                 
035200       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
035300     END-SEARCH                                                           
035400     .                                                                    
