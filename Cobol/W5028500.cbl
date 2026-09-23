000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5028500.                                                
000300 AUTHOR.         GUN LÖFGREN.                                             
000400 DATE-WRITTEN.   97/08.                                                   
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        RAPPORTERA IN PRIME COUNT-UPPGIFTER FÖR                          
000900*        NDC:S ÅRSINVENTERING.                                            
001000*                                                                         
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W5T285                                              
001400*        MID:         W5I28501                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W5O28501                                            
001800                                                                          
001900                                                                          
002000                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W5028500'.            
002900                                                                          
003000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500                                                                          
003600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003700                                                                          
003800                                                                          
003900 77  INDATA-OK                   PIC X       VALUE 'J'.                   
004000                                                                          
004100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004200     88  NYCKLAR-OK                          VALUE 'J'.                   
004300     88  NYCKLAR-FEL                         VALUE 'N'.                   
004400                                                                          
004500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004600     88  ALLT-OK                             VALUE 'J'.                   
004700                                                                          
004800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004900     88  EGEN-MID                            VALUE '5285'.                
005000     88  GODK-MID                            VALUE '5281' '5282'          
005100                                                   '5283' '5284'          
005200                                                   '5285' '5286'.         
005300     88  HELP-MID                            VALUE '0551'.                
005400                                                                          
005500     EJECT                                                                
005600 01  DIVERSE.                                                             
005700     03  MAX-RAD-IX              PIC S9(9)   VALUE +13  COMP SYNC.        
005800     03  RAD-IX                  PIC S9(9)   VALUE ZERO COMP SYNC.        
005900     03  W-KVLS                  PIC X(7)    VALUE SPACE.                 
006000     03  WS-SEKTION              PIC X(32)   VALUE SPACE.                 
006100     03  WS-TIREGDAT             PIC X(6).                                
006200     03  WS-TIREGDAT-N REDEFINES WS-TIREGDAT                              
006300                                 PIC 9(6).                                
006400     03  WS-TIREGTID             PIC X(6).                                
006500     03  WS-TIREGTID-N REDEFINES WS-TIREGTID                              
006600                                 PIC 9(6).                                
006700     03  DAGENS-DATUM            PIC 9(6)    VALUE ZERO.                  
006800     03  DAGENS-TID              PIC 9(8)    VALUE ZERO.                  
006900     03  FILLER REDEFINES DAGENS-TID.                                     
007000      05 DAGENS-TID-6            PIC 9(6).                                
007100      05 FILLER                  PIC 9(2).                                
007200                                                                          
007300     SKIP3                                                                
007400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007500 01  GENERELLA-SUBPROGRAM.                                                
007600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008000     EJECT                                                                
008100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008200*01 -COPY WMEDAREA                                                        
008300     EJECT                                                                
008400 01  MESSAGE-CODES.                                                       
008500     03  ERR-DATA                PIC X(3)    VALUE '001'.                 
008600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008900     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
009000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009300     EJECT                                                                
009400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009500*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009700     SKIP3                                                                
009800*01 -COPY WMSGINIT                                                        
009900     EJECT                                                                
010000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010100*                                                                         
010200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010300     SKIP3                                                                
010400*01  MID -COPY W5I28501                                                   
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010700     SKIP3                                                                
010800*01  -COPY WMSGAREA                                                       
010900     EJECT                                                                
011000     03  MOD REDEFINES MSG-AREA.                                          
011100*      05  -COPY W5O28501                                                 
011200     EJECT                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011400*01  -COPY WMFSAREA                                                       
011500     EJECT                                                                
011600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011800                                                                          
011900 01  NYCKLAR-TILL-DLI.                                                    
012000*                                                                         
012100   03  W-WDJ7BSEQ-X.                                                      
012200     05  W-IDDC                  PIC X(2)    VALUE SPACE.                 
012300     05  W-IDUSER                PIC X(8)    VALUE SPACE.                 
012400     05  W-TIREGDAT-X.                                                    
012500      07 W-TIREGDAT              PIC S9(7)   COMP-3 VALUE ZERO.           
012600     05  W-TIREGTID-X.                                                    
012700      07 W-TIREGTID              PIC S9(7)   COMP-3 VALUE ZERO.           
012800                                                                          
012900   03  W-WDJ701KY-X.                                                      
013000     05  W-IDDC-P                PIC X(2)    VALUE SPACE.                 
013100     05  W-IDARTNR-P             PIC S9(9)   COMP-3 VALUE ZERO.           
013200                                                                          
013300     EJECT                                                                
013400*    --- STATUS-KOD FRÅN IMS                                              
013500 01  STATUS-WS                   PIC XX.                                  
013600     88  SEGMENT-FINNS                       VALUE '  '.                  
013700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013800     88  BAS-SLUT                            VALUE 'GB'.                  
013900     SKIP2                                                                
014000 01  GODK-STATUSKODER.                                                    
014100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014200     SKIP3                                                                
014300 01  SSA1                        PIC X(96).                               
014400     EJECT                                                                
014500*    --- IMS FUNKTIONSKODER                                               
014600*01  -COPY W0003                                                          
014700     EJECT                                                                
014800*    ---  DLI INPUT-OUTPUT AREA                                           
014900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
015000                                                                          
015100 01  DLI-IO-AREA.                                                         
015200*  03  -COPY WDJ701                                                       
015300     EJECT                                                                
015900 LINKAGE SECTION.                                                         
016000*01  -COPY W0009   -PRE MSG-                                              
016100                                                                          
016200*01  -COPY W0008   -PRE USEA-                                             
016300     05  FILLER                  PIC X.                                   
016400     EJECT                                                                
016500*01  -COPY W0008  -PRE ACS-                                               
016600     05  FILLER                  PIC X.                                   
016700     EJECT                                                                
016800*01  -COPY W0008  -PRE ACS2-                                              
016900     05  FILLER                  PIC X.                                   
017000     EJECT                                                                
017100                                                                          
017200 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ACS-PCB ACS2-PCB.             
017300 MAIN SECTION.                                                            
017400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ACS-PCB ACS2-PCB.             
017500                                                                          
017600     MOVE 'MAIN'     TO WS-SEKTION                                        
017700     PERFORM IMS-GET-MSG                                                  
017800     IF SEGMENT-FINNS                                                     
017900       PERFORM A-INIT                                                     
018000       PERFORM B-KOLLA-NYCKLAR                                            
018100       IF NYCKLAR-OK                                                      
018200         IF MFS-UPDATE                                                    
018300           PERFORM G-KOLLA-INPUT                                          
018400           IF INDATA-OK = JA                                              
018500             PERFORM H-UPPDATERA                                          
018600             PERFORM F-LAES-VISA-INFO                                     
018700           END-IF                                                         
018800         ELSE                                                             
018900           PERFORM E-SAMMA-SIDA                                           
019000           IF ALLT-OK                                                     
019100             PERFORM F-LAES-VISA-INFO                                     
019200           END-IF                                                         
019300         END-IF                                                           
019400       END-IF                                                             
019500       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O28501 + 4                      
019600       PERFORM IMS-INSERT-MSG                                             
019700     END-IF                                                               
019800                                                                          
019900     MOVE ZERO TO RETURN-CODE                                             
020000     GOBACK                                                               
020100     .                                                                    
020200     EJECT                                                                
020300 A-INIT SECTION.                                                          
020400                                                                          
020500     MOVE 'A-INIT'         TO WS-SEKTION                                  
020600     IF MSG-DUBBLA-TRANSKODER                                             
020700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I28501                 
020800       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
020900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
021000     ELSE                                                                 
021100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I28501                  
021200       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
021300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
021400     END-IF                                                               
021500                                                                          
021600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
021700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
021800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
021900                                                                          
022000     MOVE LOW-VALUE TO MSG-AREA                                           
022100     MOVE 'W5O285N1' TO MFS-IDMOD                                         
022200     MOVE '5285' TO MOD-IDTRANS                                           
022300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
022400                                                                          
022500     IF EGEN-MID OR HELP-MID                                              
022600       CONTINUE                                                           
022700     ELSE                                                                 
022800       MOVE SPACE TO MFS-KDTRTYP                                          
022900       MOVE '7' TO MFS-IDPFK                                              
023000     END-IF                                                               
023100                                                                          
023200     ACCEPT DAGENS-DATUM  FROM DATE                                       
023300     ACCEPT DAGENS-TID    FROM TIME                                       
023400                                                                          
023500     .                                                                    
023600     EJECT                                                                
023700 B-KOLLA-NYCKLAR SECTION.                                                 
023800                                                                          
023900     MOVE 'B-KOLLA-NYCKLAR' TO WS-SEKTION                                 
024000     MOVE JA                TO NYCKLAR-SW                                 
024100                                                                          
024200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
024300     MOVE '001'             TO MSGI-KDCALL                                
024400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
024600     MOVE '5285'            TO MSGI-IDTRANS                               
024700                                                                          
024800     IF EGEN-MID                                                          
024900       MOVE MID-IDUSER-UT        TO MOD-IDUSER-UT                         
025000       MOVE MID-TIREGDAT-UT      TO MOD-TIREGDAT-UT                       
025100       MOVE MID-TIREGTID-UT      TO MOD-TIREGTID-UT                       
025200       IF MID-IDUSER-IN NOT = ALL '+'                                     
025300         MOVE MID-IDUSER-IN      TO MOD-IDUSER-UT                         
025400         MOVE MID-IDUSER-IN      TO W-IDUSER                              
025500       END-IF                                                             
025600       IF MID-TIREGDAT-IN NOT = ALL '+'                                   
025700         MOVE MID-TIREGDAT-IN    TO MOD-TIREGDAT-UT                       
025800       END-IF                                                             
025900       IF MID-TIREGTID-IN NOT = ALL '+'                                   
026000         MOVE MID-TIREGTID-IN    TO MOD-TIREGTID-UT                       
026100       END-IF                                                             
026200     END-IF                                                               
026300                                                                          
026400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
026500                                                                          
026600     MOVE MFS-RENSA-FAELT TO MOD-IDUSER-IN                                
026700     MOVE MFS-RENSA-FAELT TO MOD-TIREGDAT-IN                              
026800     MOVE MFS-RENSA-FAELT TO MOD-TIREGTID-IN                              
026900                                                                          
027000     MOVE MSGI-IDDC       TO W-IDDC                                       
027100     MOVE MOD-IDUSER-UT   TO W-IDUSER                                     
027200                                                                          
027300     IF MOD-TIREGDAT-UT   NUMERIC                                         
027400       IF MOD-TIREGDAT-UT   > ZERO                                        
027500         MOVE MOD-TIREGDAT-UT  TO WS-TIREGDAT                             
027600         MOVE WS-TIREGDAT-N    TO W-TIREGDAT                              
027700       ELSE                                                               
027800         MOVE NEJ          TO NYCKLAR-SW                                  
027900       END-IF                                                             
028000     ELSE                                                                 
028100       MOVE NEJ  TO NYCKLAR-SW                                            
028200     END-IF                                                               
028300                                                                          
028400     IF MOD-TIREGTID-UT   NUMERIC                                         
028500       IF MOD-TIREGTID-UT   > ZERO                                        
028600         MOVE MOD-TIREGTID-UT  TO WS-TIREGTID                             
028700         MOVE WS-TIREGTID-N    TO W-TIREGTID                              
028800       ELSE                                                               
028900         MOVE NEJ          TO NYCKLAR-SW                                  
029000       END-IF                                                             
029100     ELSE                                                                 
029200       MOVE NEJ  TO NYCKLAR-SW                                            
029300     END-IF                                                               
029400                                                                          
029500     IF GODK-MID OR NYCKLAR-OK                                            
029600       CONTINUE                                                           
029700     ELSE                                                                 
029800       MOVE MFS-RENSA-FAELT TO MOD-IDUSER-UT                              
029900                               MOD-TIREGDAT-UT                            
030000                               MOD-TIREGTID-UT                            
030100     END-IF                                                               
030200                                                                          
030300     IF NYCKLAR-FEL                                                       
030400       MOVE 'GB'          TO MED-IDSKYLT                                  
030500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
030600       CALL WMEDKONV USING MED-WMEDAREA                                   
030700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
030800       MOVE +1 TO RAD-IX                                                  
030900       PERFORM UNTIL RAD-IX > MAX-RAD-IX                                  
031000         PERFORM MFS-RENSA-FAELT-IN                                       
031100         PERFORM MFS-RENSA-FAELT-UT                                       
031200         ADD +1 TO RAD-IX                                                 
031300       END-PERFORM                                                        
031400     END-IF                                                               
031500                                                                          
031600     .                                                                    
031700     EJECT                                                                
031800                                                                          
031900 E-SAMMA-SIDA SECTION.                                                    
032000                                                                          
032100     MOVE 'E-SAMMA-SIDA'      TO WS-SEKTION                               
032200                                                                          
032300     IF MID-KVLS-IN  (1)     = ALL '+'                                    
032400       AND MID-KVLS-IN  (2)  = ALL '+'                                    
032500       AND MID-KVLS-IN  (3)  = ALL '+'                                    
032600       AND MID-KVLS-IN  (4)  = ALL '+'                                    
032700       AND MID-KVLS-IN  (5)  = ALL '+'                                    
032800       AND MID-KVLS-IN  (6)  = ALL '+'                                    
032900       AND MID-KVLS-IN  (7)  = ALL '+'                                    
033000       AND MID-KVLS-IN  (8)  = ALL '+'                                    
033100       AND MID-KVLS-IN  (9)  = ALL '+'                                    
033200       AND MID-KVLS-IN  (10) = ALL '+'                                    
033300       AND MID-KVLS-IN  (11) = ALL '+'                                    
033400       AND MID-KVLS-IN  (12) = ALL '+'                                    
033500       AND MID-KVLS-IN  (13) = ALL '+'                                    
033600         MOVE JA TO ALLT-SW                                               
033700     ELSE                                                                 
033800       MOVE +1 TO RAD-IX                                                  
033900       PERFORM UNTIL (RAD-IX > MAX-RAD-IX)  OR                            
034000                     (MID-IDARTNR (RAD-IX) =  ALL '+')                    
034100         IF MID-KVLS-IN (RAD-IX) NOT = ALL '+'                            
034200           MOVE MID-KVLS-IN (RAD-IX) TO MOD-KVLS (RAD-IX)                 
034300         ELSE                                                             
034400           MOVE MFS-RENSA-FAELT      TO MOD-KVLS (RAD-IX)                 
034500         END-IF                                                           
034600         PERFORM MFS-ROER-EJ-FAELT-UT                                     
034700         ADD +1 TO RAD-IX                                                 
034800       END-PERFORM                                                        
034900       PERFORM UNTIL (RAD-IX > MAX-RAD-IX)                                
035000         MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR (RAD-IX)                
035100         MOVE MFS-RENSA-FAELT      TO MOD-KVLS (RAD-IX)                   
035200         ADD +1 TO RAD-IX                                                 
035300       END-PERFORM                                                        
035400                                                                          
035500       MOVE NEJ TO ALLT-SW                                                
035600       MOVE 'GB'           TO MED-IDSKYLT                                 
035700       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
035800       CALL WMEDKONV USING MED-WMEDAREA                                   
035900       MOVE MED-MFSINF TO MOD-TEMFSFEL                                    
036000     END-IF                                                               
036100                                                                          
036200     .                                                                    
036300     EJECT                                                                
036400 F-LAES-VISA-INFO SECTION.                                                
036500                                                                          
036600     MOVE 'F-LAES-VISA-INFO'  TO WS-SEKTION                               
036700                                                                          
036800     PERFORM IMS-GET-ACS-NEXT                                             
036900     IF SEGMENT-FINNS                                                     
037000       MOVE +1 TO RAD-IX                                                  
037100       PERFORM UNTIL RAD-IX > MAX-RAD-IX  OR SEGMENT-SAKNAS               
037200         IF ACS-IDUSER-PCOUNT-REG = SPACE                                 
037300           PERFORM FA-LAEGG-UT-RAD                                        
037400           ADD +1 TO RAD-IX                                               
037500         END-IF                                                           
037600         PERFORM IMS-GET-ACS-NEXT                                         
037700       END-PERFORM                                                        
037800       PERFORM UNTIL RAD-IX > MAX-RAD-IX                                  
037900         PERFORM MFS-RENSA-FAELT-UT                                       
038000         MOVE MFS-RENSA-FAELT  TO MOD-KVLS (RAD-IX)                       
038100         ADD +1  TO RAD-IX                                                
038200       END-PERFORM                                                        
038300     ELSE                                                                 
038400       MOVE +1 TO RAD-IX                                                  
038500       PERFORM UNTIL RAD-IX > MAX-RAD-IX                                  
038600         PERFORM MFS-RENSA-FAELT-UT                                       
038700         ADD +1  TO RAD-IX                                                
038800       END-PERFORM                                                        
038900       MOVE 'GB'             TO MED-IDSKYLT                               
039000       MOVE ERR-PART-MISSING TO MED-IDMFSFEL                              
039100       CALL WMEDKONV USING MED-WMEDAREA                                   
039200       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
039300     END-IF                                                               
039400                                                                          
039500                                                                          
039600     MOVE '002'  TO MSGI-KDCALL                                           
039700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
039800                                                                          
039900     .                                                                    
040000     EJECT                                                                
040100 FA-LAEGG-UT-RAD  SECTION.                                                
040200                                                                          
040300     MOVE 'FA-LAEGG-UT-RAD'   TO WS-SEKTION                               
040400                                                                          
040500     MOVE ACS-ADLAGOMR        TO MOD-ADLAGOMR   (RAD-IX)                  
040600     MOVE ACS-ADGANG          TO MOD-ADGANG     (RAD-IX)                  
040700     MOVE ACS-ADPLATS         TO MOD-ADPLATS    (RAD-IX)                  
040800     MOVE ACS-IDARTNR         TO MOD-IDARTNR    (RAD-IX)                  
040900     MOVE ACS-BEART           TO MOD-BEART      (RAD-IX)                  
041000     MOVE MFS-OPEN-ALPHA-NOMOD  TO MOD-KVLS-ATTR  (RAD-IX)                
041100     MOVE MFS-ADD-SAETT-CURSOR  TO  MOD-KVLS-ATTR (1)                     
041200     MOVE MFS-RENSA-FAELT     TO MOD-KVLS       (RAD-IX)                  
041300                                                                          
041400     .                                                                    
041500     EJECT                                                                
041600 G-KOLLA-INPUT     SECTION.                                               
041700                                                                          
041800     MOVE 'G-KOLLA-INPUT'     TO WS-SEKTION                               
041900     MOVE JA   TO INDATA-OK                                               
042000     MOVE +1   TO RAD-IX                                                  
042100     PERFORM UNTIL (RAD-IX > MAX-RAD-IX) OR                               
042200                   (MID-IDARTNR (RAD-IX) = ALL '+')                       
042300       IF MID-KVLS-IN (RAD-IX) = ALL '+'                                  
042400         MOVE MFS-NUM-FAELT-FEL   TO                                      
042500                      MOD-KVLS-ATTR (RAD-IX)                              
042600         MOVE MFS-RENSA-FAELT     TO MOD-KVLS (RAD-IX)                    
042700         MOVE NEJ TO INDATA-OK                                            
042800       ELSE                                                               
042900         INSPECT MID-KVLS-IN (RAD-IX) REPLACING LEADING                   
043000         SPACE BY ZERO                                                    
043100         IF MID-KVLS-IN (RAD-IX) NUMERIC                                  
043200           MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVLS-ATTR (RAD-IX)            
043300           MOVE MID-KVLS-IN (RAD-IX)  TO MOD-KVLS (RAD-IX)                
043400         ELSE                                                             
043500           MOVE MFS-NUM-FAELT-FEL   TO                                    
043600                        MOD-KVLS-ATTR (RAD-IX)                            
043700           MOVE MFS-ROER-EJ-FAELT   TO MOD-KVLS (RAD-IX)                  
043800           MOVE NEJ TO INDATA-OK                                          
043900         END-IF                                                           
044000       END-IF                                                             
044100       PERFORM MFS-ROER-EJ-FAELT-UT                                       
044200       ADD 1   TO RAD-IX                                                  
044300     END-PERFORM                                                          
044400                                                                          
044500     PERFORM UNTIL (RAD-IX > MAX-RAD-IX)                                  
044600       MOVE MFS-RENSA-FAELT     TO MOD-KVLS    (RAD-IX)                   
044700       MOVE MFS-RENSA-FAELT     TO MOD-IDARTNR (RAD-IX)                   
044800       ADD 1   TO RAD-IX                                                  
044900     END-PERFORM                                                          
045000                                                                          
045100     IF INDATA-OK = JA                                                    
045200       CONTINUE                                                           
045300     ELSE                                                                 
045400       MOVE 'GB'          TO MED-IDSKYLT                                  
045500       MOVE ERR-DATA      TO MED-IDMFSFEL                                 
045600       CALL WMEDKONV USING MED-WMEDAREA                                   
045700       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
045800     END-IF                                                               
045900     MOVE NEJ TO ALLT-SW                                                  
046000*    CALL FELLOG                                                          
046100     .                                                                    
046200     EJECT                                                                
046300 H-UPPDATERA       SECTION.                                               
046400                                                                          
046500     MOVE 'H-UPPDATERA'       TO WS-SEKTION                               
046600     MOVE 1     TO RAD-IX                                                 
046700     PERFORM UNTIL RAD-IX > MAX-RAD-IX  OR                                
046800                   MID-IDARTNR (RAD-IX) = ALL '+'                         
046900       INSPECT MID-IDARTNR (RAD-IX) REPLACING                             
047000                 LEADING SPACE BY ZERO                                    
047100       MOVE MID-IDARTNR   (RAD-IX) TO W-IDARTNR-P                         
047200       MOVE MSGI-IDDC              TO W-IDDC-P                            
047300       PERFORM IMS-GET-ACS-HOLD                                           
047400       INSPECT MID-KVLS-IN (RAD-IX) REPLACING                             
047500                        LEADING SPACE BY ZERO                             
047600       MOVE MID-KVLS-IN (RAD-IX) TO W-KVLS                                
047700       MOVE W-KVLS               TO ACS-KVPCOUNT                          
047800       IF ACS-KVPCOUNT NOT = ACS-KVLS                                     
047900         MOVE NEJ                TO ACS-FLKLAR                            
048000       END-IF                                                             
048100       MOVE MSGI-IDUSER          TO ACS-IDUSER-PCOUNT-REG                 
048200       MOVE DAGENS-DATUM         TO ACS-TIREGDAT-PCOUNT-REG               
048300       MOVE DAGENS-TID-6         TO ACS-TIREGTID-PCOUNT-REG               
048400       PERFORM IMS-REPL-ACS                                               
048500       MOVE 'GB'                 TO MED-IDSKYLT                           
048600       MOVE INF-UPDATE-DONE      TO MED-IDMFSINF                          
048700       CALL WMEDKONV USING MED-WMEDAREA                                   
048800       MOVE MED-MFSINF           TO MOD-TEMFSINF                          
048900       ADD 1                     TO RAD-IX                                
049000     END-PERFORM                                                          
049100                                                                          
049200     .                                                                    
049300     EJECT                                                                
049400 MFS-RENSA-FAELT-UT SECTION.                                              
049500                                                                          
049600     MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR  (RAD-IX)                       
049700                             MOD-ADGANG    (RAD-IX)                       
049800                             MOD-ADPLATS   (RAD-IX)                       
049900                             MOD-IDARTNR   (RAD-IX)                       
050000                             MOD-KVLS      (RAD-IX)                       
050100                             MOD-BEART     (RAD-IX)                       
050200     .                                                                    
050300     SKIP3                                                                
050400 MFS-RENSA-FAELT-IN SECTION.                                              
050500                                                                          
050600     MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR (RAD-IX)                        
050700                             MOD-ADGANG   (RAD-IX)                        
050800                             MOD-ADPLATS  (RAD-IX)                        
050900                             MOD-IDARTNR  (RAD-IX)                        
051000                             MOD-KVLS     (RAD-IX)                        
051100     .                                                                    
051200     EJECT                                                                
051300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
051400                                                                          
051500     MOVE MFS-ROER-EJ-FAELT TO MOD-ADLAGOMR  (RAD-IX)                     
051600                               MOD-ADGANG    (RAD-IX)                     
051700                               MOD-ADPLATS   (RAD-IX)                     
051800                               MOD-IDARTNR   (RAD-IX)                     
051900                               MOD-BEART     (RAD-IX)                     
052000     .                                                                    
052100     SKIP3                                                                
052200 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
052300                                                                          
052400     MOVE MFS-ROER-EJ-FAELT TO MOD-ADLAGOMR     (RAD-IX)                  
052500                               MOD-ADGANG   (RAD-IX)                      
052600                               MOD-ADPLATS  (RAD-IX)                      
052700                               MOD-IDARTNR  (RAD-IX)                      
052800                               MOD-KVLS     (RAD-IX)                      
052900     .                                                                    
053000     EJECT                                                                
053100* --- IMS SEKTIONER ---                                                   
053200                                                                          
053300 IMS-GET-MSG SECTION.                                                     
053400                                                                          
053500     MOVE '  QC' TO GODK-STATUSKODER                                      
053600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
053700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
053800     PERFORM IMS-STATUSKONTROLL                                           
053900     .                                                                    
054000     SKIP3                                                                
054100 IMS-INSERT-MSG SECTION.                                                  
054200                                                                          
054300     IF ENGLISH-TEXT                                                      
054400       MOVE 'N' TO MFS-KDHUVOMR                                           
054500     END-IF                                                               
054600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
054700     MOVE SPACE TO GODK-STATUSKODER                                       
054800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
054900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
055000     PERFORM IMS-STATUSKONTROLL                                           
055100     .                                                                    
055200     SKIP3                                                                
055300 IMS-GET-ACS   SECTION.                                                   
055400                                                                          
055500     STRING 'WDJ701  (WDJ7BSEQ =' W-WDJ7BSEQ-X ')'                        
055600          DELIMITED BY SIZE INTO SSA1                                     
055700     MOVE '  GE' TO GODK-STATUSKODER                                      
055800     CALL CBLTDLI USING GU ACS-PCB DLI-IO-AREA SSA1                       
055900     MOVE ACS-STATUS-CODE TO STATUS-WS                                    
056000     PERFORM IMS-STATUSKONTROLL                                           
056100     .                                                                    
056200     SKIP3                                                                
056300 IMS-GET-ACS-NEXT SECTION.                                                
056400                                                                          
056500     STRING 'WDJ701  (WDJ7BSEQ =' W-WDJ7BSEQ-X ')'                        
056600          DELIMITED BY SIZE INTO SSA1                                     
056700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
056800     CALL CBLTDLI USING GN ACS-PCB DLI-IO-AREA SSA1                       
056900     MOVE ACS-STATUS-CODE TO STATUS-WS                                    
057000     PERFORM IMS-STATUSKONTROLL                                           
057100     .                                                                    
057200     EJECT                                                                
057300 IMS-GET-ACS-HOLD  SECTION.                                               
057400                                                                          
057500     STRING 'WDJ701  (WDJ701KY =' W-WDJ701KY-X ')'                        
057600          DELIMITED BY SIZE INTO SSA1                                     
057700     MOVE '    ' TO GODK-STATUSKODER                                      
057800     CALL CBLTDLI USING GHU ACS2-PCB DLI-IO-AREA SSA1                     
057900     MOVE ACS2-STATUS-CODE TO STATUS-WS                                   
058000     PERFORM IMS-STATUSKONTROLL                                           
058100     .                                                                    
058200     SKIP3                                                                
058300 IMS-REPL-ACS      SECTION.                                               
058400                                                                          
058500     MOVE '    ' TO GODK-STATUSKODER                                      
058600     CALL CBLTDLI USING REPL ACS2-PCB DLI-IO-AREA                         
058700     MOVE ACS2-STATUS-CODE TO STATUS-WS                                   
058800     PERFORM IMS-STATUSKONTROLL                                           
058900     .                                                                    
059000     EJECT                                                                
059100 IMS-STATUSKONTROLL SECTION.                                              
059200                                                                          
059300     SET STATUS-IX TO 1                                                   
059400     SEARCH GODK-STATUS                                                   
059500       AT END                                                             
059600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
059700         DELIMITED BY SIZE INTO FELTEXT                                   
059800         CALL FELLOG                                                      
059900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
060000         CONTINUE                                                         
060100     END-SEARCH                                                           
060200     .                                                                    
