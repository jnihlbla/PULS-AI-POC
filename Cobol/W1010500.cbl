000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1010500.                                                
000300 AUTHOR.         EGHOLT CONNY.                                            
000400 DATE-WRITTEN.   05/09/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER MPNR-BASEN WDF7 OCH GER INFO OM EV. KOPPLING               
000900*        I MASTER PART NUMBER REGISTRY (MOT FORD-ARTIKEL)                 
001000*                                                                         
001100*        LÄSER MPNR-BASEN WDF7A OCH GER INFO OM EV. KOPPLING              
001200*        FÖR FORD-ARTIKEL MOT VOLVO-ARTIKEL                               
001300*                                                                         
001400*        PROGRAMMET LÄSER      WDF7                                       
001500*        PROGRAMMET LÄSER      WDF7A                                      
001600*        PROGRAMMET LÄSER      WDD2                                       
001700*        PROGRAMMET LÄSER      WDK6                                       
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W1T105                                              
002100*        MID:         W1I10501                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W1O10501                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 DATA DIVISION.                                                           
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W1010500'.            
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  YES                         PIC X       VALUE 'Y'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000                                                                          
004100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004200                                                                          
004300 77  IDARTNR-WS                  PIC X(9)   VALUE SPACE.                  
004400 77  IDARTPFX-WS                 PIC X(6)   VALUE SPACE.                  
004500 77  IDARTBAS-WS                 PIC X(8)   VALUE SPACE.                  
004600 77  IDARTSFX-WS                 PIC X(8)   VALUE SPACE.                  
004700                                                                          
004800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004900     88  NYCKLAR-OK                          VALUE 'J'.                   
005000     88  NYCKLAR-FEL                         VALUE 'N'.                   
005100                                                                          
005200 77  LAES-SW                     PIC X       VALUE ' '.                   
005300     88  LAES-VOLVO                          VALUE 'J'.                   
005400     88  LAES-FORD                           VALUE 'N'.                   
005500                                                                          
005600     EJECT                                                                
005700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005800 01  GENERELLA-SUBPROGRAM.                                                
005900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     EJECT                                                                
006400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006500*01 -COPY WMEDAREA                                                        
006600     SKIP3                                                                
006700 01  MESSAGE-CODES.                                                       
006800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
006900     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007200*                                                                         
007300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007400     SKIP3                                                                
007500*01 -COPY WMSGINIT                                                        
007600     EJECT                                                                
007700*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
007800*                                                                         
007900 01  SPAR-AREA.                                                           
008000     03  SPAR-IDTRANS           PIC X(4)    VALUE '1105'.                 
008100     03  SPAR-IDARTFMC.                                                   
008200       05 SPAR-IDARTPFX         PIC X(6).                                 
008300       05 SPAR-IDARTBAS         PIC X(8).                                 
008400       05 SPAR-IDARTSFX         PIC X(8).                                 
008500     EJECT                                                                
008600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008700*                                                                         
008800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008900     SKIP3                                                                
009000*01  MID -COPY W1I10501                                                   
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009300     SKIP3                                                                
009400*01  -COPY WMSGAREA                                                       
009500     EJECT                                                                
009600     03  MOD REDEFINES MSG-AREA.                                          
009700*      05  -COPY W1O10501                                                 
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010000     SKIP3                                                                
010100*01  -COPY WMFSAREA                                                       
010200     EJECT                                                                
010300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010400*                                                                         
010500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010600     SKIP3                                                                
010700 01  NYCKLAR-TILL-DLI.                                                    
010800                                                                          
010900     03  W-IDARTNR-X.                                                     
011000         05  W-IDARTNR-PULS      PIC S9(9)   VALUE ZERO COMP-3.           
011100                                                                          
011200     03  W-WDF701KY-X.                                                    
011300*          --- KOPPLINGEN VOLVO-ARTIKEL MOT FORD                          
011400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011500         05  W-IDPRTNER-FORD     PIC S9(5)   VALUE +1   COMP-3.           
011600     SKIP2                                                                
011700*    --- KOPPLINGEN FORD-ARTIKEL MOT VOLVO                                
011800     03  W-WDF7A-MIN-X.                                                   
011900         05 W-IDARTFMC-MIN-X.                                             
012000           07 W-IDARTPFX-MIN     PIC X(6)    VALUE SPACE.                 
012100           07 W-IDARTBAS-MIN     PIC X(8)    VALUE SPACE.                 
012200           07 W-IDARTSFX-MIN     PIC X(8)    VALUE SPACE.                 
012300         05 FLGEMFMC-MIN         PIC X       VALUE LOW-VALUE.             
012400         05 TIDATIME9-MIN        PIC X(14)   VALUE LOW-VALUE.             
012500         05 IDARTNR-MIN          PIC S9(9) COMP-3 VALUE ZERO.             
012600                                                                          
012700     03  W-WDF7A-MAX-X.                                                   
012800         05 W-IDARTFMC-MAX-X.                                             
012900           07 W-IDARTPFX-MAX     PIC X(6)    VALUE SPACE.                 
013000           07 W-IDARTBAS-MAX     PIC X(8)    VALUE SPACE.                 
013100           07 W-IDARTSFX-MAX     PIC X(8)    VALUE SPACE.                 
013200         05 FLGEMFMC-MAX         PIC X       VALUE HIGH-VALUE.            
013300         05 TIDATIME9-MAX        PIC X(14)   VALUE HIGH-VALUE.            
013310         05 IDARTNR-MAX        PIC S9(9) COMP-3 VALUE +999999999.         
013400     SKIP2                                                                
013500*    --- STATUS-KOD FRÅN IMS                                              
013600 01  STATUS-WS                   PIC XX.                                  
013700     88  SEGMENT-FINNS                       VALUE '  '.                  
013800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014000                                                                          
014100 01  SPAR-STATUS-WS              PIC XX.                                  
014200                                                                          
014300     SKIP2                                                                
014400 01  GODK-STATUSKODER.                                                    
014500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014600     SKIP3                                                                
014700 01  SSA1                        PIC X(128).                              
014800 01  SSA2                        PIC X(128).                              
014900     EJECT                                                                
015000*    --- IMS FUNKTIONSKODER                                               
015100*01  -COPY W0003                                                          
015200     EJECT                                                                
015300*    ---  DLI INPUT-OUTPUT AREA                                           
015400                                                                          
015500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF701'.                      
015600 01  DLI-IO-WDF701.                                                       
015700*    03  -COPY WDF701                                                     
015800     EJECT                                                                
015900                                                                          
016000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF7A1'.                      
016100 01  DLI-IO-WDF7A1.                                                       
016200*    03  -COPY WDF7A1                                                     
016300     EJECT                                                                
016400                                                                          
016500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
016600 01  DLI-IO-WDK601.                                                       
016700*    03  -COPY WDK601 -PRE WDK6-                                          
016800     EJECT                                                                
016900                                                                          
017000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD201'.                      
017100 01  DLI-IO-WDD201.                                                       
017200*    03  -COPY WDD201 -PRE WDD2-                                          
017300     EJECT                                                                
017400                                                                          
017500 LINKAGE SECTION.                                                         
017600*01  -COPY W0009   -PRE MSG-                                              
017700*01  -COPY W0008   -PRE WDP7-                                             
017800     05  FILLER                  PIC X.                                   
017900                                                                          
018000*01  -COPY W0008  -PRE WDF7-                                              
018100     05  FILLER                  PIC X.                                   
018200     EJECT                                                                
018300*01  -COPY W0008  -PRE WDF7A-                                             
018400     05  FILLER                  PIC X.                                   
018500     EJECT                                                                
018600*01  -COPY W0008  -PRE WDD2-                                              
018700     05  FILLER                  PIC X.                                   
018800     EJECT                                                                
018900*01  -COPY W0008  -PRE WDK6-                                              
019000     05  FILLER                  PIC X.                                   
019100     EJECT                                                                
019200                                                                          
019300 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDF7-PCB WDF7A-PCB            
019400                                   WDD2-PCB WDK6-PCB.                     
019500 MAIN SECTION.                                                            
019600     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDF7-PCB WDF7A-PCB            
019700                                   WDD2-PCB WDK6-PCB.                     
019800                                                                          
019900     PERFORM IMS-GET-MSG                                                  
020000     IF SEGMENT-FINNS                                                     
020100       PERFORM A-INIT                                                     
020200       PERFORM B-KOLLA-NYCKLAR                                            
020300       IF NYCKLAR-OK                                                      
020400         PERFORM F-LAES-VISA-INFO                                         
020500       END-IF                                                             
020600       COMPUTE MSG-KVLL = LENGTH OF MOD-W1O10501 + 4                      
020700       PERFORM IMS-INSERT-MSG                                             
020800     END-IF                                                               
020900                                                                          
021000     MOVE ZERO TO RETURN-CODE                                             
021100     GOBACK                                                               
021200     .                                                                    
021300     EJECT                                                                
021400 A-INIT SECTION.                                                          
021500                                                                          
021600     IF MSG-DUBBLA-TRANSKODER                                             
021700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I10501                 
021800       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
021900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
022000     ELSE                                                                 
022100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I10501                  
022200       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
022300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
022400     END-IF                                                               
022500                                                                          
022600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
022700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
022800                                                                          
022900     MOVE LOW-VALUE TO MSG-AREA                                           
023000     MOVE 'W1O105N1' TO MFS-IDMOD                                         
023100     MOVE '1105' TO MOD-IDTRANS                                           
023200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
023300                                                                          
023400     MOVE SPACE TO MFS-KDTRTYP                                            
023500     MOVE '7' TO MFS-IDPFK                                                
023600     .                                                                    
023700     EJECT                                                                
023800 B-KOLLA-NYCKLAR SECTION.                                                 
023900                                                                          
024000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
024100     MOVE '001'             TO MSGI-KDCALL                                
024200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
024300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024400     MOVE '1105'            TO MSGI-IDTRANS                               
024500                                                                          
024600     IF MFS-IDTRANS = '1105'                                              
024700       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
024800     ELSE                                                                 
024900       MOVE '++++++'   TO MID-IDARTPFX-IN                                 
025000       MOVE '++++++++' TO MID-IDARTBAS-IN                                 
025100       MOVE '++++++++' TO MID-IDARTSFX-IN                                 
025200     END-IF                                                               
025300                                                                          
025400     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
025500                                                                          
025600     MOVE MSGI-IDARTNR   TO IDARTNR-WS                                    
025700     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
025800                                                                          
025900     INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                   
026000                                                                          
026100                                                                          
026200*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
026300     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
026400                                                                          
026500     MOVE JA TO NYCKLAR-SW                                                
026600                                                                          
026700*    -- KONTROLL AV ARTIKELNUMMER                                         
026800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
026900                             MOD-IDARTPFX-IN                              
027000                             MOD-IDARTBAS-IN                              
027100                             MOD-IDARTSFX-IN                              
027200* Kolla vilken nyckel man ska köra på                                     
027300                                                                          
027400     IF   MID-IDARTPFX-IN = ALL '+'                                       
027500     AND  MID-IDARTBAS-IN = ALL '+'                                       
027600     AND  MID-IDARTSFX-IN = ALL '+'                                       
027700*      --- Volvo-nyckel                                                   
027800       IF IDARTNR-WS NUMERIC                                              
027900         MOVE IDARTNR-WS TO  W-IDARTNR   W-IDARTNR-PULS                   
028000                             MOD-IDARTNR-UT                               
028100         INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE           
028200         SET LAES-VOLVO TO TRUE                                           
028300       ELSE                                                               
028400         MOVE NEJ TO NYCKLAR-SW                                           
028500       END-IF                                                             
028600     ELSE                                                                 
028700*      --- Ford-nyckel                                                    
028800       IF MFS-IDTRANS = '1105'                                            
028900         INSPECT MID-IDARTPFX-IN REPLACING ALL '+' BY SPACE               
029000         INSPECT MID-IDARTBAS-IN REPLACING ALL '+' BY SPACE               
029100         INSPECT MID-IDARTSFX-IN REPLACING ALL '+' BY SPACE               
029200         MOVE MID-IDARTPFX-IN TO SPAR-IDARTPFX                            
029300                                 W-IDARTPFX-MIN W-IDARTPFX-MAX            
029400         MOVE MID-IDARTBAS-IN TO SPAR-IDARTBAS                            
029500                                 W-IDARTBAS-MIN W-IDARTBAS-MAX            
029600         MOVE MID-IDARTSFX-IN TO SPAR-IDARTSFX                            
029700                                 W-IDARTSFX-MIN W-IDARTSFX-MAX            
029800         SET LAES-FORD TO TRUE                                            
029900                                                                          
030000         MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                
030100         MOVE '002'      TO MSGI-KDCALL                                   
030200         CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                       
030300       END-IF                                                             
030400     END-IF                                                               
030500                                                                          
030600     IF NYCKLAR-FEL                                                       
030700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
030800       CALL WMEDKONV USING MED-WMEDAREA                                   
030900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
031000       PERFORM MFS-RENSA-FAELT-IN                                         
031100       PERFORM MFS-RENSA-FAELT-UT                                         
031200     END-IF                                                               
031300     .                                                                    
031400     EJECT                                                                
031500                                                                          
031600 F-LAES-VISA-INFO SECTION.                                                
031700     SKIP2                                                                
031800                                                                          
031900     IF LAES-VOLVO                                                        
032000       PERFORM IMS-GU-WDF701                                              
032100       IF SEGMENT-SAKNAS                                                  
032200         MOVE ERR-PART-MISSING TO MED-IDMFSFEL                            
032300         CALL WMEDKONV USING MED-WMEDAREA                                 
032400         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
032500       END-IF                                                             
032600     ELSE                                                                 
032700*      --- LÄSER MED FORD-NUMMER                                          
032800       PERFORM IMS-GN-WDF7A1                                              
032900       IF SEGMENT-FINNS                                                   
033000         MOVE SEQA-IDARTNR TO W-IDARTNR-PULS                              
033100                              W-IDARTNR                                   
033200                              MSGI-IDARTNR                                
033300*        Spara volvonumret i MSGI                                         
033400         MOVE '001'             TO MSGI-KDCALL                            
033500         CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                       
033600                                                                          
033700*        -- LÄSER WDF701 UNIKT                                            
033800         PERFORM IMS-GU-WDF701                                            
033900       ELSE                                                               
034000         MOVE ERR-PART-MISSING TO MED-IDMFSFEL                            
034100         CALL WMEDKONV USING MED-WMEDAREA                                 
034200         STRING 'FORD-' MED-TEMFSFEL DELIMITED BY SIZE                    
034300                                     INTO MOD-TEMFSFEL                    
034400       END-IF                                                             
034500     END-IF                                                               
034600                                                                          
034700     IF SEGMENT-FINNS                                                     
034800       PERFORM FA-FYLL-SKARMEN                                            
034900     ELSE                                                                 
035000       PERFORM MFS-RENSA-FAELT-UT                                         
035100*      MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
035200     END-IF                                                               
035300     .                                                                    
035400     EJECT                                                                
035500 FA-FYLL-SKARMEN    SECTION.                                              
035600     SKIP2                                                                
035700     MOVE W-IDARTNR             TO MOD-IDARTNR-UT                         
035800     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
035900                                                                          
036000     PERFORM FAA-LAES-WDK6                                                
036100     PERFORM FAB-LAES-WDD2                                                
036200                                                                          
036300     MOVE MPNR-KDARTUTF         TO MOD-KDARTUTF                           
036400     MOVE MPNR-BEARTFMC-KDP     TO MOD-BEARTFMC-KDP                       
036500     MOVE MPNR-BEARTFMC-FORD    TO MOD-BEARTFMC-FORD                      
036600     MOVE MPNR-DAREGFMC         TO MOD-DAREGFMC                           
036700     MOVE MPNR-TIREGFMC         TO MOD-TIREGFMC                           
036800     MOVE MPNR-IDCDS            TO MOD-IDCDS                              
036900     MOVE MPNR-FLGEMFMC         TO MOD-FLGEMFMC                           
037000     IF MOD-FLGEMFMC = JA                                                 
037100        MOVE  YES               TO MOD-FLGEMFMC                           
037200     END-IF                                                               
037300     STRING MPNR-IDARTPFX '-' MPNR-IDARTBAS '-'                           
037400            MPNR-IDARTSFX  DELIMITED BY SIZE INTO                         
037500                                   MOD-IDARTFMC                           
037600     .                                                                    
037700     EJECT                                                                
037800                                                                          
037900 FAA-LAES-WDK6   SECTION.                                                 
038000     SKIP2                                                                
038100     PERFORM IMS-GU-WDK601                                                
038200     IF SEGMENT-FINNS                                                     
038300        MOVE YES TO MOD-FLAGGAK6                                          
038400     ELSE                                                                 
038500        MOVE NEJ TO MOD-FLAGGAK6                                          
038600     END-IF                                                               
038700     .                                                                    
038800     EJECT                                                                
038900                                                                          
039000 FAB-LAES-WDD2   SECTION.                                                 
039100     SKIP2                                                                
039200     PERFORM IMS-GU-WDD201                                                
039300     IF SEGMENT-FINNS                                                     
039400        MOVE YES TO MOD-FLAGGAD2                                          
039500     ELSE                                                                 
039600        MOVE NEJ TO MOD-FLAGGAD2                                          
039700     END-IF                                                               
039800     .                                                                    
039900     EJECT                                                                
040000 MFS-RENSA-FAELT-UT SECTION.                                              
040100                                                                          
040200*    --- ALLA UTDATA-FÄLT                                                 
040300                                                                          
040400     MOVE MFS-RENSA-FAELT TO MOD-KDARTUTF                                 
040500                             MOD-BEARTFMC-FORD                            
040600                             MOD-BEARTFMC-KDP                             
040700                             MOD-DAREGFMC                                 
040800                             MOD-TIREGFMC                                 
040900                             MOD-IDCDS                                    
041000                             MOD-FLGEMFMC                                 
041100                             MOD-IDARTFMC                                 
041200                             MOD-FLAGGAD2                                 
041300                             MOD-FLAGGAK6                                 
041400     .                                                                    
041500     SKIP3                                                                
041600 MFS-RENSA-FAELT-IN SECTION.                                              
041700                                                                          
041800*    --- ALLA INDATA-FÄLT                                                 
041900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
042000                             MOD-IDARTPFX-IN                              
042100                             MOD-IDARTBAS-IN                              
042200                             MOD-IDARTSFX-IN                              
042300     .                                                                    
042400     EJECT                                                                
042500* --- IMS SEKTIONER ---                                                   
042600     SKIP3                                                                
042700 IMS-GET-MSG SECTION.                                                     
042800                                                                          
042900     MOVE '  QC' TO GODK-STATUSKODER                                      
043000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
043100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043200     PERFORM IMS-STATUSKONTROLL                                           
043300     .                                                                    
043400     SKIP3                                                                
043500 IMS-INSERT-MSG SECTION.                                                  
043600                                                                          
043700     IF MSGI-IDLAND-SPR = 'SE'                                            
043800       MOVE 'N' TO MFS-KDHUVOMR                                           
043900     END-IF                                                               
044000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
044100     MOVE SPACE TO GODK-STATUSKODER                                       
044200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
044300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
044400     PERFORM IMS-STATUSKONTROLL                                           
044500     .                                                                    
044600     EJECT                                                                
044700 IMS-GU-WDF701 SECTION.                                                   
044800                                                                          
044900     STRING 'WDF701  (WDF701KY =' W-WDF701KY-X ')'                        
045000          DELIMITED BY SIZE INTO SSA1                                     
045100     MOVE '  GE' TO GODK-STATUSKODER                                      
045200     CALL CBLTDLI USING GU WDF7-PCB DLI-IO-WDF701 SSA1                    
045300     MOVE WDF7-STATUS-CODE TO STATUS-WS                                   
045400     PERFORM IMS-STATUSKONTROLL                                           
045500     .                                                                    
045600     EJECT                                                                
045700 IMS-GN-WDF7A1 SECTION.                                                   
045800                                                                          
045900     STRING 'WDF7A1  (WDF7A1KY>=' W-WDF7A-MIN-X                           
046000                     '&WDF7A1KY<=' W-WDF7A-MAX-X ')'                      
046100          DELIMITED BY SIZE INTO SSA1                                     
046200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
046300     CALL CBLTDLI USING GN WDF7A-PCB DLI-IO-WDF7A1 SSA1                   
046400     MOVE WDF7A-STATUS-CODE TO STATUS-WS                                  
046500     PERFORM IMS-STATUSKONTROLL                                           
046600     .                                                                    
046700     EJECT                                                                
046800 IMS-GU-WDD201 SECTION.                                                   
046900                                                                          
047000     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
047100          DELIMITED BY SIZE INTO SSA1                                     
047200     MOVE '  GE' TO GODK-STATUSKODER                                      
047300     CALL CBLTDLI USING GU WDD2-PCB DLI-IO-WDD201 SSA1                    
047400     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
047500     PERFORM IMS-STATUSKONTROLL                                           
047600     .                                                                    
047700     EJECT                                                                
047800 IMS-GU-WDK601 SECTION.                                                   
047900                                                                          
048000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
048100          DELIMITED BY SIZE INTO SSA1                                     
048200     MOVE '  GE' TO GODK-STATUSKODER                                      
048300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
048400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
048500     PERFORM IMS-STATUSKONTROLL                                           
048600     .                                                                    
048700     EJECT                                                                
048800 IMS-STATUSKONTROLL SECTION.                                              
048900                                                                          
049000     SET STATUS-IX TO 1                                                   
049100     SEARCH GODK-STATUS                                                   
049200       AT END                                                             
049300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
049400         DELIMITED BY SIZE INTO FELTEXT                                   
049500         CALL FELLOG                                                      
049600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
049700         CONTINUE                                                         
049800     END-SEARCH                                                           
049900     .                                                                    
