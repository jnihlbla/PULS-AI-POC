001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W5021400.                                                
001500 AUTHOR.         JONNY SANDSTEN.                                          
001600 DATE-WRITTEN.   98/06/23.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        PROGRAMMET SYFTE ÄR ATT HANTERA NYREGISTERING                    
002100*        AV EKONOMISK HUVUDHÄNDELSETYP M.H.A AV VÄRDEN                    
002200*        INMATADE FRÅN BILD 5214                                          
002300*                                                                         
002410*        PROGRAMMET LÄSER/UPPDATERAR WDH5                                 
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W5T214, W5T214U                                     
002800*        MID:         W5I21401                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W5O21401                                            
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003701                                                                          
003710*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W5021400'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004900                                                                          
005002 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005003     88  ALLT-OK                             VALUE 'J'.                   
005004                                                                          
005005 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005006     88  INDATA-OK                           VALUE 'J'.                   
005010     88  INDATA-FEL                          VALUE 'N'.                   
005100                                                                          
005200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005300     88  NYCKLAR-OK                          VALUE 'J'.                   
005400     88  NYCKLAR-FEL                         VALUE 'N'.                   
005500                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  EGEN-MID                            VALUE '5214'.                
005800     88  GODK-MID                            VALUE '5211' '5212'          
005900                                                   '5213' '5214'          
006000                                                   '5215' '5216'          
006100                                                   '5217' '5218'          
006200                                                   '5219'.                
006300     88  HELP-MID                            VALUE '0551'.                
006400     EJECT                                                                
006500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006600 01  GENERELLA-SUBPROGRAM.                                                
006700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007400*01 -COPY WMEDAREA                                                        
007500     SKIP3                                                                
007600 01  MESSAGE-CODES.                                                       
007701     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007702     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007703     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007710     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007720     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008000     03  RAD-FINNS-REDAN         PIC X(3)    VALUE '245'.                 
008010     03  KDEKHHT-MISSING         PIC X(3)    VALUE '269'.                 
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008500     SKIP3                                                                
008600*01 -COPY WMSGINIT                                                        
008800     EJECT                                                                
008900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009200     SKIP3                                                                
009300*01  MID -COPY W5I21401                                                   
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009600     SKIP3                                                                
009700*01  -COPY WMSGAREA                                                       
009800     EJECT                                                                
009900     03  MOD REDEFINES MSG-AREA.                                          
010000*      05  -COPY W5O21401                                                 
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010300     SKIP3                                                                
010400*01  -COPY WMFSAREA                                                       
010500     EJECT                                                                
010600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010700*                                                                         
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011000     SKIP3                                                                
011100 01  NYCKLAR-TILL-DLI.                                                    
011201     03  W-WDH501KY-X.                                                    
011202         05  W-IDFTG             PIC 9(2)    VALUE ZERO.                  
011210         05  W-KDEKHHT           PIC X(3)    VALUE SPACE.                 
011300     SKIP2                                                                
011400*    --- STATUS-KOD FRÅN IMS                                              
011500 01  STATUS-WS                   PIC XX.                                  
011600     88  SEGMENT-FINNS                       VALUE '  '.                  
011700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011900     SKIP2                                                                
012000 01  GODK-STATUSKODER.                                                    
012100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012200     SKIP3                                                                
012300 01  SSA1                        PIC X(64).                               
012400 01  SSA2                        PIC X(64).                               
012500     EJECT                                                                
012600*    --- IMS FUNKTIONSKODER                                               
012700*01  -COPY W0003                                                          
012900     EJECT                                                                
013000*    ---  DLI INPUT-OUTPUT AREA                                           
013100                                                                          
013201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
013202 01  DLI-IO-WDH501.                                                       
013210*    03  -COPY WDH501                                                     
013500     EJECT                                                                
013600 LINKAGE SECTION.                                                         
013700*01  -COPY W0009   -PRE MSG-                                              
013800*01  -COPY W0008   -PRE USEA-                                             
013900     05  FILLER                  PIC X.                                   
014001                                                                          
014002*01  -COPY W0008  -PRE WDH5-                                              
014010     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014201 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDH5-PCB.                     
014202 MAIN SECTION.                                                            
014210     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDH5-PCB.                     
014300                                                                          
014500     PERFORM IMS-GET-MSG                                                  
014600     IF SEGMENT-FINNS                                                     
014700       PERFORM A-INIT                                                     
014800       PERFORM B-KOLLA-NYCKLAR                                            
014900       IF NYCKLAR-OK                                                      
015001         IF MFS-UPDATE                                                    
015002           PERFORM G-KOLLA-INPUT                                          
015003           IF INDATA-OK                                                   
015004             PERFORM H-UPPDATERA                                          
015005           END-IF                                                         
015010         ELSE                                                             
015201           IF MFS-FIRST                                                   
015202             PERFORM C-FOERSTA-SIDA                                       
015203           ELSE                                                           
015204             PERFORM E-SAMMA-SIDA                                         
015210           END-IF                                                         
015310         END-IF                                                           
015320         IF ALLT-OK                                                       
015400           PERFORM F-LAES-VISA-INFO                                       
015410         END-IF                                                           
015500       END-IF                                                             
015800       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O21401 + 4                      
015900       PERFORM IMS-INSERT-MSG                                             
016000     END-IF                                                               
016200                                                                          
016300     MOVE ZERO TO RETURN-CODE                                             
016400     GOBACK                                                               
016500     .                                                                    
016600     EJECT                                                                
016700 A-INIT SECTION.                                                          
016800                                                                          
016900     IF MSG-DUBBLA-TRANSKODER                                             
017000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I21401                 
017100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017300     ELSE                                                                 
017400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I21401                  
017500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017700     END-IF                                                               
017800                                                                          
017900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018200                                                                          
018300     MOVE LOW-VALUE TO MSG-AREA                                           
018400     MOVE 'W5O214N1' TO MFS-IDMOD                                         
018500     MOVE '5214' TO MOD-IDTRANS                                           
018600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018700                                                                          
018800     IF EGEN-MID OR HELP-MID                                              
018900       CONTINUE                                                           
019000     ELSE                                                                 
019100       MOVE SPACE TO MFS-KDTRTYP                                          
019200       MOVE '7' TO MFS-IDPFK                                              
019300     END-IF                                                               
019600     .                                                                    
019700     EJECT                                                                
019800 B-KOLLA-NYCKLAR SECTION.                                                 
019900                                                                          
020000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020100     MOVE '001'             TO MSGI-KDCALL                                
020200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020400     MOVE '5214'            TO MSGI-IDTRANS                               
020410     IF EGEN-MID                                                          
020411       MOVE MID-KDEKHHT-IN    TO MSGI-KDEKHHT                             
020492     END-IF                                                               
020530     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020540                                                                          
020550     IF MSGI-IDLAND-SPR = 'GB'                                            
020560       MOVE 'GB' TO MED-IDSKYLT                                           
020570     ELSE                                                                 
020580       MOVE 'S' TO MED-IDSKYLT                                            
020590     END-IF                                                               
020900                                                                          
021000     MOVE JA TO ALLT-SW                                                   
021010     MOVE JA TO NYCKLAR-SW                                                
021100                                                                          
021200     MOVE MFS-RENSA-FAELT   TO MOD-KDEKHHT-IN                             
021400                                                                          
021410*    -- KONTROLL AV NYCKLAR                                               
021420                                                                          
021480     IF MSGI-KDEKHHT NUMERIC AND MSGI-KDEKHHT > ZERO                      
021490       MOVE MSGI-KDEKHHT    TO W-KDEKHHT                                  
021491     ELSE                                                                 
021492       MOVE NEJ             TO NYCKLAR-SW                                 
021493     END-IF                                                               
021494                                                                          
021495     IF MSGI-IDFTG   NUMERIC AND MSGI-IDFTG   > ZERO                      
021496       MOVE MSGI-IDFTG      TO W-IDFTG                                    
021497     END-IF                                                               
021498                                                                          
021499     IF GODK-MID OR NYCKLAR-OK                                            
021500       MOVE MSGI-KDEKHHT    TO MOD-KDEKHHT-UT                             
021510       INSPECT MOD-KDEKHHT-UT REPLACING LEADING ZERO BY SPACE             
021511       MOVE MSGI-IDFTG      TO MOD-IDFTG-UT                               
021514     ELSE                                                                 
021515       MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-UT                             
021516                               MOD-IDFTG-UT                               
021521     END-IF                                                               
021522                                                                          
021530     IF NYCKLAR-FEL                                                       
021540*---FÖR ATT INTE FÅ NYCKLAR FEL NÄR MAN KOMMER FRÅN EN ICKE               
021550*---GODKÄND BILD                                                          
021560       IF GODK-MID                                                        
021600         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
021700         CALL WMEDKONV USING MED-WMEDAREA                                 
021800         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
021900         PERFORM MFS-RENSA-FAELT-IN                                       
022000         PERFORM MFS-RENSA-FAELT-UT                                       
022100       END-IF                                                             
022110     END-IF                                                               
022200     .                                                                    
022400     EJECT                                                                
022501 C-FOERSTA-SIDA SECTION.                                                  
022502                                                                          
022503     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
022504     CALL WMEDKONV USING MED-WMEDAREA                                     
022505     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
022506     PERFORM MFS-RENSA-FAELT-IN                                           
022507     .                                                                    
022508     EJECT                                                                
022509 E-SAMMA-SIDA SECTION.                                                    
022510                                                                          
022511     IF EGEN-MID OR HELP-MID                                              
022512       IF MID-BEEKHHT = ALL '+'                                           
022514         PERFORM MFS-RENSA-FAELT-IN                                       
022515       ELSE                                                               
022516         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
022517         CALL WMEDKONV USING MED-WMEDAREA                                 
022518         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
022519         PERFORM MFS-ROER-EJ-FAELT-IN                                     
022520         PERFORM MFS-LAES-IN-IGEN                                         
022521         MOVE NEJ TO ALLT-SW                                              
022522       END-IF                                                             
022523     ELSE                                                                 
022524       PERFORM MFS-RENSA-FAELT-IN                                         
022525     END-IF                                                               
022526     .                                                                    
022527     EJECT                                                                
022600 F-LAES-VISA-INFO SECTION.                                                
022700                                                                          
022910     PERFORM IMS-GET-HHT                                                  
022920     MOVE HHT-BEEKHHT TO MOD-BEEKHHT                                      
022930                                                                          
023000     IF SEGMENT-SAKNAS                                                    
023010        MOVE KDEKHHT-MISSING TO MED-IDMFSFEL                              
023200        CALL WMEDKONV USING MED-WMEDAREA                                  
023300        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
023400        PERFORM MFS-RENSA-FAELT-UT                                        
023500     ELSE                                                                 
023510        IF MFS-UPDATE                                                     
023511          PERFORM MFS-RENSA-FAELT-UT                                      
023520        ELSE                                                              
023521          MOVE HHT-BEEKHHT TO MOD-BEEKHHT                                 
023530        END-IF                                                            
023700     END-IF                                                               
023800     .                                                                    
023900     EJECT                                                                
024802 G-KOLLA-INPUT SECTION.                                                   
024803                                                                          
024804     MOVE JA  TO INDATA-SW                                                
024805     IF MID-KDEKHHT-IN = ALL '+'                                          
024806      AND MID-BEEKHHT = ALL '+'                                           
024807       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
024808       CALL WMEDKONV USING MED-WMEDAREA                                   
024809       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024810       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024811       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024812       MOVE NEJ TO INDATA-SW                                              
024813     ELSE                                                                 
024814*---BEEKHHT FÅR EJ VARA TOMT                                              
024815       IF MID-BEEKHHT = ALL '+'                                           
024816          MOVE MFS-ALFA-FAELT-FEL TO MOD-BEEKHHT-ATTR                     
024817          MOVE NEJ TO INDATA-SW                                           
024818       ELSE                                                               
024819          MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEEKHHT-ATTR                   
024820       END-IF                                                             
024821                                                                          
024846       IF INDATA-FEL                                                      
024847         MOVE NEJ TO ALLT-SW                                              
024848         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
024849         CALL WMEDKONV USING MED-WMEDAREA                                 
024850         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
024851         PERFORM MFS-ROER-EJ-FAELT-UT                                     
024852         PERFORM MFS-ROER-EJ-FAELT-IN                                     
024865       END-IF                                                             
024866     END-IF                                                               
024867     .                                                                    
024868     EJECT                                                                
024869 H-UPPDATERA SECTION.                                                     
024870                                                                          
024871     MOVE MSGI-IDFTG     TO HHT-IDFTG                                     
024872     MOVE MSGI-KDEKHHT   TO HHT-KDEKHHT                                   
024874     MOVE MID-BEEKHHT    TO HHT-BEEKHHT                                   
024875     PERFORM IMS-ISRT-HHT                                                 
024876     IF SEGMENT-FINNS-REDAN                                               
024877       MOVE RAD-FINNS-REDAN TO MED-IDMFSINF                               
024878     ELSE                                                                 
024879       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
024880     END-IF                                                               
024881                                                                          
024883     CALL WMEDKONV USING MED-WMEDAREA                                     
024884     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
024885     PERFORM MFS-FORM-ATTR                                                
024886     PERFORM MFS-RENSA-FAELT-IN                                           
024890     .                                                                    
024900     EJECT                                                                
025000 MFS-RENSA-FAELT-UT SECTION.                                              
025100                                                                          
025200*    --- ALLA UTDATA-FÄLT                                                 
025400     MOVE MFS-RENSA-FAELT TO MOD-BEEKHHT                                  
025600     .                                                                    
025800     SKIP3                                                                
025900 MFS-RENSA-FAELT-IN SECTION.                                              
026000                                                                          
026100*    --- ALLA INDATA-FÄLT                                                 
026200     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-IN                               
026300                             MOD-BEEKHHT                                  
026400     .                                                                    
026500     EJECT                                                                
026600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026700                                                                          
026800*    --- ALLA UTDATA-FÄLT                                                 
027000     MOVE MFS-ROER-EJ-FAELT TO MOD-BEEKHHT                                
027300     .                                                                    
027400     SKIP3                                                                
027500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027600                                                                          
027700*    --- ALLA INDATA-FÄLT                                                 
027800     MOVE MFS-ROER-EJ-FAELT TO MOD-BEEKHHT                                
028000     .                                                                    
028100     EJECT                                                                
028200 MFS-FORM-ATTR SECTION.                                                   
028300                                                                          
028400*    --- ALLA INDATA-FÄLT                                                 
028500     MOVE MFS-FORMATETS-ATTR TO MOD-BEEKHHT-ATTR                          
028700     .                                                                    
028800     SKIP2                                                                
028900 MFS-LAES-IN-IGEN SECTION.                                                
029000                                                                          
029100*    --- ALLA INDATA-FÄLT                                                 
029200     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEEKHHT-ATTR                       
029400     .                                                                    
029500     EJECT                                                                
029600* --- IMS SEKTIONER ---                                                   
029700     SKIP3                                                                
029800 IMS-GET-MSG SECTION.                                                     
029900                                                                          
030000     MOVE '  QC' TO GODK-STATUSKODER                                      
030100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030300     PERFORM IMS-STATUSKONTROLL                                           
030400     .                                                                    
030500     SKIP3                                                                
030600 IMS-INSERT-MSG SECTION.                                                  
030700                                                                          
031100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031200     MOVE SPACE TO GODK-STATUSKODER                                       
031300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031500     PERFORM IMS-STATUSKONTROLL                                           
031600     .                                                                    
031701     EJECT                                                                
031702 IMS-GET-HHT SECTION.                                                     
031703                                                                          
031704     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031705          DELIMITED BY SIZE INTO SSA1                                     
031706     MOVE '  GE' TO GODK-STATUSKODER                                      
031707     CALL CBLTDLI USING GHU WDH5-PCB DLI-IO-WDH501 SSA1                   
031708     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031709     PERFORM IMS-STATUSKONTROLL                                           
031710     .                                                                    
031711     SKIP3                                                                
031712 IMS-ISRT-HHT SECTION.                                                    
031713                                                                          
031714     MOVE 'WDH501   ' TO SSA1                                             
031715     MOVE '  II' TO GODK-STATUSKODER                                      
031716     CALL CBLTDLI USING ISRT WDH5-PCB DLI-IO-WDH501 SSA1                  
031717     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031718     PERFORM IMS-STATUSKONTROLL                                           
031720     .                                                                    
031800     EJECT                                                                
031900 IMS-STATUSKONTROLL SECTION.                                              
032000                                                                          
032100     SET STATUS-IX TO 1                                                   
032200     SEARCH GODK-STATUS                                                   
032300       AT END                                                             
032400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032500         DELIMITED BY SIZE INTO FELTEXT                                   
032600         CALL FELLOG                                                      
032700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032800         CONTINUE                                                         
032900     END-SEARCH                                                           
033000     .                                                                    
