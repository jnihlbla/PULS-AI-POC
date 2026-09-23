000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4048100.                                                
000300 AUTHOR.         GÖRAN KJELLSON  GUIDE                                    
000400 DATE-WRITTEN.   NOVEMBER  2006                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KUNDREGISTER.GODSMOTTAGARE NAMN/ADRESS                           
000900*        FIX FÖR ATT JUSTERA AADRESSEN I SAMBAND MED                      
001000*        UPPDELNING AV POSTADRESSFÄLT I POSTNUMMER STAD                   
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLGMTA (WDB2)                              
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W4T481                                              
001600*        MID:         W4I48101                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W4O48101                                            
002000*                                                                         
002100*                                                                         
002200                                                                          
002300                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800 77  IDPGM                       PIC X(08)   VALUE 'W4048100'.            
002900                                                                          
003000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500                                                                          
003600 77  CURRENT-SECTION             PIC X(16)   VALUE 'MAIN'.                
003700 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE 'MAIN'.                
003800                                                                          
003900 77  WS-IDDISTR                  PIC 9(4)    VALUE ZERO.                  
004000 77  WS-IDKUNDNR                 PIC 9(6)    VALUE ZERO.                  
004100                                                                          
004200 01  WS-ADGMT-PADR-L.                                                     
004300     03  WS-ADPOSTNR-L           PIC X(10).                               
004400     03  WS-ADCITY-L             PIC X(25).                               
004500 01  WS-ADGMT-PADR-R.                                                     
004600     03  WS-ADCITY-R             PIC X(25).                               
004700     03  WS-ADPOSTNR-R           PIC X(10).                               
004800                                                                          
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005000                                                                          
005100 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005200     88  ALLT-OK                             VALUE 'J'.                   
005300                                                                          
005400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005500     88  INDATA-OK                           VALUE 'J'.                   
005600     88  INDATA-FEL                          VALUE 'N'.                   
005700                                                                          
005800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005900     88  NYCKLAR-OK                          VALUE 'J'.                   
006000     88  NYCKLAR-FEL                         VALUE 'N'.                   
006100                                                                          
006200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006300     88  EGEN-MID                            VALUE '4481'.                
006400     88  GODK-MID                            VALUE '4481'.                
006500     88  HELP-MID                            VALUE '0551'.                
006600                                                                          
006700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006800 01  GENERELLA-SUBPROGRAM.                                                
006900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200                                                                          
007300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007400*01 -COPY WMEDAREA                                                        
007500 01  MESSAGE-CODES.                                                       
007600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008000                                                                          
008100                                                                          
008200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008500*01  MID -COPY W4I48101                                                   
008600                                                                          
008700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
008800*01  -COPY WMSGAREA                                                       
008900                                                                          
009000     03  MOD REDEFINES MSG-AREA.                                          
009100*      05  -COPY W4O48101                                                 
009200                                                                          
009300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009400*01  -COPY WMFSAREA                                                       
009500                                                                          
009600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009700*                                                                         
009800                                                                          
009900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010000     SKIP3                                                                
010100 01  NYCKLAR-TILL-DLI.                                                    
010200     03  W-IDGMT-X.                                                       
010300         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
010400         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
010500                                                                          
010600*    --- STATUS-KOD FRÅN IMS                                              
010700 01  STATUS-WS                   PIC XX.                                  
010800     88  SEGMENT-FINNS                       VALUE '  '.                  
010900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011000                                                                          
011100 01  GODK-STATUSKODER.                                                    
011200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011300                                                                          
011400 01  SSA1                        PIC X(64).                               
011500                                                                          
011600*    --- IMS FUNKTIONSKODER                                               
011700*01  -COPY W0003                                                          
011800                                                                          
011900*    ---  DLI INPUT-OUTPUT AREA                                           
012000 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
012100 01  DLI-IO-WDB201.                                                       
012200*    03  -COPY WDB201                                                     
012300                                                                          
012400 LINKAGE SECTION.                                                         
012500*01  -COPY W0009   -PRE MSG-                                              
012600                                                                          
012700*01  -COPY W0008  -PRE WDB2-                                              
012800     05  FILLER                  PIC X.                                   
012900                                                                          
013000 PROCEDURE DIVISION  USING MSG-PCB WDB2-PCB.                              
013100 MAIN SECTION.                                                            
013200     ENTRY 'DLITCBL' USING MSG-PCB WDB2-PCB.                              
013300                                                                          
013400     PERFORM IMS-GET-MSG                                                  
013500     IF SEGMENT-FINNS                                                     
013600       PERFORM A-INIT                                                     
013700       PERFORM B-KOLLA-NYCKLAR                                            
013800       IF NYCKLAR-OK                                                      
013900         IF MFS-UPDATE                                                    
014000           PERFORM G-KOLLA-INPUT                                          
014100           IF INDATA-OK                                                   
014200             PERFORM H-UPPDATERA                                          
014300           END-IF                                                         
014400         ELSE                                                             
014500           IF MFS-FIRST                                                   
014600             PERFORM C-FOERSTA-SIDA                                       
014700           ELSE                                                           
014800              IF MFS-NEXT                                                 
014900                 PERFORM D-NEXT-PAGE                                      
015000              END-IF                                                      
015100           END-IF                                                         
015200           IF ALLT-OK                                                     
015300              PERFORM F-LAES-VISA-INFO                                    
015400           END-IF                                                         
015500         END-IF                                                           
015600       END-IF                                                             
015700                                                                          
015800       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O48101 + 4                      
015900       PERFORM IMS-INSERT-MSG                                             
016000     END-IF                                                               
016100                                                                          
016200     MOVE ZERO TO RETURN-CODE                                             
016300     GOBACK                                                               
016400     .                                                                    
016500                                                                          
016600 A-INIT SECTION.                                                          
016700     MOVE 'A-INIT'  TO CURRENT-SECTION                                    
016800                                                                          
016900     IF MSG-DUBBLA-TRANSKODER                                             
017000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I48101                 
017100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017300     ELSE                                                                 
017400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I48101                  
017500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017700     END-IF                                                               
017800                                                                          
017900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018200                                                                          
018300     MOVE LOW-VALUE TO MSG-AREA                                           
018400     MOVE 'W4O481N1' TO MFS-IDMOD                                         
018500     MOVE '4481' TO MOD-IDTRANS                                           
018600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018700                                                                          
018800     IF EGEN-MID OR HELP-MID                                              
018900       CONTINUE                                                           
019000     ELSE                                                                 
019100       MOVE SPACE TO MFS-KDTRTYP                                          
019200       MOVE '7' TO MFS-IDPFK                                              
019300     END-IF                                                               
019400                                                                          
019500     MOVE 'GB' TO MED-IDSKYLT                                             
019600     .                                                                    
019700                                                                          
019800 B-KOLLA-NYCKLAR SECTION.                                                 
019900     MOVE 'B-KOLLA NYCKLAR'  TO CURRENT-SECTION                           
020000                                                                          
020100     MOVE JA TO NYCKLAR-SW                                                
020200                                                                          
020300*    -- KONTROLL AV IDDISTR                                               
020400     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
020500                                                                          
020600     IF MID-IDDISTR-IN NOT = ALL '+'                                      
020700       MOVE '7'             TO MFS-IDPFK                                  
020800       MOVE SPACE           TO MFS-KDTRTYP                                
020900       MOVE MID-IDDISTR-IN  TO W-IDDISTR                                  
021000                               MOD-IDDISTR-UT                             
021100                                                                          
021200       IF MID-IDKUNDNR-IN NOT = ALL '+'                                   
021300          MOVE MID-IDKUNDNR-IN TO W-IDKUNDNR                              
021400                                  MOD-IDKUNDNR-UT                         
021500       ELSE                                                               
021600          MOVE ZERO            TO W-IDKUNDNR                              
021700       END-IF                                                             
021800     ELSE                                                                 
021900       MOVE ZERO            TO W-IDDISTR                                  
022000     END-IF                                                               
022100                                                                          
022200*    -- KONTROLL AV IDKUNDNR                                              
022300     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
022400     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
022500        MOVE '7'             TO MFS-IDPFK                                 
022600        MOVE SPACE           TO MFS-KDTRTYP                               
022700        MOVE MID-IDKUNDNR-IN TO W-IDKUNDNR                                
022800                                MOD-IDKUNDNR-UT                           
022900     ELSE                                                                 
023000        MOVE ZERO            TO W-IDKUNDNR                                
023100     END-IF                                                               
023200                                                                          
023300     IF MID-FLSHOW = 'J' OR 'Y'                                           
023400        MOVE JA  TO MID-FLSHOW                                            
023500                    MOD-FLSHOW                                            
023600     ELSE                                                                 
023700        MOVE NEJ TO MID-FLSHOW                                            
023800                    MOD-FLSHOW                                            
023900     END-IF                                                               
024000     .                                                                    
024100                                                                          
024200 C-FOERSTA-SIDA SECTION.                                                  
024300     MOVE 'C-FOERSTA-SIDA '  TO CURRENT-SECTION                           
024400                                                                          
024500     MOVE JA TO ALLT-SW                                                   
024600     PERFORM MFS-RENSA-FAELT-IN                                           
024700     IF MID-IDDISTR-IN = ALL '+'                                          
024800        MOVE MID-IDDISTR-UT  TO WS-IDDISTR                                
024900        MOVE WS-IDDISTR      TO W-IDDISTR                                 
025000     END-IF                                                               
025100     .                                                                    
025200                                                                          
025300 D-NEXT-PAGE    SECTION.                                                  
025400     MOVE 'D-NEXT-PAGE    '  TO CURRENT-SECTION                           
025500                                                                          
025600     MOVE JA TO ALLT-SW                                                   
025700     MOVE MID-IDDISTR-UT  TO WS-IDDISTR                                   
025800     MOVE WS-IDDISTR      TO W-IDDISTR                                    
025900     MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR                                  
026000     MOVE WS-IDKUNDNR     TO W-IDKUNDNR                                   
026100     ADD +1               TO W-IDKUNDNR                                   
026200*    PERFORM MFS-RENSA-FAELT-IN                                           
026300     .                                                                    
026400                                                                          
026500 F-LAES-VISA-INFO SECTION.                                                
026600     MOVE 'F-LAES-VISA-INFO      '  TO CURRENT-SECTION                    
026700                                                                          
026800     PERFORM IMS-01-GU-WDB201                                             
026900     IF SEGMENT-SAKNAS                                                    
027000        MOVE 'GOODS RECEIVER MISSING' TO MOD-TEMFSFEL                     
027100        PERFORM MFS-RENSA-FAELT-UT                                        
027200     ELSE                                                                 
027300        IF MID-FLSHOW = 'N'                                               
027400           PERFORM UNTIL SEGMENT-SAKNAS OR                                
027500                         GMT-KDPOSTNR = SPACE                             
027600              PERFORM IMS-02-GN-WDB201                                    
027700           END-PERFORM                                                    
027800*          IF SEGMENT-FINNS                                               
027900*             IF W-IDDISTR > 0                                            
028000*                IF W-IDDISTR NOT = GMT-IDDISTR                           
028100*                   MOVE 'GE'  TO STATUS-WS                               
028200*                   MOVE 'GOODS RECEIVER MISSING' TO MOD-TEMFSFEL         
028300*                   PERFORM MFS-RENSA-FAELT-UT                            
028400*                END-IF                                                   
028500*             END-IF                                                      
028600*          END-IF                                                         
028700        END-IF                                                            
028800                                                                          
028900        IF SEGMENT-FINNS                                                  
029000           MOVE GMT-IDDISTR         TO WS-IDDISTR                         
029100           MOVE WS-IDDISTR          TO MOD-IDDISTR-UT                     
029200           MOVE GMT-IDKUNDNR        TO WS-IDKUNDNR                        
029300           MOVE WS-IDKUNDNR         TO MOD-IDKUNDNR-UT                    
029400           MOVE GMT-BEGMT-RAD1      TO MOD-BEGMT-RAD1                     
029500                                           MOD-BEGMT-RAD1-NEW             
029600           MOVE GMT-BEGMT-RAD2      TO MOD-BEGMT-RAD2                     
029700                                           MOD-BEGMT-RAD2-NEW             
029800           MOVE GMT-ADGMT-GATA      TO MOD-ADGMT-GATA                     
029900                                           MOD-ADGMT-GATA-NEW             
030000           IF GMT-KDPOSTNR = 'L'                                          
030100              MOVE GMT-ADPOSTNR IN GMT-ADPOST-PNRORT                      
030200                                    TO MOD-ADPOSTNR                       
030300                                        WS-ADPOSTNR-L                     
030400              MOVE GMT-ADCITY   IN GMT-ADPOST-PNRORT                      
030500                                    TO MOD-ADCITY                         
030600                                        WS-ADCITY-L                       
030700              MOVE WS-ADGMT-PADR-L  TO MOD-ADGMT-PADR                     
030800           ELSE                                                           
030900              MOVE GMT-ADPOSTNR IN GMT-ADPOST-ORTPNR                      
031000                                    TO MOD-ADPOSTNR                       
031100                                        WS-ADPOSTNR-R                     
031200              MOVE GMT-ADCITY   IN GMT-ADPOST-ORTPNR                      
031300                                    TO MOD-ADCITY                         
031400                                        WS-ADCITY-R                       
031500              MOVE WS-ADGMT-PADR-R  TO MOD-ADGMT-PADR                     
031600           END-IF                                                         
031700           MOVE GMT-ADGMT-LAND      TO MOD-ADGMT-LAND                     
031800                                           MOD-ADGMT-LAND-NEW             
031900           MOVE GMT-KDPOSTNR        TO MOD-KDPOSTNR                       
032000        END-IF                                                            
032100     END-IF                                                               
032200     .                                                                    
032300     EJECT                                                                
032400 G-KOLLA-INPUT SECTION.                                                   
032500     MOVE 'G-KOLLA-INPUT         '  TO CURRENT-SECTION                    
032600                                                                          
032700     MOVE JA  TO INDATA-SW                                                
032800                                                                          
032900     MOVE MID-IDDISTR-UT  TO WS-IDDISTR                                   
033000     MOVE WS-IDDISTR      TO W-IDDISTR                                    
033100     MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR                                  
033200     MOVE WS-IDKUNDNR     TO W-IDKUNDNR                                   
033300     PERFORM IMS-GHU-WDB201                                               
033400     IF MID-INPUT = ALL '+'                                               
033500        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
033600        CALL WMEDKONV USING MED-WMEDAREA                                  
033700        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
033800        PERFORM MFS-ROER-EJ-FAELT-IN                                      
033900        PERFORM MFS-ROER-EJ-FAELT-UT                                      
034000        MOVE NEJ TO INDATA-SW                                             
034100     ELSE                                                                 
034200        IF MID-BEGMT-RAD1-NEW NOT = ALL '+'                               
034300           IF MID-BEGMT-RAD1-NEW NOT = SPACE                              
034400             MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEGMT-RAD1-ATTR             
034500           ELSE                                                           
034600             MOVE MFS-ALFA-FAELT-FEL   TO MOD-BEGMT-RAD1-ATTR             
034700             MOVE NEJ TO INDATA-SW                                        
034800           END-IF                                                         
034900        END-IF                                                            
035000                                                                          
035100        IF MID-BEGMT-RAD2-NEW NOT = ALL '+'                               
035200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEGMT-RAD2-ATTR               
035300        END-IF                                                            
035400                                                                          
035500        IF MID-ADGMT-GATA-NEW NOT = ALL '+'                               
035600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADGMT-GATA-ATTR               
035700        END-IF                                                            
035800                                                                          
035900        IF MID-ADPOSTNR   NOT = ALL '+'                                   
036000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADPOSTNR-ATTR                 
036100        END-IF                                                            
036200                                                                          
036300        IF MID-KDPOSTNR   NOT = ALL '+'                                   
036400           IF MID-KDPOSTNR = 'L' OR 'R'                                   
036500              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPOSTNR-ATTR              
036600           ELSE                                                           
036700              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDPOSTNR-ATTR              
036800              MOVE NEJ TO INDATA-SW                                       
036900           END-IF                                                         
037000        ELSE                                                              
037100           MOVE MFS-ALFA-FAELT-FEL      TO MOD-KDPOSTNR-ATTR              
037200           MOVE NEJ TO INDATA-SW                                          
037300        END-IF                                                            
037400                                                                          
037500        IF MID-ADCITY     NOT = ALL '+'                                   
037600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADCITY-ATTR                   
037700        END-IF                                                            
037800                                                                          
037900        IF MID-ADGMT-LAND-NEW NOT = ALL '+'                               
038000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADGMT-LAND-ATTR               
038100        END-IF                                                            
038200                                                                          
038300                                                                          
038400        IF INDATA-FEL                                                     
038500           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
038600           CALL WMEDKONV USING MED-WMEDAREA                               
038700           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
038800           MOVE NEJ TO ALLT-SW                                            
038900           PERFORM MFS-ROER-EJ-FAELT-UT                                   
039000           PERFORM MFS-ROER-EJ-FAELT-IN                                   
039100        END-IF                                                            
039200     END-IF                                                               
039300     .                                                                    
039400                                                                          
039500                                                                          
039600 H-UPPDATERA SECTION.                                                     
039700     MOVE 'H-UPPDATERA           '  TO CURRENT-SECTION                    
039800                                                                          
039900     PERFORM IMS-GHU-WDB201                                               
040000                                                                          
040100     IF MID-BEGMT-RAD1-NEW NOT = ALL '+'                                  
040200        MOVE MID-BEGMT-RAD1-NEW    TO GMT-BEGMT-RAD1                      
040300        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEGMT-RAD1-ATTR                 
040400     ELSE                                                                 
040500        MOVE MFS-ROER-EJ-FAELT     TO MOD-BEGMT-RAD1-ATTR                 
040600     END-IF                                                               
040700                                                                          
040800     IF MID-BEGMT-RAD2-NEW NOT = ALL '+'                                  
040900        MOVE MID-BEGMT-RAD2-NEW    TO GMT-BEGMT-RAD2                      
041000        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEGMT-RAD2-ATTR                 
041100     ELSE                                                                 
041200        MOVE MFS-ROER-EJ-FAELT     TO MOD-BEGMT-RAD2-ATTR                 
041300     END-IF                                                               
041400                                                                          
041500     IF MID-ADGMT-GATA-NEW NOT = ALL '+'                                  
041600        MOVE MID-ADGMT-GATA-NEW    TO GMT-ADGMT-GATA                      
041700        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADGMT-GATA-ATTR                 
041800     ELSE                                                                 
041900        MOVE MFS-ROER-EJ-FAELT     TO MOD-ADGMT-GATA-ATTR                 
042000     END-IF                                                               
042100                                                                          
042200     IF MID-KDPOSTNR   NOT = ALL '+'                                      
042300        MOVE MID-KDPOSTNR          TO GMT-KDPOSTNR                        
042400        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDPOSTNR-ATTR                   
042500     ELSE                                                                 
042600        MOVE MFS-ROER-EJ-FAELT     TO MOD-KDPOSTNR-ATTR                   
042700     END-IF                                                               
042800                                                                          
042900     IF MID-ADPOSTNR   NOT = ALL '+'                                      
043000        IF MID-KDPOSTNR = 'L'                                             
043100           MOVE MID-ADPOSTNR       TO GMT-ADPOSTNR                        
043200                                   IN GMT-ADPOST-PNRORT                   
043300        ELSE                                                              
043400           MOVE MID-ADPOSTNR       TO GMT-ADPOSTNR                        
043500                                   IN GMT-ADPOST-ORTPNR                   
043600        END-IF                                                            
043700        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADPOSTNR-ATTR                   
043800     ELSE                                                                 
043900        MOVE MFS-ROER-EJ-FAELT     TO MOD-ADPOSTNR-ATTR                   
044000     END-IF                                                               
044100                                                                          
044200     IF MID-ADCITY     NOT = ALL '+'                                      
044300        IF MID-KDPOSTNR = 'L'                                             
044400           MOVE MID-ADCITY         TO GMT-ADCITY                          
044500                                   IN GMT-ADPOST-PNRORT                   
044600        ELSE                                                              
044700           MOVE MID-ADCITY         TO GMT-ADCITY                          
044800                                   IN GMT-ADPOST-ORTPNR                   
044900        END-IF                                                            
045000        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADCITY-ATTR                     
045100     ELSE                                                                 
045200        MOVE MFS-ROER-EJ-FAELT     TO MOD-ADCITY-ATTR                     
045300     END-IF                                                               
045400                                                                          
045500     IF MID-ADGMT-LAND-NEW NOT = ALL '+'                                  
045600        MOVE MID-ADGMT-LAND-NEW    TO GMT-ADGMT-LAND                      
045700        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-ADGMT-LAND-ATTR                 
045800     ELSE                                                                 
045900        MOVE MFS-ROER-EJ-FAELT     TO MOD-ADGMT-LAND-ATTR                 
046000     END-IF                                                               
046100                                                                          
046200     PERFORM IMS-REPL-WDB201                                              
046300                                                                          
046400     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
046500     CALL WMEDKONV USING MED-WMEDAREA                                     
046600     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
046700     PERFORM MFS-FORM-ATTR                                                
046800                                                                          
046900     PERFORM MFS-ROER-EJ-FAELT-IN                                         
047000     PERFORM MFS-ROER-EJ-FAELT-UT                                         
047100     .                                                                    
047200                                                                          
047300                                                                          
047400                                                                          
047500 MFS-RENSA-FAELT-UT SECTION.                                              
047600                                                                          
047700*    --- ALLA UTDATA-FÄLT                                                 
047800     MOVE MFS-RENSA-FAELT TO MOD-BEGMT-RAD1-NEW                           
047900                             MOD-BEGMT-RAD2-NEW                           
048000                             MOD-ADGMT-GATA-NEW                           
048100                             MOD-ADPOSTNR                                 
048200                             MOD-KDPOSTNR                                 
048300                             MOD-ADCITY                                   
048400                             MOD-ADGMT-LAND-NEW                           
048500     .                                                                    
048600     SKIP3                                                                
048700 MFS-RENSA-FAELT-IN SECTION.                                              
048800                                                                          
048900*    --- ALLA INDATA-FÄLT                                                 
049000     MOVE MFS-RENSA-FAELT TO MOD-BEGMT-RAD1-NEW                           
049100                             MOD-BEGMT-RAD2-NEW                           
049200                             MOD-ADGMT-GATA-NEW                           
049300                             MOD-ADPOSTNR                                 
049400                             MOD-KDPOSTNR                                 
049500                             MOD-ADCITY                                   
049600                             MOD-ADGMT-LAND-NEW                           
049700     .                                                                    
049800     EJECT                                                                
049900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
050000                                                                          
050100*    --- ALLA UTDATA-FÄLT                                                 
050200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-UT                             
050300                               MOD-IDKUNDNR-UT                            
050400                               MOD-FLSHOW                                 
050500                               MOD-BEGMT-RAD1                             
050600                               MOD-BEGMT-RAD2                             
050700                               MOD-ADGMT-GATA                             
050800                               MOD-ADGMT-PADR                             
050900                               MOD-ADGMT-LAND                             
051000                               MOD-BEGMT-RAD1-NEW                         
051100                               MOD-BEGMT-RAD2-NEW                         
051200                               MOD-ADGMT-GATA-NEW                         
051300                               MOD-ADPOSTNR                               
051400                               MOD-KDPOSTNR                               
051500                               MOD-ADCITY                                 
051600                               MOD-ADGMT-LAND-NEW                         
051700     .                                                                    
051800     SKIP3                                                                
051900 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
052000                                                                          
052100*    --- ALLA INDATA-FÄLT                                                 
052200     MOVE MFS-ROER-EJ-FAELT TO MOD-BEGMT-RAD1-NEW                         
052300*                              MOD-BEGMT-RAD2-NEW                         
052400                               MOD-ADGMT-GATA-NEW                         
052500                               MOD-ADPOSTNR                               
052600                               MOD-KDPOSTNR                               
052700                               MOD-ADCITY                                 
052800                               MOD-ADGMT-LAND-NEW                         
052900     .                                                                    
053000     EJECT                                                                
053100 MFS-FORM-ATTR SECTION.                                                   
053200                                                                          
053300*    --- ALLA INDATA-FÄLT                                                 
053400     MOVE MFS-FORMATETS-ATTR TO MOD-BEGMT-RAD1-ATTR                       
053500                                MOD-BEGMT-RAD2-ATTR                       
053600                                MOD-ADGMT-GATA-ATTR                       
053700                                MOD-ADPOSTNR-ATTR                         
053800                                MOD-KDPOSTNR-ATTR                         
053900                                MOD-ADCITY-ATTR                           
054000                                MOD-ADGMT-LAND-ATTR                       
054100     .                                                                    
054200     EJECT                                                                
054300* --- IMS SEKTIONER ---                                                   
054400     SKIP3                                                                
054500 IMS-GET-MSG SECTION.                                                     
054600                                                                          
054700     MOVE '  QC' TO GODK-STATUSKODER                                      
054800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
054900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
055000     PERFORM IMS-STATUSKONTROLL                                           
055100     .                                                                    
055200     SKIP3                                                                
055300 IMS-INSERT-MSG SECTION.                                                  
055400                                                                          
055500     MOVE 'N' TO MFS-KDHUVOMR                                             
055600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
055700     MOVE SPACE TO GODK-STATUSKODER                                       
055800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
055900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
056000     PERFORM IMS-STATUSKONTROLL                                           
056100     .                                                                    
056200                                                                          
056300 IMS-01-GU-WDB201     SECTION.                                            
056400     MOVE 'IMS-01'  TO CURRENT-IMS-SECTION                                
056500                                                                          
056600     STRING 'WDB201  (IDGMT   >=' W-IDGMT-X ')'                           
056700          DELIMITED BY SIZE INTO SSA1                                     
056800     MOVE '  GE'              TO GODK-STATUSKODER                         
056900     CALL CBLTDLI USING GU       WDB2-PCB DLI-IO-WDB201 SSA1              
057000     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
057100     PERFORM IMS-STATUSKONTROLL                                           
057200     .                                                                    
057300                                                                          
057400 IMS-02-GN-WDB201     SECTION.                                            
057500     MOVE 'IMS-02'  TO CURRENT-IMS-SECTION                                
057600                                                                          
057700     MOVE 'WDB201'            TO SSA1                                     
057800     MOVE '  GEGB'            TO GODK-STATUSKODER                         
057900     CALL CBLTDLI USING GN       WDB2-PCB DLI-IO-WDB201 SSA1              
058000     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
058100     PERFORM IMS-STATUSKONTROLL                                           
058200     .                                                                    
058300                                                                          
058400 IMS-GHU-WDB201 SECTION.                                                  
058500     MOVE 'IMS-03'  TO CURRENT-IMS-SECTION                                
058600                                                                          
058700     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
058800          DELIMITED BY SIZE INTO SSA1                                     
058900     MOVE '    ' TO GODK-STATUSKODER                                      
059000     CALL CBLTDLI USING GHU WDB2-PCB DLI-IO-WDB201 SSA1                   
059100     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
059200     PERFORM IMS-STATUSKONTROLL                                           
059300     .                                                                    
059400                                                                          
059500 IMS-REPL-WDB201 SECTION.                                                 
059600     MOVE 'IMS-04'  TO CURRENT-IMS-SECTION                                
059700                                                                          
059800     MOVE '  ' TO GODK-STATUSKODER                                        
059900     CALL CBLTDLI USING REPL WDB2-PCB DLI-IO-WDB201                       
060000     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
060100     PERFORM IMS-STATUSKONTROLL                                           
060200     .                                                                    
060300                                                                          
060400 IMS-STATUSKONTROLL SECTION.                                              
060500                                                                          
060600     SET STATUS-IX TO 1                                                   
060700     SEARCH GODK-STATUS                                                   
060800       AT END                                                             
060900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
061000         DELIMITED BY SIZE INTO FELTEXT                                   
061100         CALL FELLOG                                                      
061200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
061300         CONTINUE                                                         
061400     END-SEARCH                                                           
061500     .                                                                    
