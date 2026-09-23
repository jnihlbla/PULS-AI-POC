000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3031100.                                                
000300 AUTHOR.         THOMAS LARSSON.                                          
000400 DATE-WRITTEN.   93/10/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERAR MARKNADSBOLAGSKOD OCH PRISOMRÅDE                      
000900*        FÖR BETALARE.                                                    
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDB1                                       
001200*                   LÄSER      WDC2                                       
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W3T311                                              
001600*        MID:         W3I31101                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W3O31101                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W3031100'.            
002900                                                                          
003000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500                                                                          
003600*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003800 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
003900*   OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = MOD-LÄNGD + 4                    
004000*   OM PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17                   
004100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +297  COMP SYNC.        
004200                                                                          
004300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004400 77  WS-IDPARTNR                 PIC X(9)    VALUE SPACE.                 
004500 77  WS-IDFTG                    PIC X(2)    VALUE SPACE.                 
004600 01  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
004700                                                                          
004800 77  IX                          PIC S9(3)  VALUE ZERO COMP-3.            
004900 77  MAX-IX                      PIC S9(3)  VALUE +7   COMP-3.            
005000                                                                          
005100 01  WS-IDPROMR                  PIC X(3)    VALUE SPACE.                 
005200 01  FILLER REDEFINES WS-IDPROMR.                                         
005300     03  WS-MARKBOLAG            PIC X(1).                                
005400     03  FILLER                  PIC X(2).                                
005500                                                                          
005600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005700     88  INDATA-OK                           VALUE 'J'.                   
005800     88  INDATA-FEL                          VALUE 'N'.                   
005900                                                                          
006000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006100     88  NYCKLAR-OK                          VALUE 'J'.                   
006200     88  NYCKLAR-FEL                         VALUE 'N'.                   
006300                                                                          
006400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006500     88  EGEN-MID                            VALUE '3311'.                
006600     88  GODK-MID                            VALUE '3311'.                
006700     88  HELP-MID                            VALUE '0551'.                
006800     EJECT                                                                
006900                                                                          
007300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007400 01  GENERELLA-SUBPROGRAM.                                                
007500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008100*01 -COPY WMEDAREA                                                        
008200     SKIP3                                                                
008300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008400*01 -COPY WMSGINIT                                                        
008500     EJECT                                                                
008600 01  MESSAGE-CODES.                                                       
008700     03  ERR-CORR-HILITE-FLDS            PIC X(3)    VALUE '001'.         
008800     03  INF-PRESS-PF11                  PIC X(3)    VALUE '003'.         
008900     03  ERR-PF11-AND-NO-DATA            PIC X(3)    VALUE '011'.         
009000     03  INF-UPDATE-DONE                 PIC X(3)    VALUE '101'.         
009100     03  INF-FIRST-PAGE                  PIC X(3)    VALUE '006'.         
009200     03  INF-MORE-INFO-EXISTS            PIC X(3)    VALUE '105'.         
009300     03  ERR-WRONG-KEY                   PIC X(3)    VALUE '401'.         
009400     03  INF-FINANCIAL-CUSTOMER-MISSING  PIC X(3)    VALUE '145'.         
009500     EJECT                                                                
009600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009700*                                                                         
009800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009900     SKIP3                                                                
010000*01  MID -COPY W3I31101                                                   
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010300     SKIP3                                                                
010400*01  -COPY WMSGAREA                                                       
010500     EJECT                                                                
010600     03  MOD REDEFINES MSG-AREA.                                          
010700*      05  -COPY W3O31101                                                 
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011000     SKIP3                                                                
011100*01  -COPY WMFSAREA                                                       
011200     EJECT                                                                
011300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011400*                                                                         
011500     EJECT                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011700     SKIP3                                                                
011800 01  NYCKLAR-TILL-DLI.                                                    
011900     03  W-WDB101KY-X.                                                    
012000         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
012100         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
012200                                                                          
012300     03  W-IDPROMR-X.                                                     
012400         05  W-IDPROMR           PIC X(3)    VALUE SPACE.                 
012500*   NYCKLAR TILL KUNDREG             ***********                          
012600                                                                          
012700     03  W-IDGMT-MAX-X.                                                   
012800         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
012900         05  W-IDKUNDNR-MAX      PIC S9(7)   VALUE 9999999                
013000                                                        COMP-3.           
013100                                                                          
013200     03  W-IDGMT-MIN-X.                                                   
013300         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
013400         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
013500                                                                          
013600*    --- STATUS-KOD FRÅN IMS                                              
013700 01  STATUS-WS                   PIC XX.                                  
013800     88  SEGMENT-FINNS                       VALUE '  '.                  
013900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014100     88  BAS-SLUT                            VALUE 'GB'.                  
014200     SKIP2                                                                
014300 01  GODK-STATUSKODER.                                                    
014400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014500     SKIP3                                                                
014600 01  SSA1                        PIC X(64).                               
014700 01  SSA2                        PIC X(64).                               
014800     EJECT                                                                
014900*    --- IMS FUNKTIONSKODER                                               
015000*01  -COPY W0003                                                          
015100     EJECT                                                                
015200*    ---  DLI INPUT-OUTPUT AREA                                           
015300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
015400     SKIP3                                                                
015500 01  DLI-IO-AREA.                                                         
015600*  03  -COPY WDB101  -PRE WDB1-                                           
015700     EJECT                                                                
015800 01  DLI-IO-AREA2.                                                        
015900*  03  -COPY WDC201  -PRE WDC201-                                         
016000     EJECT                                                                
016100**   KUNDREGISTER                                                         
016200 01  DLI-IO-AREA3.                                                        
016300*  03  WDB201    -COPY WDB201 -PRE WDB2-                                  
016400     EJECT                                                                
016500 LINKAGE SECTION.                                                         
016600                                                                          
016700*01  -COPY W0009   -PRE MSG-                                              
016800     EJECT                                                                
016900*01  -COPY W0008   -PRE WDP7-                                             
017000     05  FILLER                  PIC X.                                   
017100     EJECT                                                                
017200*01  -COPY W0008  -PRE WDB1-                                              
017300     05  FILLER                  PIC X.                                   
017400     EJECT                                                                
017500*01  -COPY W0008  -PRE WDC2-                                              
017600     05  FILLER                  PIC X.                                   
017700     EJECT                                                                
017800*01  -COPY W0008  -PRE WDB2-                                              
017900     05  FILLER                  PIC X.                                   
018000     EJECT                                                                
018100 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB                               
018200                           WDB1-PCB WDC2-PCB WDB2-PCB.                    
018300 MAIN SECTION.                                                            
018400     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB                               
018500                           WDB1-PCB WDC2-PCB WDB2-PCB.                    
018600                                                                          
018700     PERFORM IMS-GET-MSG                                                  
018800     IF SEGMENT-FINNS                                                     
018900       PERFORM A-INIT                                                     
019000       PERFORM B-KOLLA-NYCKLAR                                            
019100       IF NYCKLAR-OK                                                      
019200         IF MFS-UPDATE                                                    
019300           PERFORM G-KOLLA-INPUT                                          
019400           IF INDATA-OK                                                   
019500             PERFORM H-UPPDATERA                                          
019600           END-IF                                                         
019700         ELSE                                                             
019800           IF MFS-FIRST                                                   
019900             PERFORM C-FOERSTA-SIDA                                       
020000           ELSE                                                           
020100             PERFORM E-SAMMA-SIDA                                         
020200           END-IF                                                         
020300         END-IF                                                           
020400         PERFORM F-LAES-VISA-INFO                                         
020500       END-IF                                                             
020600       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
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
021700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I31101                 
021800       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
021900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
022000     ELSE                                                                 
022100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I31101                  
022200       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
022300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
022400     END-IF                                                               
022500                                                                          
022600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
022700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
022800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
022900                                                                          
023000     MOVE LOW-VALUE TO MSG-AREA                                           
023100     MOVE 'W3O311N1' TO MFS-IDMOD                                         
023200     MOVE '3311' TO MOD-IDTRANS                                           
023300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
023400                                                                          
023500     IF EGEN-MID OR HELP-MID                                              
023600       CONTINUE                                                           
023700     ELSE                                                                 
023800       MOVE SPACE TO MFS-KDTRTYP                                          
023900       MOVE '7' TO MFS-IDPFK                                              
024000     END-IF                                                               
024100                                                                          
024200     IF ENGLISH-TEXT                                                      
024300       MOVE +2 TO SPRAK-IX                                                
024400       MOVE 'GB ' TO MED-IDSKYLT                                          
024500     ELSE                                                                 
024600       MOVE +1 TO SPRAK-IX                                                
024700       MOVE 'S  ' TO MED-IDSKYLT                                          
024800     END-IF                                                               
024900     .                                                                    
025000     EJECT                                                                
025100 B-KOLLA-NYCKLAR SECTION.                                                 
025200                                                                          
025300     MOVE JA                     TO NYCKLAR-SW                            
025400     MOVE ALL '+'                TO MSGI-WMSGINIT                         
025500     MOVE '001'                  TO MSGI-KDCALL                           
025600     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
025700     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
025800     MOVE '3311'                 TO MSGI-IDTRANS                          
025900                                                                          
026000     IF EGEN-MID                                                          
026100       IF MID-IDPARTNR-IN = ALL '+' AND                                   
026200          MID-IDFTG-IN = ALL '+' AND                                      
026300          MID-IDDISTR-IN NOT = ALL '+'                                    
026400         MOVE SPACE              TO MSGI-IDPARTNR                         
026500         MOVE SPACE              TO MSGI-IDFTG-KEY                        
026600         MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                          
026700       ELSE                                                               
026800         IF MID-IDPARTNR-IN NOT = ALL '+' OR                              
026900            MID-IDFTG-IN NOT = ALL '+'                                    
027000           MOVE MID-IDPARTNR-IN  TO MSGI-IDPARTNR                         
027100           MOVE MID-IDFTG-IN     TO MSGI-IDFTG-KEY                        
027200           MOVE SPACE            TO MSGI-IDDISTR                          
027300         ELSE                                                             
027400           MOVE MID-IDPARTNR-IN  TO MSGI-IDPARTNR                         
027500           MOVE MID-IDFTG-IN     TO MSGI-IDFTG-KEY                        
027600           MOVE MID-IDDISTR-IN   TO MSGI-IDDISTR                          
027700         END-IF                                                           
027800       END-IF                                                             
027900     END-IF                                                               
028000                                                                          
028100     CALL W005INIT            USING MSGI-WMSGINIT WDP7-PCB                
028200                                                                          
028300     MOVE MFS-RENSA-FAELT        TO MOD-IDPARTNR-IN                       
028400                                    MOD-IDFTG-IN                          
028500                                    MOD-IDDISTR-IN                        
028600                                                                          
028700     IF MID-IDPARTNR-IN NOT = ALL '+'                                     
028800       MOVE '7'                  TO MFS-IDPFK                             
028900       MOVE SPACE                TO MFS-KDTRTYP                           
029000     END-IF                                                               
029100     MOVE MSGI-IDPARTNR          TO WS-IDPARTNR                           
029200                                                                          
029300     IF EGEN-MID                                                          
029400       IF MID-IDFTG-IN NOT = ALL '+'                                      
029500         MOVE '7'                TO MFS-IDPFK                             
029600         MOVE SPACE              TO MFS-KDTRTYP                           
029700       END-IF                                                             
029800       MOVE MSGI-IDFTG-KEY       TO WS-IDFTG                              
029900     ELSE                                                                 
030000       MOVE MSGI-IDFTG           TO WS-IDFTG                              
030100     END-IF                                                               
030200                                                                          
030300     INSPECT WS-IDFTG REPLACING LEADING SPACE BY ZERO                     
030400                                                                          
030500     IF WS-IDFTG = ZERO                                                   
030600       MOVE MSGI-IDFTG           TO WS-IDFTG                              
030700     END-IF                                                               
030800*    -- KONTROLL AV IDDISTR                                               
030900*    -- OM NYCKEL IDPARTNER IFYLLD HOPPAS DISTRIKTNYCKELN ÖVER            
031000                                                                          
031100     IF MID-IDPARTNR-IN = ALL '+' AND                                     
031200        MID-IDFTG-IN    = ALL '+'                                         
031300       IF MID-IDDISTR-IN NOT = ALL '+'                                    
031400         MOVE '7'                TO MFS-IDPFK                             
031500         MOVE SPACE              TO MFS-KDTRTYP                           
031600       END-IF                                                             
031700                                                                          
031800       MOVE MSGI-IDDISTR         TO WS-IDDISTR                            
031900                                                                          
032000       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
032100                                                                          
032200       IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                        
032300         MOVE WS-IDDISTR         TO W-IDDISTR-MAX                         
032400                                    W-IDDISTR-MIN                         
032500         MOVE SPACE              TO WS-IDPARTNR                           
032600         MOVE ZEROES             TO WS-IDFTG                              
032700       END-IF                                                             
032800     ELSE                                                                 
032900       MOVE ZERO                 TO WS-IDDISTR                            
033000     END-IF                                                               
033100                                                                          
033200     IF WS-IDPARTNR = SPACE OR WS-IDFTG = ZERO                            
033300       IF WS-IDDISTR NUMERIC AND WS-IDDISTR = ZERO                        
033400         MOVE NEJ                TO NYCKLAR-SW                            
033500       END-IF                                                             
033600     ELSE                                                                 
033700       MOVE WS-IDPARTNR          TO W-WDB1-IDPARTNR                       
033800       MOVE WS-IDFTG             TO W-WDB1-IDFTG                          
033900     END-IF                                                               
034000                                                                          
034100     IF EGEN-MID OR GODK-MID                                              
034200       MOVE WS-IDPARTNR          TO MOD-IDPARTNR-UT                       
034300       MOVE WS-IDFTG             TO MOD-IDFTG-UT                          
034400       INSPECT MOD-IDFTG-UT REPLACING LEADING ZERO BY SPACE               
034500       MOVE WS-IDDISTR           TO MOD-IDDISTR-UT                        
034600       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
034700     ELSE                                                                 
034800       MOVE MFS-RENSA-FAELT      TO MOD-IDPARTNR-UT                       
034900                                    MOD-IDFTG-UT                          
035000                                    MOD-IDDISTR-UT                        
035100       MOVE NEJ                  TO NYCKLAR-SW                            
035200     END-IF                                                               
035300                                                                          
035400     IF NYCKLAR-FEL                                                       
035500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
035600       CALL WMEDKONV USING MED-WMEDAREA                                   
035700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
035800       PERFORM MFS-RENSA-FAELT-IN                                         
035900       PERFORM MFS-RENSA-FAELT-UT                                         
036000     END-IF                                                               
036100     .                                                                    
036200     EJECT                                                                
036300 C-FOERSTA-SIDA SECTION.                                                  
036400                                                                          
036500*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
036600     PERFORM MFS-RENSA-FAELT-IN                                           
036700     .                                                                    
036800     EJECT                                                                
036900 E-SAMMA-SIDA SECTION.                                                    
037000     IF EGEN-MID OR HELP-MID                                              
037100       IF MID-INPUT = ALL '+'                                             
037200         PERFORM MFS-RENSA-FAELT-IN                                       
037300       ELSE                                                               
037400         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
037500         CALL WMEDKONV USING MED-WMEDAREA                                 
037600         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
037700         PERFORM EA-MID-INDATA-TILL-MOD                                   
037800       END-IF                                                             
037900     ELSE                                                                 
038000       PERFORM MFS-RENSA-FAELT-IN                                         
038100     END-IF                                                               
038200     .                                                                    
038300     EJECT                                                                
038400 EA-MID-INDATA-TILL-MOD SECTION.                                          
038500                                                                          
038600* * * * * FÖR VARJE MID-FÄLT                                              
038700* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
038800* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
038900* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
039000     IF MID-IDMARKBO-IN NOT = ALL '+'                                     
039100       MOVE MID-IDMARKBO-IN TO MOD-IDMARKBO-IN                            
039200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDMARKBO-ATTR                    
039300     ELSE                                                                 
039400       MOVE MFS-RENSA-FAELT TO MOD-IDMARKBO-IN                            
039500     END-IF                                                               
039600     IF MID-IDPROMR-IN NOT = ALL '+'                                      
039700       MOVE MID-IDPROMR-IN TO MOD-IDPROMR-IN                              
039800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPROMR-ATTR                     
039900     ELSE                                                                 
040000       MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-IN                             
040100     END-IF                                                               
040110     IF MID-FLARTRAB-IN NOT = ALL '+'                                     
040120       MOVE MID-FLARTRAB-IN TO MOD-FLARTRAB-IN                            
040130       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLARTRAB-ATTR                    
040140     ELSE                                                                 
040150       MOVE MFS-RENSA-FAELT TO MOD-FLARTRAB-IN                            
040160     END-IF                                                               
040200     .                                                                    
040300     EJECT                                                                
040400 F-LAES-VISA-INFO SECTION.                                                
040500                                                                          
040600****DISTRIKT*****                                                         
040700     IF WS-IDDISTR NUMERIC                                                
040800       IF WS-IDDISTR >  ZERO                                              
040900         PERFORM IMS-GET-WDB201                                           
041000         IF SEGMENT-FINNS                                                 
041100           MOVE WDB2-GMT-IDPARTNR  TO W-WDB1-IDPARTNR                     
041200           MOVE WDB2-GMT-IDFTG     TO W-WDB1-IDFTG                        
041300         END-IF                                                           
041400       END-IF                                                             
041500     END-IF                                                               
041600     PERFORM IMS-GHU-WDB101                                               
041700                                                                          
041800     IF SEGMENT-SAKNAS                                                    
041900        MOVE INF-FINANCIAL-CUSTOMER-MISSING TO MED-IDMFSFEL               
042000        CALL WMEDKONV USING MED-WMEDAREA                                  
042100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
042200        PERFORM MFS-RENSA-FAELT-UT                                        
042300     ELSE                                                                 
042400       MOVE WDB1-BET-IDPARTNR         TO MOD-IDPARTNR-INFO                
042500       MOVE WDB1-BET-BEBETRAD-1       TO MOD-BEBETRAD-1                   
042600       MOVE WDB1-BET-BEBETRAD-2       TO MOD-BEBETRAD-2                   
042700       MOVE WDB1-BET-ADBETRAD-1       TO MOD-ADBETRAD-1                   
042800       MOVE WDB1-BET-ADBETRAD-2       TO MOD-ADBETRAD-2                   
042900       MOVE WDB1-BET-IDMARKBO         TO MOD-IDMARKBO-UT                  
043000       MOVE WDB1-BET-IDPROMR          TO MOD-IDPROMR-UT                   
043001       IF WDB1-BET-FLARTRAB = 'J'                                         
043002         MOVE 'Y'                     TO MOD-FLARTRAB-UT                  
043003       ELSE                                                               
043004         MOVE WDB1-BET-FLARTRAB       TO MOD-FLARTRAB-UT                  
043005       END-IF                                                             
043010       MOVE 'SEK'                     TO MOD-KDVALIS1                     
043020                                         MOD-KDVALIS2                     
043030                                         MOD-KDVALIS3                     
043040                                         MOD-KDVALIS5                     
043050                                         MOD-KDVALIS6                     
043200     END-IF                                                               
043300     .                                                                    
043400     EJECT                                                                
044400 G-KOLLA-INPUT SECTION.                                                   
044500                                                                          
044600     MOVE JA  TO INDATA-SW                                                
044700     IF MID-INPUT = ALL '+'                                               
044800       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
044900       CALL WMEDKONV USING MED-WMEDAREA                                   
045000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
045100       PERFORM MFS-ROER-EJ-FAELT-IN                                       
045200       PERFORM MFS-ROER-EJ-FAELT-UT                                       
045300       MOVE NEJ TO INDATA-SW                                              
045400     ELSE                                                                 
045500       IF MID-INPUT NOT =  ALL '+'                                        
045600         IF MID-IDMARKBO-IN = ( 'A' OR 'B' OR 'C' OR                      
045700                      'Y' OR 'X' OR 'E' OR 'F' OR SPACE )                 
045800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDMARKBO-ATTR                 
045900         ELSE                                                             
046000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDMARKBO-ATTR                   
046100           MOVE NEJ TO INDATA-SW                                          
046200         END-IF                                                           
046300                                                                          
046400         IF MID-IDPROMR-IN NOT = ALL '+'                                  
046500           MOVE MID-IDPROMR-IN TO WS-IDPROMR                              
046600           IF WS-MARKBOLAG = MID-IDMARKBO-IN                              
046700             IF WS-MARKBOLAG = SPACE                                      
046800               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROMR-ATTR              
046900             ELSE                                                         
047000               MOVE MID-IDPROMR-IN TO W-IDPROMR                           
047100               PERFORM IMS-GU-WDC201                                      
047200               IF SEGMENT-FINNS                                           
047300                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROMR-ATTR            
047400               ELSE                                                       
047500                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROMR-ATTR              
047600                 MOVE NEJ TO INDATA-SW                                    
047700               END-IF                                                     
047800             END-IF                                                       
047900           ELSE                                                           
048000             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROMR-ATTR                  
048100             MOVE NEJ TO INDATA-SW                                        
048200           END-IF                                                         
048300         ELSE                                                             
048400           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROMR-ATTR                    
048500           MOVE NEJ TO INDATA-SW                                          
048600         END-IF                                                           
048700                                                                          
048710         IF MID-FLARTRAB-IN = ( 'Y' OR 'J' OR 'N' )                       
048730           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLARTRAB-ATTR                 
048740         ELSE                                                             
048750           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLARTRAB-ATTR                   
048760           MOVE NEJ TO INDATA-SW                                          
048770         END-IF                                                           
048780                                                                          
048800       ELSE                                                               
048900           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDMARKBO-ATTR                   
049000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROMR-ATTR                    
049010           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLARTRAB-ATTR                   
049100           MOVE NEJ TO INDATA-SW                                          
049200       END-IF                                                             
049300                                                                          
049400       IF INDATA-FEL                                                      
049500         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
049600         CALL WMEDKONV USING MED-WMEDAREA                                 
049700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
049800         PERFORM MFS-ROER-EJ-FAELT-UT                                     
049900         PERFORM MFS-ROER-EJ-FAELT-IN                                     
050000       END-IF                                                             
050100     END-IF                                                               
050200     .                                                                    
050300     EJECT                                                                
050400 H-UPPDATERA SECTION.                                                     
050500                                                                          
050600     IF WS-IDDISTR NUMERIC                                                
050700       IF WS-IDDISTR >  ZERO                                              
050800         PERFORM IMS-GET-WDB201                                           
050900         IF SEGMENT-FINNS                                                 
051000           MOVE WDB2-GMT-IDPARTNR  TO W-WDB1-IDPARTNR                     
051100           MOVE WDB2-GMT-IDFTG     TO W-WDB1-IDFTG                        
051200         END-IF                                                           
051300       END-IF                                                             
051400     END-IF                                                               
051500     PERFORM IMS-GHU-WDB101                                               
051600     IF SEGMENT-FINNS                                                     
051700       IF MID-IDMARKBO-IN NOT = ALL '+'                                   
051800         MOVE MID-IDMARKBO-IN TO WDB1-BET-IDMARKBO                        
051900         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDMARKBO-ATTR                  
052000       ELSE                                                               
052100         MOVE MFS-ROER-EJ-FAELT TO MOD-IDMARKBO-UT                        
052200       END-IF                                                             
052300                                                                          
052400       IF MID-IDPROMR-IN NOT = ALL '+'                                    
052500         MOVE MID-IDPROMR-IN TO WDB1-BET-IDPROMR                          
052600         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPROMR-ATTR                   
052700       ELSE                                                               
052800         MOVE MFS-ROER-EJ-FAELT TO MOD-IDPROMR-UT                         
052900       END-IF                                                             
052901                                                                          
052910       IF MID-FLARTRAB-IN NOT = ALL '+'                                   
052911         IF MID-FLARTRAB-IN = ( 'Y' OR 'J' )                              
052912            MOVE 'J'             TO WDB1-BET-FLARTRAB                     
052913         ELSE                                                             
052920            MOVE MID-FLARTRAB-IN TO WDB1-BET-FLARTRAB                     
052921         END-IF                                                           
052930         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLARTRAB-ATTR                  
052940       ELSE                                                               
052950         MOVE MFS-ROER-EJ-FAELT TO MOD-FLARTRAB-UT                        
052960       END-IF                                                             
052970                                                                          
053000       PERFORM IMS-REPL-WDB1                                              
053100                                                                          
053200       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
053300       CALL WMEDKONV USING MED-WMEDAREA                                   
053400       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
053500       PERFORM MFS-FORM-ATTR                                              
053600       PERFORM MFS-RENSA-FAELT-IN                                         
053700* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
053800     END-IF                                                               
053900     .                                                                    
054000     EJECT                                                                
054100 MFS-RENSA-FAELT-UT SECTION.                                              
054200                                                                          
054300*    --- ALLA UTDATA-FÄLT                                                 
054400*    --- INKL. BLÄDDRINGSNYCKLAR                                          
054500     MOVE MFS-RENSA-FAELT TO MOD-BEBETRAD-1                               
054600                             MOD-BEBETRAD-2                               
054700                             MOD-ADBETRAD-1                               
054800                             MOD-ADBETRAD-2                               
054900     .                                                                    
055000     SKIP2                                                                
055100 MFS-RENSA-FAELT-IN SECTION.                                              
055200                                                                          
055300*    --- ALLA INDATA-FÄLT                                                 
055400     MOVE MFS-RENSA-FAELT TO MOD-IDMARKBO-IN                              
055500                             MOD-IDPROMR-IN                               
055510                             MOD-FLARTRAB-IN                              
055600     .                                                                    
055700     EJECT                                                                
055800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
055900                                                                          
056000*    --- ALLA UTDATA-FÄLT                                                 
056100*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
056200     MOVE MFS-ROER-EJ-FAELT TO MOD-BEBETRAD-1                             
056300                               MOD-BEBETRAD-2                             
056400                               MOD-ADBETRAD-1                             
056500                               MOD-ADBETRAD-2                             
056600     .                                                                    
056700     SKIP3                                                                
056800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
056900                                                                          
057000*    --- ALLA INDATA-FÄLT                                                 
057100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDMARKBO-IN                            
057200                               MOD-IDPROMR-IN                             
057210                               MOD-FLARTRAB-IN                            
057300     .                                                                    
057400     SKIP2                                                                
057500 MFS-FORM-ATTR SECTION.                                                   
057600                                                                          
057700*    --- ALLA INDATA-FÄLT                                                 
057800     MOVE MFS-FORMATETS-ATTR TO MOD-IDMARKBO-ATTR                         
057900                                MOD-IDPROMR-ATTR                          
057910                                MOD-FLARTRAB-ATTR                         
058000     .                                                                    
058100     EJECT                                                                
058200* --- IMS SEKTIONER ---                                                   
058300     SKIP3                                                                
058400 IMS-GET-MSG SECTION.                                                     
058500                                                                          
058600     MOVE '  QC' TO GODK-STATUSKODER                                      
058700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
058800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
058900     PERFORM IMS-STATUSKONTROLL                                           
059000     .                                                                    
059100     SKIP3                                                                
059200 IMS-INSERT-MSG SECTION.                                                  
059300                                                                          
059400     IF NOT ENGLISH-TEXT                                                  
059500       MOVE '0' TO MFS-KDHUVOMR                                           
059600     END-IF                                                               
059700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
059800     MOVE SPACE TO GODK-STATUSKODER                                       
059900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
060000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
060100     PERFORM IMS-STATUSKONTROLL                                           
060200     .                                                                    
060300     EJECT                                                                
060400 IMS-GHU-WDB101 SECTION.                                                  
060500                                                                          
060600     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
060700          DELIMITED BY SIZE INTO SSA1                                     
060800     MOVE '  GE' TO GODK-STATUSKODER                                      
060900     CALL CBLTDLI USING GHU WDB1-PCB DLI-IO-AREA SSA1                     
061000     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
061100     PERFORM IMS-STATUSKONTROLL                                           
061200     .                                                                    
061300     SKIP3                                                                
061400 IMS-REPL-WDB1 SECTION.                                                   
061500                                                                          
061600     MOVE '  ' TO GODK-STATUSKODER                                        
061700     CALL CBLTDLI USING REPL WDB1-PCB DLI-IO-AREA                         
061800     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
061900     PERFORM IMS-STATUSKONTROLL                                           
062000     .                                                                    
062100     EJECT                                                                
062200 IMS-GU-WDC201 SECTION.                                                   
062300     STRING 'WDC201  (IDPROMR  =' W-IDPROMR-X ')'                         
062400          DELIMITED BY SIZE INTO SSA1                                     
062500     MOVE '  GE' TO GODK-STATUSKODER                                      
062600     CALL CBLTDLI USING GU WDC2-PCB DLI-IO-AREA2 SSA1                     
062700     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
062800     PERFORM IMS-STATUSKONTROLL                                           
062900     .                                                                    
063000     SKIP3                                                                
063100 IMS-GET-WDB201    SECTION.                                               
063200                                                                          
063300     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
063400                   '&IDGMT   <=' W-IDGMT-MAX-X ')'                        
063500             DELIMITED BY SIZE INTO SSA1                                  
063600     MOVE '  GE' TO GODK-STATUSKODER                                      
063700     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA3 SSA1                     
063800     MOVE WDB2-STATUS-CODE  TO STATUS-WS                                  
063900     PERFORM IMS-STATUSKONTROLL                                           
064000     .                                                                    
064100     SKIP2                                                                
064200 IMS-STATUSKONTROLL SECTION.                                              
064300                                                                          
064400     SET STATUS-IX TO 1                                                   
064500     SEARCH GODK-STATUS                                                   
064600       AT END                                                             
064700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
064800         DELIMITED BY SIZE INTO FELTEXT                                   
064900         CALL FELLOG                                                      
065000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
065100         CONTINUE                                                         
065200     END-SEARCH                                                           
065300     .                                                                    
