000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6015100.                                                
000300 AUTHOR.         ARVIDSSON LENA.                                          
000400 DATE-WRITTEN.   02/02/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        MPP PROGRAM FÖR JUSTERING AV LAGERSALDO PÅ PLOCKPLATS            
000900*        PÅ SVS (FROG)                                                    
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDM9 (HISTORIKBAS)                         
001200*                              WDP7 (USERDATABASEN)                       
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR WDK6                                       
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W6T151                                              
001800*        MID:         W6I15101                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W6O15101                                            
002200*                                                                         
002300*    E-TRACKER : 7450319  VOHF                                            
002400*    E-TRACKER : 10254592 DECOMISSION VOHF                                
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W6015100'.            
003200 77  IX                          PIC S9(2)   VALUE ZERO.                  
003300 77  IY                          PIC S9(2)   VALUE ZERO.                  
003400 77  MAX-IX                      PIC S9(2)   VALUE +5.                    
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100                                                                          
004200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004300                                                                          
004400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004500     88  INDATA-OK                           VALUE 'J'.                   
004600     88  INDATA-FEL                          VALUE 'N'.                   
004700                                                                          
004800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004900     88  NYCKLAR-OK                          VALUE 'J'.                   
005000     88  NYCKLAR-FEL                         VALUE 'N'.                   
005100                                                                          
005200 77  AREA-SW                     PIC X       VALUE 'J'.                   
005300     88  AREA-OK                             VALUE 'J'.                   
005400     88  AREA-FEL                            VALUE 'N'.                   
005500                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  EGEN-MID                            VALUE '6151'.                
005800     88  GODK-MID                            VALUE '6151' '6152'          
005900                                                   '6153' '6154'          
006000                                                   '6155' '6156'          
006100                                                   '6157' '6158'          
006200                                                   '6159'.                
006300     88  HELP-MID                            VALUE '0551'.                
006400     EJECT                                                                
006500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006600 01  GENERELLA-SUBPROGRAM.                                                
006700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007300*01 -COPY WMEDAREA                                                        
007400     SKIP3                                                                
007500 01  MESSAGE-CODES.                                                       
007600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007900     03  PART-MISSING            PIC X(3)    VALUE '017'.                 
008000     03  FIELDS-ARE-NOT-NUMERIC  PIC X(3)    VALUE '020'.                 
008100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008200     03  BASIC-STOCK-TO-LOW      PIC X(3)    VALUE '322'.                 
008300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008400     03  AREA-MISSING            PIC X(3)    VALUE '705'.                 
008500     03  ZERO-NOT-ALLOWED        PIC X(3)    VALUE '724'.                 
008600     03  INFORMATION-MISSING     PIC X(3)    VALUE '760'.                 
008700     EJECT                                                                
008800*                                                                         
008900 01  FELMED.                                                              
009000     03  FELMED1  PIC X(36)                                               
009100           VALUE 'TILLÅTNA VÄRDEN, UPP(U) ELLER NER(N)'.                  
009200     EJECT                                                                
009300*                                                                         
009400*    --- PARAMETRAR TILLSUBPROGRAM W005INIT                               
009500*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009700     SKIP3                                                                
009800*01 -COPY WMSGINIT                                                        
009900     EJECT                                                                
010000*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
010100*                                                                         
010200 01  SPAR-AREA.                                                           
010300     03  SPAR-IDTRANS           PIC X(4)    VALUE '6151'.                 
010400     EJECT                                                                
010500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010600*                                                                         
010700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010800     SKIP3                                                                
010900*01  MID -COPY W6I15101                                                   
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011200     SKIP3                                                                
011300*01  -COPY WMSGAREA                                                       
011400     EJECT                                                                
011500     03  MOD REDEFINES MSG-AREA.                                          
011600*      05  -COPY W6O15101                                                 
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011900     SKIP3                                                                
012000*01  -COPY WMFSAREA                                                       
012100     EJECT                                                                
012200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012300*                                                                         
012400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012500     SKIP3                                                                
012600 01  NYCKLAR-TILL-DLI.                                                    
012700     03  W-IDARTNR-X.                                                     
012800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012900     03  W-KDSEGKEY-X.                                                    
013000         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
013100     SKIP2                                                                
013200*    --- STATUS-KOD FRÅN IMS                                              
013300 01  STATUS-WS                   PIC XX.                                  
013400     88  SEGMENT-FINNS                       VALUE '  '.                  
013500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013700     SKIP2                                                                
013800 01  GODK-STATUSKODER.                                                    
013900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014000     SKIP3                                                                
014100 01  SSA1                        PIC X(64).                               
014200 01  SSA2                        PIC X(64).                               
014300     EJECT                                                                
014400*    --- IMS FUNKTIONSKODER                                               
014500*01  -COPY W0003                                                          
014600     EJECT                                                                
014700*    ---  DLI INPUT-OUTPUT AREA                                           
014800                                                                          
014900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
015000 01  DLI-IO-WDK601.                                                       
015100*    03  -COPY WDK601                                                     
015200     EJECT                                                                
015300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
015400 01  DLI-IO-WDK611.                                                       
015500*    03  -COPY WDK611                                                     
015600     EJECT                                                                
015700 LINKAGE SECTION.                                                         
015800*01  -COPY W0009   -PRE MSG-                                              
015900*01  -COPY W0008   -PRE WDP7-                                             
016000     05  FILLER                  PIC X.                                   
016100                                                                          
016200*01  -COPY W0008  -PRE WDK6-                                              
016300     05  FILLER                  PIC X.                                   
016400     EJECT                                                                
016500 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDK6-PCB.                     
016600 MAIN SECTION.                                                            
016700     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDK6-PCB.                     
016800                                                                          
016900     PERFORM IMS-GET-MSG                                                  
017000     IF SEGMENT-FINNS                                                     
017100       PERFORM A-INIT                                                     
017200       PERFORM B-KOLLA-NYCKLAR                                            
017300       IF NYCKLAR-OK                                                      
017400         IF MFS-UPDATE                                                    
017500           PERFORM G-KOLLA-INPUT                                          
017600           IF INDATA-OK                                                   
017700             PERFORM H-UPPDATERA                                          
017800           END-IF                                                         
017900         ELSE                                                             
018000           IF MFS-FIRST                                                   
018100             PERFORM C-FOERSTA-SIDA                                       
018200           ELSE                                                           
018300             PERFORM E-SAMMA-SIDA                                         
018400           END-IF                                                         
018500         END-IF                                                           
018600         PERFORM F-LAES-VISA-INFO                                         
018700       END-IF                                                             
018800*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
018900*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
019000       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O15101 + 4                      
019100       PERFORM IMS-INSERT-MSG                                             
019200     END-IF                                                               
019300                                                                          
019400     MOVE ZERO TO RETURN-CODE                                             
019500     GOBACK                                                               
019600     .                                                                    
019700     EJECT                                                                
019800 A-INIT SECTION.                                                          
019900                                                                          
020000     IF MSG-DUBBLA-TRANSKODER                                             
020100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I15101                 
020200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
020300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
020400     ELSE                                                                 
020500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I15101                  
020600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
020700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020800     END-IF                                                               
020900                                                                          
021000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
021100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
021200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
021300                                                                          
021400     MOVE LOW-VALUE TO MSG-AREA                                           
021500     MOVE 'W6O15101' TO MFS-IDMOD                                         
021600     MOVE '6151' TO MOD-IDTRANS                                           
021700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
021800                                                                          
021900     IF EGEN-MID OR HELP-MID                                              
022000       CONTINUE                                                           
022100     ELSE                                                                 
022200       MOVE SPACE TO MFS-KDTRTYP                                          
022300       MOVE '7' TO MFS-IDPFK                                              
022400     END-IF                                                               
022500     .                                                                    
022600     EJECT                                                                
022700 B-KOLLA-NYCKLAR SECTION.                                                 
022800                                                                          
022900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
023000     MOVE '001'             TO MSGI-KDCALL                                
023100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
023200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
023300     MOVE '6151'            TO MSGI-IDTRANS                               
023400     IF EGEN-MID                                                          
023500         MOVE MID-IDARTNR        TO MSGI-IDARTNR                          
023600     END-IF                                                               
023700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
023800     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
023900                                                                          
024000*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
024100     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
024200                                                                          
024300     MOVE JA TO NYCKLAR-SW                                                
024400                                                                          
024500                                                                          
024600*    -- KONTROLL AV IDARTNR                                               
024700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
024800                                                                          
024900     IF MID-IDARTNR NOT = ALL '+'                                         
025000       MOVE '7'         TO MFS-IDPFK                                      
025100       MOVE SPACE       TO MFS-KDTRTYP                                    
025200     END-IF                                                               
025300     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
025400     IF MSGI-IDARTNR NUMERIC                                              
025500       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
025600     ELSE                                                                 
025700       MOVE NEJ TO NYCKLAR-SW                                             
025800     END-IF                                                               
025900                                                                          
026000     IF NYCKLAR-OK                                                        
026100       MOVE MSGI-IDARTNR        TO MOD-IDARTNR-UT                         
026200       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
026300     ELSE                                                                 
026400       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
026500     END-IF                                                               
026600                                                                          
026700     IF NYCKLAR-FEL                                                       
026800       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
026900       CALL WMEDKONV USING MED-WMEDAREA                                   
027000       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
027100       PERFORM MFS-RENSA-FAELT-IN                                         
027200       PERFORM MFS-RENSA-FAELT-UT                                         
027300     END-IF                                                               
027400     .                                                                    
027500     EJECT                                                                
027600 C-FOERSTA-SIDA SECTION.                                                  
027700                                                                          
027800     PERFORM MFS-RENSA-FAELT-IN                                           
027900     .                                                                    
028000     EJECT                                                                
028100 E-SAMMA-SIDA SECTION.                                                    
028200                                                                          
028300     IF EGEN-MID OR HELP-MID                                              
028400       IF MID-W6I15101 = ALL '+'                                          
028500         PERFORM MFS-RENSA-FAELT-IN                                       
028600       ELSE                                                               
028700         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
028800         CALL WMEDKONV USING MED-WMEDAREA                                 
028900         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
029000         PERFORM EA-MID-INDATA-TILL-MOD                                   
029100       END-IF                                                             
029200     ELSE                                                                 
029300       PERFORM MFS-RENSA-FAELT-IN                                         
029400     END-IF                                                               
029500     .                                                                    
029600     EJECT                                                                
029700 EA-MID-INDATA-TILL-MOD SECTION.                                          
029800                                                                          
029900* * * * * FÖR VARJE MID-FÄLT                                              
030000* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
030100* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
030200* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
030300     MOVE +1     TO IX                                                    
030400     PERFORM UNTIL IX > MAX-IX                                            
030500       IF MID-CMD (IX)       = ALL '+'                                    
030600          MOVE MFS-RENSA-FAELT           TO MOD-CMD (IX)                  
030700       ELSE                                                               
030800          MOVE MID-CMD (IX)              TO MOD-CMD (IX)                  
030900          MOVE MFS-ADD-LAES-IN-FAELT     TO MOD-CMD-ATTR (IX)             
031000       END-IF                                                             
031100*                                                                         
031200       IF MID-KVANTAL-JUST (IX) = ALL '+'                                 
031300          MOVE MFS-RENSA-FAELT           TO MOD-KVANTAL-JUST (IX)         
031400       ELSE                                                               
031500          MOVE MID-KVANTAL-JUST (IX)    TO MOD-KVANTAL-JUST (IX)          
031600          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVANTAL-JUST-ATTR (IX)        
031700       END-IF                                                             
031800       ADD +1 TO IX                                                       
031900     END-PERFORM                                                          
032000     .                                                                    
032100     EJECT                                                                
032200 F-LAES-VISA-INFO SECTION.                                                
032300                                                                          
032400     PERFORM IMS-GU-WDK611                                                
032500     IF SEGMENT-SAKNAS                                                    
032600       MOVE PART-MISSING TO MED-IDMFSFEL                                  
032700       CALL WMEDKONV USING MED-WMEDAREA                                   
032800       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
032900       PERFORM MFS-RENSA-FAELT-UT                                         
033000     ELSE                                                                 
033100       MOVE +1                 TO IX                                      
033200       MOVE CLAG-KVLS          TO MOD-KVLS-CDC                            
033300       MOVE CLAG-ADLAGOMR      TO MOD-ADLAGOMR-CDC                        
033400       MOVE CLAG-ADGANG        TO MOD-ADGANG-CDC                          
033500       MOVE CLAG-ADPLATS       TO MOD-ADPLATS-CDC                         
033600       IF CLAG-ADLAGOMR-SVS > 0                                           
033700       OR CLAG-KVLS-SVS > 0                                               
033800         MOVE CLAG-KVLS-SVS         TO MOD-KVLS (IX)                      
033900         MOVE CLAG-ADLAGOMR-SVS     TO MOD-ADLAGOMR (IX)                  
034000         MOVE CLAG-ADGANG-SVS       TO MOD-ADGANG (IX)                    
034100         MOVE CLAG-ADPLATS-SVS      TO MOD-ADPLATS (IX)                   
034200         MOVE 'SVS'                 TO MOD-TYP (IX)                       
034300         ADD +1                 TO IX                                     
034400       ELSE                                                               
034500         PERFORM MFS-STAENG-FAELT-SVS                                     
034600         MOVE +2                TO IX                                     
034700       END-IF                                                             
034800                                                                          
034900       MOVE +1                      TO IY                                 
035000       PERFORM UNTIL (IX > MAX-IX OR IY > +4)                             
035100         IF CLAG-ADLAGOMR-CD (IY) > 0                                     
035200         OR CLAG-KVLS-CD (IY ) > 0                                        
035300           MOVE CLAG-KVLS-CD (IY )       TO MOD-KVLS (IX)                 
035400           MOVE CLAG-ADLAGOMR-CD (IY )   TO MOD-ADLAGOMR (IX)             
035500           MOVE CLAG-ADGANG-CD (IY )     TO MOD-ADGANG (IX)               
035600           MOVE CLAG-ADPLATS-CD (IY )    TO MOD-ADPLATS (IX)              
035700           MOVE 'CD'                        TO MOD-TYP (IX)               
035800         ELSE                                                             
035900           PERFORM MFS-STAENG-FAELT-CD                                    
036000         END-IF                                                           
036100         ADD +1 TO IX                                                     
036200         ADD +1 TO IY                                                     
036300       END-PERFORM                                                        
036400*                                                                         
036500     END-IF                                                               
036600     .                                                                    
036700     EJECT                                                                
036800 G-KOLLA-INPUT SECTION.                                                   
036900                                                                          
037000     MOVE JA  TO INDATA-SW                                                
037100     IF MID-W6I15101 = ALL '+'                                            
037200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
037300       CALL WMEDKONV USING MED-WMEDAREA                                   
037400       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
037500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
037600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
037700       MOVE NEJ TO INDATA-SW                                              
037800     ELSE                                                                 
037900* KOLLA OM ARTNR SAKNAS                                                   
038000       PERFORM IMS-GHU-WDK611                                             
038100       IF SEGMENT-SAKNAS                                                  
038200         MOVE PART-MISSING TO MED-IDMFSFEL                                
038300         CALL WMEDKONV USING MED-WMEDAREA                                 
038400         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
038500         MOVE NEJ TO INDATA-SW                                            
038600       ELSE                                                               
038700* KOLLA OM TILLÅTNA VÄRDEN UPP/U ELLER NER/N PÅ COMMAND(CMD)              
038800         MOVE +1 TO IX                                                    
038900         PERFORM UNTIL IX > MAX-IX                                        
039000           IF MID-CMD (IX) NOT = ALL '+'                                  
039100             IF MID-CMD (IX) = 'UPP' OR 'U  ' OR 'NER' OR 'N  '           
039200               MOVE MFS-ADD-LAES-IN-FAELT                                 
039300                                       TO MOD-CMD-ATTR (IX)               
039400             ELSE                                                         
039500               MOVE FELMED1 TO MOD-TEMFSFEL                               
039600               MOVE MFS-ALFA-FAELT-FEL                                    
039700                                       TO MOD-CMD-ATTR (IX)               
039800               MOVE NEJ TO INDATA-SW                                      
039900             END-IF                                                       
040000           END-IF                                                         
040100*     KOLLA OM UPPGIFT SAKNAS PÅ COMMAND (CMD).                           
040200           IF MID-CMD (IX) NOT = ALL '+'                                  
040300* KOLLA OM UPPGIFT SAKNAS PÅ JUSTERAT ANTAL (JUST).                       
040400           AND MID-KVANTAL-JUST (IX) NOT = ALL '+'                        
040500*                                                                         
040600*   KOLLA OM MID-KVANTAL-JUST ÄR NUMERISKT OCH EJ=0.                      
040700*   KOLLA SÅ ATT LAGERSALDO (SVS) EJ JUSTERAS TILL UNDER NOLL.            
040800       INSPECT MID-KVANTAL-JUST (IX)                                      
040900                       REPLACING LEADING SPACE BY ZERO                    
041000             IF MID-KVANTAL-JUST (IX) NUMERIC                             
041100               IF MID-KVANTAL-JUST (IX) NOT = 0                           
041200                 MOVE MFS-ADD-LAES-IN-FAELT                               
041300                                  TO MOD-KVANTAL-JUST-ATTR (IX)           
041400                 IF MID-CMD (IX) = 'NER' OR 'N  '                         
041500                   IF IX = 1                                              
041600                     IF (CLAG-KVLS-SVS                                    
041700                                     - MID-KVANTAL-JUST (IX)) < 0         
041800                       MOVE BASIC-STOCK-TO-LOW TO MED-IDMFSFEL            
041900                       CALL WMEDKONV USING MED-WMEDAREA                   
042000                       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                  
042100                       MOVE MFS-NUM-FAELT-FEL                             
042200                                    TO MOD-KVANTAL-JUST-ATTR(IX)          
042300                       MOVE NEJ TO INDATA-SW                              
042400                     ELSE                                                 
042500                       MOVE MFS-ADD-LAES-IN-FAELT                         
042600                                    TO MOD-KVANTAL-JUST-ATTR(IX)          
042700                     END-IF                                               
042800                   ELSE                                                   
044300                     IF (CLAG-KVLS-CD (IX - 1)                            
044400                                     - MID-KVANTAL-JUST (IX)) < 0         
044500                       MOVE BASIC-STOCK-TO-LOW TO MED-IDMFSFEL            
044600                       CALL WMEDKONV USING MED-WMEDAREA                   
044700                       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                  
044800                       MOVE MFS-NUM-FAELT-FEL                             
044900                                   TO MOD-KVANTAL-JUST-ATTR (IX)          
045000                       MOVE NEJ TO INDATA-SW                              
045100                     ELSE                                                 
045200                       MOVE MFS-ADD-LAES-IN-FAELT                         
045300                                    TO MOD-KVANTAL-JUST-ATTR (IX)         
045400                     END-IF                                               
045600                   END-IF                                                 
045700                 END-IF                                                   
045800               ELSE                                                       
045900                 MOVE ZERO-NOT-ALLOWED TO MED-IDMFSFEL                    
046000                 CALL WMEDKONV USING MED-WMEDAREA                         
046100                 MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                        
046200                 MOVE MFS-ADD-LAES-IN-FAELT-HI                            
046300                                   TO MOD-KVANTAL-JUST-ATTR (IX)          
046400                 MOVE NEJ TO INDATA-SW                                    
046500               END-IF                                                     
046600             ELSE                                                         
046700               MOVE FIELDS-ARE-NOT-NUMERIC TO MED-IDMFSFEL                
046800               CALL WMEDKONV USING MED-WMEDAREA                           
046900               MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                          
047000               MOVE MFS-ADD-LAES-IN-FAELT-HI                              
047100                                   TO MOD-KVANTAL-JUST-ATTR (IX)          
047200               MOVE NEJ TO INDATA-SW                                      
047300             END-IF                                                       
047400           ELSE                                                           
047500             IF MID-CMD (IX) NOT = ALL '+'                                
047600             OR MID-KVANTAL-JUST (IX) NOT = ALL '+'                       
047700               IF MID-CMD (IX) = ALL '+'                                  
047800                 MOVE FELMED1 TO MOD-TEMFSFEL                             
047900                 MOVE MFS-ALFA-FAELT-FEL                                  
048000                                       TO MOD-CMD-ATTR (IX)               
048100                 MOVE NEJ TO INDATA-SW                                    
048200               END-IF                                                     
048300               IF MID-KVANTAL-JUST (IX) = ALL '+'                         
048400                 MOVE FIELDS-ARE-NOT-NUMERIC TO MED-IDMFSFEL              
048500                 CALL WMEDKONV USING MED-WMEDAREA                         
048600                 MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                        
048700                 MOVE MFS-ADD-LAES-IN-FAELT-HI                            
048800                                   TO MOD-KVANTAL-JUST-ATTR (IX)          
048900                 MOVE NEJ TO INDATA-SW                                    
049000               END-IF                                                     
049100             END-IF                                                       
049200           END-IF                                                         
049300           IF INDATA-FEL                                                  
049400             PERFORM MFS-ROER-EJ-FAELT-UT                                 
049500             PERFORM MFS-ROER-EJ-FAELT-IN                                 
049600           END-IF                                                         
049700           ADD +1 TO IX                                                   
049800         END-PERFORM                                                      
049900       END-IF                                                             
050000     END-IF                                                               
050100     .                                                                    
050200     EJECT                                                                
050300 H-UPPDATERA SECTION.                                                     
050400                                                                          
050500     MOVE +1 TO IX                                                        
050600     PERFORM UNTIL IX > MAX-IX                                            
050700       MOVE MFS-RENSA-FAELT                                               
050800                           TO MOD-KVANTAL-JUST (IX)                       
050900       IF MID-CMD (IX) = 'UPP' OR 'U  '                                   
051000         IF IX = 1                                                        
051100           ADD MID-KVANTAL-JUST (IX) TO CLAG-KVLS-SVS                     
051200         ELSE                                                             
051600           ADD MID-KVANTAL-JUST (IX) TO CLAG-KVLS-CD (IX - 1)             
051800         END-IF                                                           
051900       ELSE                                                               
052000         IF MID-CMD (IX) = 'NER' OR 'N  '                                 
052100           IF IX = 1                                                      
052200             SUBTRACT MID-KVANTAL-JUST (IX) FROM CLAG-KVLS-SVS            
052300           ELSE                                                           
052700             SUBTRACT MID-KVANTAL-JUST (IX)                               
052800                                      FROM CLAG-KVLS-CD (IX - 1)          
053000           END-IF                                                         
053100         END-IF                                                           
053200       END-IF                                                             
053300       ADD +1 TO IX                                                       
053400     END-PERFORM                                                          
053500     PERFORM IMS-REPL-WDK611                                              
053600     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
053700     CALL WMEDKONV USING MED-WMEDAREA                                     
053800     MOVE MED-TEMFSINF TO MOD-TEMFSINF                                    
053900     PERFORM MFS-FORM-ATTR                                                
054000     PERFORM MFS-RENSA-FAELT-IN                                           
054100     .                                                                    
054200     EJECT                                                                
054300* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
054400 MFS-RENSA-FAELT-UT SECTION.                                              
054500                                                                          
054600*    --- ALLA UTDATA-FÄLT                                                 
054700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                               
054800                             MOD-KVLS-CDC                                 
054900                             MOD-ADLAGOMR-CDC                             
055000                             MOD-ADGANG-CDC                               
055100                             MOD-ADPLATS-CDC                              
055200     MOVE +1   TO IX                                                      
055300     PERFORM UNTIL IX > MAX-IX                                            
055400     MOVE MFS-RENSA-FAELT TO MOD-CMD (IX)                                 
055500                             MOD-KVANTAL-JUST (IX)                        
055600                             MOD-KVLS (IX)                                
055700                             MOD-ADLAGOMR (IX)                            
055800                             MOD-ADGANG (IX)                              
055900                             MOD-ADPLATS (IX)                             
056000                             MOD-TYP (IX)                                 
056100     ADD +1 TO IX                                                         
056200     END-PERFORM                                                          
056300     .                                                                    
056400     SKIP3                                                                
056500 MFS-RENSA-FAELT-IN SECTION.                                              
056600                                                                          
056700*    --- ALLA INDATA-FÄLT                                                 
056800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
056900     MOVE +1   TO IX                                                      
057000     PERFORM UNTIL IX > MAX-IX                                            
057100     MOVE MFS-RENSA-FAELT TO MOD-CMD (IX)                                 
057200                             MOD-KVANTAL-JUST (IX)                        
057300     ADD +1 TO IX                                                         
057400     END-PERFORM                                                          
057500     .                                                                    
057600     EJECT                                                                
057700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
057800                                                                          
057900*    --- ALLA UTDATA-FÄLT                                                 
058000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                             
058100                               MOD-KVLS-CDC                               
058200                               MOD-ADLAGOMR-CDC                           
058300                               MOD-ADGANG-CDC                             
058400                               MOD-ADPLATS-CDC                            
058500     MOVE +1   TO IX                                                      
058600     PERFORM UNTIL IX > MAX-IX                                            
058700     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD (IX)                               
058800                               MOD-KVANTAL-JUST (IX)                      
058900                               MOD-KVLS (IX)                              
059000                               MOD-ADLAGOMR (IX)                          
059100                               MOD-ADGANG (IX)                            
059200                               MOD-ADPLATS (IX)                           
059300                               MOD-TYP (IX)                               
059400     ADD +1 TO IX                                                         
059500     END-PERFORM                                                          
059600     .                                                                    
059700     SKIP3                                                                
059800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
059900                                                                          
060000*    --- ALLA INDATA-FÄLT                                                 
060100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-IN                             
060200     MOVE +1   TO IX                                                      
060300     PERFORM UNTIL IX > MAX-IX                                            
060400       MOVE MFS-ROER-EJ-FAELT TO MOD-CMD (IX)                             
060500                                 MOD-KVANTAL-JUST (IX)                    
060600     ADD +1 TO IX                                                         
060700     END-PERFORM                                                          
060800     .                                                                    
060900     EJECT                                                                
061000 MFS-STAENG-FAELT-SVS SECTION.                                            
061100     MOVE MFS-STAENG-FAELT  TO MOD-CMD-ATTR (IX)                          
061200                               MOD-KVANTAL-JUST-ATTR (IX)                 
061300     .                                                                    
061400     EJECT                                                                
062000 MFS-STAENG-FAELT-CD  SECTION.                                            
062100     MOVE MFS-STAENG-FAELT  TO MOD-CMD-ATTR (IX)                          
062200                               MOD-KVANTAL-JUST-ATTR (IX)                 
062300     .                                                                    
062400     EJECT                                                                
062500 MFS-FORM-ATTR SECTION.                                                   
062600                                                                          
062700*    --- ALLA INDATA-FÄLT                                                 
062800     MOVE +1   TO IX                                                      
062900     PERFORM UNTIL IX > MAX-IX                                            
063000       MOVE MFS-FORMATETS-ATTR TO MOD-CMD-ATTR (IX)                       
063100                                  MOD-KVANTAL-JUST-ATTR (IX)              
063200     ADD +1 TO IX                                                         
063300     END-PERFORM                                                          
063400     .                                                                    
063500     SKIP2                                                                
063600* --- IMS SEKTIONER ---                                                   
063700     SKIP3                                                                
063800 IMS-GET-MSG SECTION.                                                     
063900                                                                          
064000     MOVE '  QC' TO GODK-STATUSKODER                                      
064100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
064200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
064300     PERFORM IMS-STATUSKONTROLL                                           
064400     .                                                                    
064500     SKIP3                                                                
064600 IMS-INSERT-MSG SECTION.                                                  
064700                                                                          
064800     IF MSGI-IDLAND-SPR = 'SE'                                            
064900       MOVE '0' TO MFS-KDHUVOMR                                           
065000     END-IF                                                               
065100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
065200     MOVE SPACE TO GODK-STATUSKODER                                       
065300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
065400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
065500     PERFORM IMS-STATUSKONTROLL                                           
065600     .                                                                    
065700     EJECT                                                                
065800 IMS-GU-WDK611 SECTION.                                                   
065900                                                                          
066000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
066100          DELIMITED BY SIZE INTO SSA1                                     
066200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
066300          DELIMITED BY SIZE INTO SSA2                                     
066400     MOVE '  GE' TO GODK-STATUSKODER                                      
066500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
066600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
066700     PERFORM IMS-STATUSKONTROLL                                           
066800     .                                                                    
066900     SKIP3                                                                
067000 IMS-GHU-WDK611 SECTION.                                                  
067100                                                                          
067200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
067300          DELIMITED BY SIZE INTO SSA1                                     
067400     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
067500          DELIMITED BY SIZE INTO SSA2                                     
067600     MOVE '  GE' TO GODK-STATUSKODER                                      
067700     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
067800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
067900     PERFORM IMS-STATUSKONTROLL                                           
068000     .                                                                    
068100     SKIP3                                                                
068200 IMS-REPL-WDK611 SECTION.                                                 
068300                                                                          
068400     MOVE '  ' TO GODK-STATUSKODER                                        
068500     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
068600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
068700     PERFORM IMS-STATUSKONTROLL                                           
068800     .                                                                    
068900     EJECT                                                                
069000 IMS-STATUSKONTROLL SECTION.                                              
069100                                                                          
069200     SET STATUS-IX TO 1                                                   
069300     SEARCH GODK-STATUS                                                   
069400       AT END                                                             
069500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
069600         DELIMITED BY SIZE INTO FELTEXT                                   
069700         CALL FELLOG                                                      
069800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
069900         CONTINUE                                                         
070000     END-SEARCH                                                           
070100     .                                                                    
