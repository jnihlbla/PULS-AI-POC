000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3018500.                                                
000300 AUTHOR.         INGVAR SKJELBRED.                                        
000400 DATE-WRITTEN.   99/01/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LISTS EXCHANGE CLEARING IN STATUS 2 OCH 3                        
000900*                                                                         
001000*        PROGRAMMET LÄSER      WL3171 (WDGX)                              
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W3T185                                              
001400*        MID:         W3I18501                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W3O18501                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W3018500'.            
002700                                                                          
002800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003300 77  KLART-SW                    PIC X       VALUE 'N'.                   
003400     88  KLART-SLUT                          VALUE 'J'.                   
003500                                                                          
003600                                                                          
003700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003900 77  MAX-INDX                    PIC S9(4)  VALUE +15   COMP SYNC.        
004000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004100                                                                          
004200                                                                          
004300 77  BYT-SW                      PIC X       VALUE 'J'.                   
004400     88  BYT-EJ-BILD                         VALUE 'N'.                   
004500                                                                          
004600                                                                          
004700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004800     88  NYCKLAR-OK                          VALUE 'J'.                   
004900     88  NYCKLAR-FEL                         VALUE 'N'.                   
005000                                                                          
005100 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
005200                                                                          
005300                                                                          
005400 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005500     88  ALLT-OK                             VALUE 'J'.                   
005600                                                                          
005700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005800     88  EGEN-MID                            VALUE '3185'.                
005900     88  GODK-MID                            VALUE '3182' '3183'          
006000                                                   '3184'.                
006100     88  HELP-MID                            VALUE '0551'.                
006200                                                                          
006300                                                                          
006400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006500     88  INDATA-OK                           VALUE 'J'.                   
006600     88  INDATA-FEL                          VALUE 'N'.                   
006700                                                                          
006800 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
006900 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
007000 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
007100 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
007200                                                                          
007300*      --- VALID IDDC CODES                                               
007400*                                                                         
007500*01    -COPY WWDC99 -PRE TEST-                                            
007600       EJECT                                                              
007700     EJECT                                                                
007800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007900 01  GENERELLA-SUBPROGRAM.                                                
008000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008600*01 -COPY WMEDAREA                                                        
008700     SKIP3                                                                
008800 01  MESSAGE-CODES.                                                       
008900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009300     03  WRONG-STATUS            PIC X(3)    VALUE '079'.                 
009400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009700     03  INVOICE-MISSING         PIC X(3)    VALUE '320'.                 
009800     03  USER-NOT-ALLOWED        PIC X(3)    VALUE '405'.                 
009900     EJECT                                                                
010000*01  -COPY WDATAREA                                                       
010100     EJECT                                                                
010200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010300*                                                                         
010400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010500     SKIP3                                                                
010600*01 -COPY WMSGINIT                                                        
010700     EJECT                                                                
010800*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
010900*                                                                         
011000 01  FILLER                      PIC X(09)   VALUE 'SPAR-AREA'.           
011100 01  SPAR-AREA.                                                           
011200     03  SPAR-IDTRANS            PIC X(4)    VALUE '3185'.                
011300     03  SPAR-IDFAKT-ENTER       PIC S9(7)        COMP-3.                 
011400     03  SPAR-IDFAKT-NEXT        PIC S9(7)        COMP-3.                 
011500     03  SPAR-IDDC-REC-ENTER         PIC X(2).                            
011600     03  SPAR-IDDC-REC-NEXT          PIC X(2).                            
011700     03  SPAR-IDDC-SEND-ENTER         PIC X(2).                           
011800     03  SPAR-IDDC-SEND-NEXT          PIC X(2).                           
011900     EJECT                                                                
012000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012100*                                                                         
012200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012300     SKIP3                                                                
012400*01  MID -COPY W3I18501                                                   
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012700     SKIP3                                                                
012800*01  -COPY WMSGAREA                                                       
012900     EJECT                                                                
013000     05  MOD-MID REDEFINES MSG-MID-OUT.                                   
013100*      07  -COPY W3I18401 -PRE MOD-                                       
013200     EJECT                                                                
013300   03  MOD REDEFINES MSG-AREA.                                            
013400*    05  -COPY W3O18501                                                   
013500     EJECT                                                                
013600                                                                          
013700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013800     SKIP3                                                                
013900*01  -COPY WMFSAREA                                                       
014000     EJECT                                                                
014100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014200*                                                                         
014300     EJECT                                                                
014400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014500     SKIP3                                                                
014600 01  NYCKLAR-TILL-DLI.                                                    
014700*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
014800     03  FILLER               PIC X(12)   VALUE 'W-IDFAKT-MIN'.           
014900     03  W-IDFAKT-MIN-X.                                                  
015000         05  W-IDFAKT-MIN     PIC S9(7)        COMP-3.                    
015100                                                                          
015200     03  W-IDDC-MIN-X.                                                    
015300         05  W-IDDC-MIN     PIC X(2).                                     
015400                                                                          
015500     03  W-IDDC-BEH-X.                                                    
015600         05  W-IDDC-BEH          PIC X(2)   VALUE SPACE.                  
015700     03  W-IDHTYP-X.                                                      
015800         05  W-IDHTYP            PIC X(4)   VALUE '3171'.                 
015900         05  FILLER              PIC X(26)  VALUE LOW-VALUE.              
016000     03  FILLER               PIC X(08)   VALUE 'W-IDFAKT'.               
016100     03  W-IDFAKT-X.                                                      
016200         05  W-IDFAKT            PIC S9(7)   VALUE ZERO COMP-3.           
016300     03  FILLER               PIC X(10)   VALUE 'W-IDDC-REC'.             
016400     03  W-IDDC-REC-X.                                                    
016500         05  W-IDDC-REC          PIC X(2)    VALUE SPACE.                 
016600     03  FILLER               PIC X(11)   VALUE 'W-IDDC-SEND'.            
016700     03  W-IDDC-SEND-X.                                                   
016800         05  W-IDDC-SEND         PIC X(2)    VALUE SPACE.                 
016900     SKIP2                                                                
017000*    --- STATUS-KOD FRÅN IMS                                              
017100 01  STATUS-WS                   PIC XX.                                  
017200     88  SEGMENT-FINNS                       VALUE '  '.                  
017300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017500     88  SEGMENT-DATA                        VALUE 'GA'.                  
017600     88  BAS-SLUT                            VALUE 'GB'.                  
017700     SKIP2                                                                
017800 01  GODK-STATUSKODER.                                                    
017900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018000     SKIP3                                                                
018100 01  SSA1                        PIC X(64).                               
018200 01  SSA2                        PIC X(64).                               
018300     EJECT                                                                
018400*    --- IMS FUNKTIONSKODER                                               
018500*01  -COPY W0003                                                          
018600     EJECT                                                                
018700*    ---  DLI INPUT-OUTPUT AREA                                           
018800                                                                          
018900 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-AREA'.          
019000 01  DLI-IO-AREA.                                                         
019100     03  IO-AREA                  PIC X(700)  VALUE SPACE.                
019200     03  WL317101 REDEFINES IO-AREA.                                      
019300*        05  -COPY WDGX01    -PRE 3171-                                   
019400     03  WL317111 REDEFINES IO-AREA.                                      
019500*        05  -COPY WDGX3172  -PRE 3171-                                   
019600     03  WL317121 REDEFINES IO-AREA.                                      
019700*        05  -COPY WDGX3174  -PRE 3171-                                   
019800     EJECT                                                                
019900 LINKAGE SECTION.                                                         
020000*01  -COPY W0009   -PRE MSG-                                              
020100                                                                          
020200*01  -COPY W0009   -PRE ALT-                                              
020300                                                                          
020400*01  -COPY W0008   -PRE USEA-                                             
020500     05  FILLER                  PIC X.                                   
020600                                                                          
020700*01  -COPY W0008  -PRE 3171-                                              
020800     05  FILLER                  PIC X.                                   
020900     EJECT                                                                
021000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB 3171-PCB.             
021100 MAIN SECTION.                                                            
021200     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB 3171-PCB.             
021300                                                                          
021400     PERFORM IMS-GET-MSG                                                  
021500     IF SEGMENT-FINNS                                                     
021600       PERFORM A-INIT                                                     
021700       PERFORM B-KOLLA-NYCKLAR                                            
021800       IF NYCKLAR-OK                                                      
021900         IF MFS-SPLIT                                                     
022000           PERFORM I-TILL-3184                                            
022100         ELSE                                                             
022200            IF MFS-UPDATE                                                 
022300              PERFORM G-KOLLA-INPUT                                       
022400              IF INDATA-OK                                                
022500                PERFORM H-UPPDATERA                                       
022600              END-IF                                                      
022700            ELSE                                                          
022800              IF MFS-FIRST                                                
022900                PERFORM C-FOERSTA-SIDA                                    
023000              ELSE                                                        
023100                IF MFS-NEXT                                               
023200                  PERFORM D-NAESTA-SIDA                                   
023300                ELSE                                                      
023400                  PERFORM E-SAMMA-SIDA                                    
023500                END-IF                                                    
023600              END-IF                                                      
023700            END-IF                                                        
023800            IF INDATA-OK                                                  
023900            PERFORM F-LAES-VISA-INFO                                      
024000            END-IF                                                        
024100         END-IF                                                           
024200       END-IF                                                             
024300*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
024400*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
024500       IF MFS-SPLIT                                                       
024600          CONTINUE                                                        
024700       ELSE                                                               
024800          COMPUTE MSG-KVLL = LENGTH OF MOD-W3O18501 + 4                   
024900          PERFORM IMS-INSERT-MSG                                          
025000       END-IF                                                             
025100     END-IF                                                               
025200                                                                          
025300     MOVE ZERO TO RETURN-CODE                                             
025400     GOBACK                                                               
025500     .                                                                    
025600     EJECT                                                                
025700 A-INIT SECTION.                                                          
025800                                                                          
025900     MOVE 'A-INIT'  TO WS-SEKTION                                         
026000     IF MSG-DUBBLA-TRANSKODER                                             
026100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I18501                 
026200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
026300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
026400     ELSE                                                                 
026500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I18501                  
026600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
026700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026800     END-IF                                                               
026900                                                                          
027000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
027100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
027200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
027300                                                                          
027400     MOVE LOW-VALUE TO MSG-AREA                                           
027500     MOVE 'W3O185N1' TO MFS-IDMOD                                         
027600     MOVE '3185' TO MOD-IDTRANS                                           
027700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
027800                                                                          
027900     MOVE ZERO        TO W-IDDC-REC                                       
028000     MOVE ZERO        TO W-IDDC-SEND                                      
028100     MOVE ZERO        TO W-IDFAKT-MIN                                     
028200     MOVE ZERO        TO W-IDFAKT                                         
028300                                                                          
028400     IF EGEN-MID OR HELP-MID                                              
028500       CONTINUE                                                           
028600     ELSE                                                                 
028700       MOVE SPACE TO MFS-KDTRTYP                                          
028800       MOVE '7' TO MFS-IDPFK                                              
028900     END-IF                                                               
029000     .                                                                    
029100     EJECT                                                                
029200 B-KOLLA-NYCKLAR SECTION.                                                 
029300     MOVE 'B-KOLLA-NYCKLAR'  TO WS-SEKTION                                
029500                                                                          
029600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
029700     MOVE '001'             TO MSGI-KDCALL                                
029800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
029900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
030000     MOVE '3185'            TO MSGI-IDTRANS                               
030100                                                                          
030200     IF EGEN-MID                                                          
030300         MOVE MID-IDDC-REC-IN TO MSGI-IDDC-REC                            
030400         MOVE MID-IDDC-SEND-IN TO MSGI-IDDC-SEND                          
030500     END-IF                                                               
030600                                                                          
030700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
030800     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
030900                                                                          
031000     MOVE JA TO NYCKLAR-SW                                                
031100     MOVE NEJ TO BYT-SW                                                   
031200                                                                          
031300                                                                          
031400     MOVE MSGI-IDDC             TO W-IDDC-BEH                             
031500                                                                          
031600*    -- KONTROLL AV IDFAKT                                                
031700     MOVE MFS-RENSA-FAELT TO MOD-IDDC-REC-IN                              
031800                                                                          
031900     IF MID-IDDC-REC-IN NOT = ALL '+'                                     
032000       MOVE '7'         TO MFS-IDPFK                                      
032100       MOVE SPACE       TO MFS-KDTRTYP                                    
032200     END-IF                                                               
032300                                                                          
032400     IF MID-IDDC-SEND-IN NOT = ALL '+'                                    
032500       MOVE '7'         TO MFS-IDPFK                                      
032600       MOVE SPACE       TO MFS-KDTRTYP                                    
032700     END-IF                                                               
032800                                                                          
032900     INSPECT MSGI-IDDC-REC REPLACING LEADING SPACE BY ZERO                
033000                                                                          
033100     IF MSGI-IDDC-REC NUMERIC AND MSGI-IDDC-REC > ZERO                    
033200        MOVE MSGI-IDDC-REC  TO TEST-WS-IDDC                               
033300        IF TEST-SDC-NL-ET                                                 
033500           MOVE MSGI-IDDC-REC TO W-IDDC-REC                               
033600                            W-IDDC-MIN                                    
033700        ELSE                                                              
033800          MOVE NEJ     TO NYCKLAR-SW                                      
033900        END-IF                                                            
034000     ELSE                                                                 
034100        MOVE NEJ       TO NYCKLAR-SW                                      
034200     END-IF                                                               
034300                                                                          
034400     MOVE MFS-RENSA-FAELT TO MOD-IDDC-SEND-IN                             
034500                                                                          
034600     INSPECT MSGI-IDDC-SEND REPLACING LEADING SPACE BY ZERO               
034700                                                                          
034800     IF MSGI-IDDC-SEND NUMERIC AND MSGI-IDDC-SEND > ZERO                  
034900        MOVE MSGI-IDDC-SEND TO TEST-WS-IDDC                               
035100        IF  TEST-NDC-AU OR TEST-NDC-JP                                    
035200           MOVE MSGI-IDDC-SEND TO WS-IDDC                                 
035300                                 W-IDDC-SEND                              
035400        ELSE                                                              
035500          MOVE NEJ            TO NYCKLAR-SW                               
035600        END-IF                                                            
035700     ELSE                                                                 
035800        MOVE ZERO             TO W-IDDC-SEND                              
035900        MOVE SPACE            TO WS-IDDC                                  
036000     END-IF                                                               
036100                                                                          
036200     IF GODK-MID OR NYCKLAR-OK                                            
036300       MOVE MSGI-IDDC-REC      TO MOD-IDDC-REC-UT                         
036400       MOVE WS-IDDC            TO MOD-IDDC-SEND-UT                        
036500     ELSE                                                                 
036600       MOVE MFS-RENSA-FAELT    TO MOD-IDDC-REC-UT                         
036700       MOVE MFS-RENSA-FAELT    TO MOD-IDDC-SEND-UT                        
036800     END-IF                                                               
036900                                                                          
037000     IF NYCKLAR-FEL                                                       
037100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
037200       CALL WMEDKONV USING MED-WMEDAREA                                   
037300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
037400       PERFORM MFS-RENSA-FAELT-BILD                                       
037500     END-IF                                                               
037600     .                                                                    
037700     EJECT                                                                
037800 C-FOERSTA-SIDA SECTION.                                                  
037900     MOVE 'C-FOERSTA-SIDA'   TO WS-SEKTION                                
038100                                                                          
038200     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
038300     CALL WMEDKONV USING MED-WMEDAREA                                     
038400     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
038500                                                                          
038600     PERFORM MFS-RENSA-FAELT-IN                                           
038700     .                                                                    
038800     EJECT                                                                
038900 D-NAESTA-SIDA SECTION.                                                   
039000     MOVE 'D-NAESTA-SIDA'   TO WS-SEKTION                                 
039200                                                                          
039300     IF SPAR-IDTRANS = '3185'                                             
039400       IF SPAR-IDFAKT-NEXT > ZERO                                         
039500          MOVE SPAR-IDFAKT-NEXT TO W-IDFAKT-MIN                           
039600       ELSE                                                               
039700          MOVE SPAR-IDFAKT-ENTER TO W-IDFAKT-MIN                          
039800       END-IF                                                             
039900                                                                          
040000       IF SPAR-IDDC-REC-NEXT = SPACE                                      
040100          MOVE SPAR-IDDC-REC-ENTER TO W-IDDC-REC                          
040200       ELSE                                                               
040300          MOVE SPAR-IDDC-REC-NEXT TO W-IDDC-REC                           
040400       END-IF                                                             
040500       IF SPAR-IDDC-SEND-NEXT = SPACE                                     
040600          MOVE SPAR-IDDC-SEND-ENTER TO W-IDDC-SEND                        
040700       ELSE                                                               
040800          MOVE SPAR-IDDC-SEND-NEXT TO W-IDDC-SEND                         
040900       END-IF                                                             
041000     ELSE                                                                 
041100       PERFORM MFS-RENSA-FAELT-IN                                         
041200     END-IF                                                               
041300     .                                                                    
041400     EJECT                                                                
041500 E-SAMMA-SIDA SECTION.                                                    
041600     MOVE 'E-SAMMA-SIDA'   TO WS-SEKTION                                  
041800                                                                          
041900     IF SPAR-IDTRANS = '3185' OR '0551'                                   
042000          MOVE SPAR-IDFAKT-ENTER TO W-IDFAKT-MIN                          
042100          IF SPAR-IDDC-REC-ENTER = SPACE                                  
042200             MOVE ZERO                TO W-IDDC-REC                       
042300          ELSE                                                            
042400             MOVE SPAR-IDDC-REC-ENTER TO W-IDDC-REC                       
042500          END-IF                                                          
042600          IF SPAR-IDDC-SEND-ENTER = SPACE                                 
042700             MOVE ZERO                 TO W-IDDC-SEND                     
042800          ELSE                                                            
042900             MOVE SPAR-IDDC-SEND-ENTER TO W-IDDC-SEND                     
043000          END-IF                                                          
043100          PERFORM MFS-RENSA-FAELT-IN                                      
043200     ELSE                                                                 
043300        PERFORM MFS-RENSA-FAELT-IN                                        
043400     END-IF                                                               
043500     .                                                                    
043600     EJECT                                                                
043700                                                                          
043800 F-LAES-VISA-INFO SECTION.                                                
043900     MOVE 'F-LAES-VISA-INFO'   TO WS-SEKTION                              
044100                                                                          
044200     PERFORM FA-LAES-GRUNDDATA                                            
044300                                                                          
044400     IF SEGMENT-SAKNAS                                                    
044500        MOVE INVOICE-MISSING  TO MED-IDMFSFEL                             
044600        CALL WMEDKONV USING MED-WMEDAREA                                  
044700        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
044800        PERFORM MFS-RENSA-FAELT-UT                                        
044900     ELSE                                                                 
045000                                                                          
045100       MOVE +1 TO INDX                                                    
045200                                                                          
045300       MOVE 3171-3172-IDFAKT TO SPAR-IDFAKT-ENTER                         
045400       IF W-IDDC-REC > ZERO                                               
045500            MOVE 3171-3172-IDDC-REC TO SPAR-IDDC-REC-ENTER                
045600       ELSE                                                               
045700            MOVE SPACE              TO SPAR-IDDC-REC-ENTER                
045800       END-IF                                                             
045900       IF W-IDDC-SEND  > ZERO                                             
046000            MOVE 3171-3172-IDDC-SEND TO SPAR-IDDC-SEND-ENTER              
046100       ELSE                                                               
046200            MOVE SPACE               TO SPAR-IDDC-SEND-ENTER              
046300       END-IF                                                             
046400                                                                          
046500       PERFORM UNTIL INDX > MAX-INDX                                      
046600         IF SEGMENT-FINNS                                                 
046700           MOVE 3171-3172-IDFAKT TO MOD-IDFAKT (INDX)                     
046800           MOVE 3171-3172-IDDC-REC TO MOD-IDDC-REC (INDX)                 
046900           MOVE 3171-3172-IDDC-SEND TO MOD-IDDC-SEND (INDX)               
047000           MOVE 3171-3172-KDTRSTAT  TO MOD-KDTRSTAT  (INDX)               
047100           MOVE 3171-3172-DASNDDAT  TO MOD-DAANKDAG  (INDX)               
047200           PERFORM FB-LAES-RADDATA                                        
047300         ELSE                                                             
047400           MOVE MFS-RENSA-FAELT TO MOD-IDFAKT (INDX)                      
047500                                   MOD-IDDC-REC (INDX)                    
047600                                   MOD-IDDC-SEND (INDX)                   
047700                                   MOD-DAANKDAG (INDX)                    
047800                                   MOD-KDTRSTAT (INDX)                    
047900         END-IF                                                           
048000         ADD 1 TO INDX                                                    
048100       END-PERFORM                                                        
048200                                                                          
048300       IF SEGMENT-FINNS                                                   
048400         MOVE 3171-3172-IDFAKT TO SPAR-IDFAKT-NEXT                        
048500         IF W-IDDC-REC > ZERO                                             
048600            MOVE 3171-3172-IDDC-REC TO SPAR-IDDC-REC-NEXT                 
048700         ELSE                                                             
048800            MOVE ZERO               TO SPAR-IDDC-REC-NEXT                 
048900         END-IF                                                           
049000         IF W-IDDC-SEND  > ZERO                                           
049100            MOVE 3171-3172-IDDC-SEND TO SPAR-IDDC-SEND-NEXT               
049200         ELSE                                                             
049300            MOVE ZERO                TO SPAR-IDDC-SEND-NEXT               
049400         END-IF                                                           
049500         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
049600         CALL WMEDKONV USING MED-WMEDAREA                                 
049700         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
049800       ELSE                                                               
049900         MOVE ZERO             TO SPAR-IDFAKT-NEXT                        
050000         MOVE SPACE          TO SPAR-IDDC-REC-NEXT                        
050100         MOVE SPACE          TO SPAR-IDDC-SEND-NEXT                       
050200       END-IF                                                             
050300                                                                          
050400       MOVE '002'      TO MSGI-KDCALL                                     
050500       MOVE '3185'   TO SPAR-IDTRANS                                      
050600       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
050700       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
050800     END-IF                                                               
050900     .                                                                    
051000     EJECT                                                                
051100 FA-LAES-GRUNDDATA SECTION.                                               
051200     MOVE 'FA-LAES-GRUNDDATA'   TO WS-SEKTION                             
051400                                                                          
051500     MOVE NEJ      TO KLART-SW                                            
051600                                                                          
051700     IF W-IDFAKT-MIN > ZERO                                               
051800        MOVE W-IDFAKT-MIN TO W-IDFAKT                                     
051900        PERFORM IMS-GET-ROOT                                              
052000        PERFORM IMS-GNP-INVOICE                                           
052100        PERFORM UNTIL BAS-SLUT                                            
052200            OR SEGMENT-SAKNAS                                             
052300            OR KLART-SLUT                                                 
052400            IF 3171-3172-KDTRSTAT = 2 OR 3                                
052500               IF 3171-3172-IDFAKT   = W-IDFAKT                           
052600                  MOVE JA       TO KLART-SW                               
052700               END-IF                                                     
052800            END-IF                                                        
052900            IF KLART-SLUT                                                 
053000               CONTINUE                                                   
053100            ELSE                                                          
053200               PERFORM IMS-GNP-INVOICE                                    
053300            END-IF                                                        
053400        END-PERFORM                                                       
053500     ELSE                                                                 
053600        PERFORM IMS-GET-ROOT                                              
053700        IF W-IDDC-REC > ZERO                                              
053800        AND W-IDDC-SEND  > ZERO                                           
053900            PERFORM IMS-GNP-INVOICE                                       
054000            PERFORM UNTIL BAS-SLUT                                        
054100               OR SEGMENT-SAKNAS                                          
054200               OR KLART-SLUT                                              
054300                  IF 3171-3172-KDTRSTAT = 2 OR 3                          
054400                     IF 3171-3172-IDDC-REC = W-IDDC-REC                   
054500                     AND 3171-3172-IDDC-SEND = W-IDDC-SEND                
054600                        MOVE JA       TO KLART-SW                         
054700                     END-IF                                               
054800                  END-IF                                                  
054900                  IF KLART-SLUT                                           
055000                     CONTINUE                                             
055100                  ELSE                                                    
055200                     PERFORM IMS-GNP-INVOICE                              
055300                  END-IF                                                  
055400            END-PERFORM                                                   
055500        ELSE                                                              
055600           IF W-IDDC-REC > ZERO                                           
055700              PERFORM IMS-GNP-INVOICE                                     
055800              PERFORM UNTIL BAS-SLUT                                      
055900               OR SEGMENT-SAKNAS                                          
056000               OR KLART-SLUT                                              
056100                  IF 3171-3172-KDTRSTAT = 2 OR 3                          
056200                     IF 3171-3172-IDDC-REC = W-IDDC-REC                   
056300                        MOVE JA       TO KLART-SW                         
056400                     END-IF                                               
056500                  END-IF                                                  
056600                  IF KLART-SLUT                                           
056700                     CONTINUE                                             
056800                  ELSE                                                    
056900                     PERFORM IMS-GNP-INVOICE                              
057000                  END-IF                                                  
057100              END-PERFORM                                                 
057200           ELSE                                                           
057300              IF W-IDDC-SEND > ZERO                                       
057400                 PERFORM IMS-GNP-INVOICE                                  
057500                 PERFORM UNTIL BAS-SLUT                                   
057600                 OR SEGMENT-SAKNAS                                        
057700                 OR KLART-SLUT                                            
057800                   IF 3171-3172-KDTRSTAT = 2 OR 3                         
057900                     IF 3171-3172-IDDC-SEND = W-IDDC-SEND                 
058000                        MOVE JA       TO KLART-SW                         
058100                     END-IF                                               
058200                   END-IF                                                 
058300                   IF KLART-SLUT                                          
058400                     CONTINUE                                             
058500                   ELSE                                                   
058600                     PERFORM IMS-GNP-INVOICE                              
058700                   END-IF                                                 
058800                 END-PERFORM                                              
058900              END-IF                                                      
059000           END-IF                                                         
059100        END-IF                                                            
059200                                                                          
059300     END-IF                                                               
059400                                                                          
059500     .                                                                    
059600     EJECT                                                                
059700 FB-LAES-RADDATA SECTION.                                                 
059800     MOVE 'FB-LAES-RADDDATA'   TO WS-SEKTION                              
060000                                                                          
060100     MOVE NEJ      TO KLART-SW                                            
060200                                                                          
060300     IF W-IDDC-REC > ZERO                                                 
060400     AND W-IDDC-SEND  > ZERO                                              
060500         PERFORM IMS-GNP-INVOICE                                          
060600         PERFORM UNTIL BAS-SLUT                                           
060700            OR SEGMENT-SAKNAS                                             
060800            OR KLART-SLUT                                                 
060900               IF 3171-3172-KDTRSTAT = 2 OR 3                             
061000                  IF 3171-3172-IDDC-REC = W-IDDC-REC                      
061100                  AND 3171-3172-IDDC-SEND = W-IDDC-SEND                   
061200                     MOVE JA       TO KLART-SW                            
061300                  END-IF                                                  
061400               END-IF                                                     
061500               IF KLART-SLUT                                              
061600                  CONTINUE                                                
061700               ELSE                                                       
061800                  PERFORM IMS-GNP-INVOICE                                 
061900               END-IF                                                     
062000         END-PERFORM                                                      
062100     ELSE                                                                 
062200        IF W-IDDC-REC > ZERO                                              
062300           PERFORM IMS-GNP-INVOICE                                        
062400           PERFORM UNTIL BAS-SLUT                                         
062500              OR SEGMENT-SAKNAS                                           
062600              OR KLART-SLUT                                               
062700                 IF 3171-3172-KDTRSTAT = 2 OR 3                           
062800                    IF 3171-3172-IDDC-REC = W-IDDC-REC                    
062900                       MOVE JA       TO KLART-SW                          
063000                    END-IF                                                
063100                 END-IF                                                   
063200                 IF KLART-SLUT                                            
063300                    CONTINUE                                              
063400                 ELSE                                                     
063500                    PERFORM IMS-GNP-INVOICE                               
063600                 END-IF                                                   
063700           END-PERFORM                                                    
063800        ELSE                                                              
063900           IF W-IDDC-SEND > ZERO                                          
064000              PERFORM IMS-GNP-INVOICE                                     
064100              PERFORM UNTIL BAS-SLUT                                      
064200                OR SEGMENT-SAKNAS                                         
064300                OR KLART-SLUT                                             
064400                IF 3171-3172-KDTRSTAT = 2 OR 3                            
064500                   IF 3171-3172-IDDC-SEND = W-IDDC-SEND                   
064600                      MOVE JA       TO KLART-SW                           
064700                   END-IF                                                 
064800                END-IF                                                    
064900                IF KLART-SLUT                                             
065000                   CONTINUE                                               
065100                ELSE                                                      
065200                   PERFORM IMS-GNP-INVOICE                                
065300                END-IF                                                    
065400              END-PERFORM                                                 
065500           END-IF                                                         
065600        END-IF                                                            
065700     END-IF                                                               
065800                                                                          
065900     .                                                                    
066000     EJECT                                                                
066100 G-KOLLA-INPUT SECTION.                                                   
066200                                                                          
066300     MOVE 'G-KOLLA-INPUT' TO WS-SEKTION                                   
066500     MOVE JA  TO INDATA-SW                                                
066600     MOVE +1 TO INDX                                                      
066700     PERFORM UNTIL INDX > MAX-INDX                                        
066800       IF MID-KDTRSTAT (INDX) NOT = ALL '+'                               
066900         INSPECT MID-IDFAKT  (INDX)                                       
067000                    REPLACING LEADING SPACE BY ZERO                       
067100         MOVE MID-IDFAKT (INDX)    TO W-IDFAKT                            
067200         IF W-IDFAKT > ZERO                                               
067300           PERFORM IMS-GHU-INVOICE                                        
067400           IF SEGMENT-FINNS                                               
067500             IF W-IDDC-BEH = 3171-3172-IDDC-REC                           
067600               IF 3171-3172-KDTRSTAT = 2                                  
067700                 IF MID-KDTRSTAT (INDX) = 3                               
067800                   MOVE MFS-NUM-FAELT-RAETT TO                            
067900                                    MOD-KDTRSTAT-ATTR (INDX)              
068000                 ELSE                                                     
068100                   MOVE MFS-NUM-FAELT-FEL TO                              
068200                                    MOD-KDTRSTAT-ATTR (INDX)              
068300                   MOVE NEJ TO INDATA-SW                                  
068400                   MOVE WRONG-STATUS TO MED-IDMFSINF                      
068500                   CALL WMEDKONV USING MED-WMEDAREA                       
068600                   MOVE MED-MFSINF TO MOD-TEMFSINF                        
068700                 END-IF                                                   
068800               ELSE                                                       
068900                 MOVE MFS-NUM-FAELT-FEL TO                                
069000                                    MOD-KDTRSTAT-ATTR (INDX)              
069100                 MOVE WRONG-STATUS TO MED-IDMFSINF                        
069200                 CALL WMEDKONV USING MED-WMEDAREA                         
069300                 MOVE MED-MFSINF TO MOD-TEMFSINF                          
069400                 MOVE NEJ TO INDATA-SW                                    
069500               END-IF                                                     
069600             ELSE                                                         
069700               MOVE MFS-NUM-FAELT-FEL TO                                  
069800                                    MOD-KDTRSTAT-ATTR (INDX)              
069900               MOVE NEJ TO INDATA-SW                                      
070000             END-IF                                                       
070100           END-IF                                                         
070200         ELSE                                                             
070300           MOVE MFS-NUM-FAELT-FEL TO                                      
070400                     MOD-KDTRSTAT-ATTR (INDX)                             
070500           MOVE NEJ TO INDATA-SW                                          
070600           MOVE WRONG-STATUS TO MED-IDMFSINF                              
070700           CALL WMEDKONV USING MED-WMEDAREA                               
070800           MOVE MED-MFSINF TO MOD-TEMFSINF                                
070900         END-IF                                                           
071000       END-IF                                                             
071100       ADD +1 TO INDX                                                     
071200     END-PERFORM                                                          
071300                                                                          
071400     IF INDATA-FEL                                                        
071500        IF W-IDDC-BEH = 3171-3172-IDDC-REC                                
071600           MOVE NEJ TO ALLT-SW                                            
071700           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
071800           CALL WMEDKONV USING MED-WMEDAREA                               
071900           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
072000        ELSE                                                              
072100           IF W-IDFAKT > ZERO                                             
072200              MOVE USER-NOT-ALLOWED TO MED-IDMFSINF                       
072300              CALL WMEDKONV USING MED-WMEDAREA                            
072400              MOVE MED-MFSINF TO MOD-TEMFSINF                             
072500           ELSE                                                           
072600              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
072700              CALL WMEDKONV USING MED-WMEDAREA                            
072800              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
072900           END-IF                                                         
073000        END-IF                                                            
073100        PERFORM MFS-ROER-EJ-FAELT-UT                                      
073200        PERFORM MFS-ROER-EJ-FAELT-IN                                      
073300     END-IF                                                               
073400                                                                          
073500     .                                                                    
073600     EJECT                                                                
073700 H-UPPDATERA SECTION.                                                     
073800                                                                          
073900     MOVE 'H-UPPDATERA  ' TO WS-SEKTION                                   
074100     MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSINF                            
074200     MOVE +1 TO INDX                                                      
074300*    -- KONTROLL AV KDTRSTAT                                              
074400     PERFORM UNTIL INDX > MAX-INDX                                        
074500       IF MID-KDTRSTAT (INDX) NOT = ALL '+'                               
074600         MOVE MID-IDFAKT (INDX)    TO W-IDFAKT                            
074700         PERFORM IMS-GHU-INVOICE                                          
074800         IF SEGMENT-FINNS                                                 
074900           IF 3171-3172-KDTRSTAT = 2                                      
075000             MOVE MID-KDTRSTAT (INDX) TO 3171-3172-KDTRSTAT               
075100             MOVE FUNCTION CURRENT-DATE (1:8)                             
075200                                      TO 3171-3172-DAANKDAG               
075300             PERFORM IMS-REPL-3171-INVOICE                                
075400                                                                          
075500             PERFORM IMS-GHNP-KOLLI                                       
075600             PERFORM UNTIL SEGMENT-SAKNAS                                 
075700               MOVE MID-KDTRSTAT (INDX) TO 3171-3174-KDTRSTAT             
075800               PERFORM IMS-REPL-3171-KOLLI                                
075900                                                                          
076000               PERFORM IMS-GHNP-KOLLI                                     
076100             END-PERFORM                                                  
076200                                                                          
076300             MOVE INF-UPDATE-DONE     TO MED-IDMFSINF                     
076400           END-IF                                                         
076500         ELSE                                                             
076600           MOVE INVOICE-MISSING       TO MED-IDMFSINF                     
076700         END-IF                                                           
076800       END-IF                                                             
076900       ADD +1 TO INDX                                                     
077000     END-PERFORM                                                          
077100     CALL WMEDKONV USING MED-WMEDAREA                                     
077200     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
077300     PERFORM MFS-ROER-EJ-FAELT-UT                                         
077400                                                                          
077500     .                                                                    
077600     EJECT                                                                
077700                                                                          
077800 I-TILL-3184 SECTION.                                                     
077900     MOVE 'I-TILL-3184  ' TO WS-SEKTION                                   
078100                                                                          
078200     MOVE LOW-VALUE    TO MSG-KDZ1 MSG-KDZ2                               
078300     MOVE '3185'       TO MSG-IDTRANS-1                                   
078400     MOVE '2'          TO MSG-KDMFSFOR-1                                  
078500                                                                          
078600     COMPUTE MSG-KVLL = LENGTH OF MOD-MID-W3I18401 + 17                   
078700                                                                          
078800     MOVE +1 TO INDX                                                      
078900                                                                          
079000     PERFORM UNTIL INDX > MAX-INDX                                        
079100       IF MID-KDCMD  (INDX) = '+' OR SPACE                                
079200         ADD +1 TO INDX                                                   
079300       ELSE                                                               
079400          IF MID-KDCMD  (INDX) = 'S'                                      
079500                                                                          
079600            INSPECT MID-IDFAKT  (INDX)                                    
079700                    REPLACING LEADING SPACE BY ZERO                       
079800            INSPECT MID-IDDC-REC (INDX)                                   
079900                    REPLACING LEADING SPACE BY ZERO                       
080000            MOVE ALL '+'   TO MOD-MID-W3I18401                            
080100            MOVE MID-IDFAKT  (INDX) TO MOD-MID-IDFAKT-IN                  
080200            MOVE 'N'                 TO MOD-MID-FLINLI-IN                 
080300            MOVE MID-IDDC-REC (INDX) TO MOD-MID-IDDC-REC-IN               
080400            MOVE 'W3T184 7' TO MSG-KDTRANS-1                              
080500            PERFORM IMS-INSERT-ALT-MSG                                    
080600            MOVE +9999 TO INDX                                            
080700          ELSE                                                            
080800            ADD +1 TO INDX                                                
080900          END-IF                                                          
081000       END-IF                                                             
081100     END-PERFORM                                                          
081200     .                                                                    
081300     EJECT                                                                
081400                                                                          
081500 MFS-RENSA-FAELT-BILD SECTION.                                            
081600     MOVE 'MFS-RENSA-FAELT-BILD'  TO WS-SEKTION                           
081800                                                                          
081900     MOVE +1 TO INDX                                                      
082000     PERFORM UNTIL INDX > MAX-INDX                                        
082100         MOVE MFS-RENSA-FAELT TO MOD-KDCMD   (INDX)                       
082200                                 MOD-IDDC-REC    (INDX)                   
082300                                 MOD-IDDC-SEND   (INDX)                   
082400                                 MOD-IDFAKT  (INDX)                       
082500                                 MOD-KDTRSTAT (INDX)                      
082600                                 MOD-DAANKDAG (INDX)                      
082700         ADD +1 TO INDX                                                   
082800     END-PERFORM                                                          
082900                                                                          
083000                                                                          
083100                                                                          
083200     .                                                                    
083300     EJECT                                                                
083400 MFS-RENSA-FAELT-UT SECTION.                                              
083500                                                                          
083600*    --- ALLA UTDATA-FÄLT                                                 
083700*    --- INKL. BLÄDDRINGSNYCKLAR                                          
083800     MOVE MFS-RENSA-FAELT TO MOD-IDDC-REC-UT                              
083900                                MOD-IDDC-SEND-UT                          
084000     PERFORM MFS-RENSA-RAD-FAELT-UT                                       
084100     .                                                                    
084200     SKIP3                                                                
084300 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
084400                                                                          
084500*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
084510     MOVE +1 TO INDX                                                      
084520     PERFORM UNTIL INDX > MAX-INDX                                        
084521       MOVE MFS-RENSA-FAELT TO MOD-IDFAKT (INDX)                          
084522                               MOD-IDDC-REC (INDX)                        
084523                               MOD-IDDC-SEND (INDX)                       
084524                               MOD-KDTRSTAT  (INDX)                       
084525                               MOD-DAANKDAG  (INDX)                       
084540       ADD +1 TO INDX                                                     
084550     END-PERFORM                                                          
085200     .                                                                    
085300     SKIP3                                                                
085400 MFS-RENSA-FAELT-IN SECTION.                                              
085500                                                                          
085600*    --- ALLA INDATA-FÄLT                                                 
085700     MOVE MFS-RENSA-FAELT TO MOD-IDDC-REC-IN                              
085800                             MOD-IDDC-SEND-IN                             
085900     .                                                                    
086000     EJECT                                                                
086100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
086200                                                                          
086300*    --- ALLA UTDATA-FÄLT                                                 
086400     MOVE +1 TO INDX                                                      
086500     PERFORM UNTIL INDX > MAX-INDX                                        
086600       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
086700       ADD +1 TO INDX                                                     
086800     END-PERFORM                                                          
086900     .                                                                    
087000     SKIP2                                                                
087100 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
087200                                                                          
087300*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
087400     MOVE MFS-ROER-EJ-FAELT TO     MOD-IDFAKT (INDX)                      
087500                                   MOD-IDDC-REC (INDX)                    
087600                                   MOD-IDDC-SEND (INDX)                   
087700                                   MOD-DAANKDAG (INDX)                    
087800                                   MOD-KDTRSTAT (INDX)                    
087900     .                                                                    
088000     SKIP2                                                                
088100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
088200                                                                          
088300*    --- ALLA INDATA-FÄLT                                                 
088400     MOVE +1 TO INDX                                                      
088500     PERFORM UNTIL INDX > MAX-INDX                                        
088600       MOVE MFS-ROER-EJ-FAELT TO MOD-KDTRSTAT (INDX)                      
088700                                 MOD-KDCMD    (INDX)                      
088800       ADD +1 TO INDX                                                     
088900     END-PERFORM                                                          
089000     .                                                                    
089100     SKIP2                                                                
089200* --- IMS SEKTIONER ---                                                   
089300     SKIP3                                                                
089400 IMS-GET-MSG SECTION.                                                     
089500     MOVE 'IMS-GET-MSG ' TO WS-IMS-SEKTION                                
089700                                                                          
089800     MOVE '  QC' TO GODK-STATUSKODER                                      
089900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
090000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
090100     PERFORM IMS-STATUSKONTROLL                                           
090200     .                                                                    
090300     SKIP3                                                                
090400 IMS-INSERT-MSG SECTION.                                                  
090500     MOVE 'IMS-INSERT-MSG ' TO WS-IMS-SEKTION                             
090700                                                                          
090800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
090900     MOVE SPACE TO GODK-STATUSKODER                                       
091000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
091100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
091200     PERFORM IMS-STATUSKONTROLL                                           
091300     .                                                                    
091400     EJECT                                                                
091500 IMS-INSERT-ALT-MSG SECTION.                                              
091600                                                                          
091700     MOVE SPACE TO GODK-STATUSKODER                                       
091800     CALL CBLTDLI USING PURG ALT-PCB MSG-IO-AREA                          
091900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
092000     PERFORM IMS-STATUSKONTROLL                                           
092100     .                                                                    
092200     SKIP3                                                                
092300 IMS-GET-ROOT SECTION.                                                    
092400     MOVE 'IMS-GU-ROOT' TO WS-IMS-SEKTION                                 
092600                                                                          
092700     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
092800          DELIMITED BY SIZE INTO SSA1                                     
092900     MOVE '  ' TO  GODK-STATUSKODER                                       
093000     CALL CBLTDLI USING GU 3171-PCB DLI-IO-AREA SSA1                      
093100     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
093200     PERFORM IMS-STATUSKONTROLL                                           
093300     .                                                                    
093400     EJECT                                                                
093500 IMS-GNP-INVOICE SECTION.                                                 
093600     MOVE 'IMS-GNP-INVOICE'         TO WS-IMS-SEKTION                     
093800                                                                          
093900     MOVE 'WL317111 ' TO SSA1                                             
094000     MOVE '  GEGB' TO  GODK-STATUSKODER                                   
094100     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-AREA SSA1                     
094200     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
094300     PERFORM IMS-STATUSKONTROLL                                           
094400     .                                                                    
094500     EJECT                                                                
094600                                                                          
094700 IMS-GHU-INVOICE SECTION.                                                 
094800     MOVE 'IMS-GHU-INVOICE' TO WS-IMS-SEKTION                             
095000                                                                          
095100     STRING 'WL317101(WDGXKEY  =' W-IDHTYP-X ')'                          
095200          DELIMITED BY SIZE INTO SSA1                                     
095300     STRING 'WL317111(IDFAKT   =' W-IDFAKT-X ')'                          
095400          DELIMITED BY SIZE INTO SSA2                                     
095500     MOVE '  GE' TO GODK-STATUSKODER                                      
095600     CALL CBLTDLI USING GHU 3171-PCB DLI-IO-AREA SSA1 SSA2                
095700     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
095800     PERFORM IMS-STATUSKONTROLL                                           
095900     .                                                                    
096000     SKIP3                                                                
096100 IMS-REPL-3171-INVOICE SECTION.                                           
096200     MOVE 'IMS-REPL-3171-INVOICE'   TO WS-IMS-SEKTION                     
096400                                                                          
096500     MOVE 'WL317111 ' TO SSA1                                             
096600     MOVE '  ' TO GODK-STATUSKODER                                        
096700     CALL CBLTDLI USING REPL 3171-PCB DLI-IO-AREA SSA1                    
096800     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
096900     PERFORM IMS-STATUSKONTROLL                                           
097000     .                                                                    
097100     EJECT                                                                
097200 IMS-GHNP-KOLLI SECTION.                                                  
097300     MOVE 'IMS-GHU-KOLLI' TO WS-IMS-SEKTION                               
097500                                                                          
097600     MOVE 'WL317121' TO SSA1                                              
097700     MOVE '  GE' TO GODK-STATUSKODER                                      
097800     CALL CBLTDLI USING GHNP 3171-PCB DLI-IO-AREA SSA1                    
097900     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
098000     PERFORM IMS-STATUSKONTROLL                                           
098100     .                                                                    
098200     SKIP3                                                                
098300 IMS-REPL-3171-KOLLI SECTION.                                             
098400     MOVE 'IMS-REPL-3171-KOLLI' TO WS-IMS-SEKTION                         
098600                                                                          
098700     MOVE 'WL317121 ' TO SSA1                                             
098800     MOVE '  ' TO GODK-STATUSKODER                                        
098900     CALL CBLTDLI USING REPL 3171-PCB DLI-IO-AREA SSA1                    
099000     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
099100     PERFORM IMS-STATUSKONTROLL                                           
099200     .                                                                    
099300     EJECT                                                                
099400 IMS-STATUSKONTROLL SECTION.                                              
099500                                                                          
099600     SET STATUS-IX TO 1                                                   
099700     SEARCH GODK-STATUS                                                   
099800       AT END                                                             
099900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
100000         DELIMITED BY SIZE INTO FELTEXT                                   
100100         CALL FELLOG                                                      
100200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
100300         CONTINUE                                                         
100400     END-SEARCH                                                           
100500     .                                                                    
