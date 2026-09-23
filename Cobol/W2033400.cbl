000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2033400.                                                
000300 AUTHOR.         JONNY SANDSTEN.                                          
000400 DATE-WRITTEN.   98/10/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        FRÅGEBILD PÅ DIREKTLEVERANSER                                    
000900*        ANGE ARTIKELNUMMER, OM ARTIKELN INGÅR I NÅGON GRUPP              
001000*        VISAS BILDEN ANNARS FÅS "PART MISSING IN GROUP"                  
001100*                                                                         
001200*        PROGRAMMET LÄSER      WLLEVG (WDF2)                              
001300*        PROGRAMMET LÄSER      WLLEVF (WDF2)                              
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W2T334                                              
001700*        MID:         W2I33401                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W2O33401                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W2033400'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003900 77  MAX-INDX                    PIC S9(4)  VALUE +28   COMP SYNC.        
004000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004100                                                                          
004200                                                                          
004300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004400     88  NYCKLAR-OK                          VALUE 'J'.                   
004500     88  NYCKLAR-FEL                         VALUE 'N'.                   
004600                                                                          
004700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004800     88  EGEN-MID                            VALUE '2334'.                
004900     88  GODK-MID                            VALUE '2331' '2332'          
005000                                                   '2333' '2334'          
005100                                                   '2335' '2336'          
005200                                                   '2337' '2338'          
005300                                                   '2339'.                
005400     88  HELP-MID                            VALUE '0551'.                
005500     EJECT                                                                
005600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005700 01  GENERELLA-SUBPROGRAM.                                                
005800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006200     EJECT                                                                
006300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006400*01 -COPY WMEDAREA                                                        
006500     SKIP3                                                                
006600 01  MESSAGE-CODES.                                                       
006700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
006800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
006900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007000     03  PART-MISSING            PIC X(3)    VALUE '279'.                 
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007300*                                                                         
007400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007500     SKIP3                                                                
007600*01 -COPY WMSGINIT                                                        
007700     EJECT                                                                
007800*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
007900*                                                                         
008000 01  SPAR-AREA.                                                           
008100     03  SPAR-IDTRANS             PIC X(4)   VALUE '2334'.                
008200     03  SPAR-IDARTNR-ENTER       PIC S9(9) COMP-3 VALUE ZERO.            
008300     03  SPAR-IDARTNR-NEXT        PIC S9(9) COMP-3 VALUE ZERO.            
008400     03  SPAR-IDLEVNR-ENTER       PIC  X(5) VALUE SPACE.                  
008500     03  SPAR-IDLEVNR-NEXT        PIC  X(5) VALUE SPACE.                  
008600     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W2I33401                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W2O33401                                                 
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100     SKIP3                                                                
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011000*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
011100     03  W-WDF2A1KY-MIN-X.                                                
011200         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
011300         05  W-IDLEVNR-MIN       PIC  X(5)   VALUE SPACE.                 
011400                                                                          
011500     03  W-WDF2A1KY-MAX-X.                                                
011600         05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
011700         05  W-IDLEVNR-MAX       PIC  X(5)   VALUE SPACE.                 
011800                                                                          
011900     03  W-WDF2A1KY-X.                                                    
012000         05  W-IDARTNR-ASEQ      PIC S9(9)   VALUE ZERO COMP-3.           
012100         05  W-IDLEVNR-ASEQ      PIC  X(5)   VALUE SPACE.                 
012200                                                                          
012300     03  W-WDF201KY-X.                                                    
012400         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
012500         05  W-IDDIRGRP          PIC X(10)   VALUE SPACE.                 
012600     SKIP2                                                                
012700*    --- STATUS-KOD FRÅN IMS                                              
012800 01  STATUS-WS                   PIC XX.                                  
012900     88  SEGMENT-FINNS                       VALUE '  '.                  
013000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013200     88  BAS-SLUT                            VALUE 'GB'.                  
013300     SKIP2                                                                
013400 01  GODK-STATUSKODER.                                                    
013500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013600     SKIP3                                                                
013700 01  SSA1                        PIC X(64).                               
013800 01  SSA2                        PIC X(64).                               
013900     EJECT                                                                
014000*    --- IMS FUNKTIONSKODER                                               
014100*01  -COPY W0003                                                          
014200     EJECT                                                                
014300*    ---  DLI INPUT-OUTPUT AREA                                           
014400                                                                          
014500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVG01'.                    
014600 01  DLI-IO-WLLEVG01.                                                     
014700*    03  -COPY WDF2A1                                                     
014800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVF01'.                    
014900 01  DLI-IO-WLLEVF01.                                                     
015000*    03  -COPY WDF201                                                     
015100     EJECT                                                                
015200 LINKAGE SECTION.                                                         
015300*01  -COPY W0009   -PRE MSG-                                              
015400*01  -COPY W0008   -PRE USEA-                                             
015500     05  FILLER                  PIC X.                                   
015600                                                                          
015700*01  -COPY W0008  -PRE LEVG-                                              
015800     05  FILLER                  PIC X.                                   
015900                                                                          
016000*01  -COPY W0008  -PRE LEVF-                                              
016100     05  FILLER                  PIC X.                                   
016200     EJECT                                                                
016300 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB LEVG-PCB LEVF-PCB.            
016400 MAIN SECTION.                                                            
016500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB LEVG-PCB LEVF-PCB.            
016600                                                                          
016700     PERFORM IMS-GET-MSG                                                  
016800     IF SEGMENT-FINNS                                                     
016900       PERFORM A-INIT                                                     
017000       PERFORM B-KOLLA-NYCKLAR                                            
017100       IF NYCKLAR-OK                                                      
017200           IF MFS-FIRST                                                   
017300             PERFORM C-FOERSTA-SIDA                                       
017400           ELSE                                                           
017500             IF MFS-NEXT                                                  
017600               PERFORM D-NAESTA-SIDA                                      
017700             ELSE                                                         
017800               PERFORM E-SAMMA-SIDA                                       
017900             END-IF                                                       
018000           END-IF                                                         
018100         PERFORM F-LAES-VISA-INFO                                         
018200       END-IF                                                             
018300       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O33401 + 4                      
018400       PERFORM IMS-INSERT-MSG                                             
018500     END-IF                                                               
018600                                                                          
018700     MOVE ZERO TO RETURN-CODE                                             
018800     GOBACK                                                               
018900     .                                                                    
019000     EJECT                                                                
019100 A-INIT SECTION.                                                          
019200                                                                          
019300     IF MSG-DUBBLA-TRANSKODER                                             
019400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I33401                 
019500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
019600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019700     ELSE                                                                 
019800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I33401                  
019900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
020000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020100     END-IF                                                               
020200                                                                          
020300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
020400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
020500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
020600                                                                          
020700     MOVE LOW-VALUE  TO MSG-AREA                                          
020800     MOVE 'W2O334N1' TO MFS-IDMOD                                         
020900     MOVE '2334'     TO MOD-IDTRANS                                       
021000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
021100                                                                          
021200     IF EGEN-MID OR HELP-MID                                              
021300       CONTINUE                                                           
021400     ELSE                                                                 
021500       MOVE SPACE TO MFS-KDTRTYP                                          
021600       MOVE '7'   TO MFS-IDPFK                                            
021700     END-IF                                                               
021800     .                                                                    
021900     EJECT                                                                
022000 B-KOLLA-NYCKLAR SECTION.                                                 
022100                                                                          
022200     MOVE LOW-VALUE           TO W-WDF2A1KY-MIN-X                         
022300     MOVE HIGH-VALUE          TO W-WDF2A1KY-MAX-X                         
022400     MOVE ALL '+'             TO MSGI-WMSGINIT                            
022500     MOVE '001'               TO MSGI-KDCALL                              
022600     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
022700     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
022800     MOVE '2334'              TO MSGI-IDTRANS                             
022900     IF EGEN-MID                                                          
023000       MOVE MID-IDARTNR-IN    TO MSGI-IDARTNR                             
023100     ELSE                                                                 
023200       IF MID-IDARTNR-IN NUMERIC                                          
023300         MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                             
023400       END-IF                                                             
023500     END-IF                                                               
023600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023700     MOVE MSGI-SPAR-AREA      TO SPAR-AREA                                
023800                                                                          
023900     IF MSGI-IDLAND-SPR = 'GB'                                            
024000       MOVE 'GB' TO MED-IDSKYLT                                           
024100     ELSE                                                                 
024200       MOVE 'S' TO MED-IDSKYLT                                            
024300     END-IF                                                               
024400                                                                          
024500     MOVE JA TO NYCKLAR-SW                                                
024600     MOVE SPACE TO MED-IDMFSFEL                                           
024700     MOVE SPACE TO MED-IDMFSINF                                           
024800                                                                          
024900*    -- KONTROLL AV IDARTNR                                               
025000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
025100                                                                          
025200     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
025300     IF MSGI-IDARTNR NUMERIC AND MSGI-IDARTNR > ZERO                      
025400       MOVE MSGI-IDARTNR    TO W-IDARTNR-ASEQ                             
025500                               W-IDARTNR-MIN                              
025600                               W-IDARTNR-MAX                              
025700     ELSE                                                                 
025800       MOVE NEJ             TO NYCKLAR-SW                                 
025900     END-IF                                                               
026000                                                                          
026100     IF GODK-MID OR NYCKLAR-OK                                            
026200       MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                             
026300       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
026400     ELSE                                                                 
026500       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
026600     END-IF                                                               
026700                                                                          
026800     IF NYCKLAR-FEL                                                       
026900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
027000       CALL WMEDKONV USING MED-WMEDAREA                                   
027100       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
027200       PERFORM MFS-RENSA-FAELT-IN                                         
027300       PERFORM MFS-RENSA-FAELT-UT                                         
027400     END-IF                                                               
027500     .                                                                    
027600     EJECT                                                                
027700 C-FOERSTA-SIDA SECTION.                                                  
027800                                                                          
027900     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
028000     CALL WMEDKONV USING MED-WMEDAREA                                     
028100     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
028200                                                                          
028300     PERFORM MFS-RENSA-FAELT-IN                                           
028400     .                                                                    
028500     EJECT                                                                
028600 D-NAESTA-SIDA SECTION.                                                   
028700                                                                          
028800     IF SPAR-IDTRANS = '2334'                                             
028900       MOVE SPAR-IDARTNR-NEXT TO W-IDARTNR-MIN                            
029000                                 W-IDARTNR-MAX                            
029100       MOVE SPAR-IDLEVNR-NEXT TO W-IDLEVNR-MIN                            
029200     ELSE                                                                 
029300       PERFORM MFS-RENSA-FAELT-IN                                         
029400     END-IF                                                               
029500     .                                                                    
029600     EJECT                                                                
029700 E-SAMMA-SIDA SECTION.                                                    
029800                                                                          
029900     IF SPAR-IDTRANS = '2334' OR '0551'                                   
030000       CONTINUE                                                           
030100     ELSE                                                                 
030200       PERFORM MFS-RENSA-FAELT-IN                                         
030300     END-IF                                                               
030400     .                                                                    
030500     EJECT                                                                
030600 F-LAES-VISA-INFO SECTION.                                                
030700                                                                          
030800     PERFORM IMS-GU-SEQA                                                  
030900                                                                          
031000     IF SEGMENT-SAKNAS                                                    
031100       MOVE ZERO                  TO SPAR-IDARTNR-NEXT                    
031200       MOVE SPACE                 TO SPAR-IDLEVNR-NEXT                    
031300       MOVE PART-MISSING TO MED-IDMFSFEL                                  
031400       CALL WMEDKONV USING MED-WMEDAREA                                   
031500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
031600       PERFORM MFS-RENSA-FAELT-UT                                         
031700     ELSE                                                                 
031800       MOVE +1 TO INDX                                                    
031900       PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                    
032000       OR BAS-SLUT                                                        
032100         IF SEGMENT-FINNS                                                 
032300           MOVE SEQA-IDLEVNR       TO MOD-IDLEVNR      (INDX)             
032400                                      W-IDLEVNR                           
032500           MOVE SEQA-DASTADAT(3:6) TO MOD-TISTADAT-ART (INDX)             
032600           MOVE SEQA-IDDIRGRP      TO MOD-IDDIRGRP     (INDX)             
032700                                      W-IDDIRGRP                          
032800           PERFORM FA-HAEMTA-GRUPPDATUM                                   
033000         ELSE                                                             
033100           PERFORM MFS-RENSA-RAD-FAELT-UT                                 
033200         END-IF                                                           
033300                                                                          
033400         ADD +1 TO INDX                                                   
033600         PERFORM IMS-GN-SEQA                                              
033700       END-PERFORM                                                        
033800                                                                          
033900       IF SEGMENT-FINNS                                                   
034000         MOVE SEQA-IDARTNR          TO SPAR-IDARTNR-NEXT                  
034100         MOVE SEQA-IDLEVNR          TO SPAR-IDLEVNR-NEXT                  
034200         MOVE INF-MORE-INFO-EXISTS  TO MED-IDMFSINF                       
034300         CALL WMEDKONV USING MED-WMEDAREA                                 
034400         MOVE MED-TEMFSINF          TO MOD-TEMFSINF                       
034500       ELSE                                                               
034600         MOVE ZERO                  TO SPAR-IDARTNR-NEXT                  
034700         MOVE SPACE                 TO SPAR-IDLEVNR-NEXT                  
034800       END-IF                                                             
034900     END-IF                                                               
035000                                                                          
035100     MOVE '002'     TO MSGI-KDCALL                                        
035200     MOVE '2334'    TO SPAR-IDTRANS                                       
035300     MOVE SPAR-AREA TO MSGI-SPAR-AREA                                     
035400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
035500     .                                                                    
035600     EJECT                                                                
035700 FA-HAEMTA-GRUPPDATUM SECTION.                                            
035800                                                                          
035900     PERFORM IMS-GU-LEV                                                   
036000                                                                          
036100     IF SEGMENT-FINNS                                                     
036200       MOVE LEV-DASTADAT(3:6) TO MOD-TISTADAT-GRP (INDX)                  
036300     END-IF                                                               
036400     .                                                                    
036500     EJECT                                                                
036600 MFS-RENSA-FAELT-UT SECTION.                                              
036700                                                                          
036800*    --- ALLA UTDATA-FÄLT                                                 
036900*    --- INKL. BLÄDDRINGSNYCKLAR                                          
037000     MOVE +1 TO INDX                                                      
037100     PERFORM UNTIL INDX > MAX-INDX                                        
037200       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
037300       ADD +1 TO INDX                                                     
037400     END-PERFORM                                                          
037500     .                                                                    
037600     SKIP3                                                                
037700 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
037800                                                                          
037900*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
038000     MOVE MFS-RENSA-FAELT TO MOD-IDDIRGRP     (INDX)                      
038100                             MOD-IDLEVNR      (INDX)                      
038200                             MOD-TISTADAT-GRP (INDX)                      
038300                             MOD-TISTADAT-ART (INDX)                      
038400     .                                                                    
038500     SKIP3                                                                
038600 MFS-RENSA-FAELT-IN SECTION.                                              
038700                                                                          
038800*    --- ALLA INDATA-FÄLT                                                 
038900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
039000     .                                                                    
039100     EJECT                                                                
039200* --- IMS SEKTIONER ---                                                   
039300     SKIP3                                                                
039400 IMS-GET-MSG SECTION.                                                     
039500                                                                          
039600     MOVE '  QC' TO GODK-STATUSKODER                                      
039700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
039800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039900     PERFORM IMS-STATUSKONTROLL                                           
040000     .                                                                    
040100     SKIP3                                                                
040200 IMS-INSERT-MSG SECTION.                                                  
040300                                                                          
040400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
040500     MOVE SPACE TO GODK-STATUSKODER                                       
040600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
040700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040800     PERFORM IMS-STATUSKONTROLL                                           
040900     .                                                                    
041000     EJECT                                                                
041100 IMS-GU-SEQA SECTION.                                                     
041200                                                                          
041300     STRING 'WLLEVG01(WDF2A1KY>=' W-WDF2A1KY-MIN-X                        
041400                    '&WDF2A1KY<=' W-WDF2A1KY-MAX-X ')'                    
041500          DELIMITED BY SIZE INTO SSA1                                     
041600     MOVE '  GE' TO GODK-STATUSKODER                                      
041700     CALL CBLTDLI USING GU LEVG-PCB DLI-IO-WLLEVG01 SSA1                  
041800     MOVE LEVG-STATUS-CODE TO STATUS-WS                                   
041900     PERFORM IMS-STATUSKONTROLL                                           
042000     .                                                                    
042100     EJECT                                                                
042200 IMS-GN-SEQA SECTION.                                                     
042300                                                                          
042400     STRING 'WLLEVG01(WDF2A1KY>=' W-WDF2A1KY-MIN-X                        
042500                    '&WDF2A1KY<=' W-WDF2A1KY-MAX-X ')'                    
042600          DELIMITED BY SIZE INTO SSA1                                     
042700     MOVE '  GE' TO GODK-STATUSKODER                                      
042800     CALL CBLTDLI USING GN LEVG-PCB DLI-IO-WLLEVG01 SSA1                  
042900     MOVE LEVG-STATUS-CODE TO STATUS-WS                                   
043000     PERFORM IMS-STATUSKONTROLL                                           
043100     .                                                                    
043200     EJECT                                                                
043300 IMS-GU-LEV SECTION.                                                      
043400                                                                          
043500     STRING 'WLLEVF01(WDF201KY =' W-WDF201KY-X ')'                        
043600          DELIMITED BY SIZE INTO SSA1                                     
043700     MOVE '  GE' TO GODK-STATUSKODER                                      
043800     CALL CBLTDLI USING GU LEVF-PCB DLI-IO-WLLEVF01 SSA1                  
043900     MOVE LEVF-STATUS-CODE TO STATUS-WS                                   
044000     PERFORM IMS-STATUSKONTROLL                                           
044100     .                                                                    
044200     EJECT                                                                
044300 IMS-STATUSKONTROLL SECTION.                                              
044400                                                                          
044500     SET STATUS-IX TO 1                                                   
044600     SEARCH GODK-STATUS                                                   
044700       AT END                                                             
044800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
044900         DELIMITED BY SIZE INTO FELTEXT                                   
045000         CALL FELLOG                                                      
045100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
045200         CONTINUE                                                         
045300     END-SEARCH                                                           
045400     .                                                                    
