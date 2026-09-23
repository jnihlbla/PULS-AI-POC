000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6015700.                                                
000300 AUTHOR.         KENT JEBSEN.                                             
000400 DATE-WRITTEN.   04/09/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*        PROGRAMMET ÄR EN MPP FÖR ATT LÄSA/UPPDATERA UPPFÖLJNINGS-        
001000*        PARAMETRAR FÖR OLIKA FÖRPACKNINGSTYPER.                          
001100*        ANVÄNDS FÖR ATT STYRA BERÄKNINGAR PÅ HUR                         
001200*        MYCKET SOM FÖRPACKAS PÅ SVS.                                     
001300*        (ALLA WDGX6318-SEGMENT LÄSES NER PÅ FIL I PGM W61313).           
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WDR2 (WDGX6318).                           
001600*                                                                         
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W6T157/W6T157U                                      
002000*        MID:         W6I15701                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W6O15701                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W6015700'.            
003200 01  WS-SEC                      PIC X(4)    VALUE   '****'.              
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900                                                                          
004000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004200 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
004300 77  S-KEY                       PIC S9(2)  VALUE ZERO  COMP SYNC.        
004400 77  WS-KVGRP                    PIC S9(2)  VALUE ZERO  COMP SYNC.        
004500 77  WS-KVCMD                    PIC S9(2)  VALUE ZERO  COMP SYNC.        
004600 77  WS-KVDELETE                 PIC S9(2)  VALUE ZERO  COMP SYNC.        
004700 77  WS-CHANGE-INDX              PIC S9(2)  VALUE ZERO  COMP SYNC.        
004710 77  WS-BEFT                     PIC  9(2)  VALUE ZERO.                   
004711 77  WS-KDFPOMR                  PIC  X(3)  VALUE SPACE.                  
004720 77  WS-KDFPGRP                  PIC  X(2)  VALUE SPACE.                  
004800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004900                                                                          
005000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005100     88  INDATA-OK                           VALUE 'J'.                   
005200     88  INDATA-FEL                          VALUE 'N'.                   
005300                                                                          
005400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005500     88  NYCKLAR-OK                          VALUE 'J'.                   
005600     88  NYCKLAR-FEL                         VALUE 'N'.                   
005700                                                                          
005800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005900     88  EGEN-MID                            VALUE '6157'.                
006000     88  GODK-MID                            VALUE '6151' '6152'          
006100                                                   '6153' '6154'          
006200                                                   '6155' '6156'          
006300                                                   '6157' '6158'          
006400                                                   '6159'.                
006500     88  HELP-MID                            VALUE '0551'.                
006600     EJECT                                                                
006700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006800 01  GENERELLA-SUBPROGRAM.                                                
006900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     EJECT                                                                
007400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007500*01 -COPY WMEDAREA                                                        
007600     SKIP3                                                                
007700 01  MESSAGE-CODES.                                                       
007800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008500     EJECT                                                                
008600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008700*                                                                         
008800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008900     SKIP3                                                                
009000*01 -COPY WMSGINIT                                                        
009100     EJECT                                                                
009200*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
009300*                                                                         
009400 01  SPAR-AREA.                                                           
009500     03  SPAR-IDTRANS          PIC X(4)    VALUE '6157'.                  
009600     03  SPAR-BEFT-ENTER       PIC S9(3)        COMP-3.                   
009700     03  SPAR-BEFT-NEXT        PIC S9(3)        COMP-3.                   
009800     03  SPAR-CHANGE-INDX      PIC S9(2)        COMP-3.                   
009900     03  SPAR-S-KEY            PIC S9(2)   VALUE ZERO COMP-3.             
009910     03  SPAR-KDFPGRP          PIC  X(3).                                 
009920     03  SPAR-KDFPOMR          PIC  X(3).                                 
010000     EJECT                                                                
010100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010200*                                                                         
010300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010400     SKIP3                                                                
010500*01  MID -COPY W6I15701                                                   
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010800     SKIP3                                                                
010900*01  -COPY WMSGAREA                                                       
011000     EJECT                                                                
011100     03  MOD REDEFINES MSG-AREA.                                          
011200*      05  -COPY W6O15701                                                 
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011500     SKIP3                                                                
011600*01  -COPY WMFSAREA                                                       
011700     EJECT                                                                
011800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011900*                                                                         
012000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012100     SKIP3                                                                
012200 01  NYCKLAR-TILL-DLI.                                                    
012300*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
012400     03  W-BEFT-MIN-X.                                                    
012500         05  W-BEFT-MIN     PIC S9(3)        VALUE ZERO COMP-3.           
012600                                                                          
012700     03  W-WDGXKEY-6317-X.                                                
012800         05  W-IDHTYP            PIC X(4)    VALUE '6317'.                
012900         05  W-6317-LOWVALUE     PIC X(26)   VALUE LOW-VALUE.             
013000     03  W-BEFT-X.                                                        
013100         05  W-BEFT              PIC S9(3)   VALUE ZERO COMP-3.           
013110     03  W-FLFPOMRC-X.                                                    
013120         05  W-FLFPOMRC          PIC X       VALUE SPACE.                 
013121     03  W-FLFPOMRS-X.                                                    
013122         05  W-FLFPOMRS          PIC X       VALUE SPACE.                 
013130     03  W-FLFPGRST-X.                                                    
013140         05  W-FLFPGRST          PIC X       VALUE SPACE.                 
013150     03  W-FLFPGR28-X.                                                    
013160         05  W-FLFPGR28          PIC X       VALUE SPACE.                 
013170     03  W-FLFPGRGR-X.                                                    
013180         05  W-FLFPGRGR          PIC X       VALUE SPACE.                 
013190     03  W-FLFPGRMA-X.                                                    
013191         05  W-FLFPGRMA          PIC X       VALUE SPACE.                 
013192     03  W-FLFPGRSA-X.                                                    
013193         05  W-FLFPGRSA          PIC X       VALUE SPACE.                 
013200     SKIP2                                                                
013300*    --- STATUS-KOD FRÅN IMS                                              
013400 01  STATUS-WS                   PIC XX.                                  
013500     88  SEGMENT-FINNS                       VALUE '  '.                  
013600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013800     SKIP2                                                                
013900 01  GODK-STATUSKODER.                                                    
014000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014100     SKIP3                                                                
014200 01  SSA1                        PIC X(64).                               
014300 01  SSA2                        PIC X(64).                               
014400     EJECT                                                                
014500*    --- IMS FUNKTIONSKODER                                               
014600*01  -COPY W0003                                                          
014700     EJECT                                                                
014800*    ---  DLI INPUT-OUTPUT AREA                                           
014900                                                                          
015000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6318'.                    
015100 01  DLI-IO-WDGX6318.                                                     
015200*    03  -COPY WDGX6318                                                   
015300     EJECT                                                                
015400 LINKAGE SECTION.                                                         
015500*01  -COPY W0009   -PRE MSG-                                              
015600*01  -COPY W0008   -PRE WDP7-                                             
015700     05  FILLER                  PIC X.                                   
015800                                                                          
015900*01  -COPY W0008  -PRE 6318-                                              
016000     05  FILLER                  PIC X.                                   
016100     EJECT                                                                
016200 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB 6318-PCB.                     
016300 MAIN SECTION.                                                            
016400     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB 6318-PCB.                     
016500                                                                          
016600     PERFORM IMS-GET-MSG                                                  
016700     IF SEGMENT-FINNS                                                     
016800       PERFORM A-INIT                                                     
016900       PERFORM B-KOLLA-NYCKLAR                                            
017000       IF NYCKLAR-OK                                                      
017100         IF MFS-UPDATE                                                    
017200           PERFORM G-KOLLA-INPUT                                          
017300           IF INDATA-OK                                                   
017400             PERFORM H-UPPDATERA                                          
017500           END-IF                                                         
017600         ELSE                                                             
017700           IF MFS-FIRST                                                   
017800             PERFORM C-FOERSTA-SIDA                                       
017900           ELSE                                                           
018000             IF MFS-NEXT                                                  
018100               PERFORM D-NAESTA-SIDA                                      
018200             ELSE                                                         
018300               PERFORM E-SAMMA-SIDA                                       
018400             END-IF                                                       
018500           END-IF                                                         
018600         END-IF                                                           
018700         PERFORM F-LAES-VISA-INFO                                         
018800       END-IF                                                             
018900       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O15701 + 4                      
019000       PERFORM IMS-INSERT-MSG                                             
019100     END-IF                                                               
019200                                                                          
019300     MOVE ZERO TO RETURN-CODE                                             
019400     GOBACK                                                               
019500     .                                                                    
019600     EJECT                                                                
019700 A-INIT SECTION.                                                          
019800                                                                          
019900     MOVE 'A' TO WS-SEC                                                   
020000                                                                          
020100     IF MSG-DUBBLA-TRANSKODER                                             
020200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I15701                 
020300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
020400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
020500     ELSE                                                                 
020600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I15701                  
020700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
020800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020900     END-IF                                                               
021000                                                                          
021100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
021200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
021300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
021400                                                                          
021500     MOVE LOW-VALUE TO MSG-AREA                                           
021600     MOVE 'W6O15701' TO MFS-IDMOD                                         
021700     MOVE '6157' TO MOD-IDTRANS                                           
021800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
021900                                                                          
022000     IF EGEN-MID OR HELP-MID                                              
022100       CONTINUE                                                           
022200     ELSE                                                                 
022300       MOVE SPACE TO MFS-KDTRTYP                                          
022400       MOVE '7' TO MFS-IDPFK                                              
022500     END-IF                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 B-KOLLA-NYCKLAR SECTION.                                                 
022900                                                                          
023000     MOVE 'B'               TO WS-SEC                                     
023100                                                                          
023200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
023300     MOVE '001'             TO MSGI-KDCALL                                
023400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
023500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
023600     MOVE '6157'            TO MSGI-IDTRANS                               
023700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
023800     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
023900                                                                          
024000*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
024100     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
024200                                                                          
024300     MOVE JA TO NYCKLAR-SW                                                
024400                                                                          
024401     IF MID-BEFT-IN = ALL '+' AND                                         
024402        MID-KDFPOMR-IN = ALL '+' AND MID-KDFPGRP-IN = ALL '+'             
024403       IF MID-BEFT-UT NUMERIC                                             
024410         MOVE MID-BEFT-UT TO WS-BEFT                                      
024420       ELSE                                                               
024430         MOVE ZERO        TO WS-BEFT                                      
024440       END-IF                                                             
024441       INSPECT MID-KDFPOMR-UT REPLACING LEADING '+' BY SPACE              
024442       MOVE MID-KDFPOMR-UT TO WS-KDFPOMR                                  
024443       INSPECT MID-KDFPGRP-UT REPLACING LEADING '+' BY SPACE              
024444       MOVE MID-KDFPGRP-UT TO WS-KDFPGRP                                  
024445     ELSE                                                                 
024446       IF MID-BEFT-IN NOT = ALL '+'                                       
024447         MOVE MID-BEFT-IN TO WS-BEFT                                      
024448       END-IF                                                             
024449       IF MID-KDFPOMR-IN NOT = ALL '+'                                    
024450         MOVE MID-KDFPOMR-IN TO WS-KDFPOMR                                
024451       END-IF                                                             
024452       IF MID-KDFPGRP-IN NOT = ALL '+'                                    
024454         MOVE MID-KDFPGRP-IN TO WS-KDFPGRP                                
024455       END-IF                                                             
024456     END-IF                                                               
024470                                                                          
024500     IF WS-BEFT = ZERO   AND WS-KDFPOMR = SPACE AND                       
024600        WS-KDFPGRP = SPACE                                                
024700        MOVE 1 TO S-KEY                                                   
024800** INGEN NYCKEL ANGIVEN - ALL DATA LÄSES UT                               
024900     ELSE                                                                 
025000       IF WS-KDFPOMR = SPACE AND WS-KDFPGRP = SPACE                       
025100** BEFT ANGIVET SOM NYCKEL                                                
025200         MOVE 2 TO S-KEY                                                  
025300       ELSE                                                               
025400         IF WS-BEFT = ZERO AND WS-KDFPGRP = SPACE                         
025500** FÖRPACKNINGSOMRÅDE ANGIVET SOM NYCKEL                                  
025600           MOVE 3 TO S-KEY                                                
025700         ELSE                                                             
025800           IF WS-BEFT = ZERO AND WS-KDFPOMR = SPACE                       
025900** FÖRPACKNINGSGRUPP ANGIVET SOM NYCKEL                                   
026000             MOVE 4 TO S-KEY                                              
026100           ELSE                                                           
026200** FLER ÄN ETT NYCKELFÄLT ANGIVET, SÖKNING EJ MÖJLIG                      
026300             MOVE NEJ TO NYCKLAR-SW                                       
026400           END-IF                                                         
026500         END-IF                                                           
026600       END-IF                                                             
026700     END-IF                                                               
026800                                                                          
026900     MOVE MFS-RENSA-FAELT TO MOD-BEFT-IN                                  
027000                             MOD-KDFPOMR-IN                               
027100                             MOD-KDFPGRP-IN                               
027200                                                                          
027300     IF S-KEY = 2                                                         
027400*    -- KONTROLL AV BEFT                                                  
027500                                                                          
027600       IF WS-BEFT = ZERO                                                  
027700         MOVE '7'         TO MFS-IDPFK                                    
027800         MOVE SPACE       TO MFS-KDTRTYP                                  
027900       END-IF                                                             
028100       IF WS-BEFT NUMERIC                                                 
028200         MOVE WS-BEFT TO W-BEFT                                           
028300       ELSE                                                               
028400         MOVE NEJ TO NYCKLAR-SW                                           
028500       END-IF                                                             
028600       IF GODK-MID OR NYCKLAR-OK                                          
028700         MOVE WS-BEFT         TO MOD-BEFT-UT                              
028800       ELSE                                                               
028900         MOVE MFS-RENSA-FAELT TO MOD-BEFT-UT                              
029000       END-IF                                                             
029100     END-IF                                                               
029200                                                                          
029300     IF S-KEY = 3                                                         
029400*    -- KONTROLL AV KDFPOMR                                               
029500                                                                          
029600       IF WS-KDFPOMR = 'SVS' OR 'CDC'                                     
029700         MOVE WS-KDFPOMR TO MOD-KDFPOMR-UT                                
029800       ELSE                                                               
029900         MOVE MFS-RENSA-FAELT TO MOD-KDFPOMR-UT                           
030000         MOVE NEJ TO NYCKLAR-SW                                           
030100       END-IF                                                             
030200     END-IF                                                               
030300                                                                          
030400     IF S-KEY = 4                                                         
030500*    -- KONTROLL AV KDFPGRP                                               
030600                                                                          
030700       IF WS-KDFPGRP = 'ST' OR '28' OR 'GR' OR 'MA' OR 'SA'               
030800         MOVE WS-KDFPGRP TO MOD-KDFPGRP-UT                                
030900       ELSE                                                               
031000         MOVE MFS-RENSA-FAELT TO MOD-KDFPGRP-UT                           
031100         MOVE NEJ TO NYCKLAR-SW                                           
031200       END-IF                                                             
031300     END-IF                                                               
031400                                                                          
031500     IF NYCKLAR-FEL                                                       
031600       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
031700       CALL WMEDKONV USING MED-WMEDAREA                                   
031800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
031900       PERFORM MFS-RENSA-FAELT-IN                                         
032000       PERFORM MFS-RENSA-FAELT-UT                                         
032100                                                                          
032200       MOVE +1 TO INDX                                                    
032300       PERFORM UNTIL INDX > MAX-INDX                                      
032400         MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR(INDX)                    
032500         ADD +1 TO INDX                                                   
032600       END-PERFORM                                                        
032700     ELSE                                                                 
032800       MOVE +1 TO INDX                                                    
032900       PERFORM UNTIL INDX > MAX-INDX                                      
033000         IF MID-KDCMD(INDX) = 'B' OR MID-KDCMD(INDX) = 'C'                
033100           ADD +1 TO WS-KVCMD                                             
033200         END-IF                                                           
033300         ADD 1 TO INDX                                                    
033400       END-PERFORM                                                        
033500     END-IF                                                               
033600     .                                                                    
033700     EJECT                                                                
033800 C-FOERSTA-SIDA SECTION.                                                  
033900                                                                          
034000     MOVE 'C'               TO WS-SEC                                     
034100                                                                          
034300     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
034400     CALL WMEDKONV USING MED-WMEDAREA                                     
034500     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
034600                                                                          
034700     PERFORM MFS-RENSA-FAELT-IN                                           
034800     .                                                                    
034900     EJECT                                                                
035000 D-NAESTA-SIDA SECTION.                                                   
035100                                                                          
035200     MOVE 'D'               TO WS-SEC                                     
035300                                                                          
035500     MOVE SPAR-BEFT-NEXT TO W-BEFT-MIN                                    
035600     MOVE SPAR-S-KEY     TO S-KEY                                         
035610     MOVE SPAR-KDFPOMR   TO WS-KDFPOMR                                    
035620     MOVE SPAR-KDFPGRP   TO WS-KDFPGRP                                    
035800     PERFORM MFS-RENSA-FAELT-IN                                           
036000     .                                                                    
036100     EJECT                                                                
036200 E-SAMMA-SIDA SECTION.                                                    
036300                                                                          
036400     MOVE 'E'               TO WS-SEC                                     
036500                                                                          
036600     IF SPAR-IDTRANS = '6157' OR '0551'                                   
036900       IF MID-UPD = ALL '+' AND WS-KVCMD NOT = 1                          
037000         PERFORM MFS-RENSA-FAELT-IN                                       
037100       ELSE                                                               
037200         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
037300         CALL WMEDKONV USING MED-WMEDAREA                                 
037400         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
037500         PERFORM EA-MID-INDATA-TILL-MOD                                   
037600         MOVE SPAR-CHANGE-INDX TO WS-CHANGE-INDX                          
037700         IF SPAR-CHANGE-INDX = 1                                          
037800           MOVE MFS-STAENG-FAELT TO MOD-BEFT-UPD-ATTR                     
037900         END-IF                                                           
037910         MOVE SPAR-BEFT-ENTER TO W-BEFT-MIN                               
037920         MOVE SPAR-S-KEY      TO S-KEY                                    
037930         MOVE SPAR-KDFPOMR    TO WS-KDFPOMR                               
037940         MOVE SPAR-KDFPGRP    TO WS-KDFPGRP                               
038000       END-IF                                                             
038100     ELSE                                                                 
038200       PERFORM MFS-RENSA-FAELT-IN                                         
038300     END-IF                                                               
038400     .                                                                    
038500     EJECT                                                                
038600 EA-MID-INDATA-TILL-MOD SECTION.                                          
038700                                                                          
038800     MOVE 'EA'              TO WS-SEC                                     
038900                                                                          
039000     IF MID-BEFT-UPD NOT = ALL '+'                                        
039100       MOVE MID-BEFT-UPD TO MOD-BEFT-UPD                                  
039200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEFT-UPD-ATTR                    
039300     ELSE                                                                 
039400       MOVE MFS-RENSA-FAELT       TO MOD-BEFT-UPD                         
039500     END-IF                                                               
039600                                                                          
039700     IF MID-FLFPOMR-CDC-UPD NOT = ALL '+'                                 
039800       MOVE MID-FLFPOMR-CDC-UPD TO MOD-FLFPOMR-CDC-UPD                    
039900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLFPOMR-CDC-UPD-ATTR             
040000     ELSE                                                                 
040100       MOVE MFS-RENSA-FAELT       TO MOD-FLFPOMR-CDC-UPD                  
040200     END-IF                                                               
040300                                                                          
040400     IF MID-FLFPOMR-SVS-UPD NOT = ALL '+'                                 
040500       MOVE MID-FLFPOMR-SVS-UPD TO MOD-FLFPOMR-SVS-UPD                    
040600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLFPOMR-SVS-UPD-ATTR             
040700     ELSE                                                                 
040800       MOVE MFS-RENSA-FAELT       TO MOD-FLFPOMR-SVS-UPD                  
040900     END-IF                                                               
041000                                                                          
041100     IF MID-FLFPGRP-ST-UPD NOT = ALL '+'                                  
041200       MOVE MID-FLFPGRP-ST-UPD    TO MOD-FLFPGRP-ST-UPD                   
041300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLFPGRP-ST-UPD-ATTR              
041400     ELSE                                                                 
041500       MOVE MFS-RENSA-FAELT       TO MOD-FLFPGRP-ST-UPD                   
041600     END-IF                                                               
041700                                                                          
041800     IF MID-FLFPGRP-28-UPD NOT = ALL '+'                                  
041900       MOVE MID-FLFPGRP-28-UPD TO MOD-FLFPGRP-28-UPD                      
042000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLFPGRP-28-UPD-ATTR              
042100     ELSE                                                                 
042200       MOVE MFS-RENSA-FAELT       TO MOD-FLFPGRP-28-UPD                   
042300     END-IF                                                               
042400                                                                          
042500     IF MID-FLFPGRP-GR-UPD NOT = ALL '+'                                  
042600       MOVE MID-FLFPGRP-GR-UPD TO MOD-FLFPGRP-GR-UPD                      
042700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLFPGRP-GR-UPD-ATTR              
042800     ELSE                                                                 
042900       MOVE MFS-RENSA-FAELT       TO MOD-FLFPGRP-GR-UPD                   
043000     END-IF                                                               
043100                                                                          
043200     IF MID-FLFPGRP-MA-UPD NOT = ALL '+'                                  
043300       MOVE MID-FLFPGRP-MA-UPD TO MOD-FLFPGRP-MA-UPD                      
043400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLFPGRP-MA-UPD-ATTR              
043500     ELSE                                                                 
043600       MOVE MFS-RENSA-FAELT       TO MOD-FLFPGRP-MA-UPD                   
043700     END-IF                                                               
043800                                                                          
043900     IF MID-FLFPGRP-SA-UPD NOT = ALL '+'                                  
044000       MOVE MID-FLFPGRP-SA-UPD TO MOD-FLFPGRP-SA-UPD                      
044100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLFPGRP-SA-UPD-ATTR              
044200     ELSE                                                                 
044300       MOVE MFS-RENSA-FAELT       TO MOD-FLFPGRP-SA-UPD                   
044400     END-IF                                                               
044500                                                                          
044600     IF MID-REFPCDC-UPD NOT = ALL '+' AND MID-REFPCDC-UPD NUMERIC         
044700       MOVE MID-REFPCDC-UPD TO MOD-REFPCDC-UPD                            
044800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-REFPCDC-UPD-ATTR                 
044900     ELSE                                                                 
045000       MOVE MFS-RENSA-FAELT       TO MOD-REFPCDC-UPD                      
045100     END-IF                                                               
045200                                                                          
045300     IF MID-FLSATSIN-UPD NOT = ALL '+'                                    
045400       MOVE MID-FLSATSIN-UPD TO MOD-FLSATSIN-UPD                          
045500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLSATSIN-UPD-ATTR                
045600     ELSE                                                                 
045700       MOVE MFS-RENSA-FAELT       TO MOD-FLSATSIN-UPD                     
045800     END-IF                                                               
045900     .                                                                    
046000     EJECT                                                                
046100 F-LAES-VISA-INFO SECTION.                                                
046200                                                                          
046300     MOVE 'F'          TO WS-SEC                                          
046400                                                                          
046500     IF S-KEY = 2                                                         
046600       PERFORM FD-LAES-UNIK                                               
046700     ELSE                                                                 
046800       PERFORM FA-LAES-GRUNDDATA                                          
046900                                                                          
046910       MOVE W-BEFT-MIN TO W-BEFT                                          
047000       MOVE +1 TO INDX                                                    
047100       PERFORM FB-LAES-RADDATA                                            
047200       IF SEGMENT-FINNS                                                   
047300         MOVE 6318-BEFT  TO SPAR-BEFT-ENTER                               
047400                            SPAR-BEFT-NEXT                                
047500       ELSE                                                               
047600         MOVE W-BEFT-MIN TO SPAR-BEFT-ENTER                               
047700                            SPAR-BEFT-NEXT                                
047800       END-IF                                                             
047900                                                                          
048000       PERFORM UNTIL INDX > MAX-INDX                                      
048100         IF SEGMENT-FINNS                                                 
048200           MOVE 6318-BEFT        TO MOD-BEFT-RAD(INDX)                    
048300           MOVE 6318-FLFPOMR-CDC TO MOD-FLFPOMR-CDC-RAD(INDX)             
048400           MOVE 6318-FLFPOMR-SVS TO MOD-FLFPOMR-SVS-RAD(INDX)             
048500           MOVE 6318-FLFPGRP-ST  TO MOD-FLFPGRP-ST-RAD(INDX)              
048600           MOVE 6318-FLFPGRP-28  TO MOD-FLFPGRP-28-RAD(INDX)              
048700           MOVE 6318-FLFPGRP-GR  TO MOD-FLFPGRP-GR-RAD(INDX)              
048800           MOVE 6318-FLFPGRP-MA  TO MOD-FLFPGRP-MA-RAD(INDX)              
048900           MOVE 6318-FLFPGRP-SA  TO MOD-FLFPGRP-SA-RAD(INDX)              
049000           MOVE 6318-REFPCDC     TO MOD-REFPCDC-RAD(INDX)                 
049100           MOVE 6318-FLSATSIN    TO MOD-FLSATSIN-RAD(INDX)                
049200           PERFORM FB-LAES-RADDATA                                        
049300         ELSE                                                             
049400           MOVE MFS-RENSA-FAELT TO  MOD-KDCMD (INDX)                      
049500                                    MOD-BEFT-RAD (INDX)                   
049600                                    MOD-FLFPOMR-CDC-RAD(INDX)             
049700                                    MOD-FLFPOMR-SVS-RAD(INDX)             
049800                                    MOD-FLFPGRP-ST-RAD(INDX)              
049900                                    MOD-FLFPGRP-28-RAD(INDX)              
050000                                    MOD-FLFPGRP-GR-RAD(INDX)              
050100                                    MOD-FLFPGRP-MA-RAD(INDX)              
050200                                    MOD-FLFPGRP-SA-RAD(INDX)              
050300                                    MOD-REFPCDC-RAD(INDX)                 
050400                                    MOD-FLSATSIN-RAD(INDX)                
050500           MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR(INDX)                  
050600         END-IF                                                           
050700         ADD 1 TO INDX                                                    
050800       END-PERFORM                                                        
050900     END-IF                                                               
051000                                                                          
051100     PERFORM S01-CHECK-KDCMD                                              
051200     IF WS-CHANGE-INDX > ZERO                                             
051300       PERFORM FC-FLYTTA-TILL-UPD                                         
051400     END-IF                                                               
051500                                                                          
051600     IF SEGMENT-FINNS                                                     
051700       MOVE 6318-BEFT TO SPAR-BEFT-NEXT                                   
051710       IF S-KEY NOT = 2                                                   
051800         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
051900         CALL WMEDKONV USING MED-WMEDAREA                                 
052000         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
052010       END-IF                                                             
052100     END-IF                                                               
052200                                                                          
052300     MOVE '002'          TO MSGI-KDCALL                                   
052400     MOVE '6157'         TO SPAR-IDTRANS                                  
052500     MOVE WS-CHANGE-INDX TO SPAR-CHANGE-INDX                              
052510     MOVE S-KEY          TO SPAR-S-KEY                                    
052511     MOVE WS-KDFPOMR     TO SPAR-KDFPOMR                                  
052520     MOVE WS-KDFPGRP     TO SPAR-KDFPGRP                                  
052600     MOVE SPAR-AREA      TO MSGI-SPAR-AREA                                
052700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
052800     .                                                                    
052900     EJECT                                                                
053000 FA-LAES-GRUNDDATA SECTION.                                               
053100                                                                          
053200     MOVE 'FA'  TO WS-SEC                                                 
053300                                                                          
053400     PERFORM IMS-GET-WDGX6317                                             
053500     .                                                                    
053600     EJECT                                                                
053700 FB-LAES-RADDATA SECTION.                                                 
053800                                                                          
053900     MOVE 'FB'  TO WS-SEC                                                 
054000                                                                          
054100     IF S-KEY = 1                                                         
054110       IF INDX = 1                                                        
054112         PERFORM IMS-GET-WDGX6318                                         
054120       ELSE                                                               
054200         PERFORM IMS-GNP-WDGX6318                                         
054210       END-IF                                                             
054300     ELSE                                                                 
054400       IF S-KEY = 3                                                       
054500         IF WS-KDFPOMR = 'CDC'                                            
054600           MOVE JA TO W-FLFPOMRC                                          
054700           PERFORM IMS-GNP-WDGX6318-CDC                                   
054800         ELSE                                                             
054900           IF WS-KDFPOMR = 'SVS'                                          
055000             MOVE JA TO W-FLFPOMRS                                        
055100             PERFORM IMS-GNP-WDGX6318-SVS                                 
055200           END-IF                                                         
055300         END-IF                                                           
055400       ELSE                                                               
055500         IF S-KEY = 4                                                     
055600           IF WS-KDFPGRP = 'ST'                                           
055700             MOVE JA TO W-FLFPGRST                                        
055800             PERFORM IMS-GNP-WDGX6318-ST                                  
055900           ELSE                                                           
056000             IF WS-KDFPGRP = '28'                                         
056100               MOVE JA TO W-FLFPGR28                                      
056200               PERFORM IMS-GNP-WDGX6318-28                                
056300             ELSE                                                         
056400               IF WS-KDFPGRP = 'GR'                                       
056500                 MOVE JA TO W-FLFPGRGR                                    
056600                 PERFORM IMS-GNP-WDGX6318-GR                              
056700               ELSE                                                       
056800                 IF WS-KDFPGRP = 'MA'                                     
056900                   MOVE JA TO W-FLFPGRMA                                  
057000                   PERFORM IMS-GNP-WDGX6318-MA                            
057100                 ELSE                                                     
057200                   IF WS-KDFPGRP = 'SA'                                   
057300                     MOVE JA TO W-FLFPGRSA                                
057400                     PERFORM IMS-GNP-WDGX6318-SA                          
057500                   END-IF                                                 
057600                 END-IF                                                   
057700               END-IF                                                     
057800             END-IF                                                       
057900           END-IF                                                         
058000         END-IF                                                           
058010       END-IF                                                             
058100     END-IF                                                               
058200     .                                                                    
058300     EJECT                                                                
058400 FC-FLYTTA-TILL-UPD SECTION.                                              
058500     MOVE 'FC'              TO WS-SEC                                     
058600                                                                          
058700     MOVE WS-CHANGE-INDX            TO INDX                               
058800     MOVE MID-BEFT-RAD (INDX)       TO MOD-BEFT-UPD                       
058900     MOVE MID-FLFPOMR-CDC-RAD(INDX) TO MOD-FLFPOMR-CDC-UPD                
059000     MOVE MID-FLFPOMR-CDC-RAD(INDX) TO MOD-FLFPOMR-CDC-UPD                
059100     MOVE MID-FLFPOMR-SVS-RAD(INDX) TO MOD-FLFPOMR-SVS-UPD                
059200     MOVE MID-FLFPGRP-ST-RAD(INDX)  TO MOD-FLFPGRP-ST-UPD                 
059300     MOVE MID-FLFPGRP-28-RAD(INDX)  TO MOD-FLFPGRP-28-UPD                 
059400     MOVE MID-FLFPGRP-GR-RAD(INDX)  TO MOD-FLFPGRP-GR-UPD                 
059500     MOVE MID-FLFPGRP-MA-RAD(INDX)  TO MOD-FLFPGRP-MA-UPD                 
059600     MOVE MID-FLFPGRP-SA-RAD(INDX)  TO MOD-FLFPGRP-SA-UPD                 
059700     INSPECT MID-REFPCDC-RAD(INDX) REPLACING LEADING SPACE BY ZERO        
059800     MOVE MID-REFPCDC-RAD(INDX)     TO MOD-REFPCDC-UPD                    
059900     MOVE MID-FLSATSIN-RAD(INDX)    TO MOD-FLSATSIN-UPD                   
060000                                                                          
060100     PERFORM MFS-LAES-IN-IGEN                                             
060200     MOVE MFS-STAENG-FAELT TO MOD-BEFT-UPD-ATTR                           
060300                                                                          
060400     MOVE +1 TO INDX                                                      
060500     PERFORM UNTIL INDX > MAX-INDX                                        
060600       MOVE MFS-RENSA-FAELT TO MOD-KDCMD(INDX)                            
060700       ADD +1 TO INDX                                                     
060800     END-PERFORM                                                          
060900     .                                                                    
061000     EJECT                                                                
061100 FD-LAES-UNIK SECTION.                                                    
061200                                                                          
061300     MOVE 'FD' TO WS-SEC                                                  
061400                                                                          
061500     MOVE +1 TO INDX                                                      
061600     PERFORM IMS-GU-WDGX6318                                              
061700     IF SEGMENT-FINNS                                                     
061800       MOVE 6318-BEFT        TO SPAR-BEFT-ENTER                           
061900                                SPAR-BEFT-NEXT                            
062000                                MOD-BEFT-RAD(INDX)                        
062100       MOVE 6318-FLFPOMR-CDC TO MOD-FLFPOMR-CDC-RAD(INDX)                 
062200       MOVE 6318-FLFPOMR-SVS TO MOD-FLFPOMR-SVS-RAD(INDX)                 
062300       MOVE 6318-FLFPGRP-ST  TO MOD-FLFPGRP-ST-RAD(INDX)                  
062400       MOVE 6318-FLFPGRP-28  TO MOD-FLFPGRP-28-RAD(INDX)                  
062500       MOVE 6318-FLFPGRP-GR  TO MOD-FLFPGRP-GR-RAD(INDX)                  
062600       MOVE 6318-FLFPGRP-MA  TO MOD-FLFPGRP-MA-RAD(INDX)                  
062700       MOVE 6318-FLFPGRP-SA  TO MOD-FLFPGRP-SA-RAD(INDX)                  
062800       MOVE 6318-REFPCDC     TO MOD-REFPCDC-RAD(INDX)                     
062900       MOVE 6318-FLSATSIN    TO MOD-FLSATSIN-RAD(INDX)                    
063000     ELSE                                                                 
063100       MOVE W-BEFT-MIN       TO SPAR-BEFT-ENTER                           
063200                                SPAR-BEFT-NEXT                            
063300       MOVE MFS-RENSA-FAELT  TO MOD-KDCMD (INDX)                          
063400                                MOD-BEFT-RAD (INDX)                       
063500                                MOD-FLFPOMR-CDC-RAD(INDX)                 
063600                                MOD-FLFPOMR-SVS-RAD(INDX)                 
063700                                MOD-FLFPGRP-ST-RAD(INDX)                  
063800                                MOD-FLFPGRP-28-RAD(INDX)                  
063900                                MOD-FLFPGRP-GR-RAD(INDX)                  
064000                                MOD-FLFPGRP-MA-RAD(INDX)                  
064100                                MOD-FLFPGRP-SA-RAD(INDX)                  
064200                                MOD-REFPCDC-RAD(INDX)                     
064300                                MOD-FLSATSIN-RAD(INDX)                    
064400       MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR(INDX)                      
064500     END-IF                                                               
064600                                                                          
064700     ADD 1 TO INDX                                                        
064800     PERFORM UNTIL INDX > MAX-INDX                                        
064900       MOVE MFS-RENSA-FAELT TO  MOD-KDCMD (INDX)                          
065000                                MOD-BEFT-RAD (INDX)                       
065100                                MOD-FLFPOMR-CDC-RAD(INDX)                 
065200                                MOD-FLFPOMR-SVS-RAD(INDX)                 
065300                                MOD-FLFPGRP-ST-RAD(INDX)                  
065400                                MOD-FLFPGRP-28-RAD(INDX)                  
065500                                MOD-FLFPGRP-GR-RAD(INDX)                  
065600                                MOD-FLFPGRP-MA-RAD(INDX)                  
065700                                MOD-FLFPGRP-SA-RAD(INDX)                  
065800                                MOD-REFPCDC-RAD(INDX)                     
065900                                MOD-FLSATSIN-RAD(INDX)                    
066000       MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR(INDX)                      
066100       ADD 1 TO INDX                                                      
066200     END-PERFORM                                                          
066300     .                                                                    
066400     EJECT                                                                
066500 G-KOLLA-INPUT SECTION.                                                   
066600                                                                          
066700     MOVE 'G'  TO WS-SEC                                                  
066800                                                                          
066900     MOVE JA  TO INDATA-SW                                                
067000     IF MID-UPD = ALL '+' AND WS-KVCMD = 0                                
067100       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
067200       CALL WMEDKONV USING MED-WMEDAREA                                   
067300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
067400       PERFORM MFS-ROER-EJ-FAELT-IN                                       
067500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
067600       MOVE NEJ TO INDATA-SW                                              
067700     ELSE                                                                 
067800                                                                          
067900       PERFORM S01-CHECK-KDCMD                                            
068000       IF WS-KVDELETE = 0                                                 
068100         IF MID-BEFT-UPD NOT NUMERIC                                      
068200            MOVE MFS-NUM-FAELT-FEL TO MOD-BEFT-UPD-ATTR                   
068300            MOVE NEJ TO INDATA-SW                                         
068400         ELSE                                                             
068500            MOVE MFS-NUM-FAELT-RAETT TO MOD-BEFT-UPD-ATTR                 
068600         END-IF                                                           
068700                                                                          
068800         IF MID-FLFPOMR-CDC-UPD NOT = JA  AND                             
068900            MID-FLFPOMR-SVS-UPD NOT = JA                                  
069000           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLFPOMR-CDC-UPD-ATTR          
069100                                        MOD-FLFPOMR-SVS-UPD-ATTR          
069200           MOVE NEJ TO INDATA-SW                                          
069300         ELSE                                                             
069400           IF MID-FLFPOMR-CDC-UPD = JA        OR                          
069500              MID-FLFPOMR-CDC-UPD = ' '       OR                          
069600              MID-FLFPOMR-CDC-UPD = ALL '+'                               
069700             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLFPOMR-CDC-UPD-ATTR        
069800           ELSE                                                           
069900             MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLFPOMR-CDC-UPD-ATTR        
070000             MOVE NEJ TO INDATA-SW                                        
070100           END-IF                                                         
070200           IF MID-FLFPOMR-SVS-UPD = JA  OR                                
070300              MID-FLFPOMR-SVS-UPD = ' ' OR                                
070400              MID-FLFPOMR-SVS-UPD = ALL '+'                               
070500                                                                          
070600             MOVE ZERO TO WS-KVGRP                                        
070700             IF MID-FLFPGRP-ST-UPD = JA  OR                               
070800                MID-FLFPGRP-ST-UPD = ' ' OR                               
070900                MID-FLFPGRP-ST-UPD = ALL '+'                              
071000               MOVE MFS-ALFA-FAELT-RAETT TO                               
071100                                           MOD-FLFPGRP-ST-UPD-ATTR        
071200               IF MID-FLFPGRP-ST-UPD = JA                                 
071300                 ADD 1 TO WS-KVGRP                                        
071400               END-IF                                                     
071500             ELSE                                                         
071600               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLFPGRP-ST-UPD-ATTR         
071700               MOVE NEJ TO INDATA-SW                                      
071800             END-IF                                                       
071900             IF MID-FLFPGRP-28-UPD = JA  OR                               
072000                MID-FLFPGRP-28-UPD = ' ' OR                               
072100                MID-FLFPGRP-28-UPD = ALL '+'                              
072200               MOVE MFS-ALFA-FAELT-RAETT TO                               
072300                                           MOD-FLFPGRP-28-UPD-ATTR        
072400               IF MID-FLFPGRP-28-UPD = JA                                 
072500                 ADD 1 TO WS-KVGRP                                        
072600               END-IF                                                     
072700             ELSE                                                         
072800               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLFPGRP-28-UPD-ATTR         
072900               MOVE NEJ TO INDATA-SW                                      
073000             END-IF                                                       
073100             IF MID-FLFPGRP-GR-UPD = JA  OR                               
073200                MID-FLFPGRP-GR-UPD = ' ' OR                               
073300                MID-FLFPGRP-GR-UPD = ALL '+'                              
073400               MOVE MFS-ALFA-FAELT-RAETT TO                               
073500                                           MOD-FLFPGRP-GR-UPD-ATTR        
073600               IF MID-FLFPGRP-GR-UPD = JA                                 
073700                 ADD 1 TO WS-KVGRP                                        
073800               END-IF                                                     
073900             ELSE                                                         
074000               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLFPGRP-GR-UPD-ATTR         
074100               MOVE NEJ TO INDATA-SW                                      
074200             END-IF                                                       
074300             IF MID-FLFPGRP-MA-UPD = JA  OR                               
074400                MID-FLFPGRP-MA-UPD = ' ' OR                               
074500                MID-FLFPGRP-MA-UPD = ALL '+'                              
074600               MOVE MFS-ALFA-FAELT-RAETT TO                               
074700                                           MOD-FLFPGRP-MA-UPD-ATTR        
074800               IF MID-FLFPGRP-MA-UPD = JA                                 
074900                 ADD 1 TO WS-KVGRP                                        
075000               END-IF                                                     
075100             ELSE                                                         
075200               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLFPGRP-MA-UPD-ATTR         
075300               MOVE NEJ TO INDATA-SW                                      
075400             END-IF                                                       
075500             IF MID-FLFPGRP-SA-UPD = JA  OR                               
075600                MID-FLFPGRP-SA-UPD = ' ' OR                               
075700                MID-FLFPGRP-SA-UPD = ALL '+'                              
075800               MOVE MFS-ALFA-FAELT-RAETT TO                               
075900                                           MOD-FLFPGRP-SA-UPD-ATTR        
076000               IF MID-FLFPGRP-SA-UPD = JA                                 
076100                 ADD 1 TO WS-KVGRP                                        
076200               END-IF                                                     
076300             ELSE                                                         
076400               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLFPGRP-SA-UPD-ATTR         
076500               MOVE NEJ TO INDATA-SW                                      
076600             END-IF                                                       
076700                                                                          
076800             IF MID-FLFPOMR-SVS-UPD = JA                                  
076900** EN FÖRPACKNINGSGRUPP MÅSTE VARA ANGIVEN OM OMR SVS ÄR MARKERAT         
077000               IF WS-KVGRP = 1                                            
077100                 MOVE MFS-ALFA-FAELT-RAETT TO                             
077200                                          MOD-FLFPOMR-SVS-UPD-ATTR        
077300               ELSE                                                       
077400                 MOVE MFS-ALFA-FAELT-FEL   TO                             
077500                                          MOD-FLFPOMR-SVS-UPD-ATTR        
077600                 MOVE NEJ TO INDATA-SW                                    
077700               END-IF                                                     
077800             ELSE                                                         
077900               IF WS-KVGRP > 0                                            
078000                 MOVE MFS-ALFA-FAELT-FEL   TO                             
078100                                          MOD-FLFPOMR-SVS-UPD-ATTR        
078200                 MOVE NEJ TO INDATA-SW                                    
078300               ELSE                                                       
078400                 MOVE MFS-ALFA-FAELT-RAETT TO                             
078500                                          MOD-FLFPOMR-SVS-UPD-ATTR        
078600               END-IF                                                     
078700             END-IF                                                       
078800           ELSE                                                           
078900             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLFPOMR-SVS-UPD-ATTR          
079000             MOVE NEJ TO INDATA-SW                                        
079100           END-IF                                                         
079200         END-IF                                                           
079400         IF MID-REFPCDC-UPD = ALL '+' OR MID-REFPCDC-UPD NUMERIC          
079500            MOVE MFS-NUM-FAELT-RAETT TO MOD-REFPCDC-UPD-ATTR              
079600         ELSE                                                             
079700            MOVE MFS-NUM-FAELT-FEL   TO MOD-REFPCDC-UPD-ATTR              
079800            MOVE NEJ TO INDATA-SW                                         
079900         END-IF                                                           
080000                                                                          
080100         IF MID-FLSATSIN-UPD = ALL '+' OR MID-FLSATSIN-UPD = JA OR        
080200            MID-FLSATSIN-UPD = ' '                                        
080300            MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSATSIN-UPD-ATTR            
080400         ELSE                                                             
080500            MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLSATSIN-UPD-ATTR            
080600            MOVE NEJ TO INDATA-SW                                         
080700         END-IF                                                           
080800                                                                          
080900         IF INDATA-FEL                                                    
081000           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
081100           CALL WMEDKONV USING MED-WMEDAREA                               
081200           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
081300           PERFORM MFS-ROER-EJ-FAELT-UT                                   
081400           PERFORM MFS-ROER-EJ-FAELT-IN                                   
081500         END-IF                                                           
081600       END-IF                                                             
081700     END-IF                                                               
081800     .                                                                    
081900     EJECT                                                                
082000 H-UPPDATERA SECTION.                                                     
082100                                                                          
082200     MOVE 'H'                   TO WS-SEC                                 
082300                                                                          
082400     IF WS-KVDELETE > 0                                                   
082500       PERFORM HA-HANDLE-DELETE                                           
082600     ELSE                                                                 
082700                                                                          
082800       MOVE MID-BEFT-UPD          TO 6318-BEFT                            
082900                                     W-BEFT                               
083000                                                                          
083100       PERFORM IMS-GHU-WDGX6318                                           
083200                                                                          
083300       IF MID-FLFPOMR-CDC-UPD = JA                                        
083400         MOVE MID-FLFPOMR-CDC-UPD TO 6318-FLFPOMR-CDC                     
083500       ELSE                                                               
083600         MOVE ' '                 TO 6318-FLFPOMR-CDC                     
083700       END-IF                                                             
083800       IF MID-FLFPOMR-SVS-UPD = JA                                        
083900         MOVE MID-FLFPOMR-SVS-UPD TO 6318-FLFPOMR-SVS                     
084000       ELSE                                                               
084100         MOVE ' '                 TO 6318-FLFPOMR-SVS                     
084200       END-IF                                                             
084300       IF MID-FLFPGRP-ST-UPD = JA                                         
084400         MOVE MID-FLFPGRP-ST-UPD  TO 6318-FLFPGRP-ST                      
084500       ELSE                                                               
084600         MOVE ' '                 TO 6318-FLFPGRP-ST                      
084700       END-IF                                                             
084800       IF MID-FLFPGRP-28-UPD = JA                                         
084900         MOVE MID-FLFPGRP-28-UPD  TO 6318-FLFPGRP-28                      
085000       ELSE                                                               
085100         MOVE ' '                 TO 6318-FLFPGRP-28                      
085200       END-IF                                                             
085300       IF MID-FLFPGRP-GR-UPD = JA                                         
085400         MOVE MID-FLFPGRP-GR-UPD  TO 6318-FLFPGRP-GR                      
085500       ELSE                                                               
085600         MOVE ' '                 TO 6318-FLFPGRP-GR                      
085700       END-IF                                                             
085800       IF MID-FLFPGRP-MA-UPD = JA                                         
085900         MOVE MID-FLFPGRP-MA-UPD  TO 6318-FLFPGRP-MA                      
086000       ELSE                                                               
086100         MOVE ' '                 TO 6318-FLFPGRP-MA                      
086200       END-IF                                                             
086300       IF MID-FLFPGRP-SA-UPD = JA                                         
086400         MOVE MID-FLFPGRP-SA-UPD  TO 6318-FLFPGRP-SA                      
086500       ELSE                                                               
086600         MOVE ' '                 TO 6318-FLFPGRP-SA                      
086700       END-IF                                                             
086800       IF MID-REFPCDC-UPD NUMERIC AND MID-REFPCDC-UPD > 0                 
086900         MOVE MID-REFPCDC-UPD     TO 6318-REFPCDC                         
087000       ELSE                                                               
087100         MOVE ZERO                TO 6318-REFPCDC                         
087200       END-IF                                                             
087300       IF MID-FLSATSIN-UPD = JA                                           
087400         MOVE MID-FLSATSIN-UPD    TO 6318-FLSATSIN                        
087500       ELSE                                                               
087600         MOVE ' '                 TO 6318-FLSATSIN                        
087700       END-IF                                                             
087800                                                                          
087900       IF SEGMENT-FINNS                                                   
088000         PERFORM IMS-REPL-WDGX6318                                        
088100       ELSE                                                               
088200         PERFORM IMS-ISRT-WDGX6318                                        
088300       END-IF                                                             
088400     END-IF                                                               
088500                                                                          
088600     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
088700     CALL WMEDKONV USING MED-WMEDAREA                                     
088800     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
088900     PERFORM MFS-FORM-ATTR                                                
089000     PERFORM MFS-RENSA-FAELT-IN                                           
089100     .                                                                    
089200     EJECT                                                                
089300 HA-HANDLE-DELETE SECTION.                                                
089400     MOVE 'HA-'                    TO WS-SEC                              
089500                                                                          
089600     PERFORM VARYING INDX FROM +1 BY +1 UNTIL INDX > MAX-INDX             
089700       IF MID-KDCMD        (INDX) = 'B'                                   
089800         MOVE MID-BEFT-RAD (INDX)  TO W-BEFT                              
089900         PERFORM IMS-GHU-WDGX6318                                         
090000         IF SEGMENT-FINNS                                                 
090100           PERFORM IMS-DLET-WDGX6318                                      
090200         END-IF                                                           
090300       END-IF                                                             
090400     END-PERFORM                                                          
090500     .                                                                    
090600     EJECT                                                                
090700 S01-CHECK-KDCMD       SECTION.                                           
090800     MOVE 'S01'                    TO WS-SEC                              
090900                                                                          
091000     MOVE ZERO TO WS-KVDELETE                                             
091100                                                                          
091200     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-INDX               
091300                                                                          
091400       IF MID-KDCMD (INDX) = ALL '+'                                      
091500       OR MID-KDCMD (INDX) = SPACE                                        
091600       OR MID-KDCMD (INDX) < SPACE                                        
091700         MOVE MFS-RENSA-FAELT      TO MOD-KDCMD        (INDX)             
091800       ELSE                                                               
091900         IF  MID-KDCMD (INDX) = 'C'                                       
092000         AND WS-CHANGE-INDX = ZERO                                        
092100         AND NOT MFS-UPDATE                                               
092200           MOVE INDX               TO WS-CHANGE-INDX                      
092300           MOVE MFS-RENSA-FAELT    TO MOD-KDCMD        (INDX)             
092400         ELSE                                                             
092500           IF  MID-KDCMD (INDX) = 'B'                                     
092600             MOVE MFS-ADD-LAES-IN-FAELT                                   
092700                                   TO MOD-KDCMD-ATTR   (INDX)             
092800             ADD 1                 TO WS-KVDELETE                         
092900           ELSE                                                           
093000             MOVE MFS-ALFA-FAELT-FEL                                      
093100                                   TO MOD-KDCMD-ATTR   (INDX)             
093200             MOVE NEJ              TO INDATA-SW                           
093300           END-IF                                                         
093400         END-IF                                                           
093500       END-IF                                                             
093600     END-PERFORM                                                          
093700     .                                                                    
093800     EJECT                                                                
093900 MFS-RENSA-FAELT-UT SECTION.                                              
094000                                                                          
094100*    --- ALLA UTDATA-FÄLT                                                 
094200*    --- INKL. BLÄDDRINGSNYCKLAR                                          
094300     MOVE MFS-RENSA-FAELT TO MOD-BEFT-IN                                  
094400                                MOD-BEFT-UT                               
094500                                MOD-KDFPOMR-IN                            
094600                                MOD-KDFPOMR-UT                            
094700                                MOD-KDFPGRP-IN                            
094800                                MOD-KDFPGRP-UT                            
094900     .                                                                    
095000     SKIP3                                                                
095100 MFS-RENSA-FAELT-IN SECTION.                                              
095200                                                                          
095300*    --- ALLA INDATA-FÄLT                                                 
095400     MOVE MFS-RENSA-FAELT TO MOD-BEFT-UPD                                 
095500                             MOD-FLFPOMR-CDC-UPD                          
095600                             MOD-FLFPOMR-SVS-UPD                          
095700                             MOD-FLFPGRP-ST-UPD                           
095800                             MOD-FLFPGRP-28-UPD                           
095900                             MOD-FLFPGRP-GR-UPD                           
096000                             MOD-FLFPGRP-MA-UPD                           
096100                             MOD-FLFPGRP-SA-UPD                           
096200                             MOD-REFPCDC-UPD                              
096300                             MOD-FLSATSIN-UPD                             
096400     MOVE +1 TO INDX                                                      
096500     PERFORM UNTIL INDX > MAX-INDX                                        
096600       MOVE MFS-RENSA-FAELT TO MOD-KDCMD(INDX)                            
096700       ADD +1 TO INDX                                                     
096800     END-PERFORM                                                          
096900     .                                                                    
097000     EJECT                                                                
097100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
097200                                                                          
097300*    --- ALLA UTDATA-FÄLT                                                 
097400*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
097500     MOVE MFS-ROER-EJ-FAELT TO MOD-BEFT-UT                                
097600                                 MOD-KDFPOMR-UT                           
097700                                 MOD-KDFPGRP-UT                           
097800     MOVE +1 TO INDX                                                      
097900     PERFORM UNTIL INDX > MAX-INDX                                        
098000       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
098100       ADD +1 TO INDX                                                     
098200     END-PERFORM                                                          
098300     .                                                                    
098400     SKIP2                                                                
098500 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
098600                                                                          
098700*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
098800     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD(INDX)                            
098900                             MOD-BEFT-RAD (INDX)                          
099000                             MOD-FLFPOMR-CDC-RAD(INDX)                    
099100                             MOD-FLFPOMR-SVS-RAD(INDX)                    
099200                             MOD-FLFPGRP-ST-RAD(INDX)                     
099300                             MOD-FLFPGRP-28-RAD(INDX)                     
099400                             MOD-FLFPGRP-GR-RAD(INDX)                     
099500                             MOD-FLFPGRP-MA-RAD(INDX)                     
099600                             MOD-FLFPGRP-SA-RAD(INDX)                     
099700                             MOD-REFPCDC-RAD(INDX)                        
099800                             MOD-FLSATSIN-RAD(INDX)                       
099900     .                                                                    
100000     SKIP3                                                                
100100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
100200                                                                          
100300*    --- ALLA INDATA-FÄLT                                                 
100400     MOVE MFS-ROER-EJ-FAELT TO MOD-BEFT-UPD                               
100500                             MOD-FLFPOMR-CDC-UPD                          
100600                             MOD-FLFPOMR-SVS-UPD                          
100700                             MOD-FLFPGRP-ST-UPD                           
100800                             MOD-FLFPGRP-28-UPD                           
100900                             MOD-FLFPGRP-GR-UPD                           
101000                             MOD-FLFPGRP-MA-UPD                           
101100                             MOD-FLFPGRP-SA-UPD                           
101200                             MOD-REFPCDC-UPD                              
101300                             MOD-FLSATSIN-UPD                             
101400     .                                                                    
101500     EJECT                                                                
101600 MFS-FORM-ATTR SECTION.                                                   
101700                                                                          
101800*    --- ALLA INDATA-FÄLT                                                 
101900     MOVE MFS-FORMATETS-ATTR TO MOD-BEFT-UPD-ATTR                         
102000                                MOD-FLFPOMR-CDC-UPD-ATTR                  
102100                                MOD-FLFPOMR-SVS-UPD-ATTR                  
102200                                MOD-FLFPGRP-ST-UPD-ATTR                   
102300                                MOD-FLFPGRP-28-UPD-ATTR                   
102400                                MOD-FLFPGRP-GR-UPD-ATTR                   
102500                                MOD-FLFPGRP-MA-UPD-ATTR                   
102600                                MOD-FLFPGRP-SA-UPD-ATTR                   
102700                                MOD-REFPCDC-UPD-ATTR                      
102800                                MOD-FLSATSIN-UPD-ATTR                     
102900     MOVE +1 TO INDX                                                      
103000     PERFORM UNTIL INDX > MAX-INDX                                        
103100       MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-ATTR(INDX)                    
103200       ADD +1 TO INDX                                                     
103300     END-PERFORM                                                          
103400     .                                                                    
103500     SKIP2                                                                
103600 MFS-LAES-IN-IGEN SECTION.                                                
103700                                                                          
103800*    --- ALLA INDATA-FÄLT                                                 
103900     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEFT-UPD-ATTR                      
104000                                MOD-FLFPOMR-CDC-UPD-ATTR                  
104100                                MOD-FLFPOMR-SVS-UPD-ATTR                  
104200                                MOD-FLFPGRP-ST-UPD-ATTR                   
104300                                MOD-FLFPGRP-28-UPD-ATTR                   
104400                                MOD-FLFPGRP-GR-UPD-ATTR                   
104500                                MOD-FLFPGRP-MA-UPD-ATTR                   
104600                                MOD-FLFPGRP-SA-UPD-ATTR                   
104700                                MOD-REFPCDC-UPD-ATTR                      
104800                                MOD-FLSATSIN-UPD-ATTR                     
104900     .                                                                    
105000     EJECT                                                                
105100* --- IMS SEKTIONER ---                                                   
105200     SKIP3                                                                
105300 IMS-GET-MSG SECTION.                                                     
105400                                                                          
105500     MOVE '  QC' TO GODK-STATUSKODER                                      
105600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
105700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
105800     PERFORM IMS-STATUSKONTROLL                                           
105900     .                                                                    
106000     SKIP3                                                                
106100 IMS-INSERT-MSG SECTION.                                                  
106200                                                                          
106300     IF MSGI-IDLAND-SPR = 'SE'                                            
106400       MOVE '0' TO MFS-KDHUVOMR                                           
106500     END-IF                                                               
106600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
106700     MOVE SPACE TO GODK-STATUSKODER                                       
106800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
106900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
107000     PERFORM IMS-STATUSKONTROLL                                           
107100     .                                                                    
107200     EJECT                                                                
107300 IMS-GET-WDGX6317 SECTION.                                                
107400                                                                          
107500     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6317-X ')'                    
107600          DELIMITED BY SIZE INTO SSA1                                     
107700     MOVE '  ' TO GODK-STATUSKODER                                        
107800     CALL CBLTDLI USING GU 6318-PCB DLI-IO-WDGX6318 SSA1                  
107900     MOVE 6318-STATUS-CODE TO STATUS-WS                                   
108000     PERFORM IMS-STATUSKONTROLL                                           
108100     .                                                                    
108200     EJECT                                                                
108300 IMS-GU-WDGX6318 SECTION.                                                 
108400                                                                          
108500     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6317-X ')'                    
108600          DELIMITED BY SIZE INTO SSA1                                     
108700     STRING 'WDGX6318(BEFT     =' W-BEFT-X ')'                            
108800          DELIMITED BY SIZE INTO SSA2                                     
108900     MOVE '  GE' TO GODK-STATUSKODER                                      
109000     CALL CBLTDLI USING GU 6318-PCB DLI-IO-WDGX6318 SSA1 SSA2             
109100     MOVE 6318-STATUS-CODE TO STATUS-WS                                   
109200     PERFORM IMS-STATUSKONTROLL                                           
109300     .                                                                    
109400     EJECT                                                                
109500 IMS-GHU-WDGX6318 SECTION.                                                
109600                                                                          
109700     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6317-X ')'                    
109800          DELIMITED BY SIZE INTO SSA1                                     
109900     STRING 'WDGX6318(BEFT     =' W-BEFT-X ')'                            
110000          DELIMITED BY SIZE INTO SSA2                                     
110100     MOVE '  GE' TO GODK-STATUSKODER                                      
110200     CALL CBLTDLI USING GHU 6318-PCB DLI-IO-WDGX6318 SSA1 SSA2            
110300     MOVE 6318-STATUS-CODE TO STATUS-WS                                   
110400     PERFORM IMS-STATUSKONTROLL                                           
110500     .                                                                    
110600     EJECT                                                                
110700 IMS-GET-WDGX6318 SECTION.                                                
110800                                                                          
110810     STRING 'WDGX6318(BEFT    >=' W-BEFT-X ')'                            
110820          DELIMITED BY SIZE INTO SSA1                                     
111000     MOVE '  GE' TO GODK-STATUSKODER                                      
111100     CALL CBLTDLI USING GNP 6318-PCB DLI-IO-WDGX6318 SSA1                 
111200     MOVE 6318-STATUS-CODE TO STATUS-WS                                   
111300     PERFORM IMS-STATUSKONTROLL                                           
111400     .                                                                    
111500     SKIP3                                                                
111501 IMS-GNP-WDGX6318 SECTION.                                                
111502                                                                          
111503     MOVE 'WDGX6318' TO SSA1                                              
111504     MOVE '  GE' TO GODK-STATUSKODER                                      
111505     CALL CBLTDLI USING GNP 6318-PCB DLI-IO-WDGX6318 SSA1                 
111506     MOVE 6318-STATUS-CODE TO STATUS-WS                                   
111507     PERFORM IMS-STATUSKONTROLL                                           
111508     .                                                                    
111509     SKIP3                                                                
111510 IMS-GNP-WDGX6318-CDC SECTION.                                            
111520                                                                          
111522     STRING 'WDGX6318(BEFT    >=' W-BEFT-X                                
111524                      '&FLFPOMRC =' W-FLFPOMRC-X ')'                      
111526          DELIMITED BY SIZE INTO SSA1                                     
111540     MOVE '  GE' TO GODK-STATUSKODER                                      
111550     CALL CBLTDLI USING GNP 6318-PCB DLI-IO-WDGX6318 SSA1                 
111560     MOVE 6318-STATUS-CODE TO STATUS-WS                                   
111570     PERFORM IMS-STATUSKONTROLL                                           
111580     .                                                                    
111590     SKIP3                                                                
111591 IMS-GNP-WDGX6318-SVS SECTION.                                            
111594                                                                          
111597     STRING 'WDGX6318(BEFT    >=' W-BEFT-X                                
111598                      '&FLFPOMRS =' W-FLFPOMRS-X ')'                      
111599          DELIMITED BY SIZE INTO SSA1                                     
111600     MOVE '  GE' TO GODK-STATUSKODER                                      
111601     CALL CBLTDLI USING GNP 6318-PCB DLI-IO-WDGX6318 SSA1                 
111602     MOVE 6318-STATUS-CODE TO STATUS-WS                                   
111603     PERFORM IMS-STATUSKONTROLL                                           
111604     .                                                                    
111605     SKIP3                                                                
111606 IMS-GNP-WDGX6318-ST SECTION.                                             
111607                                                                          
111610     STRING 'WDGX6318(BEFT    >=' W-BEFT-X                                
111611                      '&FLFPGRST =' W-FLFPGRST-X ')'                      
111612          DELIMITED BY SIZE INTO SSA1                                     
111613     MOVE '  GE' TO GODK-STATUSKODER                                      
111614     CALL CBLTDLI USING GNP 6318-PCB DLI-IO-WDGX6318 SSA1                 
111615     MOVE 6318-STATUS-CODE TO STATUS-WS                                   
111616     PERFORM IMS-STATUSKONTROLL                                           
111617     .                                                                    
111618     SKIP3                                                                
111619 IMS-GNP-WDGX6318-28 SECTION.                                             
111620                                                                          
111623     STRING 'WDGX6318(BEFT    >=' W-BEFT-X                                
111624                      '&FLFPGR28 =' W-FLFPGR28-X ')'                      
111625          DELIMITED BY SIZE INTO SSA1                                     
111626     MOVE '  GE' TO GODK-STATUSKODER                                      
111627     CALL CBLTDLI USING GNP 6318-PCB DLI-IO-WDGX6318 SSA1                 
111628     MOVE 6318-STATUS-CODE TO STATUS-WS                                   
111629     PERFORM IMS-STATUSKONTROLL                                           
111630     .                                                                    
111631     SKIP3                                                                
111632 IMS-GNP-WDGX6318-GR SECTION.                                             
111633                                                                          
111636     STRING 'WDGX6318(BEFT    >=' W-BEFT-X                                
111637                      '&FLFPGRGR =' W-FLFPGRGR-X ')'                      
111638          DELIMITED BY SIZE INTO SSA1                                     
111639     MOVE '  GE' TO GODK-STATUSKODER                                      
111640     CALL CBLTDLI USING GNP 6318-PCB DLI-IO-WDGX6318 SSA1                 
111641     MOVE 6318-STATUS-CODE TO STATUS-WS                                   
111642     PERFORM IMS-STATUSKONTROLL                                           
111643     .                                                                    
111644     SKIP3                                                                
111645 IMS-GNP-WDGX6318-MA SECTION.                                             
111646                                                                          
111649     STRING 'WDGX6318(BEFT    >=' W-BEFT-X                                
111650                      '&FLFPGRMA =' W-FLFPGRMA-X ')'                      
111651          DELIMITED BY SIZE INTO SSA1                                     
111652     MOVE '  GE' TO GODK-STATUSKODER                                      
111653     CALL CBLTDLI USING GNP 6318-PCB DLI-IO-WDGX6318 SSA1                 
111654     MOVE 6318-STATUS-CODE TO STATUS-WS                                   
111655     PERFORM IMS-STATUSKONTROLL                                           
111656     .                                                                    
111657     SKIP3                                                                
111658 IMS-GNP-WDGX6318-SA SECTION.                                             
111659                                                                          
111662     STRING 'WDGX6318(BEFT    >=' W-BEFT-X                                
111663                      '&FLFPGRSA =' W-FLFPGRSA-X ')'                      
111664          DELIMITED BY SIZE INTO SSA1                                     
111665     MOVE '  GE' TO GODK-STATUSKODER                                      
111666     CALL CBLTDLI USING GNP 6318-PCB DLI-IO-WDGX6318 SSA1                 
111667     MOVE 6318-STATUS-CODE TO STATUS-WS                                   
111668     PERFORM IMS-STATUSKONTROLL                                           
111669     .                                                                    
111670     SKIP3                                                                
111680 IMS-ISRT-WDGX6318 SECTION.                                               
111700                                                                          
111800     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6317-X ')'                    
111900          DELIMITED BY SIZE INTO SSA1                                     
112000     MOVE 'WDGX6318 ' TO SSA2                                             
112100     MOVE '  ' TO GODK-STATUSKODER                                        
112200     CALL CBLTDLI USING ISRT 6318-PCB DLI-IO-WDGX6318 SSA1 SSA2           
112300     MOVE 6318-STATUS-CODE TO STATUS-WS                                   
112400     PERFORM IMS-STATUSKONTROLL                                           
112500     .                                                                    
112600     SKIP3                                                                
112700 IMS-REPL-WDGX6318 SECTION.                                               
112800                                                                          
112900     MOVE '  ' TO GODK-STATUSKODER                                        
113000     CALL CBLTDLI USING REPL 6318-PCB DLI-IO-WDGX6318                     
113100     MOVE 6318-STATUS-CODE TO STATUS-WS                                   
113200     PERFORM IMS-STATUSKONTROLL                                           
113300     .                                                                    
113400     SKIP3                                                                
113500 IMS-DLET-WDGX6318 SECTION.                                               
113600                                                                          
113700     MOVE '  ' TO GODK-STATUSKODER                                        
113800     CALL CBLTDLI USING DLET 6318-PCB DLI-IO-WDGX6318                     
113900     MOVE 6318-STATUS-CODE TO STATUS-WS                                   
114000     PERFORM IMS-STATUSKONTROLL                                           
114100     .                                                                    
114200     EJECT                                                                
114300 IMS-STATUSKONTROLL SECTION.                                              
114400                                                                          
114500     SET STATUS-IX TO 1                                                   
114600     SEARCH GODK-STATUS                                                   
114700       AT END                                                             
114800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
114900         DELIMITED BY SIZE INTO FELTEXT                                   
115000         CALL FELLOG                                                      
115100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
115200         CONTINUE                                                         
115300     END-SEARCH                                                           
115400     .                                                                    
