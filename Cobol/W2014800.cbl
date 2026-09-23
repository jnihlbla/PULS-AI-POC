000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2014800.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   10/01/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERAR PARAMETRAR FÖR AUTOMATSKROT                           
000810*                   AV 01-MÄRKTA ARTIKLAR                                 
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDR2                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W2T148                                              
001400*        MID:         W2I14801                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W2O14801                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W2014800'.            
002600                                                                          
002700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002900                                                                          
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003200                                                                          
003300*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003500 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
003510 77  CMD-IX                      PIC S9(4)  VALUE ZERO  COMP SYNC.        
003600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003700                                                                          
003800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
003900     88  INDATA-OK                           VALUE 'J'.                   
004000     88  INDATA-FEL                          VALUE 'N'.                   
004100                                                                          
004200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004300     88  NYCKLAR-OK                          VALUE 'J'.                   
004400     88  NYCKLAR-FEL                         VALUE 'N'.                   
004500                                                                          
004600 77  SW-CMD                      PIC X       VALUE 'N'.                   
004700 77  SW-RAD                      PIC X       VALUE 'N'.                   
004710 77  SW-KOPI                     PIC X       VALUE 'N'.                   
004800                                                                          
004900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005000     88  EGEN-MID                            VALUE '2148'.                
005100     88  GODK-MID                            VALUE '2141' '2142'          
005200                                                   '2143' '2144'          
005300                                                   '2145' '2146'          
005400                                                   '2147' '2148'          
005500                                                   '2149'.                
005600     88  HELP-MID                            VALUE '0551'.                
005610                                                                          
005620 01  WS-CMD                      PIC X       VALUE 'N'.                   
005630 01  ANT-K                       PIC 9(2)    VALUE ZERO.                  
005700     EJECT                                                                
005800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005900 01  GENERELLA-SUBPROGRAM.                                                
006000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400     EJECT                                                                
006500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006600*01 -COPY WMEDAREA                                                        
006700     SKIP3                                                                
006800 01  MESSAGE-CODES.                                                       
006900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007600     EJECT                                                                
007700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007800*                                                                         
007900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008000     SKIP3                                                                
008100*01 -COPY WMSGINIT                                                        
008200     EJECT                                                                
008300*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008400*                                                                         
008500 01  SPAR-AREA.                                                           
008600     03  SPAR-IDTRANS              PIC X(4)    VALUE '2148'.              
008700     03  SPAR-KDPRODSL-ENTER       PIC S9(3)        COMP-3.               
008800     03  SPAR-KDPRODSL-NEXT        PIC S9(3)        COMP-3.               
008900     EJECT                                                                
009000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009100*                                                                         
009200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009300     SKIP3                                                                
009400*01  MID -COPY W2I14801                                                   
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009700     SKIP3                                                                
009800*01  -COPY WMSGAREA                                                       
009900     EJECT                                                                
010000     03  MOD REDEFINES MSG-AREA.                                          
010100*      05  -COPY W2O14801                                                 
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010400     SKIP3                                                                
010500*01  -COPY WMFSAREA                                                       
010600     EJECT                                                                
010700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010800*                                                                         
010900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011000     SKIP3                                                                
011100 01  NYCKLAR-TILL-DLI.                                                    
011200*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
011300     03  W-KDPRODSL-MIN-X.                                                
011400         05  W-KDPRODSL-MIN     PIC S9(3)    VALUE ZERO  COMP-3.          
011500                                                                          
011600     03  W-WDGXKEY-X.                                                     
011700         05  W-IDHTYP            PIC X(04)    VALUE '2243'.               
011800         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
011900     03  W-KDPRODSL-X.                                                    
012000         05  W-KDPRODSL          PIC S9(3)   VALUE ZERO COMP-3.           
012100     SKIP2                                                                
012200*    --- STATUS KODER FRÅN IMS                                            
012300 01  STATUS-WS                   PIC XX.                                  
012400     88  SEGMENT-FINNS                       VALUE '  '.                  
012500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012700     SKIP2                                                                
012800 01  GODK-STATUSKODER.                                                    
012900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013000     SKIP3                                                                
013100 01  SSA1                        PIC X(64).                               
013200 01  SSA2                        PIC X(64).                               
013300     EJECT                                                                
013400*    --- IMS FUNKTIONSKODER                                               
013500*01  -COPY W0003                                                          
013600     EJECT                                                                
013700*    ---  DLI INPUT-OUTPUT AREA                                           
013800                                                                          
013900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR2'.                        
014000 01  DLI-IO-WDR201.                                                       
014100*    03  -COPY WDGX01                                                     
014200     EJECT                                                                
014300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2244'.                    
014400 01  DLI-IO-WDGX2244.                                                     
014500*    03  -COPY WDGX2244                                                   
014600     EJECT                                                                
014700 LINKAGE SECTION.                                                         
014800*01  -COPY W0009   -PRE MSG-                                              
014900*01  -COPY W0008   -PRE WDP7-                                             
015000     05  FILLER                  PIC X.                                   
015100                                                                          
015200*01  -COPY W0008  -PRE WDR2-                                              
015300     05  FILLER                  PIC X.                                   
015400     EJECT                                                                
015500 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDR2-PCB.                     
015600 MAIN SECTION.                                                            
015700     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDR2-PCB.                     
015800                                                                          
015900     PERFORM IMS-GET-MSG                                                  
016000     IF SEGMENT-FINNS                                                     
016100       PERFORM A-INIT                                                     
016200       PERFORM B-KOLLA-NYCKLAR                                            
016300       IF NYCKLAR-OK                                                      
016400         IF MFS-UPDATE                                                    
016500           PERFORM G-KOLLA-INPUT                                          
016600           IF INDATA-OK                                                   
016700             PERFORM H-UPPDATERA                                          
016800           END-IF                                                         
016900         ELSE                                                             
017000           IF MFS-FIRST                                                   
017100             PERFORM C-FOERSTA-SIDA                                       
017200           ELSE                                                           
017300             IF MFS-NEXT                                                  
017400               PERFORM D-NAESTA-SIDA                                      
017500             ELSE                                                         
017600               PERFORM E-SAMMA-SIDA                                       
017700             END-IF                                                       
017800           END-IF                                                         
017900         END-IF                                                           
018000         PERFORM F-LAES-VISA-INFO                                         
018100       END-IF                                                             
018200       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O14801 + 4                      
018300       PERFORM IMS-INSERT-MSG                                             
018400     END-IF                                                               
018500                                                                          
018600     MOVE ZERO TO RETURN-CODE                                             
018610*FIX                                                                      
018620*        CALL FELLOG                                                      
018700     GOBACK                                                               
018800     .                                                                    
018900     EJECT                                                                
019000 A-INIT SECTION.                                                          
019100                                                                          
019200     IF MSG-DUBBLA-TRANSKODER                                             
019300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I14801                 
019400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
019500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019600     ELSE                                                                 
019700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I14801                  
019800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
019900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020000     END-IF                                                               
020100                                                                          
020200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
020300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
020400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
020500                                                                          
020600     MOVE LOW-VALUE TO MSG-AREA                                           
020700     MOVE 'W2O14801' TO MFS-IDMOD                                         
020800     MOVE '2148' TO MOD-IDTRANS                                           
020900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
021000                                                                          
021100     IF EGEN-MID OR HELP-MID                                              
021200       CONTINUE                                                           
021300     ELSE                                                                 
021400       MOVE SPACE TO MFS-KDTRTYP                                          
021500       MOVE '7' TO MFS-IDPFK                                              
021600     END-IF                                                               
021700     .                                                                    
021800     EJECT                                                                
021900 B-KOLLA-NYCKLAR SECTION.                                                 
022000                                                                          
022100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
022200     MOVE '001'             TO MSGI-KDCALL                                
022300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
022400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
022500     MOVE '2148'            TO MSGI-IDTRANS                               
022900     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
023000     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
023010                                                                          
023020     IF SPAR-KDPRODSL-ENTER NOT NUMERIC                                   
023030        MOVE ZERO TO SPAR-KDPRODSL-ENTER                                  
023040     END-IF                                                               
023050     IF SPAR-KDPRODSL-NEXT  NOT NUMERIC                                   
023060        MOVE ZERO TO SPAR-KDPRODSL-NEXT                                   
023070     END-IF                                                               
023100                                                                          
023200*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
023300     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
023400                                                                          
023500     MOVE JA TO NYCKLAR-SW                                                
023600                                                                          
023800     IF NYCKLAR-FEL                                                       
023900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
024000       CALL WMEDKONV USING MED-WMEDAREA                                   
024100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024200       PERFORM MFS-RENSA-FAELT-IN                                         
024300       PERFORM MFS-RENSA-FAELT-UT                                         
024400     END-IF                                                               
024500     .                                                                    
024600     EJECT                                                                
024700 C-FOERSTA-SIDA SECTION.                                                  
024800                                                                          
024900     MOVE INF-FIRST-PAGE   TO MED-IDMFSINF                                
025000     CALL WMEDKONV USING MED-WMEDAREA                                     
025100     MOVE MED-MFSINF       TO MOD-TEMFSFEL                                
025200                                                                          
025300     PERFORM MFS-RENSA-FAELT-IN                                           
025400     .                                                                    
025500     EJECT                                                                
025600 D-NAESTA-SIDA SECTION.                                                   
025700                                                                          
025800     IF SPAR-IDTRANS = '2148'                                             
025900       MOVE SPAR-KDPRODSL-NEXT TO W-KDPRODSL-MIN                          
026000     ELSE                                                                 
026100       PERFORM MFS-RENSA-FAELT-IN                                         
026110       MOVE ZERO               TO W-KDPRODSL-MIN                          
026200     END-IF                                                               
026210                                                                          
026220     PERFORM MFS-RENSA-FAELT-IN                                           
026300     .                                                                    
026400     EJECT                                                                
026500 E-SAMMA-SIDA SECTION.                                                    
026600                                                                          
026700     IF SPAR-IDTRANS = '2148' OR '0551'                                   
026800       MOVE SPAR-KDPRODSL-ENTER TO W-KDPRODSL-MIN                         
026910       IF MID-RAD-CMD (1) = '+' AND MID-RAD-CMD (2) =  '+' AND            
026920          MID-RAD-CMD (3) = '+' AND MID-RAD-CMD (4) =  '+' AND            
026930          MID-RAD-CMD (5) = '+' AND MID-RAD-CMD (6) =  '+' AND            
026940          MID-RAD-CMD (7) = '+' AND MID-RAD-CMD (8) =  '+' AND            
026950          MID-RAD-CMD (9) = '+' AND MID-RAD-CMD (10) = '+' AND            
026960          MID-KDPRODSL         = ALL '+' AND                              
026970          MID-KVPERIOD-BERS    = ALL '+' AND                              
026980          MID-KVPERIOD-BTILLK  = ALL '+' AND                              
026990          MID-KVPERIOD-VERS    = ALL '+' AND                              
026991          MID-KVPERIOD-HERS    = ALL '+'                                  
027000         PERFORM MFS-RENSA-FAELT-IN                                       
027100       ELSE                                                               
027101*       *ÄR EN (OCH ENDAST EN) KOPIERING ÖNSKAD ?                         
027102         MOVE ZERO                          TO ANT-K                      
027103         MOVE ZERO                          TO CMD-IX                     
027104         MOVE +1                            TO INDX                       
027105         PERFORM UNTIL INDX > MAX-INDX                                    
027106            IF MID-RAD-CMD (INDX) = 'C' OR 'K'                            
027107               MOVE INDX                    TO CMD-IX                     
027108               ADD  +1                      TO ANT-K                      
027109            ELSE                                                          
027110               IF MID-RAD-CMD (INDX) NOT = '+'                            
027111                  ADD  +1                   TO ANT-K                      
027112               END-IF                                                     
027113            END-IF                                                        
027114            ADD +1                          TO INDX                       
027115         END-PERFORM                                                      
027116         IF CMD-IX > ZERO AND ANT-K = 1                                   
027121*          * KOPIERA TILL UPPDATERINGSRADEN                               
027122           MOVE MID-RAD-KDPRODSL(CMD-IX) TO W-KDPRODSL                    
027123           PERFORM IMS-GHU-WDGX2244-KVAL                                  
027124           IF SEGMENT-FINNS                                               
027125              MOVE 2244-KDPRODSL         TO MOD-KDPRODSL-IN               
027126              MOVE 2244-KVPERIOD-BERS    TO MOD-KVPERIOD-BERS-IN          
027127              MOVE 2244-KVPERIOD-BTILLK  TO MOD-KVPERIOD-BTILLK-IN        
027128              MOVE 2244-KVPERIOD-VERS    TO MOD-KVPERIOD-VERS-IN          
027129              MOVE 2244-KVPERIOD-HERS    TO MOD-KVPERIOD-HERS-IN          
027130              MOVE JA                    TO SW-KOPI                       
027131           END-IF                                                         
027132         ELSE                                                             
027140*          *INDATA MEN EJ PF11                                            
027200            MOVE INF-PRESS-PF11 TO MED-IDMFSINF                           
027300            CALL WMEDKONV USING MED-WMEDAREA                              
027400            MOVE MED-MFSINF TO MOD-TEMFSFEL                               
027500            PERFORM EA-MID-INDATA-TILL-MOD                                
027510         END-IF                                                           
027600       END-IF                                                             
027700     ELSE                                                                 
027800       PERFORM MFS-RENSA-FAELT-IN                                         
027900     END-IF                                                               
028000     .                                                                    
028100     EJECT                                                                
028200 EA-MID-INDATA-TILL-MOD SECTION.                                          
028300                                                                          
028400* * * * * FÖR VARJE MID-FÄLT                                              
028500* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
028600* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
028700* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
028800                                                                          
028900     MOVE +1                            TO INDX                           
029000     PERFORM UNTIL INDX > MAX-INDX                                        
029100        IF MID-RAD-CMD (INDX) NOT = ALL '+'                               
029200           MOVE MID-RAD-CMD (INDX)      TO MOD-RAD-CMD      (INDX)        
029201           MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-RAD-CMD-ATTR (INDX)        
029400        ELSE                                                              
029500           MOVE MFS-RENSA-FAELT         TO MOD-RAD-CMD      (INDX)        
029600        END-IF                                                            
029700        ADD +1                          TO INDX                           
029800     END-PERFORM                                                          
029900                                                                          
029910       INSPECT MID-KDPRODSL        REPLACING LEADING SPACE BY ZERO        
029920       INSPECT MID-KVPERIOD-BERS   REPLACING LEADING SPACE BY ZERO        
029930       INSPECT MID-KVPERIOD-BTILLK REPLACING LEADING SPACE BY ZERO        
029940       INSPECT MID-KVPERIOD-VERS   REPLACING LEADING SPACE BY ZERO        
029950       INSPECT MID-KVPERIOD-HERS   REPLACING LEADING SPACE BY ZERO        
030010     IF MID-KDPRODSL NUMERIC AND                                          
030020        MID-KDPRODSL NOT = ZERO                                           
030100        MOVE MID-KDPRODSL          TO MOD-KDPRODSL-IN                     
030200        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRODSL-IN-ATTR                
030300     ELSE                                                                 
030400       MOVE MFS-RENSA-FAELT        TO MOD-KDPRODSL-IN                     
030500     END-IF                                                               
030610     IF MID-KVPERIOD-BERS NUMERIC AND                                     
030620        MID-KVPERIOD-BERS NOT = ZERO                                      
030700        MOVE MID-KVPERIOD-BERS     TO MOD-KVPERIOD-BERS-IN                
030800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVPERIOD-BERS-IN-ATTR           
030900     ELSE                                                                 
031000       MOVE MFS-RENSA-FAELT        TO MOD-KVPERIOD-BERS-IN                
031100     END-IF                                                               
031210     IF MID-KVPERIOD-BTILLK NUMERIC AND                                   
031220        MID-KVPERIOD-BTILLK NOT = ZERO                                    
031300        MOVE MID-KVPERIOD-BTILLK   TO MOD-KVPERIOD-BTILLK-IN              
031400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVPERIOD-BTILLK-IN-ATTR         
031500     ELSE                                                                 
031600       MOVE MFS-RENSA-FAELT        TO MOD-KVPERIOD-BTILLK-IN              
031700     END-IF                                                               
031810     IF MID-KVPERIOD-VERS NUMERIC AND                                     
031820        MID-KVPERIOD-VERS NOT = ZERO                                      
031900        MOVE MID-KVPERIOD-VERS     TO MOD-KVPERIOD-VERS-IN                
032000        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVPERIOD-VERS-IN-ATTR           
032100     ELSE                                                                 
032200       MOVE MFS-RENSA-FAELT        TO MOD-KVPERIOD-VERS-IN                
032300     END-IF                                                               
032410     IF MID-KVPERIOD-HERS NUMERIC AND                                     
032420        MID-KVPERIOD-HERS NOT = ZERO                                      
032500        MOVE MID-KVPERIOD-HERS     TO MOD-KVPERIOD-HERS-IN                
032600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVPERIOD-HERS-IN-ATTR           
032700     ELSE                                                                 
032800       MOVE MFS-RENSA-FAELT        TO MOD-KVPERIOD-HERS-IN                
032900     END-IF                                                               
033000     .                                                                    
033100     EJECT                                                                
033200 F-LAES-VISA-INFO SECTION.                                                
033300                                                                          
033400     PERFORM FA-LAES-GRUNDDATA                                            
033500                                                                          
033600     IF SEGMENT-SAKNAS                                                    
033700        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
033800        CALL WMEDKONV USING MED-WMEDAREA                                  
033900        MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                
034000        PERFORM MFS-RENSA-FAELT-UT                                        
034100     ELSE                                                                 
034200*      -- POSITIONERA FÖR LÄSNING AV DATA TILL ÖVERSTA RADEN              
034300*      -- (EJ NÖDVÄNDIGT OM -MIN NYCKLAR ANVÄNDS DIREKT I SSA)            
034400                                                                          
034500       MOVE W-KDPRODSL-MIN            TO W-KDPRODSL                       
034600                                                                          
034700       MOVE +1                        TO INDX                             
034800       PERFORM FB-LAES-RADDATA                                            
034900       IF SEGMENT-FINNS                                                   
035000         MOVE 2244-KDPRODSL           TO SPAR-KDPRODSL-ENTER              
035100       ELSE                                                               
035200         MOVE W-KDPRODSL-MIN          TO SPAR-KDPRODSL-ENTER              
035300       END-IF                                                             
035400                                                                          
035500       PERFORM UNTIL INDX > MAX-INDX                                      
035600         IF SEGMENT-FINNS                                                 
035800           MOVE 2244-KDPRODSL         TO                                  
035900                MOD-RAD-KDPRODSL         (INDX)                           
036000           MOVE 2244-KVPERIOD-BERS    TO                                  
036100                MOD-RAD-KVPERIOD-BERS    (INDX)                           
036200           MOVE 2244-KVPERIOD-BTILLK  TO                                  
036300                MOD-RAD-KVPERIOD-BTILLK  (INDX)                           
036400           MOVE 2244-KVPERIOD-VERS    TO                                  
036500                MOD-RAD-KVPERIOD-VERS    (INDX)                           
036600           MOVE 2244-KVPERIOD-HERS    TO                                  
036700                MOD-RAD-KVPERIOD-HERS    (INDX)                           
036800           PERFORM FB-LAES-RADDATA                                        
036900         ELSE                                                             
037000           MOVE MFS-STAENG-FAELT TO MOD-RAD-CMD-ATTR       (INDX)         
037100           MOVE MFS-RENSA-FAELT TO MOD-RAD-CMD             (INDX)         
037200           MOVE MFS-RENSA-FAELT TO MOD-RAD-KDPRODSL        (INDX)         
037300           MOVE MFS-RENSA-FAELT TO MOD-RAD-KVPERIOD-BERS   (INDX)         
037400           MOVE MFS-RENSA-FAELT TO MOD-RAD-KVPERIOD-BTILLK (INDX)         
037500           MOVE MFS-RENSA-FAELT TO MOD-RAD-KVPERIOD-VERS   (INDX)         
037600           MOVE MFS-RENSA-FAELT TO MOD-RAD-KVPERIOD-HERS   (INDX)         
037700         END-IF                                                           
037800         ADD 1                        TO INDX                             
037900       END-PERFORM                                                        
038000                                                                          
038700       IF SEGMENT-FINNS                                                   
038800         MOVE 2244-KDPRODSL           TO SPAR-KDPRODSL-NEXT               
038900         MOVE INF-MORE-INFO-EXISTS    TO MED-IDMFSINF                     
039000         CALL WMEDKONV USING MED-WMEDAREA                                 
039010         IF NOT MFS-UPDATE                                                
039100            MOVE MED-TEMFSINF         TO MOD-TEMFSINF                     
039110         END-IF                                                           
039200       ELSE                                                               
039300         MOVE ZERO                    TO SPAR-KDPRODSL-NEXT               
039400       END-IF                                                             
039500                                                                          
039600       MOVE '002'                     TO MSGI-KDCALL                      
039700       MOVE '2148'                    TO SPAR-IDTRANS                     
039800       MOVE SPAR-AREA                 TO MSGI-SPAR-AREA                   
039900       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
040000     END-IF                                                               
040100     .                                                                    
040200     EJECT                                                                
040300 FA-LAES-GRUNDDATA SECTION.                                               
040400                                                                          
040500     PERFORM IMS-GET-WDR201                                               
040600                                                                          
040700     .                                                                    
040800     EJECT                                                                
040900 FB-LAES-RADDATA SECTION.                                                 
041000                                                                          
041100     PERFORM IMS-GET-WDGX2244                                             
041200                                                                          
041300     .                                                                    
041400     EJECT                                                                
041500 G-KOLLA-INPUT SECTION.                                                   
041600                                                                          
041700     MOVE SPAR-KDPRODSL-ENTER       TO W-KDPRODSL-MIN                     
041710     MOVE JA                        TO INDATA-SW                          
041800     MOVE NEJ                       TO SW-RAD                             
041910*       INGEN INDATA ?                                                    
041920     IF MID-RAD-CMD (1) = '+' AND MID-RAD-CMD (2) =  '+' AND              
041930        MID-RAD-CMD (3) = '+' AND MID-RAD-CMD (4) =  '+' AND              
041940        MID-RAD-CMD (5) = '+' AND MID-RAD-CMD (6) =  '+' AND              
041950        MID-RAD-CMD (7) = '+' AND MID-RAD-CMD (8) =  '+' AND              
041960        MID-RAD-CMD (9) = '+' AND MID-RAD-CMD (10) = '+' AND              
041970        MID-KDPRODSL         = ALL '+' AND                                
041980        MID-KVPERIOD-BERS    = ALL '+' AND                                
041990        MID-KVPERIOD-BTILLK  = ALL '+' AND                                
041991        MID-KVPERIOD-VERS    = ALL '+' AND                                
041992        MID-KVPERIOD-HERS    = ALL '+'                                    
042000       MOVE ERR-PF11-AND-NO-DATA    TO MED-IDMFSFEL                       
042100       CALL WMEDKONV USING MED-WMEDAREA                                   
042200       MOVE MED-MFSFEL              TO MOD-TEMFSFEL                       
042300       PERFORM MFS-ROER-EJ-FAELT-IN                                       
042400       PERFORM MFS-ROER-EJ-FAELT-UT                                       
042500       MOVE NEJ                     TO INDATA-SW                          
042600     ELSE                                                                 
042700                                                                          
042701*      * KOLLA OM UPPDATERINGSRAD ÄR IFYLLD                               
042702       PERFORM GA-KOLL-INRAD                                              
048400                                                                          
048410*      * ÄR NÅGOT CMD IFYLLT                                              
048420       PERFORM GB-KOLL-CMD                                                
050800                                                                          
050900       IF INDATA-OK                                                       
050910*         * ÄR BÅDE UPPDATERINGSRAD OCH CMD IFYLLT ?                      
051000          IF SW-RAD = JA AND SW-CMD = JA                                  
051001*           * (ÄR CMD = K ELLER C, ÄR DET OK ÄNDÅ)                        
051010            IF WS-CMD = 'K' OR 'C'                                        
051020             MOVE ZERO                    TO MID-KDPRODSL                 
051030            ELSE                                                          
051100             MOVE NEJ                     TO INDATA-SW                    
051200             MOVE +1                      TO INDX                         
051300             PERFORM UNTIL INDX > MAX-INDX                                
051400                MOVE MFS-ALFA-FAELT-FEL   TO                              
051500                   MOD-RAD-CMD-ATTR (INDX)                                
051600                ADD +1                    TO INDX                         
051700             END-PERFORM                                                  
051800             MOVE MFS-NUM-FAELT-FEL       TO                              
051900                  MOD-KDPRODSL-IN-ATTR                                    
052000                  MOD-KVPERIOD-BERS-IN-ATTR                               
052100                  MOD-KVPERIOD-BTILLK-IN-ATTR                             
052200                  MOD-KVPERIOD-VERS-IN-ATTR                               
052300                  MOD-KVPERIOD-HERS-IN-ATTR                               
052400            END-IF                                                        
052410          END-IF                                                          
052500       END-IF                                                             
052600                                                                          
052700       IF INDATA-FEL                                                      
052800         MOVE ERR-CORR-HILITE-FLDS        TO MED-IDMFSFEL                 
052900         CALL WMEDKONV USING MED-WMEDAREA                                 
053000         MOVE MED-MFSFEL                  TO MOD-TEMFSFEL                 
053100         PERFORM MFS-ROER-EJ-FAELT-UT                                     
053200         PERFORM MFS-ROER-EJ-FAELT-IN                                     
053300       END-IF                                                             
053400     END-IF                                                               
053500     .                                                                    
053600     EJECT                                                                
053700 GA-KOLL-INRAD SECTION.                                                   
053800                                                                          
053802     IF MID-KDPRODSL         NOT = ALL '+' OR                             
053803        MID-KVPERIOD-BERS    NOT = ALL '+' OR                             
053804        MID-KVPERIOD-BTILLK  NOT = ALL '+' OR                             
053805        MID-KVPERIOD-VERS    NOT = ALL '+' OR                             
053806        MID-KVPERIOD-HERS    NOT = ALL '+'                                
053807                                                                          
053808       MOVE JA                    TO SW-RAD                               
053810       INSPECT MID-KDPRODSL        REPLACING LEADING SPACE BY ZERO        
053811       INSPECT MID-KVPERIOD-BERS   REPLACING LEADING SPACE BY ZERO        
053812       INSPECT MID-KVPERIOD-BTILLK REPLACING LEADING SPACE BY ZERO        
053813       INSPECT MID-KVPERIOD-VERS   REPLACING LEADING SPACE BY ZERO        
053814       INSPECT MID-KVPERIOD-HERS   REPLACING LEADING SPACE BY ZERO        
053815       IF MID-KDPRODSL          = ALL '+' OR                              
053816          MID-KDPRODSL        NOT NUMERIC OR                              
053817          MID-KDPRODSL          = ZERO                                    
053818         MOVE MFS-NUM-FAELT-FEL     TO MOD-KDPRODSL-IN-ATTR               
053819         MOVE NEJ TO INDATA-SW                                            
053820       ELSE                                                               
053821         MOVE MFS-NUM-FAELT-RAETT   TO MOD-KDPRODSL-IN-ATTR               
053822       END-IF                                                             
053823                                                                          
053824       IF MID-KVPERIOD-BERS     = ALL '+'                                 
053827         MOVE MFS-NUM-FAELT-RAETT   TO MOD-KVPERIOD-BERS-IN-ATTR          
053828       ELSE                                                               
053830         IF MID-KVPERIOD-BERS   NOT NUMERIC OR                            
053831            MID-KVPERIOD-BERS     = ZERO    OR                            
053832            MID-KVPERIOD-BERS     > 24                                    
053833           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVPERIOD-BERS-IN-ATTR          
053834           MOVE NEJ TO INDATA-SW                                          
053835         ELSE                                                             
053836           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPERIOD-BERS-IN-ATTR          
053837         END-IF                                                           
053838       END-IF                                                             
053839                                                                          
053840       IF MID-KVPERIOD-BTILLK   = ALL '+'                                 
053841           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPERIOD-BTILLK-IN-ATTR        
053842       ELSE                                                               
053844         IF MID-KVPERIOD-BTILLK NOT NUMERIC OR                            
053845            MID-KVPERIOD-BTILLK   = ZERO    OR                            
053846            MID-KVPERIOD-BTILLK   > 24                                    
053847           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVPERIOD-BTILLK-IN-ATTR        
053848           MOVE NEJ TO INDATA-SW                                          
053849         ELSE                                                             
053850           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPERIOD-BTILLK-IN-ATTR        
053851         END-IF                                                           
053853       END-IF                                                             
053854                                                                          
053855       IF MID-KVPERIOD-VERS     = ALL '+'                                 
053856         MOVE MFS-NUM-FAELT-RAETT   TO MOD-KVPERIOD-VERS-IN-ATTR          
053857       ELSE                                                               
053859         IF MID-KVPERIOD-VERS   NOT NUMERIC OR                            
053860            MID-KVPERIOD-VERS     = ZERO    OR                            
053861            MID-KVPERIOD-VERS     > 24                                    
053862           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVPERIOD-VERS-IN-ATTR          
053863           MOVE NEJ TO INDATA-SW                                          
053864         ELSE                                                             
053865           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPERIOD-VERS-IN-ATTR          
053866         END-IF                                                           
053868       END-IF                                                             
053869                                                                          
053870       IF MID-KVPERIOD-HERS     = ALL '+'                                 
053871         MOVE MFS-NUM-FAELT-RAETT   TO MOD-KVPERIOD-HERS-IN-ATTR          
053872       ELSE                                                               
053874         IF MID-KVPERIOD-HERS   NOT NUMERIC OR                            
053875            MID-KVPERIOD-HERS     = ZERO    OR                            
053876            MID-KVPERIOD-HERS     > 24                                    
053877           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVPERIOD-HERS-IN-ATTR          
053878           MOVE NEJ TO INDATA-SW                                          
053879         ELSE                                                             
053880           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVPERIOD-HERS-IN-ATTR          
053881         END-IF                                                           
053883       END-IF                                                             
053884     END-IF                                                               
053885     .                                                                    
053886     EJECT                                                                
053887 GB-KOLL-CMD SECTION.                                                     
053888                                                                          
053889     MOVE +1                            TO INDX                           
053890     MOVE NEJ                           TO SW-CMD                         
053891     PERFORM UNTIL INDX > MAX-INDX                                        
053892        IF MID-RAD-CMD (INDX)    NOT =  ALL '+'                           
053893           IF SW-CMD = NEJ              AND                               
053894             (MID-RAD-CMD (INDX) = 'K'  OR                                
053895              MID-RAD-CMD (INDX) = 'C'  OR                                
053896              MID-RAD-CMD (INDX) = 'B'  OR                                
053897              MID-RAD-CMD (INDX) = 'D')                                   
053898              MOVE MFS-ALFA-FAELT-RAETT TO                                
053899                 MOD-RAD-CMD-ATTR (INDX)                                  
053900              MOVE MID-RAD-CMD (INDX)   TO WS-CMD                         
053901           ELSE                                                           
053902              MOVE MFS-ALFA-FAELT-FEL   TO                                
053903                 MOD-RAD-CMD-ATTR (INDX)                                  
053904              MOVE NEJ                  TO INDATA-SW                      
053905           END-IF                                                         
053906           IF MID-RAD-KDPRODSL (INDX) = ALL '+'                           
053907              MOVE MFS-ALFA-FAELT-FEL   TO                                
053908                 MOD-RAD-CMD-ATTR (INDX)                                  
053909              MOVE NEJ                  TO INDATA-SW                      
053910           END-IF                                                         
053911           MOVE JA                      TO SW-CMD                         
053912        END-IF                                                            
053913        ADD +1                          TO INDX                           
053914     END-PERFORM                                                          
053915     .                                                                    
053916     EJECT                                                                
053917 H-UPPDATERA SECTION.                                                     
053918                                                                          
053920*****KOLLA FÖRST UPPDATERINGSRADEN: IFYLLD => UPPDATERA DEN               
054000***** ANNARS KOLLA OM NÅGOT KOMMANDO ÄR IFYLLT (1-10)                     
054100                                                                          
054200     IF MID-KDPRODSL NOT  = ALL '+' AND                                   
054210        MID-KDPRODSL NOT  = ZERO                                          
054300***     * UPPDATERINGSRAD ÄR IFYLLD                                       
054400        MOVE MID-KDPRODSL           TO W-KDPRODSL                         
054500        PERFORM IMS-GHU-WDGX2244-KVAL                                     
054600        IF SEGMENT-FINNS                                                  
054700*         * ÄNDRING AV BEFINTLIGT                                         
054710          IF MID-KVPERIOD-BERS     NOT = ALL '+'                          
054800             MOVE MID-KVPERIOD-BERS    TO 2244-KVPERIOD-BERS              
054810          END-IF                                                          
054820          IF MID-KVPERIOD-BTILLK   NOT = ALL '+'                          
054900             MOVE MID-KVPERIOD-BTILLK  TO 2244-KVPERIOD-BTILLK            
054910          END-IF                                                          
054920          IF MID-KVPERIOD-VERS     NOT = ALL '+'                          
055000             MOVE MID-KVPERIOD-VERS    TO 2244-KVPERIOD-VERS              
055010          END-IF                                                          
055020          IF MID-KVPERIOD-HERS     NOT = ALL '+'                          
055100             MOVE MID-KVPERIOD-HERS    TO 2244-KVPERIOD-HERS              
055110          END-IF                                                          
055200                                                                          
055300          PERFORM IMS-REPL-WDGX2244                                       
056100        ELSE                                                              
056200*         * NYUPPLÄGG                                                     
056300          MOVE MID-KDPRODSL         TO 2244-KDPRODSL                      
056400          MOVE MID-KVPERIOD-BERS    TO 2244-KVPERIOD-BERS                 
056500          MOVE MID-KVPERIOD-BTILLK  TO 2244-KVPERIOD-BTILLK               
056600          MOVE MID-KVPERIOD-VERS    TO 2244-KVPERIOD-VERS                 
056700          MOVE MID-KVPERIOD-HERS    TO 2244-KVPERIOD-HERS                 
056800          MOVE MID-KDPRODSL         TO SPAR-KDPRODSL-ENTER                
056900                                                                          
057000          PERFORM IMS-ISRT-WDGX2244                                       
057100        END-IF                                                            
057200     ELSE                                                                 
057300*       * NÅGON CMD ÄR IFYLLD                                             
057400        MOVE +1                          TO INDX                          
057500        PERFORM UNTIL INDX > MAX-INDX                                     
057600           IF MID-RAD-CMD (INDX) NOT = ALL '+'                            
057700              MOVE INDX                  TO CMD-IX                        
057800              MOVE 10                    TO INDX                          
057900           END-IF                                                         
058000           ADD +1                        TO INDX                          
058100        END-PERFORM                                                       
058200                                                                          
058300        IF MID-RAD-CMD (CMD-IX) = 'B' OR 'D'                              
058400*          * TAG BORT RAD                                                 
058500           MOVE MID-RAD-KDPRODSL(CMD-IX) TO W-KDPRODSL                    
058600           PERFORM IMS-GHU-WDGX2244-KVAL                                  
058700           IF SEGMENT-FINNS                                               
058800              PERFORM IMS-DLET-WDGX2244                                   
058900           END-IF                                                         
059000        END-IF                                                            
059100                                                                          
059200        IF MID-RAD-CMD (CMD-IX) = 'K' OR 'C'                              
059300*          * KOPIERA TILL UPPDATERINGSRADEN                               
059400           MOVE MID-RAD-KDPRODSL(CMD-IX) TO W-KDPRODSL                    
059500           PERFORM IMS-GHU-WDGX2244-KVAL                                  
059600           IF SEGMENT-FINNS                                               
059700              MOVE 2244-KDPRODSL         TO MOD-KDPRODSL-IN               
059800              MOVE 2244-KVPERIOD-BERS    TO MOD-KVPERIOD-BERS-IN          
059900              MOVE 2244-KVPERIOD-BTILLK  TO MOD-KVPERIOD-BTILLK-IN        
060000              MOVE 2244-KVPERIOD-VERS    TO MOD-KVPERIOD-VERS-IN          
060100              MOVE 2244-KVPERIOD-HERS    TO MOD-KVPERIOD-HERS-IN          
060110              MOVE JA                    TO SW-KOPI                       
060200           END-IF                                                         
060300        END-IF                                                            
060400                                                                          
060500     END-IF                                                               
060510                                                                          
060520     IF SEGMENT-FINNS                                                     
060521       IF SW-KOPI = NEJ                                                   
060530          MOVE INF-UPDATE-DONE TO MED-IDMFSINF                            
060540          CALL WMEDKONV USING MED-WMEDAREA                                
060550          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
060551       END-IF                                                             
060560       PERFORM MFS-FORM-ATTR                                              
060570       PERFORM MFS-RENSA-FAELT-IN                                         
060580     END-IF                                                               
060600     .                                                                    
060700     EJECT                                                                
060800 MFS-RENSA-FAELT-UT SECTION.                                              
060900                                                                          
061000*    --- ALLA UTDATA-FÄLT                                                 
061100*    --- INKL. BLÄDDRINGSNYCKLAR                                          
061200     MOVE +1 TO INDX                                                      
061300     PERFORM UNTIL INDX > MAX-INDX                                        
061400        MOVE MFS-RENSA-FAELT TO MOD-RAD-CMD             (INDX)            
061500                                MOD-RAD-KDPRODSL        (INDX)            
061600                                MOD-RAD-KVPERIOD-BERS   (INDX)            
061700                                MOD-RAD-KVPERIOD-BTILLK (INDX)            
061800                                MOD-RAD-KVPERIOD-VERS   (INDX)            
061900                                MOD-RAD-KVPERIOD-HERS   (INDX)            
062000        ADD +1 TO INDX                                                    
062100     END-PERFORM                                                          
062200                                                                          
062300     MOVE MFS-RENSA-FAELT TO MOD-KDPRODSL-IN                              
062400                             MOD-KVPERIOD-BERS-IN                         
062500                             MOD-KVPERIOD-BTILLK-IN                       
062600                             MOD-KVPERIOD-VERS-IN                         
062700                             MOD-KVPERIOD-HERS-IN                         
062710     MOVE ZERO            TO SPAR-KDPRODSL-ENTER                          
062720                             SPAR-KDPRODSL-NEXT                           
062800     .                                                                    
062900     SKIP3                                                                
063000 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
063100                                                                          
063200*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
063300     MOVE +1 TO INDX                                                      
063400     PERFORM UNTIL INDX > MAX-INDX                                        
063500        MOVE MFS-RENSA-FAELT TO MOD-RAD-CMD             (INDX)            
063600                                MOD-RAD-KDPRODSL        (INDX)            
063700                                MOD-RAD-KVPERIOD-BERS   (INDX)            
063800                                MOD-RAD-KVPERIOD-BTILLK (INDX)            
063900                                MOD-RAD-KVPERIOD-VERS   (INDX)            
064000                                MOD-RAD-KVPERIOD-HERS   (INDX)            
064100        ADD +1 TO INDX                                                    
064200     END-PERFORM                                                          
064300     .                                                                    
064400     SKIP3                                                                
064500 MFS-RENSA-FAELT-IN SECTION.                                              
064600                                                                          
064700*    --- ALLA INDATA-FÄLT                                                 
064900     MOVE +1 TO INDX                                                      
065000     PERFORM UNTIL INDX > MAX-INDX                                        
065100        MOVE MFS-RENSA-FAELT TO MOD-RAD-CMD      (INDX)                   
065110        MOVE MFS-RENSA-FAELT TO MOD-RAD-KDPRODSL (INDX)                   
065200        ADD +1 TO INDX                                                    
065300     END-PERFORM                                                          
065400                                                                          
065410     IF SW-KOPI = NEJ                                                     
065500        MOVE MFS-RENSA-FAELT TO MOD-KDPRODSL-IN                           
065600                                MOD-KVPERIOD-BERS-IN                      
065700                                MOD-KVPERIOD-BTILLK-IN                    
065800                                MOD-KVPERIOD-VERS-IN                      
065900                                MOD-KVPERIOD-HERS-IN                      
065910     END-IF                                                               
066000     .                                                                    
066100     EJECT                                                                
066200 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
066300                                                                          
066400*    --- ALLA UTDATA-FÄLT                                                 
066500*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
066600     MOVE +1 TO INDX                                                      
066700     PERFORM UNTIL INDX > MAX-INDX                                        
066800       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
066900       ADD +1 TO INDX                                                     
067000     END-PERFORM                                                          
067100                                                                          
067200     MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRODSL-IN                            
067300                               MOD-KVPERIOD-BERS-IN                       
067400                               MOD-KVPERIOD-BTILLK-IN                     
067500                               MOD-KVPERIOD-VERS-IN                       
067600                               MOD-KVPERIOD-HERS-IN                       
067700     .                                                                    
067800     SKIP2                                                                
067900 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
068000                                                                          
068100*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
068200     MOVE MFS-ROER-EJ-FAELT TO MOD-RAD-CMD             (INDX)             
068300     MOVE MFS-ROER-EJ-FAELT TO MOD-RAD-KDPRODSL        (INDX)             
068400                               MOD-RAD-KVPERIOD-BERS   (INDX)             
068500                               MOD-RAD-KVPERIOD-BTILLK (INDX)             
068600                               MOD-RAD-KVPERIOD-VERS   (INDX)             
068700                               MOD-RAD-KVPERIOD-HERS   (INDX)             
068800     .                                                                    
068900     SKIP3                                                                
069000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
069100                                                                          
069200*    --- ALLA INDATA-FÄLT                                                 
069300     MOVE +1 TO INDX                                                      
069400     PERFORM UNTIL INDX > MAX-INDX                                        
069500       MOVE MFS-ROER-EJ-FAELT TO MOD-RAD-CMD (INDX)                       
069600       ADD +1 TO INDX                                                     
069700     END-PERFORM                                                          
069800                                                                          
069900     MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRODSL-IN                            
070000                               MOD-KVPERIOD-BERS-IN                       
070100                               MOD-KVPERIOD-BTILLK-IN                     
070200                               MOD-KVPERIOD-VERS-IN                       
070300                               MOD-KVPERIOD-HERS-IN                       
070400     .                                                                    
070500     EJECT                                                                
070600 MFS-FORM-ATTR SECTION.                                                   
070700                                                                          
070800*    --- ALLA INDATA-FÄLT                                                 
070900     MOVE +1 TO INDX                                                      
071000     PERFORM UNTIL INDX > MAX-INDX                                        
071100       MOVE MFS-FORMATETS-ATTR TO MOD-RAD-CMD-ATTR (INDX)                 
071200       ADD +1 TO INDX                                                     
071300     END-PERFORM                                                          
071400                                                                          
071500     MOVE MFS-FORMATETS-ATTR TO MOD-KDPRODSL-IN-ATTR                      
071600                                MOD-KVPERIOD-BERS-IN-ATTR                 
071700                                MOD-KVPERIOD-BTILLK-IN-ATTR               
071800                                MOD-KVPERIOD-VERS-IN-ATTR                 
071900                                MOD-KVPERIOD-HERS-IN-ATTR                 
072000     .                                                                    
072100     SKIP2                                                                
072200 MFS-LAES-IN-IGEN SECTION.                                                
072300                                                                          
072400*    --- ALLA INDATA-FÄLT                                                 
072500     MOVE +1 TO INDX                                                      
072600     PERFORM UNTIL INDX > MAX-INDX                                        
072700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-RAD-CMD-ATTR (INDX)              
072800       ADD +1 TO INDX                                                     
072900     END-PERFORM                                                          
073000                                                                          
073100     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRODSL-IN-ATTR                   
073200                                   MOD-KVPERIOD-BERS-IN-ATTR              
073300                                   MOD-KVPERIOD-BTILLK-IN-ATTR            
073400                                   MOD-KVPERIOD-VERS-IN-ATTR              
073500                                   MOD-KVPERIOD-HERS-IN-ATTR              
073600     .                                                                    
073700     EJECT                                                                
073800* --- IMS SEKTIONER ---                                                   
073900     SKIP3                                                                
074000 IMS-GET-MSG SECTION.                                                     
074100                                                                          
074200     MOVE '  QC' TO GODK-STATUSKODER                                      
074300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
074400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
074500     PERFORM IMS-STATUSKONTROLL                                           
074600     .                                                                    
074700     SKIP3                                                                
074800 IMS-INSERT-MSG SECTION.                                                  
074900                                                                          
075000     IF MSGI-IDLAND-SPR = 'SE'                                            
075100       MOVE '0' TO MFS-KDHUVOMR                                           
075200     END-IF                                                               
075300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
075400     MOVE SPACE TO GODK-STATUSKODER                                       
075500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
075600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075700     PERFORM IMS-STATUSKONTROLL                                           
075800     .                                                                    
075900     EJECT                                                                
076000 IMS-GET-WDR201 SECTION.                                                  
076100                                                                          
076200     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
076300          DELIMITED BY SIZE INTO SSA1                                     
076400     MOVE '  GE' TO GODK-STATUSKODER                                      
076500     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDR201 SSA1                    
076600     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
076700     PERFORM IMS-STATUSKONTROLL                                           
076800     .                                                                    
076900     EJECT                                                                
077000 IMS-GHU-WDGX2244-KVAL SECTION.                                           
077100                                                                          
077200     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
077300          DELIMITED BY SIZE INTO SSA1                                     
077400     STRING 'WDGX2244(KDPRODSL= ' W-KDPRODSL-X ')'                        
077500          DELIMITED BY SIZE INTO SSA2                                     
077600     MOVE '  GE' TO GODK-STATUSKODER                                      
077700     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-WDGX2244 SSA1 SSA2            
077800     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
077900     PERFORM IMS-STATUSKONTROLL                                           
078000     .                                                                    
078100     SKIP3                                                                
078200 IMS-GET-WDGX2244 SECTION.                                                
078300                                                                          
078400     STRING 'WDGX2244(KDPRODSL=>' W-KDPRODSL-MIN-X ')'                    
078500          DELIMITED BY SIZE INTO SSA1                                     
078600     MOVE '  GE' TO GODK-STATUSKODER                                      
078700     CALL CBLTDLI USING GHNP WDR2-PCB DLI-IO-WDGX2244 SSA1                
078800     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
078900     PERFORM IMS-STATUSKONTROLL                                           
079000     .                                                                    
079100     SKIP3                                                                
079200 IMS-ISRT-WDGX2244 SECTION.                                               
079300                                                                          
079400     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
079500          DELIMITED BY SIZE INTO SSA1                                     
079600     MOVE 'WDGX2244    ' TO SSA2                                          
079700     MOVE '  II' TO GODK-STATUSKODER                                      
079800     CALL CBLTDLI USING ISRT WDR2-PCB DLI-IO-WDGX2244 SSA1 SSA2           
079900     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
080000     PERFORM IMS-STATUSKONTROLL                                           
080100     .                                                                    
080200     SKIP3                                                                
080300 IMS-REPL-WDGX2244 SECTION.                                               
080400                                                                          
080500     MOVE '  ' TO GODK-STATUSKODER                                        
080600     CALL CBLTDLI USING REPL WDR2-PCB DLI-IO-WDGX2244                     
080700     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
080800     PERFORM IMS-STATUSKONTROLL                                           
080900     .                                                                    
081000     SKIP3                                                                
081100 IMS-DLET-WDGX2244 SECTION.                                               
081200                                                                          
081300     MOVE '  ' TO GODK-STATUSKODER                                        
081400     CALL CBLTDLI USING DLET WDR2-PCB DLI-IO-WDGX2244                     
081500     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
081600     PERFORM IMS-STATUSKONTROLL                                           
081700     .                                                                    
081800     EJECT                                                                
081900 IMS-STATUSKONTROLL SECTION.                                              
082000                                                                          
082100     SET STATUS-IX TO 1                                                   
082200     SEARCH GODK-STATUS                                                   
082300       AT END                                                             
082400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
082500         DELIMITED BY SIZE INTO FELTEXT                                   
082600         CALL FELLOG                                                      
082700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
082800         CONTINUE                                                         
082900     END-SEARCH                                                           
083000     .                                                                    
