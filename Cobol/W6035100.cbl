000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6035100.                                                
000300 AUTHOR.         NIHLBLAD JOHAN.                                          
000400 DATE-WRITTEN.   07/06/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        DC-TREE                                                          
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDR2                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W6T351                                              
001400*        MID:         W6I35101                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W6O35101                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W6035100'.            
002600                                                                          
002700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002900                                                                          
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003110 77  RAD-IX                      PIC 9(9)    VALUE ZERO.                  
003120 77  RAD-MAX                     PIC 9(9)    VALUE 26.                    
003130 77  USER-IX                     PIC 9(9)    VALUE ZERO.                  
003140 77  USER-MAX                    PIC 9(9)    VALUE 4.                     
003150 77  LVL3-IX                     PIC 9(9)    VALUE ZERO.                  
003160 77  LVL3-MAX                    PIC 9(9)    VALUE 24.                    
003170 77  SW-LVL1                     PIC X       VALUE 'N'.                   
003180 77  SW-LVL2                     PIC X       VALUE 'N'.                   
003190 77  SW-LVL3                     PIC X       VALUE 'N'.                   
003200                                                                          
003300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003400                                                                          
003500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
003600     88  INDATA-OK                           VALUE 'J'.                   
003700     88  INDATA-FEL                          VALUE 'N'.                   
003800                                                                          
003900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004000     88  NYCKLAR-OK                          VALUE 'J'.                   
004100     88  NYCKLAR-FEL                         VALUE 'N'.                   
004200                                                                          
004300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004400     88  EGEN-MID                            VALUE '6351'.                
004500     88  GODK-MID                            VALUE '6351' '6352'          
004600                                                   '6353' '6354'          
004700                                                   '6355' '6356'          
004800                                                   '6357' '6358'          
004900                                                   '6359'.                
005000     88  HELP-MID                            VALUE '0551'.                
005100     EJECT                                                                
005220*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005300 01  GENERELLA-SUBPROGRAM.                                                
005400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005800     EJECT                                                                
005900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006000*01 -COPY WMEDAREA                                                        
006100     SKIP3                                                                
006200 01  MESSAGE-CODES.                                                       
006300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
006400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
006500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
006600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
006700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
006800     EJECT                                                                
006900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007000*                                                                         
007100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007200     SKIP3                                                                
007300*01 -COPY WMSGINIT                                                        
007400     EJECT                                                                
007500*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
007600*                                                                         
007700 01  SPAR-AREA.                                                           
007800     03  SPAR-IDTRANS           PIC X(4)    VALUE '6351'.                 
007900     EJECT                                                                
008000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008300     SKIP3                                                                
008400*01  MID -COPY W6I35101                                                   
008500     EJECT                                                                
008600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
008700     SKIP3                                                                
008800*01  -COPY WMSGAREA                                                       
008900     EJECT                                                                
009000     03  MOD REDEFINES MSG-AREA.                                          
009100*      05  -COPY W6O35101                                                 
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009400     SKIP3                                                                
009500*01  -COPY WMFSAREA                                                       
009600     EJECT                                                                
009700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009800*                                                                         
009900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010000     SKIP3                                                                
010100 01  NYCKLAR-TILL-DLI.                                                    
010200     03  W-WDGXKEY-6331.                                                  
010300         05  W-6331-IDHTYP       PIC X(4)    VALUE '6331'.                
010310         05  W-6331-IDDC         PIC X(2)    VALUE '11'.                  
010320         05  W-6331-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
010400     03  W-IDDC-6332.                                                     
010500         05  W-6332-IDDC         PIC X(2)    VALUE SPACE.                 
010600     03  W-IDDC-6334.                                                     
010700         05  W-6334-IDDC         PIC X(2)    VALUE SPACE.                 
010710     03  W-IDDC-B6-X.                                                     
010720         05  W-IDDC-B6          PIC X(2)       VALUE SPACE.               
010800     SKIP2                                                                
010900*    --- STATUS-KOD FRÅN IMS                                              
011000 01  STATUS-WS                   PIC XX.                                  
011100     88  SEGMENT-FINNS                       VALUE '  '.                  
011200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011310     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011400     SKIP2                                                                
011500 01  GODK-STATUSKODER.                                                    
011600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011700     SKIP3                                                                
011800 01  SSA1                        PIC X(64).                               
011900 01  SSA2                        PIC X(64).                               
011910 01  SSA3                        PIC X(64).                               
012000     EJECT                                                                
012100*    --- IMS FUNKTIONSKODER                                               
012200*01  -COPY W0003                                                          
012300     EJECT                                                                
012400*    ---  DLI INPUT-OUTPUT AREA                                           
012500                                                                          
012600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6331'.                    
012700 01  DLI-IO-WDGX6331.                                                     
012800*    03  -COPY WDGX6331                                                   
012900     EJECT                                                                
013000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6332'.                    
013100 01  DLI-IO-WDGX6332.                                                     
013200*    03  -COPY WDGX6332                                                   
013300     EJECT                                                                
013400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6334'.                    
013500 01  DLI-IO-WDGX6334.                                                     
013600*    03  -COPY WDGX6334                                                   
013700     EJECT                                                                
013710 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
013720 01   DLI-IO-AREA-B601.                                                   
013730*     03  -COPY WDB601                                                    
013740     EJECT                                                                
013800 LINKAGE SECTION.                                                         
013900*01  -COPY W0009   -PRE MSG-                                              
014000*01  -COPY W0008   -PRE WDP7-                                             
014100     05  FILLER                  PIC X.                                   
014200                                                                          
014300*01  -COPY W0008  -PRE 6331-                                              
014301     05  FILLER                         PIC X(4).                         
014310     05  6331-KEY-FB-AREA-IDDC          PIC X(2).                         
014311     05  FILLER                         PIC X(24).                        
014320     05  6332-KEY-FB-AREA-IDDC          PIC X(2).                         
014500     EJECT                                                                
014510                                                                          
014520*01  -COPY W0008  -PRE 63312-                                             
014530     05  FILLER                  PIC X.                                   
014540     EJECT                                                                
014550*01  -COPY W0008  -PRE WDB6-                                              
014560     05  FILLER                  PIC X.                                   
014570     EJECT                                                                
014600 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB 6331-PCB                      
014610                           63312-PCB WDB6-PCB.                            
014700 MAIN SECTION.                                                            
014800     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB 6331-PCB                      
014801                           63312-PCB WDB6-PCB.                            
014900                                                                          
015000     PERFORM IMS-GET-MSG                                                  
015100     IF SEGMENT-FINNS                                                     
015200       PERFORM A-INIT                                                     
015300       PERFORM B-KOLLA-NYCKLAR                                            
015400       IF NYCKLAR-OK                                                      
015500         IF MFS-UPDATE                                                    
015600           PERFORM G-KOLLA-INPUT                                          
015700           IF INDATA-OK                                                   
015800             PERFORM H-UPPDATERA                                          
015900           END-IF                                                         
016000         ELSE                                                             
016100           IF MFS-FIRST                                                   
016200             PERFORM C-FOERSTA-SIDA                                       
016300           ELSE                                                           
016400             PERFORM E-SAMMA-SIDA                                         
016500           END-IF                                                         
016600         END-IF                                                           
016700         PERFORM F-LAES-VISA-INFO                                         
016800       END-IF                                                             
016900*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
017100       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O35101 + 4                      
017200       PERFORM IMS-INSERT-MSG                                             
017300     END-IF                                                               
017400                                                                          
017500     MOVE ZERO TO RETURN-CODE                                             
017600     GOBACK                                                               
017700     .                                                                    
017800     EJECT                                                                
017900 A-INIT SECTION.                                                          
018000                                                                          
018100     IF MSG-DUBBLA-TRANSKODER                                             
018200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I35101                 
018300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
018400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
018500     ELSE                                                                 
018600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I35101                  
018700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018900     END-IF                                                               
019000                                                                          
019100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
019200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
019300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
019400                                                                          
019500     MOVE LOW-VALUE TO MSG-AREA                                           
019600     MOVE 'W6O35101' TO MFS-IDMOD                                         
019700     MOVE '6351' TO MOD-IDTRANS                                           
019800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019900                                                                          
020000     IF EGEN-MID                                                          
020100       CONTINUE                                                           
020200     ELSE                                                                 
020300       MOVE SPACE TO MFS-KDTRTYP                                          
020400       MOVE '7' TO MFS-IDPFK                                              
020500     END-IF                                                               
020600     .                                                                    
020700     EJECT                                                                
020800 B-KOLLA-NYCKLAR SECTION.                                                 
020900                                                                          
021000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
021100     MOVE '001'             TO MSGI-KDCALL                                
021200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
021300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021400     MOVE '6351'            TO MSGI-IDTRANS                               
021500     IF EGEN-MID                                                          
021510        MOVE MID-IDDC-IN       TO MSGI-IDDC-KEY                           
021600     END-IF                                                               
021700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
021800     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
021900                                                                          
022000*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
022100     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
022200                                                                          
022300     MOVE JA TO NYCKLAR-SW                                                
022400                                                                          
022401     MOVE MSGI-IDDC-KEY TO W-IDDC-B6                                      
022402                                                                          
022410     PERFORM IMS-GU-WDB601                                                
022420     IF SEGMENT-FINNS                                                     
022430        CONTINUE                                                          
022440     ELSE                                                                 
022450        MOVE NEJ TO NYCKLAR-SW                                            
022460     END-IF                                                               
022470                                                                          
022480     IF NYCKLAR-OK                                                        
022491        MOVE MSGI-IDDC-KEY TO MOD-IDDC-UT                                 
022493     ELSE                                                                 
022494        MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                               
022495     END-IF                                                               
022500                                                                          
022600     IF NYCKLAR-FEL                                                       
022700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
022800       CALL WMEDKONV USING MED-WMEDAREA                                   
022900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
023000       PERFORM MFS-RENSA-FAELT-IN                                         
023100       PERFORM MFS-RENSA-FAELT-UT                                         
023200     END-IF                                                               
023300     .                                                                    
023400     EJECT                                                                
023500 C-FOERSTA-SIDA SECTION.                                                  
023600                                                                          
023700     PERFORM MFS-RENSA-FAELT-IN                                           
023800     .                                                                    
023900     EJECT                                                                
024000 E-SAMMA-SIDA SECTION.                                                    
024100                                                                          
024200     IF EGEN-MID OR HELP-MID                                              
024300       IF MID-INPUT = ALL '+'                                             
024400         PERFORM MFS-RENSA-FAELT-IN                                       
024500       ELSE                                                               
024501         IF MID-IDDC-IN NOT = ALL '+'                                     
024502           PERFORM MFS-RENSA-FAELT-IN                                     
024503         ELSE                                                             
024510*      CALL FELLOG                                                        
024600           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
024700           CALL WMEDKONV USING MED-WMEDAREA                               
024800           MOVE MED-MFSINF TO MOD-TEMFSINF                                
024900           PERFORM EA-MID-INDATA-TILL-MOD                                 
025000         END-IF                                                           
025010       END-IF                                                             
025100     ELSE                                                                 
025200       PERFORM MFS-RENSA-FAELT-IN                                         
025300     END-IF                                                               
025400     .                                                                    
025500     EJECT                                                                
025600 EA-MID-INDATA-TILL-MOD SECTION.                                          
025700                                                                          
025701     IF MID-CMD  NOT = ALL '+'                                            
025702        MOVE MFS-ROER-EJ-FAELT  TO MOD-CMD-IN                             
025703     ELSE                                                                 
025704        MOVE MFS-RENSA-FAELT    TO MOD-CMD-IN                             
025705     END-IF                                                               
025706     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR                           
025707                                                                          
025710     IF MID-NY-IDDC NOT = ALL '+'                                         
025720        MOVE MFS-ROER-EJ-FAELT  TO MOD-NY-IDDC-IN                         
025730     ELSE                                                                 
025740        MOVE MFS-RENSA-FAELT    TO MOD-NY-IDDC-IN                         
025750     END-IF                                                               
025760     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-NY-IDDC-ATTR                       
025770                                                                          
025780     IF MID-UNDER-IDDC NOT = ALL '+'                                      
025790        MOVE MFS-ROER-EJ-FAELT  TO MOD-UNDER-IDDC-IN                      
025800     ELSE                                                                 
025900        MOVE MFS-RENSA-FAELT    TO MOD-UNDER-IDDC-IN                      
026000     END-IF                                                               
026100     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-UNDER-IDDC-ATTR                    
026110                                                                          
026120     IF MID-KDDC NOT = ALL '+'                                            
026130        MOVE MFS-ROER-EJ-FAELT  TO MOD-KDDC-IN                            
026140     ELSE                                                                 
026150        MOVE MFS-RENSA-FAELT    TO MOD-KDDC-IN                            
026160     END-IF                                                               
026170     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDDC-ATTR                          
026180                                                                          
026190     MOVE +1 TO USER-IX                                                   
026191     PERFORM UNTIL USER-IX > USER-MAX                                     
026192        IF MID-IDUSER(USER-IX) NOT = ALL '+'                              
026193           MOVE MFS-ROER-EJ-FAELT TO MOD-IDUSER-IN(USER-IX)               
026194        ELSE                                                              
026195           MOVE MFS-RENSA-FAELT   TO MOD-IDUSER-IN(USER-IX)               
026196        END-IF                                                            
026197        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDUSER-ATTR(USER-IX)            
026198        ADD +1 TO USER-IX                                                 
026199     END-PERFORM                                                          
026200     .                                                                    
026300     EJECT                                                                
026400 F-LAES-VISA-INFO SECTION.                                                
026500                                                                          
026501     PERFORM IMS-GU-WDGX6331                                              
026502     IF SEGMENT-FINNS                                                     
026510       IF MSGI-IDDC-KEY = '11'                                            
026610         MOVE JA TO SW-LVL1                                               
026700       ELSE                                                               
026710         MOVE MSGI-IDDC-KEY  TO W-6332-IDDC                               
026720         PERFORM IMS-GNP-WDGX6332                                         
026730         IF SEGMENT-FINNS                                                 
026740           MOVE JA TO SW-LVL2                                             
026741         ELSE                                                             
026742           MOVE MSGI-IDDC-KEY TO W-6334-IDDC                              
026743           PERFORM IMS-GU-WDGX6331                                        
026744           PERFORM IMS-GNP-WDGX6334-2                                     
026745           IF SEGMENT-FINNS                                               
026747             MOVE JA TO SW-LVL3                                           
026748           END-IF                                                         
026750         END-IF                                                           
026757       END-IF                                                             
026760                                                                          
026761*      CALL FELLOG                                                        
026762       IF SW-LVL1 = JA                                                    
026763         PERFORM FA-LAES-LVL1                                             
026770       END-IF                                                             
026771       IF SW-LVL2 = JA                                                    
026772         PERFORM FB-LAES-LVL2                                             
026773       END-IF                                                             
026774       IF SW-LVL3 = JA                                                    
026775         PERFORM FC-LAES-LVL3                                             
026776       END-IF                                                             
026777     END-IF                                                               
026780                                                                          
026800*    IF SEGMENT-SAKNAS                                                    
026900     IF SW-LVL1 = NEJ AND SW-LVL2 = NEJ AND SW-LVL3 = NEJ                 
026910        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
027000        CALL WMEDKONV USING MED-WMEDAREA                                  
027100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
027200        PERFORM MFS-RENSA-FAELT-UT                                        
027400     END-IF                                                               
027500     .                                                                    
027600     EJECT                                                                
027700 FA-LAES-LVL1 SECTION.                                                    
027800                                                                          
027810****LEVEL1*****                                                           
027900     PERFORM IMS-GU-WDGX6331                                              
028000     MOVE '11'          TO MOD-LVL1-IDDC                                  
028010****LEVEL2*****                                                           
028100     PERFORM IMS-GNP-WDGX6332-2                                           
028101     MOVE +1   TO RAD-IX                                                  
028110     PERFORM UNTIL RAD-IX > RAD-MAX                                       
028120       IF SEGMENT-FINNS                                                   
028130         MOVE 6332-IDDC    TO MOD-LVL2-IDDC(RAD-IX)                       
028131                              W-6332-IDDC                                 
028132         MOVE 6332-KDDC    TO MOD-KDDC(RAD-IX)                            
028140         MOVE +1 TO USER-IX                                               
028150         PERFORM UNTIL USER-IX > USER-MAX                                 
028160           MOVE 6332-IDUSER(USER-IX)                                      
028170             TO MOD-LVL2-IDUSER(RAD-IX, USER-IX)                          
028200           ADD +1 TO USER-IX                                              
028210         END-PERFORM                                                      
028211**********                                                                
028213         PERFORM IMS-GU-WDGX6332-PCB2                                     
028214         IF SEGMENT-FINNS                                                 
028216           PERFORM IMS-GNP-WDGX6334-PCB2                                  
028217           IF SEGMENT-FINNS                                               
028219             MOVE 'Y' TO MOD-FLLVL3(RAD-IX)                               
028220           END-IF                                                         
028221         END-IF                                                           
028222*        IF 6332-IDDC = '23'                                              
028223*         CALL FELLOG                                                     
028224*        END-IF                                                           
028225**********                                                                
028226         PERFORM IMS-GNP-WDGX6332-2                                       
028227       ELSE                                                               
028228         MOVE MFS-RENSA-FAELT   TO MOD-LVL2-IDDC(RAD-IX)                  
028229                                   MOD-KDDC(RAD-IX)                       
028230         MOVE +1 TO USER-IX                                               
028231         PERFORM UNTIL USER-IX > USER-MAX                                 
028232           MOVE MFS-RENSA-FAELT                                           
028233             TO MOD-LVL2-IDUSER(RAD-IX, USER-IX)                          
028234           ADD +1 TO USER-IX                                              
028235         END-PERFORM                                                      
028236       END-IF                                                             
028237       ADD +1 TO RAD-IX                                                   
028240     END-PERFORM                                                          
028241****LEVEL3*****                                                           
028250     MOVE +1 TO LVL3-IX                                                   
028260     PERFORM UNTIL LVL3-IX > LVL3-MAX                                     
028270       MOVE MFS-RENSA-FAELT                                               
028280         TO MOD-LVL3-IDDC(LVL3-IX)                                        
028290       ADD +1 TO LVL3-IX                                                  
028291     END-PERFORM                                                          
028300     .                                                                    
028400     EJECT                                                                
028410 FB-LAES-LVL2 SECTION.                                                    
028420                                                                          
028421****LEVEL1*****                                                           
028422     MOVE '11'          TO MOD-LVL1-IDDC                                  
028423****LEVEL2*****                                                           
028425     MOVE MSGI-IDDC-KEY           TO W-6332-IDDC                          
028431     PERFORM IMS-GU-WDGX6332                                              
028440     IF SEGMENT-FINNS                                                     
028441       MOVE +1 TO RAD-IX                                                  
028450       MOVE 6332-IDDC      TO MOD-LVL2-IDDC(RAD-IX)                       
028451       MOVE 6332-KDDC      TO MOD-KDDC(RAD-IX)                            
028452       MOVE +1 TO USER-IX                                                 
028453       PERFORM UNTIL USER-IX > USER-MAX                                   
028454         MOVE 6332-IDUSER(USER-IX)                                        
028455           TO MOD-LVL2-IDUSER(RAD-IX, USER-IX)                            
028456         ADD +1 TO USER-IX                                                
028457       END-PERFORM                                                        
028458       ADD +1 TO RAD-IX                                                   
028470       PERFORM UNTIL RAD-IX > RAD-MAX                                     
028499           MOVE MFS-RENSA-FAELT TO MOD-LVL2-IDDC(RAD-IX)                  
028500                                     MOD-KDDC(RAD-IX)                     
028501           MOVE +1 TO USER-IX                                             
028502           PERFORM UNTIL USER-IX > USER-MAX                               
028503             MOVE MFS-RENSA-FAELT                                         
028504               TO MOD-LVL2-IDUSER(RAD-IX, USER-IX)                        
028505             ADD +1 TO USER-IX                                            
028506           END-PERFORM                                                    
028508         ADD +1 TO RAD-IX                                                 
028509       END-PERFORM                                                        
028510****LEVEL3*****                                                           
028512       PERFORM IMS-GNP-WDGX6334                                           
028518       MOVE +1 TO LVL3-IX                                                 
028519       PERFORM UNTIL LVL3-IX > LVL3-MAX                                   
028520         IF SEGMENT-FINNS                                                 
028521           MOVE 6334-IDDC                                                 
028522             TO MOD-LVL3-IDDC(LVL3-IX)                                    
028523           PERFORM IMS-GNP-WDGX6334                                       
028524         ELSE                                                             
028525           MOVE MFS-RENSA-FAELT                                           
028526             TO MOD-LVL3-IDDC(LVL3-IX)                                    
028527         END-IF                                                           
028528         ADD +1 TO LVL3-IX                                                
028529       END-PERFORM                                                        
028530     END-IF                                                               
028531     .                                                                    
028532     EJECT                                                                
028533 FC-LAES-LVL3 SECTION.                                                    
028534                                                                          
028535****LEVEL1*****                                                           
028536     MOVE '11'          TO MOD-LVL1-IDDC                                  
028537****LEVEL2*****                                                           
028538     MOVE 6332-KEY-FB-AREA-IDDC  TO W-6332-IDDC                           
028539     PERFORM IMS-GU-WDGX6332                                              
028540     IF SEGMENT-FINNS                                                     
028542       MOVE +1 TO RAD-IX                                                  
028543       MOVE 6332-IDDC      TO MOD-LVL2-IDDC(RAD-IX)                       
028544       MOVE 6332-KDDC      TO MOD-KDDC(RAD-IX)                            
028545       MOVE +1 TO USER-IX                                                 
028546       PERFORM UNTIL USER-IX > USER-MAX                                   
028547         MOVE 6332-IDUSER(USER-IX)                                        
028548           TO MOD-LVL2-IDUSER(RAD-IX, USER-IX)                            
028549         ADD +1 TO USER-IX                                                
028550       END-PERFORM                                                        
028551       ADD +1 TO RAD-IX                                                   
028552       PERFORM UNTIL RAD-IX > RAD-MAX                                     
028553           MOVE MFS-RENSA-FAELT TO MOD-LVL2-IDDC(RAD-IX)                  
028554                                     MOD-KDDC(RAD-IX)                     
028555           MOVE +1 TO USER-IX                                             
028556           PERFORM UNTIL USER-IX > USER-MAX                               
028557             MOVE MFS-RENSA-FAELT                                         
028558               TO MOD-LVL2-IDUSER(RAD-IX, USER-IX)                        
028559             ADD +1 TO USER-IX                                            
028560           END-PERFORM                                                    
028561         ADD +1 TO RAD-IX                                                 
028562       END-PERFORM                                                        
028563****LEVEL3*****                                                           
028564       PERFORM IMS-GNP-WDGX6334-2                                         
028565       MOVE +1 TO LVL3-IX                                                 
028566       PERFORM UNTIL LVL3-IX > LVL3-MAX                                   
028567         IF SEGMENT-FINNS                                                 
028569           MOVE 6334-IDDC                                                 
028570             TO MOD-LVL3-IDDC(LVL3-IX)                                    
028571           PERFORM IMS-GNP-WDGX6334-2                                     
028572         ELSE                                                             
028574           MOVE MFS-RENSA-FAELT                                           
028575             TO MOD-LVL3-IDDC(LVL3-IX)                                    
028576         END-IF                                                           
028577         ADD +1 TO LVL3-IX                                                
028578       END-PERFORM                                                        
028579     END-IF                                                               
028580*    CALL FELLOG                                                          
028581     .                                                                    
028582     EJECT                                                                
028583                                                                          
028590 G-KOLLA-INPUT SECTION.                                                   
028600                                                                          
028700     MOVE JA  TO INDATA-SW                                                
028800     IF MID-INPUT = ALL '+'                                               
028900       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
029000       CALL WMEDKONV USING MED-WMEDAREA                                   
029100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
029200       PERFORM MFS-ROER-EJ-FAELT-IN                                       
029300       PERFORM MFS-ROER-EJ-FAELT-UT                                       
029400       MOVE NEJ TO INDATA-SW                                              
029500     ELSE                                                                 
029600                                                                          
029700       IF MID-CMD = ALL '+'                                               
029800          MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR                         
029900          MOVE NEJ TO INDATA-SW                                           
030000       ELSE                                                               
030010         IF MID-CMD = 'N' OR 'C' OR 'D'                                   
030100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR                      
030101           PERFORM GA-KOLLA-RAD-IN                                        
030110         ELSE                                                             
030120           MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR                        
030130           MOVE NEJ TO INDATA-SW                                          
030200         END-IF                                                           
030210       END-IF                                                             
030300                                                                          
031700       IF MID-LEVEL NOT = ALL '+'                                         
031702         IF MID-LEVEL NOT NUMERIC                                         
031704            MOVE MFS-NUM-FAELT-FEL TO MOD-LEVEL-ATTR                      
031705            MOVE NEJ TO INDATA-SW                                         
031706         ELSE                                                             
031707           IF MID-LEVEL = '2' OR '3'                                      
031709             MOVE MFS-NUM-FAELT-RAETT TO MOD-LEVEL-ATTR                   
031710           ELSE                                                           
031712             MOVE MFS-NUM-FAELT-FEL TO MOD-LEVEL-ATTR                     
031713             MOVE NEJ TO INDATA-SW                                        
031714           END-IF                                                         
031715         END-IF                                                           
031716       ELSE                                                               
031718         MOVE MFS-NUM-FAELT-FEL TO MOD-LEVEL-ATTR                         
031719         MOVE NEJ TO INDATA-SW                                            
031720       END-IF                                                             
032700                                                                          
032701       IF MID-KDDC NOT = ALL '+'                                          
032702         IF MID-KDDC = 'N' OR SPACE                                       
032703           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDDC-ATTR                     
032704         ELSE                                                             
032705           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDDC-ATTR                       
032706           MOVE NEJ TO INDATA-SW                                          
032707         END-IF                                                           
032708       END-IF                                                             
032709                                                                          
032800       IF INDATA-FEL                                                      
032900         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
033000         CALL WMEDKONV USING MED-WMEDAREA                                 
033100         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
033200         PERFORM MFS-ROER-EJ-FAELT-UT                                     
033300         PERFORM MFS-ROER-EJ-FAELT-IN                                     
034400       END-IF                                                             
034500     END-IF                                                               
034600     .                                                                    
034700     EJECT                                                                
034710 GA-KOLLA-RAD-IN SECTION.                                                 
034711                                                                          
034712     IF  MID-NY-IDDC    = ALL '+'                                         
034713     AND MID-UNDER-IDDC = ALL '+'                                         
034714     AND MID-IDUSER(1)  = ALL '+'                                         
034715     AND MID-IDUSER(2)  = ALL '+'                                         
034716     AND MID-IDUSER(3)  = ALL '+'                                         
034717     AND MID-IDUSER(4)  = ALL '+'                                         
034718     AND MID-KDDC       = ALL '+'                                         
034719*      MOVE MFS-ALFA-FAELT-FEL TO MOD-KDDC-ATTR                           
034720       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
034721       CALL WMEDKONV USING MED-WMEDAREA                                   
034722       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
034723       PERFORM MFS-ROER-EJ-FAELT-IN                                       
034724       PERFORM MFS-ROER-EJ-FAELT-UT                                       
034725       MOVE NEJ TO INDATA-SW                                              
034726     END-IF                                                               
034727                                                                          
034728*****NY POST****                                                          
034729     IF MID-CMD = 'N'                                                     
034730       PERFORM GAA-KOLLA-NY                                               
034731     END-IF                                                               
034732*****ÄNDRA POST******                                                     
034733     IF MID-CMD = 'C'                                                     
034734       PERFORM GAB-KOLLA-ANDRA                                            
034735     END-IF                                                               
034736*****DELETA POST******                                                    
034737     IF MID-CMD = 'D'                                                     
034738       PERFORM GAC-KOLLA-DELETA                                           
034739     END-IF                                                               
034740     MOVE +1 TO USER-IX                                                   
034741     PERFORM UNTIL USER-IX > USER-MAX                                     
034742       IF MID-IDUSER(USER-IX) = ALL '+'                                   
034743         MOVE SPACE TO MID-IDUSER(USER-IX)                                
034744       END-IF                                                             
034745       ADD  +1 TO USER-IX                                                 
034746     END-PERFORM                                                          
034747     IF MID-KDDC = ALL '+'                                                
034748       MOVE SPACE TO MID-KDDC                                             
034749     END-IF                                                               
034750     .                                                                    
034751     EJECT                                                                
034752 GAA-KOLLA-NY SECTION.                                                    
034753                                                                          
034754     IF MID-NY-IDDC = ALL '+' OR '11'                                     
034755       MOVE MFS-ALFA-FAELT-FEL TO MOD-NY-IDDC-ATTR                        
034757       MOVE NEJ TO INDATA-SW                                              
034758     ELSE                                                                 
034759       MOVE MID-NY-IDDC   TO W-IDDC-B6                                    
034760       PERFORM IMS-GU-WDB601                                              
034761       IF SEGMENT-SAKNAS                                                  
034762         MOVE NEJ TO INDATA-SW                                            
034764         MOVE MFS-ALFA-FAELT-FEL TO MOD-NY-IDDC-ATTR                      
034766       ELSE                                                               
034767         IF MID-LEVEL = '2' OR '3'                                        
034768           PERFORM IMS-GU-WDGX6331                                        
034769           IF SEGMENT-FINNS                                               
034770             MOVE MID-NY-IDDC TO W-6332-IDDC                              
034771             PERFORM IMS-GNP-WDGX6332                                     
034772             IF SEGMENT-FINNS                                             
034773               MOVE NEJ TO INDATA-SW                                      
034775               MOVE MFS-ALFA-FAELT-FEL TO MOD-NY-IDDC-ATTR                
034776             ELSE                                                         
034777               MOVE MID-NY-IDDC TO W-6334-IDDC                            
034778               PERFORM IMS-GU-WDGX6331                                    
034779               PERFORM IMS-GNP-WDGX6334-2                                 
034780               IF SEGMENT-FINNS                                           
034781                 MOVE NEJ TO INDATA-SW                                    
034783                 MOVE MFS-ALFA-FAELT-FEL TO MOD-NY-IDDC-ATTR              
034784               END-IF                                                     
034785             END-IF                                                       
034786           END-IF                                                         
034787         END-IF                                                           
034788         IF MID-LEVEL = '3'                                               
034789           IF MID-UNDER-IDDC = ALL '+'                                    
034790             MOVE NEJ TO INDATA-SW                                        
034791             MOVE MFS-ALFA-FAELT-FEL TO MOD-UNDER-IDDC-ATTR               
034792           END-IF                                                         
034793         END-IF                                                           
034794       END-IF                                                             
034795     END-IF                                                               
034796     .                                                                    
034797     EJECT                                                                
034798                                                                          
034799 GAB-KOLLA-ANDRA SECTION.                                                 
034800                                                                          
034801     IF MID-LEVEL = '2'                                                   
034802       MOVE MID-NY-IDDC TO W-6332-IDDC                                    
034803       PERFORM IMS-GU-WDGX6332                                            
034804       IF SEGMENT-SAKNAS                                                  
034805         MOVE NEJ TO INDATA-SW                                            
034806         MOVE MFS-ALFA-FAELT-FEL TO MOD-NY-IDDC-ATTR                      
034807       END-IF                                                             
034808     ELSE                                                                 
034809       MOVE NEJ TO INDATA-SW                                              
034810       MOVE MFS-ALFA-FAELT-FEL TO MOD-LEVEL-ATTR                          
034811     END-IF                                                               
034812     .                                                                    
034813     EJECT                                                                
034814                                                                          
034815 GAC-KOLLA-DELETA SECTION.                                                
034816                                                                          
034817     IF MID-LEVEL = '2'                                                   
034818       MOVE MID-NY-IDDC TO W-6332-IDDC                                    
034819       PERFORM IMS-GU-WDGX6332                                            
034820       IF SEGMENT-SAKNAS                                                  
034822         MOVE NEJ TO INDATA-SW                                            
034824         MOVE MFS-ALFA-FAELT-FEL TO MOD-NY-IDDC-ATTR                      
034825       ELSE                                                               
034826         PERFORM IMS-GNP-WDGX6334                                         
034827         IF SEGMENT-FINNS                                                 
034828           MOVE NEJ TO INDATA-SW                                          
034830           MOVE MFS-ALFA-FAELT-FEL TO MOD-NY-IDDC-ATTR                    
034831         END-IF                                                           
034832       END-IF                                                             
034835     END-IF                                                               
034836                                                                          
034837     IF MID-LEVEL = '3'                                                   
034839       MOVE MID-UNDER-IDDC  TO W-6332-IDDC                                
034840       PERFORM IMS-GU-WDGX6332                                            
034841       IF SEGMENT-SAKNAS                                                  
034843         MOVE NEJ TO INDATA-SW                                            
034844         MOVE MFS-ALFA-FAELT-FEL TO MOD-UNDER-IDDC-ATTR                   
034845       END-IF                                                             
034846     END-IF                                                               
034848     .                                                                    
034849     EJECT                                                                
034850                                                                          
034860 H-UPPDATERA SECTION.                                                     
034900                                                                          
034910     IF MID-CMD = 'N'                                                     
034920       IF MID-LEVEL = '2'                                                 
034930         MOVE MID-NY-IDDC            TO 6332-IDDC                         
035000         PERFORM IMS-GU-WDGX6331                                          
035100         IF SEGMENT-FINNS                                                 
035111           MOVE +1 TO USER-IX                                             
035112           PERFORM UNTIL USER-IX > USER-MAX                               
035120             MOVE MID-IDUSER(USER-IX) TO 6332-IDUSER(USER-IX)             
035130             ADD +1 TO USER-IX                                            
035140           END-PERFORM                                                    
035150           MOVE MID-KDDC             TO 6332-KDDC                         
035152           PERFORM IMS-ISRT-WDGX6332                                      
035160         END-IF                                                           
035170       END-IF                                                             
035171       IF MID-LEVEL = '3'                                                 
035173         PERFORM IMS-GU-WDGX6331                                          
035174         IF SEGMENT-FINNS                                                 
035176           MOVE MID-UNDER-IDDC TO W-6332-IDDC                             
035178           PERFORM IMS-GU-WDGX6332                                        
035179           IF SEGMENT-FINNS                                               
035183             MOVE MID-NY-IDDC        TO 6334-IDDC                         
035185             PERFORM IMS-ISRT-WDGX6334                                    
035186           END-IF                                                         
035187         END-IF                                                           
035188       END-IF                                                             
035189     END-IF                                                               
035190                                                                          
035191     IF MID-CMD = 'C'                                                     
035192       IF MID-LEVEL = '2'                                                 
035193         MOVE MID-NY-IDDC            TO 6332-IDDC                         
035194                                        W-6332-IDDC                       
035810         PERFORM IMS-GHU-WDGX6332                                         
035820         IF SEGMENT-FINNS                                                 
035840           MOVE +1 TO USER-IX                                             
035850           PERFORM UNTIL USER-IX > USER-MAX                               
035860             MOVE MID-IDUSER(USER-IX) TO 6332-IDUSER(USER-IX)             
035870             ADD +1 TO USER-IX                                            
035880           END-PERFORM                                                    
035890           MOVE MID-KDDC             TO 6332-KDDC                         
035891           PERFORM IMS-REPL-WDGX6332                                      
035892         END-IF                                                           
035893       END-IF                                                             
035894     END-IF                                                               
035900                                                                          
035910     IF MID-CMD = 'D'                                                     
035920       IF MID-LEVEL = '2'                                                 
035921         MOVE MID-NY-IDDC            TO W-6332-IDDC                       
035930         PERFORM IMS-GHU-WDGX6332                                         
035940         IF SEGMENT-FINNS                                                 
035950           PERFORM IMS-DLET-WDGX6332                                      
035960         END-IF                                                           
035970       END-IF                                                             
035971       IF MID-LEVEL = '3'                                                 
035972         MOVE MID-UNDER-IDDC         TO W-6332-IDDC                       
035973         MOVE MID-NY-IDDC            TO W-6334-IDDC                       
035975         PERFORM IMS-GHU-WDGX6334                                         
035977         IF SEGMENT-FINNS                                                 
035979           PERFORM IMS-DLET-WDGX6334                                      
035980         END-IF                                                           
035981       END-IF                                                             
035982     END-IF                                                               
035990*    CALL FELLOG                                                          
036000     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
036100     CALL WMEDKONV USING MED-WMEDAREA                                     
036200     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
036300     PERFORM MFS-FORM-ATTR                                                
036400     PERFORM MFS-RENSA-FAELT-IN                                           
036700     .                                                                    
036800     EJECT                                                                
036900 MFS-RENSA-FAELT-UT SECTION.                                              
037000                                                                          
037100*    --- ALLA UTDATA-FÄLT                                                 
037101     MOVE MFS-RENSA-FAELT TO MOD-LVL1-IDDC                                
037110     MOVE +1 TO RAD-IX                                                    
037120     PERFORM UNTIL RAD-IX > RAD-MAX                                       
037130        MOVE MFS-RENSA-FAELT TO MOD-LVL2-IDDC(RAD-IX)                     
037140                                MOD-KDDC(RAD-IX)                          
037141        MOVE +1 TO USER-IX                                                
037150        PERFORM UNTIL USER-IX > USER-MAX                                  
037160          MOVE MFS-RENSA-FAELT TO                                         
037170                         MOD-LVL2-IDUSER(RAD-IX, USER-IX)                 
037180          ADD +1 TO USER-IX                                               
037190        END-PERFORM                                                       
037191        ADD +1 TO RAD-IX                                                  
037192     END-PERFORM                                                          
037400     .                                                                    
037500     SKIP3                                                                
037600 MFS-RENSA-FAELT-IN SECTION.                                              
037700                                                                          
037800*    --- ALLA INDATA-FÄLT                                                 
037810     MOVE +1 TO USER-IX                                                   
037820     PERFORM UNTIL USER-IX > USER-MAX                                     
037830       MOVE MFS-RENSA-FAELT TO                                            
037840                      MOD-IDUSER-IN(USER-IX)                              
037850       ADD +1 TO USER-IX                                                  
037860     END-PERFORM                                                          
037900     MOVE MFS-RENSA-FAELT TO MOD-CMD-IN                                   
038000                             MOD-LEVEL-IN                                 
038010                             MOD-NY-IDDC-IN                               
038020                             MOD-UNDER-IDDC-IN                            
038030                             MOD-KDDC-IN                                  
038100     .                                                                    
038200     EJECT                                                                
038300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
038400                                                                          
038500*    --- ALLA UTDATA-FÄLT                                                 
038510     MOVE MFS-ROER-EJ-FAELT    TO MOD-LVL1-IDDC                           
038520     MOVE +1 TO RAD-IX                                                    
038530     PERFORM UNTIL RAD-IX > RAD-MAX                                       
038540        MOVE MFS-ROER-EJ-FAELT TO MOD-LVL2-IDDC(RAD-IX)                   
038550                                MOD-KDDC(RAD-IX)                          
038560        MOVE +1 TO USER-IX                                                
038570        PERFORM UNTIL USER-IX > USER-MAX                                  
038580          MOVE MFS-ROER-EJ-FAELT   TO                                     
038590                         MOD-LVL2-IDUSER(RAD-IX, USER-IX)                 
038591          ADD +1 TO USER-IX                                               
038592        END-PERFORM                                                       
038593        ADD +1 TO RAD-IX                                                  
038594     END-PERFORM                                                          
038800     .                                                                    
038900     SKIP3                                                                
039000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
039100                                                                          
039200*    --- ALLA INDATA-FÄLT                                                 
039210     MOVE +1 TO USER-IX                                                   
039220     PERFORM UNTIL USER-IX > USER-MAX                                     
039230       MOVE MFS-ROER-EJ-FAELT TO                                          
039240                      MOD-IDUSER-IN(USER-IX)                              
039250       ADD +1 TO USER-IX                                                  
039260     END-PERFORM                                                          
039270     MOVE MFS-ROER-EJ-FAELT   TO MOD-CMD-IN                               
039280                                 MOD-LEVEL-IN                             
039290                                 MOD-NY-IDDC-IN                           
039291                                 MOD-UNDER-IDDC-IN                        
039292                                 MOD-KDDC-IN                              
039500     .                                                                    
039600     EJECT                                                                
039700 MFS-FORM-ATTR SECTION.                                                   
039800                                                                          
039900*    --- ALLA INDATA-FÄLT                                                 
039910     MOVE +1 TO USER-IX                                                   
039920     PERFORM UNTIL USER-IX > USER-MAX                                     
039930       MOVE MFS-FORMATETS-ATTR TO                                         
039940                      MOD-IDUSER-ATTR(USER-IX)                            
039950       ADD +1 TO USER-IX                                                  
039960     END-PERFORM                                                          
039970     MOVE MFS-FORMATETS-ATTR  TO MOD-CMD-ATTR                             
039980                                 MOD-LEVEL-ATTR                           
039990                                 MOD-NY-IDDC-ATTR                         
039991                                 MOD-UNDER-IDDC-ATTR                      
039992                                 MOD-KDDC-ATTR                            
040200     .                                                                    
040300     SKIP2                                                                
041100* --- IMS SEKTIONER ---                                                   
041200     SKIP3                                                                
041300 IMS-GET-MSG SECTION.                                                     
041400                                                                          
041500     MOVE '  QC' TO GODK-STATUSKODER                                      
041600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
041700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
041800     PERFORM IMS-STATUSKONTROLL                                           
041900     .                                                                    
042000     SKIP3                                                                
042100 IMS-INSERT-MSG SECTION.                                                  
042200                                                                          
042400     MOVE 'N' TO MFS-KDHUVOMR                                             
042600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
042700     MOVE SPACE TO GODK-STATUSKODER                                       
042800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
042900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043000     PERFORM IMS-STATUSKONTROLL                                           
043100     .                                                                    
043200     EJECT                                                                
043300 IMS-GU-WDGX6331 SECTION.                                                 
043400                                                                          
043500     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331 ')'                      
043600          DELIMITED BY SIZE INTO SSA1                                     
043700     MOVE '  GE' TO GODK-STATUSKODER                                      
043800     CALL CBLTDLI USING GU 6331-PCB DLI-IO-WDGX6331 SSA1                  
043900     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
044000     PERFORM IMS-STATUSKONTROLL                                           
044100     .                                                                    
044200     EJECT                                                                
044210 IMS-GHU-WDGX6331 SECTION.                                                
044220                                                                          
044230     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331 ')'                      
044240          DELIMITED BY SIZE INTO SSA1                                     
044250     MOVE '  GE' TO GODK-STATUSKODER                                      
044260     CALL CBLTDLI USING GHU 6331-PCB DLI-IO-WDGX6331 SSA1                 
044270     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
044280     PERFORM IMS-STATUSKONTROLL                                           
044290     .                                                                    
044291     EJECT                                                                
044300 IMS-GU-WDGX6332 SECTION.                                                 
044400                                                                          
044410     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331 ')'                      
044420          DELIMITED BY SIZE INTO SSA1                                     
044500     STRING 'WDGX6332(IDDC     =' W-IDDC-6332 ')'                         
044600          DELIMITED BY SIZE INTO SSA2                                     
044700     MOVE '  GE' TO GODK-STATUSKODER                                      
044800     CALL CBLTDLI USING GU 6331-PCB DLI-IO-WDGX6332 SSA1 SSA2             
044900     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
045000     PERFORM IMS-STATUSKONTROLL                                           
045100     .                                                                    
045200     SKIP3                                                                
045201 IMS-GHU-WDGX6332 SECTION.                                                
045202                                                                          
045203     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331 ')'                      
045204          DELIMITED BY SIZE INTO SSA1                                     
045205     STRING 'WDGX6332(IDDC     =' W-IDDC-6332 ')'                         
045206          DELIMITED BY SIZE INTO SSA2                                     
045207     MOVE '  GE' TO GODK-STATUSKODER                                      
045208     CALL CBLTDLI USING GHU 6331-PCB DLI-IO-WDGX6332 SSA1 SSA2            
045209     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
045210     PERFORM IMS-STATUSKONTROLL                                           
045211     .                                                                    
045212     SKIP3                                                                
045213 IMS-GNP-WDGX6332 SECTION.                                                
045214                                                                          
045215     STRING 'WDGX6332(IDDC     =' W-IDDC-6332 ')'                         
045216          DELIMITED BY SIZE INTO SSA1                                     
045217     MOVE '  GE' TO GODK-STATUSKODER                                      
045218     CALL CBLTDLI USING GNP 6331-PCB DLI-IO-WDGX6332 SSA1                 
045219     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
045220     PERFORM IMS-STATUSKONTROLL                                           
045221     .                                                                    
045222     SKIP3                                                                
045223 IMS-GNP-WDGX6332-2 SECTION.                                              
045230                                                                          
045241     MOVE 'WDGX6332 ' TO SSA1                                             
045250     MOVE '  GE' TO GODK-STATUSKODER                                      
045260     CALL CBLTDLI USING GNP 6331-PCB DLI-IO-WDGX6332 SSA1                 
045270     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
045280     PERFORM IMS-STATUSKONTROLL                                           
045290     .                                                                    
045291     SKIP3                                                                
045300 IMS-ISRT-WDGX6332 SECTION.                                               
045400                                                                          
045410     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331 ')'                      
045420          DELIMITED BY SIZE INTO SSA1                                     
045700     MOVE 'WDGX6332 ' TO SSA2                                             
045800     MOVE '  II' TO GODK-STATUSKODER                                      
045900     CALL CBLTDLI USING ISRT 6331-PCB DLI-IO-WDGX6332 SSA1 SSA2           
046000     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
046100     PERFORM IMS-STATUSKONTROLL                                           
046200     .                                                                    
046300     SKIP3                                                                
046400 IMS-REPL-WDGX6332 SECTION.                                               
046500                                                                          
046600     MOVE '  ' TO GODK-STATUSKODER                                        
046700     CALL CBLTDLI USING REPL 6331-PCB DLI-IO-WDGX6332                     
046800     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
046900     PERFORM IMS-STATUSKONTROLL                                           
047000     .                                                                    
047100     SKIP3                                                                
047200 IMS-DLET-WDGX6332 SECTION.                                               
047300                                                                          
047400     MOVE '  ' TO GODK-STATUSKODER                                        
047500     CALL CBLTDLI USING DLET 6331-PCB DLI-IO-WDGX6332                     
047600     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
047700     PERFORM IMS-STATUSKONTROLL                                           
047800     .                                                                    
047900     EJECT                                                                
047994 IMS-GU-WDGX6334 SECTION.                                                 
047995                                                                          
047996     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331 ')'                      
047997          DELIMITED BY SIZE INTO SSA1                                     
047998     STRING 'WDGX6332(IDDC     =' W-IDDC-6332 ')'                         
047999          DELIMITED BY SIZE INTO SSA2                                     
048000     STRING 'WDGX6334(IDDC     =' W-IDDC-6334 ')'                         
048001          DELIMITED BY SIZE INTO SSA3                                     
048002     MOVE '  GE' TO GODK-STATUSKODER                                      
048003     CALL CBLTDLI USING GU 6331-PCB DLI-IO-WDGX6334 SSA1 SSA2 SSA3        
048004     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
048005     PERFORM IMS-STATUSKONTROLL                                           
048006     .                                                                    
048007     SKIP3                                                                
048008 IMS-GHU-WDGX6334 SECTION.                                                
048009                                                                          
048010     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331 ')'                      
048011          DELIMITED BY SIZE INTO SSA1                                     
048012     STRING 'WDGX6332(IDDC     =' W-IDDC-6332 ')'                         
048013          DELIMITED BY SIZE INTO SSA2                                     
048014     STRING 'WDGX6334(IDDC     =' W-IDDC-6334 ')'                         
048015          DELIMITED BY SIZE INTO SSA3                                     
048016     MOVE '  GE' TO GODK-STATUSKODER                                      
048017     CALL CBLTDLI USING GHU 6331-PCB DLI-IO-WDGX6334 SSA1 SSA2            
048018                                                     SSA3                 
048019     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
048020     PERFORM IMS-STATUSKONTROLL                                           
048021     .                                                                    
048022     SKIP3                                                                
048030 IMS-GNP-WDGX6334 SECTION.                                                
048100                                                                          
048310     MOVE 'WDGX6334 ' TO SSA1                                             
048400     MOVE '  GE' TO GODK-STATUSKODER                                      
048500     CALL CBLTDLI USING GNP 6331-PCB DLI-IO-WDGX6334 SSA1                 
048600     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
048700     PERFORM IMS-STATUSKONTROLL                                           
048800     .                                                                    
048900     SKIP3                                                                
048910 IMS-GNP-WDGX6334-2 SECTION.                                              
048920                                                                          
048930     STRING 'WDGX6334(IDDC     =' W-IDDC-6334 ')'                         
048940          DELIMITED BY SIZE INTO SSA1                                     
048950     MOVE '  GE' TO GODK-STATUSKODER                                      
048960     CALL CBLTDLI USING GNP 6331-PCB DLI-IO-WDGX6334 SSA1                 
048970     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
048980     PERFORM IMS-STATUSKONTROLL                                           
048990     .                                                                    
048991     SKIP3                                                                
049000 IMS-ISRT-WDGX6334 SECTION.                                               
049100                                                                          
049110     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331 ')'                      
049120          DELIMITED BY SIZE INTO SSA1                                     
049130     STRING 'WDGX6332(IDDC     =' W-IDDC-6332 ')'                         
049140          DELIMITED BY SIZE INTO SSA2                                     
049400     MOVE 'WDGX6334 ' TO SSA3                                             
049500     MOVE '  II' TO GODK-STATUSKODER                                      
049600     CALL CBLTDLI USING ISRT 6331-PCB DLI-IO-WDGX6334 SSA1 SSA2           
049610                                                      SSA3                
049700     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
049800     PERFORM IMS-STATUSKONTROLL                                           
049900     .                                                                    
050000     SKIP3                                                                
050100 IMS-REPL-WDGX6334 SECTION.                                               
050200                                                                          
050300     MOVE '  ' TO GODK-STATUSKODER                                        
050400     CALL CBLTDLI USING REPL 6331-PCB DLI-IO-WDGX6334                     
050500     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
050600     PERFORM IMS-STATUSKONTROLL                                           
050700     .                                                                    
050800     SKIP3                                                                
050900 IMS-DLET-WDGX6334 SECTION.                                               
051000                                                                          
051100     MOVE '  ' TO GODK-STATUSKODER                                        
051200     CALL CBLTDLI USING DLET 6331-PCB DLI-IO-WDGX6334                     
051300     MOVE 6331-STATUS-CODE TO STATUS-WS                                   
051400     PERFORM IMS-STATUSKONTROLL                                           
051500     .                                                                    
051600     EJECT                                                                
051601 IMS-GU-WDGX6332-PCB2 SECTION.                                            
051602                                                                          
051603     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331 ')'                      
051604          DELIMITED BY SIZE INTO SSA1                                     
051605     STRING 'WDGX6332(IDDC     =' W-IDDC-6332 ')'                         
051606          DELIMITED BY SIZE INTO SSA2                                     
051610     MOVE '  GE' TO GODK-STATUSKODER                                      
051611     CALL CBLTDLI USING GU 63312-PCB DLI-IO-WDGX6334 SSA1 SSA2            
051613     MOVE 63312-STATUS-CODE TO STATUS-WS                                  
051614     PERFORM IMS-STATUSKONTROLL                                           
051615     .                                                                    
051616     SKIP3                                                                
051617 IMS-GNP-WDGX6334-PCB2 SECTION.                                           
051618                                                                          
051623     MOVE 'WDGX6334 ' TO SSA1                                             
051624     MOVE '  GE' TO GODK-STATUSKODER                                      
051625     CALL CBLTDLI USING GNP 63312-PCB DLI-IO-WDGX6334 SSA1                
051627     MOVE 63312-STATUS-CODE TO STATUS-WS                                  
051628     PERFORM IMS-STATUSKONTROLL                                           
051629     .                                                                    
051630     SKIP3                                                                
051631 IMS-GU-WDB601    SECTION.                                                
051632     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
051633          DELIMITED BY SIZE INTO SSA1                                     
051640     MOVE '  GE' TO GODK-STATUSKODER                                      
051650     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
051660     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
051670     PERFORM IMS-STATUSKONTROLL                                           
051680     .                                                                    
051690     EJECT                                                                
051700 IMS-STATUSKONTROLL SECTION.                                              
051800                                                                          
051900     SET STATUS-IX TO 1                                                   
052000     SEARCH GODK-STATUS                                                   
052100       AT END                                                             
052200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
052300         DELIMITED BY SIZE INTO FELTEXT                                   
052400         CALL FELLOG                                                      
052500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
052600         CONTINUE                                                         
052700     END-SEARCH                                                           
052800     .                                                                    
