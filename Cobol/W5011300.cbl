001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W5011300.                                                
001300*AUTHOR.         CHRISTINA BRUHN.                                         
001400*DATE-WRITTEN.   92/10/21.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        FJOLÅRETS ARTIKELINFORMATION                                     
002000*                                                                         
002110*        PROGRAMMET LÄSER      WLARTO (WDK1)                              
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W5T113                                              
002500*        MID:         W5I11301                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W5O11301                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003401                                                                          
003500 77  IDPGM                       PIC X(08)   VALUE 'W5011300'.            
003600                                                                          
003700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004110 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
004120 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
004200                                                                          
004400 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004800                                                                          
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005010 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005100                                                                          
005300                                                                          
005400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005500     88  NYCKLAR-OK                          VALUE 'J'.                   
005600     88  NYCKLAR-FEL                         VALUE 'N'.                   
005700                                                                          
005800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005900     88  EGEN-MID                            VALUE '5113'.                
006000     88  GODK-MID                            VALUE '5111' '5112'          
006100                                                   '5113' '5114'          
006200                                                   '5115' '5116'          
006300                                                   '5117' '5118'          
006400                                                   '5119'.                
006500     88  HELP-MID                            VALUE '0551'.                
006600     EJECT                                                                
006700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006800 01  GENERELLA-SUBPROGRAM.                                                
006900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
007300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007400     EJECT                                                                
007500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007600*01 -COPY WMSGINIT                                                        
007610     EJECT                                                                
007620*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007630*01 -COPY WMEDAREA                                                        
007700     SKIP3                                                                
007800 01  MESSAGE-CODES.                                                       
008100     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
008110     03  ERR-PART-DELETED        PIC X(3)    VALUE '018'.                 
008120     03  ERR-PART-SUPERSEDED     PIC X(3)    VALUE '220'.                 
008200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008210     03  INF-SPEC-COSTS          PIC X(3)    VALUE '217'.                 
008300     EJECT                                                                
008400*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
008500*01 -COPY W009CIA                                                         
008600     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W5I11301                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W5O11301                                                 
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100     SKIP3                                                                
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600     SKIP2                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011001     03  W-IDARTNR-X.                                                     
011010         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(64).                               
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013000     SKIP3                                                                
013100 01  DLI-IO-AREA.                                                         
013200     03  IO-AREA                 PIC X(435)  VALUE SPACE.                 
013301     SKIP3                                                                
013302     03  WLARTO01 REDEFINES IO-AREA.                                      
013310*        05  -COPY WDK101  -PRE ARTO-                                     
013600     EJECT                                                                
013700 LINKAGE SECTION.                                                         
013800                                                                          
013900*01  -COPY W0009   -PRE MSG-                                              
014001     EJECT                                                                
014002*01  -COPY W0008  -PRE USEA-                                              
014010     05  FILLER                  PIC X.                                   
014020     EJECT                                                                
014030*01  -COPY W0008  -PRE ARTO-                                              
014040     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014201 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ARTO-PCB.                     
014210     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ARTO-PCB.                     
014300                                                                          
014500     PERFORM IMS-GET-MSG                                                  
014600     IF SEGMENT-FINNS                                                     
014700       PERFORM A-INIT                                                     
014800       PERFORM B-KOLLA-NYCKLAR                                            
014900       IF NYCKLAR-OK                                                      
015400         PERFORM F-LAES-VISA-INFO                                         
015500       END-IF                                                             
015510       COMPUTE  MSG-KVLL  = LENGTH OF MOD-W5O11301 + 4                    
015700       PERFORM IMS-INSERT-MSG                                             
015800     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I11301                 
016900       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
017000       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I11301                  
017300       MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                   
017400       MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                  
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE  TO MSG-AREA                                          
018200     MOVE 'W5O113N1' TO MFS-IDMOD                                         
018300     MOVE '5113'     TO MOD-IDTRANS                                       
018400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018500                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE TO MFS-KDTRTYP                                          
019000       MOVE '7' TO MFS-IDPFK                                              
019100     END-IF                                                               
019200                                                                          
019300     IF ENGLISH-TEXT                                                      
019400       MOVE +2 TO SPRAK-IX                                                
019500       MOVE 'GB ' TO MED-IDSKYLT                                          
019600     ELSE                                                                 
019700       MOVE +1 TO SPRAK-IX                                                
019800       MOVE 'S  ' TO MED-IDSKYLT                                          
019900     END-IF                                                               
020200     .                                                                    
020300     EJECT                                                                
020400 B-KOLLA-NYCKLAR SECTION.                                                 
020500                                                                          
020600     MOVE JA TO NYCKLAR-SW                                                
020701                                                                          
020703     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
020704                                                                          
020705     MOVE ALL '+' TO MSGI-WMSGINIT                                        
020706     MOVE '001'             TO MSGI-KDCALL                                
020707     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020708     MOVE '5113'            TO MSGI-IDTRANS                               
020709     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020710                                                                          
020711     IF MFS-IDTRANS = '5113'                                              
020712     OR (MID-IDARTNR-IN NUMERIC                                           
020713     AND MID-IDARTNR-IN > ZERO)                                           
020714         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
020715     END-IF                                                               
020716     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020717     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
020718     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
020719                                                                          
020720     IF MID-IDARTNR-IN = ALL '+'                                          
020721       CONTINUE                                                           
020722     ELSE                                                                 
020723       MOVE '7'         TO MFS-IDPFK                                      
020724       MOVE SPACE       TO MFS-KDTRTYP                                    
020725     END-IF                                                               
020726     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
020727       MOVE WS-IDARTNR TO W-IDARTNR                                       
020728     ELSE                                                                 
020729       MOVE NEJ TO NYCKLAR-SW                                             
020730     END-IF                                                               
020801                                                                          
020803     MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                    
020804     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
020900                                                                          
021000     IF NYCKLAR-FEL                                                       
021100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021200       CALL WMEDKONV USING MED-WMEDAREA                                   
021300       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
021500       PERFORM MFS-RENSA-FAELT-UT                                         
021600     END-IF                                                               
021700     .                                                                    
021900     EJECT                                                                
022100 F-LAES-VISA-INFO SECTION.                                                
022200                                                                          
022300     PERFORM IMS-GU-ARTO                                                  
022400                                                                          
022500     IF SEGMENT-SAKNAS                                                    
022600        MOVE ERR-PART-MISSING TO MED-IDMFSFEL                             
022700        CALL WMEDKONV USING MED-WMEDAREA                                  
022800        MOVE MED-MFSFEL       TO MOD-TEMFSFEL                             
022900        PERFORM MFS-RENSA-FAELT-UT                                        
023000     ELSE                                                                 
023010       IF ARTO-ART-KDERS-UTG > +28                                        
023011         MOVE ERR-PART-DELETED TO MED-IDMFSFEL                            
023012         CALL WMEDKONV USING MED-WMEDAREA                                 
023013         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
023014         PERFORM MFS-RENSA-FAELT-UT                                       
023015       ELSE                                                               
023016         IF ARTO-ART-KDERS-UTG > +0                                       
023017           MOVE ERR-PART-SUPERSEDED TO MED-IDMFSFEL                       
023018           CALL WMEDKONV USING MED-WMEDAREA                               
023019           MOVE MED-MFSFEL          TO MOD-TEMFSFEL                       
023020           PERFORM MFS-RENSA-FAELT-UT                                     
023021         ELSE                                                             
023030           MOVE ARTO-ART-REKSIFFR TO MOD-REKSIFFR                         
023060           MOVE ARTO-ART-IDANSK   TO MOD-IDANSK                           
023070           MOVE ARTO-ART-IDINK    TO MOD-IDINK                            
023080           MOVE ZERO              TO MOD-PRHEMTAG                         
023081*          MOVE ARTO-ART-PRHEMTAG TO MOD-PRHEMTAG                         
023090           MOVE ARTO-ART-KDPRODSL TO MOD-KDPRODSL                         
023091           MOVE ARTO-ART-KDTIPPR  TO MOD-KDTIPPR                          
023092           MOVE ARTO-ART-PRARTBES TO MOD-PRARTBES                         
023093           MOVE ARTO-ART-PRARTSJK TO MOD-PRARTSJK                         
023094           MOVE ARTO-ART-PRARTSTD TO MOD-PRARTSTD                         
023095           MOVE ARTO-ART-PRDIRLON TO MOD-PRDIRLON                         
023096           MOVE ARTO-ART-PRDMTRL  TO MOD-PRDMTRL                          
023097           MOVE ARTO-ART-PRINK    TO MOD-PRINK                            
023098           MOVE ARTO-ART-PROVRPAL TO MOD-PROVRPAL                         
023099                                                                          
023100           IF ARTO-ART-FLSPKOST = +1                                      
023101             MOVE INF-SPEC-COSTS TO MED-IDMFSINF                          
023102             CALL WMEDKONV USING MED-WMEDAREA                             
023103             MOVE MED-MFSINF     TO MOD-TEMFSINF                          
023104           END-IF                                                         
023106                                                                          
023107           MOVE +1 TO IX                                                  
023108           PERFORM 6 TIMES                                                
023109             MOVE MFS-BLANKA-UT-FAELT TO MOD-PRIS-BEST(IX)                
023110             ADD +1 TO IX                                                 
023111           END-PERFORM                                                    
023112                                                                          
023113           MOVE +1 TO IX                                                  
023114           PERFORM 4 TIMES                                                
023115             MOVE MFS-BLANKA-UT-FAELT TO MOD-BESTALLNING(IX)              
023116             ADD +1 TO IX                                                 
023117           END-PERFORM                                                    
023118                                                                          
023119           MOVE +1 TO IX                                                  
023120           PERFORM UNTIL IX > +6                                          
023197                                                                          
023198             IF ARTO-ART-IDLEVNR-PR(IX) NOT = SPACE                       
023199               IF ARTO-ART-SUINLEV-PR(IX) > +0 AND                        
023200                  ARTO-ART-KDSTATUS-PR(IX) = +1                           
023201                 MOVE 'INLEV' TO MOD-STATUS-FAELT(IX)                     
023202               ELSE                                                       
023203                 IF ARTO-ART-KDSTATUS-PR(IX) = +1                         
023204                   MOVE 'GODK' TO MOD-STATUS-FAELT(IX)                    
023205                 ELSE                                                     
023206                   MOVE 'PREL' TO MOD-STATUS-FAELT(IX)                    
023207                 END-IF                                                   
023208               END-IF                                                     
023209               MOVE ARTO-ART-TIPRLIST (IX) TO                             
023210                         MOD-TIPRLIST (IX)                                
023211               MOVE ARTO-ART-IDLEVNR-PR (IX) TO                           
023212                         MOD-IDLEVNR-PR (IX)                              
023213               MOVE ARTO-ART-PRARTBES-PR(IX) TO                           
023214                         MOD-PRARTBES-PR(IX)                              
023215               MOVE ARTO-ART-PRARTBEL-PR(IX) TO                           
023216                         MOD-PRARTBEL-PR(IX)                              
023219             END-IF                                                       
023220             ADD +1 TO IX                                                 
023221           END-PERFORM                                                    
023222                                                                          
023223           MOVE +1 TO IX INDX                                             
023224           PERFORM UNTIL IX > +4 OR INDX > +5                             
023231                                                                          
023232             IF ARTO-ART-IDBEST(INDX) > +0                                
023233               IF ARTO-ART-KDBEH-BEST(INDX) = +1                          
023234                 MOVE 'BESTÄLLN ' TO MOD-JUST-FAELT(IX)                   
023235               ELSE                                                       
023236                 IF ARTO-ART-KDBEH-BEST(INDX) = +2                        
023237                   MOVE 'BEKR.BEST' TO MOD-JUST-FAELT(IX)                 
023238                 ELSE                                                     
023239                   IF ARTO-ART-KDBEH-BEST(INDX) = +5                      
023240                     MOVE 'ANNULL' TO MOD-JUST-FAELT(IX)                  
023241                   ELSE                                                   
023242                     MOVE 'BEKR.ANN' TO MOD-JUST-FAELT(IX)                
023243                   END-IF                                                 
023244                 END-IF                                                   
023245               END-IF                                                     
023246               MOVE ARTO-ART-IDBEST (INDX) TO MOD-IDBEST(IX)              
023248               MOVE ARTO-ART-IDLEVNR-BEST(INDX) TO                        
023249                         MOD-IDLEVNR-BEST(IX)                             
023250               MOVE ARTO-ART-KVBEST-BEKR (INDX) TO                        
023251                         MOD-KVBEST-BEKR (IX)                             
023252               MOVE ARTO-ART-KVBEST (INDX) TO MOD-KVBEST(IX)              
023253               MOVE ARTO-ART-TIBEST (INDX) TO MOD-TIBEST(IX)              
023254             ADD +1 TO IX                                                 
023259             END-IF                                                       
023260             ADD +1 TO INDX                                               
023261           END-PERFORM                                                    
023262           IF ARTO-ART-KDAVT = +0 OR +1                                   
023263             IF ARTO-ART-TIAVTAL(1) > +0                                  
023266               MOVE ARTO-ART-IDAVTAL  (1)   TO MOD-IDBEST      (4)        
023268               MOVE ARTO-ART-IDLEVNR-AVT(1) TO MOD-IDLEVNR-BEST(4)        
023269               MOVE ARTO-ART-KVAVTANT (1)   TO MOD-KVBEST      (4)        
023270               MOVE ARTO-ART-TIAVTAL  (1)   TO MOD-TIBEST      (4)        
023275               MOVE 'AVTAL '                TO MOD-JUST-FAELT  (4)        
023276             END-IF                                                       
023277           ELSE                                                           
023278             MOVE SPACE TO MOD-BESTALLNING(4)                             
023279             IF ARTO-ART-KDAVT = +3                                       
023280               MOVE 'KONCERN' TO MOD-JUST-FAELT(4)                        
023281             ELSE                                                         
023282               IF ARTO-ART-KDAVT = +4                                     
023283                 MOVE 'USA / CAN' TO MOD-JUST-FAELT(4)                    
023284               ELSE                                                       
023285                 MOVE ' HF ' TO MOD-JUST-FAELT(4)                         
023286               END-IF                                                     
023287             END-IF                                                       
023288           END-IF                                                         
023289         END-IF                                                           
023290       END-IF                                                             
023291     END-IF                                                               
023300     .                                                                    
023400     EJECT                                                                
024500 MFS-RENSA-FAELT-UT SECTION.                                              
024600     SKIP2                                                                
024700*    --- ALLA UTDATA-FÄLT                                                 
024900     MOVE MFS-RENSA-FAELT TO MOD-REKSIFFR                                 
025110                             MOD-REKSIFFR                                 
025150                             MOD-IDANSK                                   
025160                             MOD-IDINK                                    
025170                             MOD-PRHEMTAG                                 
025180                             MOD-KDPRODSL                                 
025190                             MOD-KDTIPPR                                  
025191                             MOD-PRARTBES                                 
025192                             MOD-PRARTSTD                                 
025193                             MOD-PRDIRLON                                 
025194                             MOD-PRDMTRL                                  
025195                             MOD-PRINK                                    
025196                             MOD-PROVRPAL                                 
025197                             MOD-IDBEST(1)                                
025198                             MOD-IDBEST(2)                                
025199                             MOD-IDBEST(3)                                
025200                             MOD-IDBEST(4)                                
025203                             MOD-IDLEVNR-BEST(1)                          
025204                             MOD-IDLEVNR-BEST(2)                          
025205                             MOD-IDLEVNR-BEST(3)                          
025206                             MOD-IDLEVNR-BEST(4)                          
025209                             MOD-KVBEST-BEKR (1)                          
025210                             MOD-KVBEST-BEKR (2)                          
025211                             MOD-KVBEST-BEKR (3)                          
025212                             MOD-KVBEST-BEKR (4)                          
025215                             MOD-KVBEST      (1)                          
025216                             MOD-KVBEST      (2)                          
025217                             MOD-KVBEST      (3)                          
025218                             MOD-KVBEST      (4)                          
025221                             MOD-TIBEST      (1)                          
025222                             MOD-TIBEST      (2)                          
025223                             MOD-TIBEST      (3)                          
025224                             MOD-TIBEST      (4)                          
025227                             MOD-JUST-FAELT  (1)                          
025228                             MOD-JUST-FAELT  (2)                          
025229                             MOD-JUST-FAELT  (3)                          
025230                             MOD-JUST-FAELT  (4)                          
025233                             MOD-TIPRLIST    (1)                          
025234                             MOD-TIPRLIST    (2)                          
025235                             MOD-TIPRLIST    (3)                          
025236                             MOD-TIPRLIST    (4)                          
025237                             MOD-TIPRLIST    (5)                          
025238                             MOD-TIPRLIST    (6)                          
025239                             MOD-IDLEVNR-PR  (1)                          
025240                             MOD-IDLEVNR-PR  (2)                          
025241                             MOD-IDLEVNR-PR  (3)                          
025242                             MOD-IDLEVNR-PR  (4)                          
025243                             MOD-IDLEVNR-PR  (5)                          
025244                             MOD-IDLEVNR-PR  (6)                          
025245                             MOD-PRARTBES-PR (1)                          
025246                             MOD-PRARTBES-PR (2)                          
025247                             MOD-PRARTBES-PR (3)                          
025248                             MOD-PRARTBES-PR (4)                          
025249                             MOD-PRARTBES-PR (5)                          
025250                             MOD-PRARTBES-PR (6)                          
025251                             MOD-PRARTBEL-PR (1)                          
025252                             MOD-PRARTBEL-PR (2)                          
025253                             MOD-PRARTBEL-PR (3)                          
025254                             MOD-PRARTBEL-PR (4)                          
025255                             MOD-PRARTBEL-PR (5)                          
025256                             MOD-PRARTBEL-PR (6)                          
025257                             MOD-STATUS-FAELT(1)                          
025258                             MOD-STATUS-FAELT(2)                          
025259                             MOD-STATUS-FAELT(3)                          
025260                             MOD-STATUS-FAELT(4)                          
025261                             MOD-STATUS-FAELT(5)                          
025262                             MOD-STATUS-FAELT(6)                          
025270                                                                          
025300     .                                                                    
029200     EJECT                                                                
029300* --- IMS SEKTIONER ---                                                   
029400     SKIP3                                                                
029500 IMS-GET-MSG SECTION.                                                     
029600                                                                          
029700     MOVE '  QC' TO GODK-STATUSKODER                                      
029800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
029900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030000     PERFORM IMS-STATUSKONTROLL                                           
030100     .                                                                    
030200     SKIP3                                                                
030300 IMS-INSERT-MSG SECTION.                                                  
030400                                                                          
030500     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
030600       MOVE '0' TO MFS-KDHUVOMR                                           
030700     END-IF                                                               
030800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
030900     MOVE SPACE TO GODK-STATUSKODER                                       
031000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031200     PERFORM IMS-STATUSKONTROLL                                           
031300     .                                                                    
031401     EJECT                                                                
031402 IMS-GU-ARTO      SECTION.                                                
031403     STRING 'WLARTO01(IDARTNR  =' W-IDARTNR-X ')'                         
031404          DELIMITED BY SIZE INTO SSA1                                     
031405     MOVE '  GE' TO GODK-STATUSKODER                                      
031406     CALL CBLTDLI USING GU ARTO-PCB DLI-IO-AREA SSA1                      
031407     MOVE ARTO-STATUS-CODE TO STATUS-WS                                   
031408     PERFORM IMS-STATUSKONTROLL                                           
031410     .                                                                    
031500     EJECT                                                                
031600 IMS-STATUSKONTROLL SECTION.                                              
031700                                                                          
031800     SET STATUS-IX TO 1                                                   
031900     SEARCH GODK-STATUS                                                   
032000       AT END                                                             
032100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032200         DELIMITED BY SIZE INTO FELTEXT                                   
032300         CALL FELLOG                                                      
032400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032500         CONTINUE                                                         
032600     END-SEARCH                                                           
032700     .                                                                    
