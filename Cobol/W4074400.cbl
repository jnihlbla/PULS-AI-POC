000010*                                                                         
000020******************************************************************        
000030*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0159      *        
000040******************************************************************        
000050*                                                                         
000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4074400.                                                
000400 AUTHOR.         LARS THELL.                                              
000500 DATE-WRITTEN.   95/05/30.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        VISAR RETURTILLSTÅNDSKÖ, ÄVEN FÖR RETURTERMINALER.               
001000*        ANVÄNDS FÖR BORTTAG AV REGISTRERADE RETURTILLSTÅND.              
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
001300*        PROGRAMMET LÄSER      WLRETE (WDA3)                              
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W4T744                                              
001700*        MID:         W4I74401                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W4O74401                                            
002010*                                                                         
002020*    E-TRACKER: 4230251 2007-03-07                                        
002030*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002601                                                                          
002610*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W4074400'.            
002800                                                                          
002900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003100                                                                          
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  YES                         PIC X       VALUE 'Y'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500 77  W-BORTTAG                   PIC X       VALUE 'B'.                   
003600 77  W-DELETE                    PIC X       VALUE 'D'.                   
003610 77  W-CDC                       PIC X(3)    VALUE 'CDC'.                 
003700                                                                          
003800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004000 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
004100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004200 77  WS-IDDISTR                  PIC  X(4)  VALUE SPACE.                  
004300 77  WS-IDKUNDNR                 PIC  X(6)  VALUE SPACE.                  
004400 77  WS-KDRETSTA                 PIC  X(1)  VALUE SPACE.                  
004500 77  WS-FLSUM                    PIC  X(1)  VALUE SPACE.                  
004600 77  WS-IDANSV                   PIC  X(5)  VALUE SPACE.                  
004700                                                                          
004800 77  W-SND-REG                   PIC  X(1)  VALUE '1'.                    
004900 77  W-SPAR-IDDISTR              PIC S9(5)  VALUE ZERO COMP-3.            
005000 77  W-SPAR-IDKUNDNR             PIC S9(7)  VALUE ZERO COMP-3.            
005100 77  W-SPAR-IDRAPPNR             PIC  9(7)  VALUE ZERO.                   
005110 77  W-SPAR-IDRT                 PIC  X(3)  VALUE SPACE.                  
005120 77  W-SPAR-IDRTLOP              PIC  9(3)  VALUE ZERO.                   
005200 77  W-KDRETSTA-NUM              PIC  X(1)  VALUE ZERO.                   
005300 77  W-KVRT                      PIC S9(5)  VALUE ZERO COMP-3.            
005400 77  W-KVRADER                   PIC S9(5)  VALUE ZERO COMP-3.            
005500 77  W-IDPERSON                  PIC  9(3)  VALUE ZERO.                   
005600 77  W-KDRETSTA                  PIC X       VALUE 'T'.                   
005700     88  W-TERM                              VALUE 'T'.                   
005800     88  W-SAENT                             VALUE 'S'.                   
005900     88  W-LOSS                              VALUE 'L'.                   
006000     88  W-MOTT                              VALUE 'M'.                   
006010     88  W-REC                               VALUE 'R'.                   
006100     88  W-PAAB                              VALUE 'P'.                   
006200                                                                          
006300                                                                          
006400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006500     88  INDATA-OK                           VALUE 'J'.                   
006600     88  INDATA-FEL                          VALUE 'N'.                   
006700                                                                          
006800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006900     88  NYCKLAR-OK                          VALUE 'J'.                   
007000     88  NYCKLAR-FEL                         VALUE 'N'.                   
007100                                                                          
007200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007300     88  EGEN-MID                            VALUE '4744'.                
007400     88  GODK-MID                            VALUE '4744'.                
007500     88  HELP-MID                            VALUE '0551'.                
007600     EJECT                                                                
007610*      --- VALID IDDC CODES                                               
007620*                                                                         
007630*01    -COPY WWDCKONS                                                     
007640       EJECT                                                              
007700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007800 01  GENERELLA-SUBPROGRAM.                                                
007900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     EJECT                                                                
008400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008500*01 -COPY WMEDAREA                                                        
008600     SKIP3                                                                
008700 01  MESSAGE-CODES.                                                       
008800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009500     03  ERR-KOLLI-SAKNAS        PIC X(3)    VALUE '758'.                 
009600     EJECT                                                                
009700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009800*                                                                         
009900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010000     SKIP3                                                                
010100*01 -COPY WMSGINIT                                                        
010200     SKIP3                                                                
010300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010400*                                                                         
010500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010600     SKIP3                                                                
010700*01  MID -COPY W4I74401                                                   
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011000     SKIP3                                                                
011100*01  -COPY WMSGAREA                                                       
011200     EJECT                                                                
011300     03  MOD REDEFINES MSG-AREA.                                          
011400*      05  -COPY W4O74401    -PRE MOD-                                    
011500     EJECT                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011700     SKIP3                                                                
011800*01  -COPY WMFSAREA                                                       
011900     EJECT                                                                
012000                                                                          
012100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012200*                                                                         
012300     EJECT                                                                
012400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012500     SKIP3                                                                
012600 01  W-MINKEY-X.                                                          
012610     03  W-MINKEY-IDTRANS          PIC  X(4)   VALUE '4744'.              
012700     03  W-MINKEY-WDA3D1KY-ENTER.                                         
012800         05  W-MINKEYD1-IDDC       PIC  X(2)          VALUE SPACE.        
012810         05  W-MINKEYD1-KDRETSTA   PIC  X(1)          VALUE ZERO.         
012900         05  W-MINKEYD1-KDARBTYP   PIC  X(8)          VALUE SPACE.        
013000         05  W-MINKEYD1-IDPERSON   PIC S9(3)   COMP-3 VALUE ZERO.         
013010         05  W-MINKEYD1-DARETANK   PIC  9(8)          VALUE ZERO.         
013100         05  W-MINKEYD1-IDRT       PIC  X(3)          VALUE SPACE.        
013200         05  W-MINKEYD1-DASNDDAT   PIC  9(8)          VALUE ZERO.         
013400         05  W-MINKEYD1-IDDISTR    PIC S9(5)   COMP-3 VALUE ZERO.         
013500         05  W-MINKEYD1-IDKUNDNR   PIC S9(7)   COMP-3 VALUE ZERO.         
013510         05  W-MINKEYD1-IDRAPPNR   PIC  9(7)          VALUE ZERO.         
013600         05  W-MINKEYD1-DAREGDAT   PIC  9(8)          VALUE ZERO.         
013610         05  W-MINKEYD1-TIKLOCK    PIC S9(9)   COMP-3 VALUE ZERO.         
013620     03  W-MINKEY-WDA3D1KY-NEXT.                                          
013630         05  W-MINKEYD1-IDDC-NEXT     PIC  X(2)       VALUE SPACE.        
013640         05  W-MINKEYD1-KDRETSTA-NEXT PIC  X(1)        VALUE ZERO.        
013650         05  W-MINKEYD1-KDARBTYP-NEXT PIC  X(8)       VALUE SPACE.        
013660         05  W-MINKEYD1-IDPERSON-NEXT PIC S9(3) COMP-3 VALUE ZERO.        
013670         05  W-MINKEYD1-DARETANK-NEXT PIC  9(8)        VALUE ZERO.        
013680         05  W-MINKEYD1-IDRT-NEXT     PIC  X(3)       VALUE SPACE.        
013690         05  W-MINKEYD1-DASNDDAT-NEXT PIC  9(8)        VALUE ZERO.        
013691         05  W-MINKEYD1-IDDISTR-NEXT  PIC S9(5) COMP-3 VALUE ZERO.        
013692         05  W-MINKEYD1-IDKUNDNR-NEXT PIC S9(7) COMP-3 VALUE ZERO.        
013693         05  W-MINKEYD1-IDRAPPNR-NEXT PIC  9(7)        VALUE ZERO.        
013694         05  W-MINKEYD1-DAREGDAT-NEXT PIC  9(8)        VALUE ZERO.        
013695         05  W-MINKEYD1-TIKLOCK-NEXT  PIC S9(9) COMP-3 VALUE ZERO.        
013700     SKIP3                                                                
013800                                                                          
013900 01  NYCKLAR-TILL-DLI.                                                    
014000     03  W-WDA301KY-X.                                                    
014100         05  W-IDDC              PIC  X(2)          VALUE SPACE.          
014110         05  W-DAREGDAT          PIC  9(8)          VALUE ZERO.           
014200         05  W-TIKLOCK           PIC S9(9)   COMP-3 VALUE ZERO.           
014300                                                                          
014310     03  W-WDA301-TR-X.                                                   
014320         05  W-IDDC-TR           PIC  X(2)          VALUE SPACE.          
014330         05  W-DAREGDAT-TR       PIC  9(8)          VALUE ZERO.           
014340         05  W-TIKLOCK-TR        PIC S9(9)   COMP-3 VALUE ZERO.           
014350                                                                          
014400     03  W-WDA3D1KY-MIN-X.                                                
014500         05  W-IDDC-D1-MIN       PIC  X(2)          VALUE SPACE.          
014510         05  W-KDRETSTA-D1-MIN   PIC  X(1)          VALUE ZERO.           
014600         05  W-KDARBTYP-D1-MIN   PIC  X(8)          VALUE SPACE.          
014700         05  W-IDPERSON-D1-MIN   PIC S9(3)   COMP-3 VALUE ZERO.           
014710         05  W-DARETANK-D1-MIN   PIC  9(8)          VALUE ZERO.           
014800         05  W-IDRT-D1-MIN       PIC  X(3)          VALUE SPACE.          
014900         05  W-DASNDDAT-D1-MIN   PIC  9(8)          VALUE ZERO.           
015100         05  W-IDDISTR-D1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
015200         05  W-IDKUNDNR-D1-MIN   PIC S9(7)   COMP-3 VALUE ZERO.           
015210         05  W-IDRAPPNR-D1-MIN   PIC  9(7)          VALUE ZERO.           
015300         05  W-DAREGDAT-D1-MIN   PIC  9(8)          VALUE ZERO.           
015310         05  W-TIKLOCK-D1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
015400                                                                          
015500     03  W-WDA3D1KY-MAX-X.                                                
015510         05  W-IDDC-D1-MAX       PIC  X(2)          VALUE SPACE.          
015600         05  W-KDRETSTA-D1-MAX   PIC  X(1)          VALUE ZERO.           
015700         05  W-KDARBTYP-D1-MAX   PIC  X(8)          VALUE SPACE.          
015800         05  W-IDPERSON-D1-MAX   PIC S9(3)   COMP-3 VALUE ZERO.           
015810         05  W-DARETANK-D1-MAX   PIC  9(8)          VALUE ZERO.           
015900         05  W-IDRT-D1-MAX       PIC  X(3)          VALUE SPACE.          
016000         05  W-DASNDDAT-C1-MAX   PIC  9(8)          VALUE ZERO.           
016200         05  W-IDDISTR-D1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
016300         05  W-IDKUNDNR-D1-MAX   PIC S9(7)   COMP-3 VALUE ZERO.           
016310         05  W-IDRAPPNR-D1-MAX   PIC  9(7)          VALUE ZERO.           
016400         05  W-DAREGDAT-D1-MAX   PIC  9(8)          VALUE ZERO.           
016410         05  W-TIKLOCK-D1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
016500                                                                          
016600     03  W-WDA3FSEQ-MIN-X.                                                
016700         05  W-IDDC-FSEQ-MIN     PIC  X(2)          VALUE SPACE.          
016710         05  W-IDDISTR-FSEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
016800         05  W-IDKUNDNR-FSEQ-MIN PIC S9(7)   COMP-3 VALUE ZERO.           
016900         05  W-IDRAPPNR-FSEQ-MIN PIC  9(7)   VALUE ZERO.                  
017000                                                                          
017010     03  W-WDA3FSEQ-MAX-X.                                                
017011         05  W-IDDC-FSEQ-MAX     PIC  X(2)          VALUE SPACE.          
017020         05  W-IDDISTR-FSEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
017030         05  W-IDKUNDNR-FSEQ-MAX PIC S9(7)   COMP-3 VALUE ZERO.           
017040         05  W-IDRAPPNR-FSEQ-MAX PIC  9(7)   VALUE ZERO.                  
017060                                                                          
017100     03  W-WDA3F1KY-MIN-X.                                                
017200         05  W-IDDC-F1-MIN       PIC  X(2)          VALUE SPACE.          
017210         05  W-IDDISTR-F1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
017300         05  W-IDKUNDNR-F1-MIN   PIC S9(7)   COMP-3 VALUE ZERO.           
017400         05  W-IDRAPPNR-F1-MIN   PIC  9(7)   VALUE ZERO.                  
017410         05  W-IDRT-F1-MIN       PIC  X(3)          VALUE SPACE.          
017420         05  W-IDRTLOP-F1-MIN    PIC  9(3)          VALUE ZERO.           
017430         05  W-IDKOLLI-F1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
017500         05  W-DAREGDAT-F1-MIN   PIC  9(8)          VALUE ZERO.           
017600         05  W-TIKLOCK-F1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
017700                                                                          
017800     03  W-WDA3F1KY-MAX-X.                                                
017810         05  W-IDDC-F1-MAX       PIC  X(2)          VALUE SPACE.          
017900         05  W-IDDISTR-F1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
018000         05  W-IDKUNDNR-F1-MAX   PIC S9(7)   COMP-3 VALUE ZERO.           
018100         05  W-IDRAPPNR-F1-MAX   PIC  9(7)   VALUE ZERO.                  
018110         05  W-IDRT-F1-MAX       PIC  X(3)          VALUE SPACE.          
018120         05  W-IDRTLOP-F1-MAX    PIC  9(3)          VALUE ZERO.           
018130         05  W-IDKOLLI-F1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
018200         05  W-DAREGDAT-F1-MAX   PIC  9(8)          VALUE ZERO.           
018300         05  W-TIKLOCK-F1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
018400                                                                          
018500     03  W-IDDISTR-MIN-X.                                                 
018600         05  W-IDDISTR-MIN       PIC S9(5)   COMP-3 VALUE ZERO.           
018700                                                                          
018800     03  W-IDDISTR-MAX-X.                                                 
018900         05  W-IDDISTR-MAX       PIC S9(5)   COMP-3 VALUE ZERO.           
019000                                                                          
019100     03  W-IDKUNDNR-MIN-X.                                                
019200         05  W-IDKUNDNR-MIN      PIC S9(7)   COMP-3 VALUE ZERO.           
019300                                                                          
019400     03  W-IDKUNDNR-MAX-X.                                                
019500         05  W-IDKUNDNR-MAX      PIC S9(7)   COMP-3 VALUE ZERO.           
019600                                                                          
019610     03  W-IDRT-MIN-X.                                                    
019620         05  W-IDRT-MIN          PIC  X(3)          VALUE SPACE.          
019630                                                                          
019640     03  W-IDRT-MAX-X.                                                    
019650         05  W-IDRT-MAX          PIC  X(3)          VALUE SPACE.          
019660                                                                          
019700     SKIP2                                                                
019800*    --- STATUS-KOD FRÅN IMS                                              
019900 01  STATUS-WS                   PIC XX.                                  
020000     88  SEGMENT-FINNS                       VALUE '  '.                  
020100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020300     SKIP2                                                                
020400 01  GODK-STATUSKODER.                                                    
020500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020600     SKIP3                                                                
020700 01  SSA1                        PIC X(256).                              
020800     EJECT                                                                
020900*    --- IMS FUNKTIONSKODER                                               
021000*01  -COPY W0003                                                          
021100     EJECT                                                                
021200*    ---  DLI INPUT-OUTPUT AREA                                           
021300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
021400     SKIP3                                                                
021500 01  DLI-IO-AREA.                                                         
021600     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
021700     SKIP3                                                                
021800     03  WLRETC01 REDEFINES IO-AREA.                                      
021900*        05  -COPY WDA301                                                 
022000     EJECT                                                                
022100     03  WLRETC01 REDEFINES IO-AREA.                                      
022200*        05  -COPY WDA3D1                                                 
022300     EJECT                                                                
022400     03  WLRETC01 REDEFINES IO-AREA.                                      
022500*        05  -COPY WDA3F1                                                 
022600     EJECT                                                                
022601 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDA301'.         
022602 01  DLI-IO-WDA301.                                                       
022603*    03  -COPY WDA301   -PRE TR-                                          
022604                                                                          
022610     EJECT                                                                
022700 LINKAGE SECTION.                                                         
022800                                                                          
022900*01  -COPY W0009   -PRE MSG-                                              
023000*01  -COPY W0008   -PRE USEA-                                             
023100     05  FILLER                  PIC X.                                   
023200     EJECT                                                                
023300*01  -COPY W0008  -PRE RETA1-                                             
023400     05  FILLER                  PIC X.                                   
023500     EJECT                                                                
023600*01  -COPY W0008  -PRE RETA2-                                             
023700     05  FILLER                  PIC X.                                   
023800     EJECT                                                                
023900*01  -COPY W0008  -PRE RETE-                                              
024000     05  FILLER                  PIC X.                                   
024100     EJECT                                                                
024200*01  -COPY W0008  -PRE RETG-                                              
024300     05  FILLER                  PIC X.                                   
024400     EJECT                                                                
024500 PROCEDURE DIVISION  USING MSG-PCB   USEA-PCB                             
024600                           RETA1-PCB RETA2-PCB RETE-PCB RETG-PCB.         
024700     ENTRY 'DLITCBL' USING MSG-PCB   USEA-PCB                             
024800                           RETA1-PCB RETA2-PCB RETE-PCB RETG-PCB.         
024900                                                                          
025000     PERFORM IMS-GET-MSG                                                  
025100     IF SEGMENT-FINNS                                                     
025200       PERFORM A-INIT                                                     
025300       PERFORM B-KOLLA-NYCKLAR                                            
025400       IF NYCKLAR-OK                                                      
025500         IF MFS-UPDATE                                                    
025600           PERFORM G-KOLLA-INPUT                                          
025700           IF INDATA-OK                                                   
025800             PERFORM H-UPPDATERA                                          
025900           END-IF                                                         
026000         ELSE                                                             
026100           IF MFS-FIRST                                                   
026200             PERFORM C-FOERSTA-SIDA                                       
026300           ELSE                                                           
026400             IF MFS-NEXT                                                  
026500               PERFORM D-NAESTA-SIDA                                      
026600             ELSE                                                         
026700               PERFORM E-SAMMA-SIDA                                       
026800             END-IF                                                       
026900           END-IF                                                         
027000         END-IF                                                           
027100         IF INDATA-OK                                                     
027200            PERFORM F-LAES-VISA-INFO                                      
027300         END-IF                                                           
027400       END-IF                                                             
027500       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O74401 + 4                      
027600       PERFORM IMS-INSERT-MSG                                             
027700     END-IF                                                               
027800                                                                          
027900     MOVE ZERO TO RETURN-CODE                                             
028000     GOBACK                                                               
028100     .                                                                    
028200     EJECT                                                                
028300 A-INIT SECTION.                                                          
028400                                                                          
028500     IF MSG-DUBBLA-TRANSKODER                                             
028600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I74401                 
028700       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
028800       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
028900     ELSE                                                                 
029000       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I74401                 
029100       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
029200       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
029300     END-IF                                                               
029400                                                                          
029500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
029600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
029700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
029800                                                                          
029900     MOVE LOW-VALUE TO MSG-AREA                                           
030000     MOVE 'W4O74401' TO MFS-IDMOD                                         
030100     MOVE '4744' TO MOD-IDTRANS                                           
030200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
030300                                                                          
030400     IF EGEN-MID OR HELP-MID                                              
030500       CONTINUE                                                           
030600     ELSE                                                                 
030700       MOVE SPACE TO MFS-KDTRTYP                                          
030800       MOVE '7' TO MFS-IDPFK                                              
030900     END-IF                                                               
031000                                                                          
031100     MOVE LOW-VALUE         TO W-WDA3D1KY-MIN-X                           
031110                               W-WDA3F1KY-MIN-X                           
031200                               W-IDDISTR-MIN-X                            
031300                               W-IDKUNDNR-MIN-X                           
031400                                                                          
031500     MOVE HIGH-VALUE        TO W-WDA3D1KY-MAX-X                           
031510                               W-WDA3F1KY-MAX-X                           
031600                               W-IDDISTR-MAX-X                            
031700                               W-IDKUNDNR-MAX-X                           
031800                                                                          
031900     .                                                                    
032000     EJECT                                                                
032100 B-KOLLA-NYCKLAR SECTION.                                                 
032200                                                                          
032300     MOVE ALL '+'                  TO MSGI-WMSGINIT                       
032400     MOVE '001'                    TO MSGI-KDCALL                         
032500     MOVE MSG-SIGNON-USERID        TO MSGI-IDUSER                         
032510     MOVE '4744'               TO MSGI-IDTRANS                            
032520     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
032600     IF EGEN-MID                                                          
032610        IF MID-IDANSV-IN NOT = ALL '+'                                    
032700          MOVE MID-IDANSV-IN(1:3)    TO MSGI-KDARBTYP                     
032800          MOVE MID-IDANSV-IN(4:3)    TO MSGI-IDPERSON                     
032810          MOVE ZERO                  TO MSGI-IDDISTR                      
032820                                        MSGI-IDKUNDNR                     
032830        ELSE                                                              
032840          IF MID-IDDISTR-IN NOT = ALL '+'                                 
032850             MOVE MID-IDDISTR-IN   TO MSGI-IDDISTR                        
032860             IF MID-IDKUNDNR-IN NOT = ALL '+'                             
032870                MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                     
032880             ELSE                                                         
032890                MOVE ZERO            TO MSGI-IDKUNDNR                     
032891             END-IF                                                       
032892             MOVE SPACE              TO MSGI-KDARBTYP                     
032893             MOVE ZERO               TO MSGI-IDPERSON                     
032894          ELSE                                                            
032895            IF MID-IDKUNDNR-IN NOT = ALL '+'                              
032896               MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                      
032897            END-IF                                                        
032898          END-IF                                                          
032899        END-IF                                                            
033100        MOVE MID-KDRETSTA-IN       TO MSGI-KDRETSTA                       
033200     END-IF                                                               
033300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033400                                                                          
033410     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
033420                                                                          
033500     MOVE JA TO NYCKLAR-SW                                                
033600                                                                          
033610     IF MSGI-IDRT-KEY              =  W-CDC                               
033621        MOVE LOW-VALUE             TO W-IDRT-MIN-X                        
033622                                      W-IDRT-MIN-X                        
033624        MOVE HIGH-VALUE            TO W-IDRT-MAX-X                        
033625                                      W-IDRT-MAX-X                        
033630     ELSE                                                                 
033700        MOVE MSGI-IDRT-KEY         TO W-IDRT-D1-MIN                       
033800                                      W-IDRT-D1-MAX                       
033801                                      W-IDRT-MIN-X                        
033802                                      W-IDRT-MAX-X                        
033810     END-IF                                                               
033820     MOVE WC-CDC-SE                TO W-IDDC                              
033830                                      W-IDDC-D1-MIN                       
033840                                      W-IDDC-D1-MAX                       
033850                                      W-IDDC-FSEQ-MIN                     
033860                                      W-IDDC-FSEQ-MAX                     
033870                                      W-IDDC-F1-MIN                       
033880                                      W-IDDC-F1-MAX                       
033900                                                                          
034000     PERFORM BA-KOLLA-IDANSV                                              
034100     PERFORM BB-KOLLA-IDDISTR                                             
034200     PERFORM BC-KOLLA-IDKUNDNR                                            
034300     PERFORM BD-KOLLA-KDRETSTA                                            
034400     PERFORM BE-KOLLA-FLSUM                                               
034500                                                                          
034600     IF GODK-MID OR NYCKLAR-OK                                            
034610        IF MSGI-KDARBTYP = SPACE                                          
034611          MOVE SPACE                TO MOD-IDANSV-UT                      
034620        ELSE                                                              
034621          MOVE MSGI-KDARBTYP        TO MOD-IDANSV-UT(1:3)                 
034622          MOVE MSGI-IDPERSON        TO MOD-IDANSV-UT(4:3)                 
034630        END-IF                                                            
034800        MOVE MSGI-IDDISTR         TO MOD-IDDISTR-UT                       
034900        INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE            
035000        MOVE MSGI-IDKUNDNR        TO MOD-IDKUNDNR-UT                      
035100        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
035200        MOVE MSGI-KDRETSTA        TO MOD-KDRETSTA-UT                      
035300        MOVE WS-FLSUM             TO MOD-FLSUM-UT                         
035400     ELSE                                                                 
035500        MOVE MFS-RENSA-FAELT      TO MOD-IDANSV-UT                        
035600                                     MOD-IDDISTR-UT                       
035700                                     MOD-IDKUNDNR-UT                      
035800                                     MOD-KDRETSTA-UT                      
035900                                     MOD-FLSUM-UT                         
036000     END-IF                                                               
036100                                                                          
036200     IF NYCKLAR-FEL                                                       
036300       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
036400       CALL WMEDKONV USING MED-WMEDAREA                                   
036500       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
036600       PERFORM MFS-RENSA-FAELT-IN                                         
036700       PERFORM MFS-RENSA-FAELT-UT                                         
036800     END-IF                                                               
036900     .                                                                    
037000     EJECT                                                                
037100                                                                          
037200 BA-KOLLA-IDANSV    SECTION.                                              
037300                                                                          
037400*    -- KONTROLL AV IDANSV DVS KDARBTYP OCH IDPERSON                      
037500                                                                          
037600     MOVE MFS-RENSA-FAELT TO MOD-IDANSV-IN                                
037700                                                                          
037800     IF MID-IDANSV-IN     NOT = ALL '+'                                   
037900       MOVE '7'           TO MFS-IDPFK                                    
038000       MOVE SPACE         TO MFS-KDTRTYP                                  
038200     END-IF                                                               
038300                                                                          
038400     IF MSGI-KDARBTYP               NOT = SPACE                           
038500        IF MSGI-IDPERSON            NUMERIC                               
038600           MOVE MSGI-KDARBTYP(1:3)  TO  W-KDARBTYP-D1-MIN                 
038700                                        W-KDARBTYP-D1-MAX                 
038800           MOVE MSGI-IDPERSON       TO  W-IDPERSON-D1-MIN                 
038900                                        W-IDPERSON-D1-MAX                 
039000        ELSE                                                              
039100           MOVE NEJ             TO NYCKLAR-SW                             
039200        END-IF                                                            
039300     END-IF                                                               
039400     .                                                                    
039500     EJECT                                                                
039600                                                                          
039700 BB-KOLLA-IDDISTR  SECTION.                                               
039800                                                                          
039900     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-IN                         
040000                                                                          
040100     IF MID-IDDISTR-IN          NOT = ALL '+'                             
040200       MOVE '7'                 TO MFS-IDPFK                              
040300       MOVE SPACE               TO MFS-KDTRTYP                            
040400     END-IF                                                               
040500                                                                          
040600     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
040700       MOVE MSGI-IDDISTR          TO W-IDDISTR-MIN                        
040800                                     W-IDDISTR-MAX                        
040900     END-IF                                                               
041000                                                                          
041100     .                                                                    
041200     EJECT                                                                
041300                                                                          
041400 BC-KOLLA-IDKUNDNR   SECTION.                                             
041500                                                                          
041600     MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-IN                        
041700                                                                          
041800     IF MID-IDKUNDNR-IN         NOT = ALL '+'                             
041900       MOVE '7'                 TO MFS-IDPFK                              
042000       MOVE SPACE               TO MFS-KDTRTYP                            
042100     END-IF                                                               
042200                                                                          
042300     IF MSGI-IDKUNDNR           NUMERIC AND MSGI-IDKUNDNR > ZERO          
042400       MOVE MSGI-IDKUNDNR       TO W-IDKUNDNR-MIN                         
042500                                   W-IDKUNDNR-MAX                         
042600     END-IF                                                               
042700                                                                          
042800     .                                                                    
042900     EJECT                                                                
043000 BD-KOLLA-KDRETSTA   SECTION.                                             
043100                                                                          
043200     MOVE MFS-RENSA-FAELT       TO MOD-KDRETSTA-IN                        
043300                                                                          
043400     IF MID-KDRETSTA-IN         NOT = ALL '+'                             
043500       MOVE '7'                 TO MFS-IDPFK                              
043600       MOVE SPACE               TO MFS-KDTRTYP                            
043700     END-IF                                                               
043800                                                                          
043900     MOVE MSGI-KDRETSTA         TO W-KDRETSTA                             
044000     IF W-TERM OR W-SAENT OR W-LOSS OR W-MOTT OR W-PAAB OR W-REC          
044100                                                                          
044110       EVALUATE TRUE                                                      
044120         WHEN W-TERM                                                      
044130           MOVE '1'             TO W-KDRETSTA-NUM                         
044131         WHEN W-SAENT                                                     
044132           MOVE '2'             TO W-KDRETSTA-NUM                         
044133         WHEN W-LOSS                                                      
044134           MOVE '3'             TO W-KDRETSTA-NUM                         
044135         WHEN W-MOTT                                                      
044136           MOVE '4'             TO W-KDRETSTA-NUM                         
044137         WHEN W-REC                                                       
044138           MOVE '4'             TO W-KDRETSTA-NUM                         
044139         WHEN W-PAAB                                                      
044140           MOVE '5'             TO W-KDRETSTA-NUM                         
044141       END-EVALUATE                                                       
044150                                                                          
044200       MOVE W-KDRETSTA-NUM      TO W-KDRETSTA-D1-MIN                      
044300                                   W-KDRETSTA-D1-MAX                      
044400     ELSE                                                                 
044500       MOVE NEJ                 TO NYCKLAR-SW                             
044600     END-IF                                                               
044700                                                                          
044800     .                                                                    
044900     EJECT                                                                
045000 BE-KOLLA-FLSUM      SECTION.                                             
045100                                                                          
045200     MOVE MFS-RENSA-FAELT       TO MOD-FLSUM-IN                           
045300                                                                          
045400     IF MID-FLSUM-IN            = ALL '+'                                 
045500       MOVE MID-FLSUM-UT        TO WS-FLSUM                               
045600     ELSE                                                                 
045700       MOVE MID-FLSUM-IN        TO WS-FLSUM                               
045800       MOVE '7'                 TO MFS-IDPFK                              
045900       MOVE SPACE               TO MFS-KDTRTYP                            
046000     END-IF                                                               
046010     IF WS-FLSUM = JA                                                     
046020       CONTINUE                                                           
046030     ELSE                                                                 
046040       MOVE NEJ    TO WS-FLSUM                                            
046050     END-IF                                                               
046100                                                                          
046200     .                                                                    
046300     EJECT                                                                
046400 C-FOERSTA-SIDA SECTION.                                                  
046500                                                                          
046600     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
046700     CALL WMEDKONV USING MED-WMEDAREA                                     
046800     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
046900                                                                          
047000*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
047100     PERFORM MFS-RENSA-FAELT-IN                                           
047200     .                                                                    
047300     EJECT                                                                
047400 D-NAESTA-SIDA SECTION.                                                   
047500                                                                          
047700     MOVE MSGI-SPAR-AREA         TO W-MINKEY-X                            
047701     IF W-MINKEY-IDTRANS = '4744'                                         
047710       MOVE W-MINKEY-WDA3D1KY-NEXT TO W-WDA3D1KY-MIN-X                    
048500     ELSE                                                                 
048600        MOVE LOW-VALUE              TO W-WDA3D1KY-MIN-X                   
048610        MOVE WC-CDC-SE              TO W-IDDC-D1-MIN                      
048700        PERFORM MFS-RENSA-FAELT-IN                                        
048800     END-IF                                                               
048900     .                                                                    
049000     EJECT                                                                
049100 E-SAMMA-SIDA SECTION.                                                    
049200                                                                          
049400     MOVE MSGI-SPAR-AREA         TO W-MINKEY-X                            
049401     IF W-MINKEY-IDTRANS = '4744'                                         
049410       MOVE W-MINKEY-WDA3D1KY-ENTER TO W-WDA3D1KY-MIN-X                   
049411**** FIXAR TILL SÅ ATT MAN BARA FÅR DET STATUSET MAN HAR BEGÄRT           
049420       MOVE W-KDRETSTA-NUM         TO W-KDRETSTA-D1-MIN                   
049430                                      W-KDRETSTA-D1-MAX                   
050200       IF MID-INPUT                =  ALL '+'                             
050300          PERFORM MFS-RENSA-FAELT-IN                                      
050400       ELSE                                                               
050500          MOVE INF-PRESS-PF11      TO MED-IDMFSINF                        
050600          CALL WMEDKONV USING MED-WMEDAREA                                
050700          MOVE MED-MFSINF          TO MOD-TEMFSFEL                        
050800          PERFORM EA-MID-INDATA-TILL-MOD                                  
050900       END-IF                                                             
051000     ELSE                                                                 
051100        MOVE LOW-VALUE           TO W-WDA3D1KY-MIN-X                      
051110        MOVE WC-CDC-SE           TO W-IDDC-D1-MIN                         
051200        PERFORM MFS-RENSA-FAELT-IN                                        
051300     END-IF                                                               
051400     .                                                                    
051500     EJECT                                                                
051600 EA-MID-INDATA-TILL-MOD SECTION.                                          
051700                                                                          
051800     MOVE +1                          TO INDX                             
051900     PERFORM UNTIL INDX               >  MAX-INDX                         
052000        IF MID-KDCMD(INDX)            NOT = ALL '+'                       
052100           MOVE MID-KDCMD(INDX)       TO MOD-KDCMD(INDX)                  
052200           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR(INDX)             
052300        ELSE                                                              
052400           MOVE MFS-RENSA-FAELT       TO MOD-KDCMD(INDX)                  
052500        END-IF                                                            
052600        ADD +1                        TO INDX                             
052700     END-PERFORM                                                          
052800     .                                                                    
052900     EJECT                                                                
053000 F-LAES-VISA-INFO SECTION.                                                
053100                                                                          
053200                                                                          
053300     MOVE ZERO         TO W-SPAR-IDDISTR                                  
053400                          W-SPAR-IDKUNDNR                                 
053500                          W-SPAR-IDRAPPNR                                 
053600                          W-KVRT                                          
053700                          W-KVRADER                                       
053800                                                                          
053900     PERFORM IMS-GU-WLRETE01                                              
054000     PERFORM FA-FIXA-ENTER-KEY                                            
054100                                                                          
054200     IF SEGMENT-SAKNAS                                                    
054300        MOVE ERR-KOLLI-SAKNAS   TO MED-IDMFSFEL                           
054400        CALL WMEDKONV USING MED-WMEDAREA                                  
054500        MOVE MED-MFSFEL         TO MOD-TEMFSFEL                           
054600        PERFORM MFS-RENSA-FAELT-UT                                        
054700     ELSE                                                                 
054800       MOVE +1                  TO INDX                                   
054900                                                                          
055000       PERFORM UNTIL INDX        > MAX-INDX                               
055100         IF SEGMENT-FINNS                                                 
055200            IF SEQD-IDDISTR      =  W-SPAR-IDDISTR AND                    
055300               SEQD-IDKUNDNR     =  W-SPAR-IDKUNDNR AND                   
055400               SEQD-IDRAPPNR     =  W-SPAR-IDRAPPNR AND                   
055401               SEQD-IDRT         =  W-SPAR-IDRT     AND                   
055402               SEQD-IDRTLOP      =  W-SPAR-IDRTLOP                        
055410               CONTINUE                                                   
055600            ELSE                                                          
055700               MOVE SEQD-IDDISTR  TO W-SPAR-IDDISTR                       
055800               MOVE SEQD-IDKUNDNR TO W-SPAR-IDKUNDNR                      
055900               MOVE SEQD-IDRAPPNR TO W-SPAR-IDRAPPNR                      
055910               MOVE SEQD-IDRT     TO W-SPAR-IDRT                          
055920               MOVE SEQD-IDRTLOP  TO W-SPAR-IDRTLOP                       
055930               IF SEQD-IDDISTR > ZERO                                     
056000                 ADD +1             TO W-KVRT                             
056100                 COMPUTE W-KVRADER  =  W-KVRADER + SEQD-KVRADER           
056110               END-IF                                                     
056200                                                                          
056300               PERFORM FB-REDIGERA-MOD                                    
056400                                                                          
056600               ADD 1             TO INDX                                  
056700            END-IF                                                        
056710            PERFORM IMS-GN-WLRETE01                                       
056800         ELSE                                                             
056900            PERFORM FC-RENSA-RAD                                          
057000            ADD 1                TO INDX                                  
057100         END-IF                                                           
057200       END-PERFORM                                                        
057300                                                                          
057400       PERFORM FD-FIXA-NEXT-KEY                                           
057500       IF SEGMENT-FINNS AND WS-FLSUM = JA                                 
057600          PERFORM FE-ADDERA-RT                                            
057700       END-IF                                                             
057800                                                                          
057900       IF WS-FLSUM             =  JA                                      
058000          MOVE W-KVRADER       TO MOD-KVRADER-RT                          
058100          MOVE W-KVRT          TO MOD-KVANT-RT                            
058200       END-IF                                                             
058300                                                                          
058400     END-IF                                                               
058500     MOVE '002'                TO MSGI-KDCALL                             
058600     MOVE '4744'               TO MSGI-IDTRANS                            
058700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
058800     .                                                                    
058900     EJECT                                                                
059000                                                                          
059100 FA-FIXA-ENTER-KEY        SECTION.                                        
059200                                                                          
059300     IF SEGMENT-FINNS                                                     
059400        MOVE SEQD-IDDC              TO W-MINKEYD1-IDDC                    
059410        MOVE SEQD-KDRETSTA          TO W-MINKEYD1-KDRETSTA                
059500        MOVE SEQD-KDARBTYP          TO W-MINKEYD1-KDARBTYP                
059600        MOVE SEQD-IDPERSON          TO W-MINKEYD1-IDPERSON                
059610        MOVE SEQD-DARETANK          TO W-MINKEYD1-DARETANK                
059700        MOVE SEQD-IDRT              TO W-MINKEYD1-IDRT                    
059710        MOVE SEQD-DASNDDAT          TO W-MINKEYD1-DASNDDAT                
059800        MOVE SEQD-IDDISTR           TO W-MINKEYD1-IDDISTR                 
059900        MOVE SEQD-IDKUNDNR          TO W-MINKEYD1-IDKUNDNR                
059910        MOVE SEQD-IDRAPPNR          TO W-MINKEYD1-IDRAPPNR                
060000        MOVE SEQD-DAREGDAT          TO W-MINKEYD1-DAREGDAT                
060100        MOVE SEQD-TIKLOCK           TO W-MINKEYD1-TIKLOCK                 
060200     ELSE                                                                 
060300        MOVE WC-CDC-SE              TO W-MINKEYD1-IDDC                    
060310        MOVE WS-KDRETSTA            TO W-MINKEYD1-KDRETSTA                
060400        MOVE SPACE                  TO W-MINKEYD1-KDARBTYP                
060500        MOVE ZERO                   TO W-MINKEYD1-IDPERSON                
060510                                       W-MINKEYD1-DARETANK                
060600        MOVE SPACE                  TO W-MINKEYD1-IDRT                    
060610        MOVE ZERO                   TO W-MINKEYD1-DASNDDAT                
060700                                       W-MINKEYD1-IDDISTR                 
060800                                       W-MINKEYD1-IDKUNDNR                
060810                                       W-MINKEYD1-IDRAPPNR                
060900                                       W-MINKEYD1-DAREGDAT                
061000                                       W-MINKEYD1-TIKLOCK                 
061100     END-IF                                                               
061110     MOVE '4744'                    TO W-MINKEY-IDTRANS                   
061200     MOVE W-MINKEY-X                TO MSGI-SPAR-AREA                     
061300                                                                          
061400     .                                                                    
061500     EJECT                                                                
061600                                                                          
061700 FB-REDIGERA-MOD         SECTION.                                         
061800                                                                          
062100     MOVE SEQD-IDPERSON    TO MOD-IDPERSON-RET (INDX)                     
062200     MOVE SEQD-IDRT        TO MOD-IDRT     (INDX)                         
062300     MOVE SEQD-DAREGDAT (3:6)    TO MOD-TIREGDAT (INDX)                   
062400     MOVE SEQD-DASNDDAT (3:6)    TO MOD-TISNDDAT (INDX)                   
062500     MOVE SEQD-IDRTLOP     TO MOD-IDRTLOP  (INDX)                         
062600     MOVE SEQD-IDDISTR     TO MOD-IDDISTR  (INDX)                         
062700     MOVE SEQD-IDKUNDNR    TO MOD-IDKUNDNR (INDX)                         
062800     MOVE SEQD-IDRAPPNR    TO MOD-IDRAPPNR (INDX)                         
062900     MOVE SEQD-KVRADER     TO MOD-KVRADER  (INDX)                         
063000     MOVE SEQD-KVKOLLI-AAF TO MOD-KVKOLLI-AAF  (INDX)                     
063010                                                                          
063020     MOVE SEQD-IDDC        TO W-IDDC-TR                                   
063030     MOVE SEQD-DAREGDAT    TO W-DAREGDAT-TR                               
063040     MOVE SEQD-TIKLOCK     TO W-TIKLOCK-TR                                
063050                                                                          
063060     PERFORM IMS-GU-WDA301-TR                                             
063070     IF SEGMENT-FINNS                                                     
063071       MOVE TR-RET-IDRT-TRANSIT   TO MOD-IDRT-TRANSIT  (INDX)             
063072       MOVE TR-RET-TIREGDAT-TRRT  TO MOD-TIREGDAT-TRRT (INDX)             
063073       MOVE TR-RET-TISNDDAT-TRRT  TO MOD-TISNDDAT-TRRT (INDX)             
063080     ELSE                                                                 
063081       MOVE SPACE                 TO MOD-IDRT-TRANSIT  (INDX)             
063082       MOVE ZERO                  TO MOD-TIREGDAT-TRRT (INDX)             
063083                                     MOD-TISNDDAT-TRRT (INDX)             
063090     END-IF                                                               
063100     .                                                                    
063200     EJECT                                                                
063300                                                                          
063400 FC-RENSA-RAD SECTION.                                                    
063500                                                                          
063600     MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-RET (INDX)                      
063700                             MOD-IDRT     (INDX)                          
063800                             MOD-TIREGDAT (INDX)                          
063900                             MOD-TISNDDAT (INDX)                          
064000                             MOD-IDRTLOP  (INDX)                          
064100                             MOD-IDDISTR  (INDX)                          
064200                             MOD-IDKUNDNR (INDX)                          
064300                             MOD-IDRAPPNR (INDX)                          
064400                             MOD-KVRADER  (INDX)                          
064500                             MOD-KVKOLLI-AAF  (INDX)                      
064600     .                                                                    
064700     EJECT                                                                
064800                                                                          
064900 FD-FIXA-NEXT-KEY        SECTION.                                         
065000                                                                          
065100     IF SEGMENT-FINNS                                                     
065200        MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                       
065300        CALL WMEDKONV USING MED-WMEDAREA                                  
065400        MOVE MED-TEMFSINF           TO MOD-TEMFSINF                       
065500                                                                          
065600        MOVE SEQD-IDDC              TO W-MINKEYD1-IDDC-NEXT               
065610        MOVE SEQD-KDRETSTA          TO W-MINKEYD1-KDRETSTA-NEXT           
065700        MOVE SEQD-KDARBTYP          TO W-MINKEYD1-KDARBTYP-NEXT           
065800        MOVE SEQD-IDPERSON          TO W-MINKEYD1-IDPERSON-NEXT           
065810        MOVE SEQD-DARETANK          TO W-MINKEYD1-DARETANK-NEXT           
065900        MOVE SEQD-IDRT              TO W-MINKEYD1-IDRT-NEXT               
065910        MOVE SEQD-DASNDDAT          TO W-MINKEYD1-DASNDDAT-NEXT           
066000        MOVE SEQD-IDDISTR           TO W-MINKEYD1-IDDISTR-NEXT            
066100        MOVE SEQD-IDKUNDNR          TO W-MINKEYD1-IDKUNDNR-NEXT           
066110        MOVE SEQD-IDRAPPNR          TO W-MINKEYD1-IDRAPPNR-NEXT           
066200        MOVE SEQD-DAREGDAT          TO W-MINKEYD1-DAREGDAT-NEXT           
066300        MOVE SEQD-TIKLOCK           TO W-MINKEYD1-TIKLOCK-NEXT            
066400     ELSE                                                                 
066500        MOVE WC-CDC-SE              TO W-MINKEYD1-IDDC-NEXT               
066510        MOVE WS-KDRETSTA            TO W-MINKEYD1-KDRETSTA-NEXT           
066600        MOVE SPACE                  TO W-MINKEYD1-KDARBTYP-NEXT           
066700        MOVE ZERO                   TO W-MINKEYD1-IDPERSON-NEXT           
066710                                       W-MINKEYD1-DARETANK-NEXT           
066800        MOVE SPACE                  TO W-MINKEYD1-IDRT-NEXT               
066810        MOVE ZERO                   TO W-MINKEYD1-DASNDDAT-NEXT           
066900                                       W-MINKEYD1-IDDISTR-NEXT            
067000                                       W-MINKEYD1-IDKUNDNR-NEXT           
067010                                       W-MINKEYD1-IDRAPPNR-NEXT           
067100                                       W-MINKEYD1-DAREGDAT-NEXT           
067200                                       W-MINKEYD1-TIKLOCK-NEXT            
067300     END-IF                                                               
067310     MOVE '4744'                    TO W-MINKEY-IDTRANS                   
067400     MOVE W-MINKEY-X                TO MSGI-SPAR-AREA                     
067500                                                                          
067600     .                                                                    
067700     EJECT                                                                
067800 FE-ADDERA-RT  SECTION.                                                   
067900                                                                          
068000     PERFORM UNTIL SEGMENT-SAKNAS                                         
068100        IF (SEQD-IDDISTR         =  W-SPAR-IDDISTR AND                    
068200           SEQD-IDKUNDNR         =  W-SPAR-IDKUNDNR AND                   
068300           SEQD-IDRAPPNR         =  W-SPAR-IDRAPPNR) OR                   
068310           SEQD-IDDISTR          = ZERO                                   
068400            CONTINUE                                                      
068500        ELSE                                                              
068600            MOVE SEQD-IDDISTR    TO W-SPAR-IDDISTR                        
068700            MOVE SEQD-IDKUNDNR   TO W-SPAR-IDKUNDNR                       
068800            MOVE SEQD-IDRAPPNR   TO W-SPAR-IDRAPPNR                       
068900            ADD +1               TO W-KVRT                                
069000            COMPUTE W-KVRADER    =  W-KVRADER + SEQD-KVRADER              
069100        END-IF                                                            
069200                                                                          
069300        PERFORM IMS-GN-WLRETE01                                           
069400     END-PERFORM                                                          
069500                                                                          
069600     .                                                                    
069700     EJECT                                                                
069800 G-KOLLA-INPUT SECTION.                                                   
069900                                                                          
070000     MOVE JA  TO INDATA-SW                                                
070100                                                                          
070200     PERFORM GA-FORMELL-KONTROLL                                          
070300     IF INDATA-OK                                                         
070400        PERFORM GB-LOGISK-KONTROLL                                        
070500     END-IF                                                               
070600                                                                          
070700     IF INDATA-FEL                                                        
070800        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
070900        CALL WMEDKONV USING MED-WMEDAREA                                  
071000        MOVE MED-MFSFEL           TO MOD-TEMFSFEL                         
071100        PERFORM MFS-ROER-EJ-FAELT-UT                                      
071200        PERFORM MFS-ROER-EJ-FAELT-IN                                      
071300     END-IF                                                               
071400                                                                          
071500     .                                                                    
071600     EJECT                                                                
071700 GA-FORMELL-KONTROLL SECTION.                                             
071800                                                                          
071900     IF MID-INPUT                = ALL '+'                                
072000       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
072100       CALL WMEDKONV USING MED-WMEDAREA                                   
072200       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
072300       PERFORM MFS-ROER-EJ-FAELT-IN                                       
072400       PERFORM MFS-ROER-EJ-FAELT-UT                                       
072500       MOVE NEJ                  TO INDATA-SW                             
072600     ELSE                                                                 
072700       PERFORM GAA-KOLLA-KDCMD                                            
072800     END-IF                                                               
072900                                                                          
073000     .                                                                    
073100     EJECT                                                                
073200 GAA-KOLLA-KDCMD      SECTION.                                            
073300                                                                          
073400     MOVE +1                           TO INDX                            
073500                                                                          
073600     PERFORM UNTIL INDX                 >  MAX-INDX                       
073700        IF MID-KDCMD(INDX)              NOT = ALL '+'                     
073800           IF MID-KDCMD(INDX)           = W-BORTTAG OR W-DELETE           
073900              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR(INDX)           
074000           ELSE                                                           
074100              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR(INDX)           
074200              MOVE NEJ                  TO INDATA-SW                      
074300           END-IF                                                         
074400        END-IF                                                            
074500        ADD +1                          TO INDX                           
074600     END-PERFORM                                                          
074700                                                                          
074800     .                                                                    
074900     EJECT                                                                
075000 GB-LOGISK-KONTROLL SECTION.                                              
075100                                                                          
075200     MOVE +1                           TO INDX                            
075300     PERFORM UNTIL INDX                > MAX-INDX                         
075400        IF MID-KDCMD(INDX)             NOT = ALL '+'                      
075500           IF MID-KDCMD(INDX)          =  W-BORTTAG OR W-DELETE           
075600              PERFORM GBA-KOLLA-VALT-RETURTILLSTAND                       
075700           ELSE                                                           
075800              MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCMD-ATTR(INDX)            
075900              MOVE NEJ                 TO INDATA-SW                       
076000           END-IF                                                         
076100        END-IF                                                            
076200        ADD +1                         TO INDX                            
076300     END-PERFORM                                                          
076400                                                                          
076500     .                                                                    
076600     EJECT                                                                
076700                                                                          
076800 GBA-KOLLA-VALT-RETURTILLSTAND    SECTION.                                
076900                                                                          
077000     MOVE MID-IDDISTR(INDX)        TO W-IDDISTR-FSEQ-MIN                  
077010                                      W-IDDISTR-FSEQ-MAX                  
077100     INSPECT MID-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO           
077200     MOVE MID-IDKUNDNR(INDX)       TO W-IDKUNDNR-FSEQ-MIN                 
077210                                      W-IDKUNDNR-FSEQ-MAX                 
077300     INSPECT MID-IDRAPPNR(INDX) REPLACING LEADING SPACE BY ZERO           
077400     MOVE MID-IDRAPPNR(INDX)       TO W-IDRAPPNR-FSEQ-MIN                 
077410                                      W-IDRAPPNR-FSEQ-MAX                 
077500     PERFORM IMS-GU-SEQF-WLRETA01                                         
077600     IF SEGMENT-FINNS                                                     
077700        PERFORM UNTIL SEGMENT-SAKNAS                                      
077800           IF RET-KDRETSTA         NOT = W-SND-REG OR                     
077900              RET-IDKOLLI          >  ZERO                                
078000              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR(INDX)             
078100              MOVE NEJ             TO INDATA-SW                           
078200           END-IF                                                         
078300           PERFORM IMS-GN-SEQF-WLRETA01                                   
078400        END-PERFORM                                                       
078500     ELSE                                                                 
078600        MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDCMD-ATTR(INDX)               
078700        MOVE NEJ                    TO INDATA-SW                          
078800     END-IF                                                               
078900     .                                                                    
079000     EJECT                                                                
079100                                                                          
079200 H-UPPDATERA SECTION.                                                     
079300                                                                          
079400     PERFORM HA-TAG-BORT-VALDA-TILLSTAND                                  
079500                                                                          
079600     MOVE INF-UPDATE-DONE        TO MED-IDMFSINF                          
079700     CALL WMEDKONV USING MED-WMEDAREA                                     
079800     MOVE MED-MFSINF             TO MOD-TEMFSINF                          
079900     PERFORM MFS-FORM-ATTR                                                
080000     PERFORM MFS-RENSA-FAELT-IN                                           
080100     .                                                                    
080200     EJECT                                                                
080300                                                                          
080400 HA-TAG-BORT-VALDA-TILLSTAND SECTION.                                     
080500                                                                          
080600     MOVE +1                       TO INDX                                
080700     PERFORM UNTIL INDX            >  MAX-INDX                            
080800                                                                          
080900        IF MID-KDCMD(INDX)         = W-BORTTAG OR W-DELETE                
081000           MOVE LOW-VALUE          TO W-WDA3F1KY-MIN-X                    
081100           MOVE HIGH-VALUE         TO W-WDA3F1KY-MAX-X                    
081200           MOVE WC-CDC-SE          TO W-IDDC-F1-MIN                       
081300                                      W-IDDC-F1-MAX                       
081310           MOVE MID-IDDISTR(INDX)  TO W-IDDISTR-F1-MIN                    
081320                                      W-IDDISTR-F1-MAX                    
081400           MOVE MID-IDKUNDNR(INDX) TO W-IDKUNDNR-F1-MIN                   
081500                                      W-IDKUNDNR-F1-MAX                   
081600           MOVE MID-IDRAPPNR(INDX) TO W-IDRAPPNR-F1-MIN                   
081700                                      W-IDRAPPNR-F1-MAX                   
081800           PERFORM IMS-GU-WLRETG01                                        
081900           IF SEGMENT-FINNS                                               
082000              MOVE SEQF-DAREGDAT   TO W-DAREGDAT                          
082100              MOVE SEQF-TIKLOCK    TO W-TIKLOCK                           
082200              PERFORM IMS-GHU-WLRETA01                                    
082300              PERFORM IMS-DLET-WLRETA01                                   
082400           ELSE                                                           
082500              CALL FELLOG                                                 
082600           END-IF                                                         
082700        END-IF                                                            
082800        ADD +1                     TO INDX                                
082900     END-PERFORM                                                          
083000     .                                                                    
083100     EJECT                                                                
083200                                                                          
083300 MFS-RENSA-FAELT-UT SECTION.                                              
083400                                                                          
083500     MOVE +1                    TO INDX                                   
083600     PERFORM UNTIL INDX         >  MAX-INDX                               
083700        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
083800        ADD +1                  TO INDX                                   
083900     END-PERFORM                                                          
084000     .                                                                    
084100     SKIP3                                                                
084200 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
084300                                                                          
084400     MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-RET (INDX)                      
084500                             MOD-IDRT     (INDX)                          
084600                             MOD-TIREGDAT (INDX)                          
084700                             MOD-TISNDDAT (INDX)                          
084800                             MOD-IDRTLOP  (INDX)                          
084900                             MOD-IDDISTR  (INDX)                          
085000                             MOD-IDKUNDNR (INDX)                          
085100                             MOD-IDRAPPNR (INDX)                          
085200                             MOD-KVRADER  (INDX)                          
085300                             MOD-KVKOLLI-AAF  (INDX)                      
085400     .                                                                    
085500     SKIP3                                                                
085600 MFS-RENSA-FAELT-IN SECTION.                                              
085700                                                                          
085800*    --- ALLA INDATA-FÄLT                                                 
085900     MOVE +1 TO INDX                                                      
086000     PERFORM UNTIL INDX         >  MAX-INDX                               
086100       MOVE MFS-RENSA-FAELT     TO MOD-KDCMD(INDX)                        
086200       ADD +1                   TO INDX                                   
086300     END-PERFORM                                                          
086400     .                                                                    
086500     EJECT                                                                
086600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
086700                                                                          
086800     MOVE +1                  TO INDX                                     
086900     PERFORM UNTIL INDX       >  MAX-INDX                                 
087000       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
087100       ADD +1                 TO INDX                                     
087200     END-PERFORM                                                          
087300     .                                                                    
087400                                                                          
087500                                                                          
087600 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
087700                                                                          
087800     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDPERSON-RET (INDX)                   
087900                                MOD-IDRT     (INDX)                       
088000                                MOD-TIREGDAT (INDX)                       
088100                                MOD-TISNDDAT (INDX)                       
088200                                MOD-IDRTLOP (INDX)                        
088300                                MOD-IDDISTR (INDX)                        
088400                                MOD-IDKUNDNR (INDX)                       
088500                                MOD-IDRAPPNR (INDX)                       
088600                                MOD-KVRADER (INDX)                        
088700                                MOD-KVKOLLI-AAF (INDX)                    
088800     .                                                                    
088900                                                                          
089000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
089100                                                                          
089200*    --- ALLA INDATA-FÄLT                                                 
089300     MOVE +1 TO INDX                                                      
089400     PERFORM UNTIL INDX         >  MAX-INDX                               
089500       MOVE MFS-ROER-EJ-FAELT   TO MOD-KDCMD(INDX)                        
089600       ADD +1                   TO INDX                                   
089700     END-PERFORM                                                          
089800     .                                                                    
089900     EJECT                                                                
090000 MFS-FORM-ATTR SECTION.                                                   
090100                                                                          
090200*    --- ALLA INDATA-FÄLT                                                 
090300     MOVE +1 TO INDX                                                      
090400     PERFORM UNTIL INDX         >  MAX-INDX                               
090500       MOVE MFS-FORMATETS-ATTR  TO MOD-KDCMD-ATTR(INDX)                   
090600       ADD +1                   TO INDX                                   
090700     END-PERFORM                                                          
090800     .                                                                    
090900     EJECT                                                                
091000                                                                          
091100* --- IMS SEKTIONER ---                                                   
091200     SKIP3                                                                
091300 IMS-GET-MSG SECTION.                                                     
091400                                                                          
091500     MOVE '  QC' TO GODK-STATUSKODER                                      
091600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
091700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
091800     PERFORM IMS-STATUSKONTROLL                                           
091900     .                                                                    
092000     SKIP3                                                                
092100 IMS-INSERT-MSG SECTION.                                                  
092200                                                                          
092300     IF ENGLISH-TEXT                                                      
092400       MOVE 'N' TO MFS-KDHUVOMR                                           
092500     END-IF                                                               
092600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
092700     MOVE SPACE TO GODK-STATUSKODER                                       
092800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
092900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
093000     PERFORM IMS-STATUSKONTROLL                                           
093100     .                                                                    
093200     EJECT                                                                
093300                                                                          
093400 IMS-GU-WDA301-TR      SECTION.                                           
093500                                                                          
093600     STRING 'WLRETA01(WDA301KY =' W-WDA301-TR-X ')'                       
093700          DELIMITED BY SIZE INTO SSA1                                     
093800     MOVE '  GE'           TO GODK-STATUSKODER                            
093900     CALL CBLTDLI USING GU RETA1-PCB DLI-IO-WDA301 SSA1                   
094000     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
094100     PERFORM IMS-STATUSKONTROLL                                           
094200     .                                                                    
094300                                                                          
094310 IMS-GHU-WLRETA01       SECTION.                                          
094320                                                                          
094330     STRING 'WLRETA01(WDA301KY =' W-WDA301KY-X ')'                        
094340          DELIMITED BY SIZE INTO SSA1                                     
094350     MOVE '  GE'           TO GODK-STATUSKODER                            
094360     CALL CBLTDLI USING GHU RETA1-PCB DLI-IO-AREA SSA1                    
094370     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
094380     PERFORM IMS-STATUSKONTROLL                                           
094390     .                                                                    
094391                                                                          
094400 IMS-DLET-WLRETA01      SECTION.                                          
094500                                                                          
094600     MOVE '    '           TO GODK-STATUSKODER                            
094700     CALL CBLTDLI USING DLET RETA1-PCB DLI-IO-AREA                        
094800     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
094900     PERFORM IMS-STATUSKONTROLL                                           
095000     .                                                                    
095100     EJECT                                                                
095200                                                                          
095300 IMS-GU-SEQF-WLRETA01       SECTION.                                      
095400                                                                          
095500     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
095510                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
095600          DELIMITED BY SIZE INTO SSA1                                     
095700     MOVE '  GE'           TO GODK-STATUSKODER                            
095800     CALL CBLTDLI USING GU RETA2-PCB DLI-IO-AREA SSA1                     
095900     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
096000     PERFORM IMS-STATUSKONTROLL                                           
096100     .                                                                    
096200                                                                          
096300 IMS-GN-SEQF-WLRETA01       SECTION.                                      
096400                                                                          
096410     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
096420                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
096600          DELIMITED BY SIZE INTO SSA1                                     
096700     MOVE '  GEGB'         TO GODK-STATUSKODER                            
096800     CALL CBLTDLI USING GN RETA2-PCB DLI-IO-AREA SSA1                     
096900     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
097000     PERFORM IMS-STATUSKONTROLL                                           
097100     .                                                                    
097200                                                                          
097300                                                                          
097400 IMS-GU-WLRETE01       SECTION.                                           
097500                                                                          
097600     STRING 'WLRETE01(WDA3D1KY>=' W-WDA3D1KY-MIN-X                        
097700                    '&WDA3D1KY<=' W-WDA3D1KY-MAX-X                        
097800                    '&IDRT    >=' W-IDRT-MIN-X                            
097900                    '&IDRT    <=' W-IDRT-MAX-X                            
097910                    '&IDDISTR >=' W-IDDISTR-MIN-X                         
097920                    '&IDDISTR <=' W-IDDISTR-MAX-X                         
098000                    '&IDKUNDNR>=' W-IDKUNDNR-MIN-X                        
098100                    '&IDKUNDNR<=' W-IDKUNDNR-MAX-X ')'                    
098200          DELIMITED BY SIZE INTO SSA1                                     
098300     MOVE '  GE'           TO GODK-STATUSKODER                            
098400     CALL CBLTDLI USING GU RETE-PCB DLI-IO-AREA SSA1                      
098500     MOVE RETE-STATUS-CODE TO STATUS-WS                                   
098600     PERFORM IMS-STATUSKONTROLL                                           
098700     .                                                                    
098800                                                                          
098900 IMS-GN-WLRETE01       SECTION.                                           
099000                                                                          
099100     STRING 'WLRETE01(WDA3D1KY>=' W-WDA3D1KY-MIN-X                        
099200                    '&WDA3D1KY<=' W-WDA3D1KY-MAX-X                        
099210                    '&IDRT    >=' W-IDRT-MIN-X                            
099220                    '&IDRT    <=' W-IDRT-MAX-X                            
099300                    '&IDDISTR >=' W-IDDISTR-MIN-X                         
099400                    '&IDDISTR <=' W-IDDISTR-MAX-X                         
099500                    '&IDKUNDNR>=' W-IDKUNDNR-MIN-X                        
099600                    '&IDKUNDNR<=' W-IDKUNDNR-MAX-X ')'                    
099700          DELIMITED BY SIZE INTO SSA1                                     
099800     MOVE '  GEGB'           TO GODK-STATUSKODER                          
099900     CALL CBLTDLI USING GN RETE-PCB DLI-IO-AREA SSA1                      
100000     MOVE RETE-STATUS-CODE TO STATUS-WS                                   
100100     PERFORM IMS-STATUSKONTROLL                                           
100200     .                                                                    
100300     EJECT                                                                
100400                                                                          
100500 IMS-GU-WLRETG01       SECTION.                                           
100600                                                                          
100700     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
100800                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
100900          DELIMITED BY SIZE INTO SSA1                                     
101000     MOVE '  GE'           TO GODK-STATUSKODER                            
101100     CALL CBLTDLI USING GU RETG-PCB DLI-IO-AREA SSA1                      
101200     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
101300     PERFORM IMS-STATUSKONTROLL                                           
101400     .                                                                    
101500                                                                          
101600                                                                          
101700 IMS-STATUSKONTROLL SECTION.                                              
101800                                                                          
101900     SET STATUS-IX TO 1                                                   
102000     SEARCH GODK-STATUS                                                   
102100       AT END                                                             
102200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
102300         DELIMITED BY SIZE INTO FELTEXT                                   
102400         CALL FELLOG                                                      
102500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
102600         CONTINUE                                                         
102700     END-SEARCH                                                           
102800     .                                                                    
