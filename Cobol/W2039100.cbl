000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2039100.                                                
000300 AUTHOR.         ARUP DATTA.                                              
000400 DATE-WRITTEN.   2015/10/05.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        PURCHASE PROPOSAL MENU FOR PARTS REFILLED FROM                   
000900*        DC 71 TO CDC.                                                    
001000*        BY SELECTING A BUYER AND PRESSING PF14 SWITCHES TO               
001100*        SCREEN 2392 AND WE GET DETAILS OF ALL PROPOSALS                  
001200*        FOR THE BUYER.                                                   
001300*                                                                         
001400*        PROGRAMMET READS              WDE3                               
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W2T391                                              
001800*        MID:         W2I39101                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W2O39101                                            
002110*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W2039100'.            
003100                                                                          
003200*    --- WORKING FIELD FOR TEXT MESSAGES FOR ERROR IN CALL/ABEND          
003300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700                                                                          
003800*    --- INDEX FOR REPETITIVE LINES                                       
003900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004000*    --- MAX NUMBER OF REPETITIVE LINES                                   
004100 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
004200*    TOTAL NUMBER OF ROWS ON SCREEN                                       
004300 77  MAX-INDX-SIDA               PIC S9(4)  VALUE +13   COMP SYNC.        
004400                                                                          
004500 01  WS.                                                                  
004600                                                                          
004700*********************************************************                 
004800*    WS-MSGI-AREA-2391                                                    
004900*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
005000*           (I MSGI-SPAR-AREA)                                            
005100*********************************************************                 
005200  05 WS-MSGI-AREA-2391.                                                   
005300    10 WS-MSGI-IDTRANS-2391      PIC X(4)    VALUE '2391'.                
005400    10 WS-MSGI-SSA-KEY-ENTER     PIC X(14)   VALUE SPACE.                 
005500    10 WS-MSGI-SSA-KEY-NEXT      PIC X(14)   VALUE SPACE.                 
005510    10 WS-MSGI-IDDC-REF-2391     PIC X(2)    VALUE SPACE.                 
005600    10 FILLER                    PIC X(166)  VALUE SPACE.                 
005700                                                                          
005800*********************************************************                 
005900*    WS-MSGI-AREA-2392                                                    
006000*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
006100*           (I MSGI-SPAR-AREA)                                            
006200*           WS-MSGI-IDTYPE ANVÄNDS FÖR ATT FÅ UPP RÄTT KÖ                 
006300*********************************************************                 
006400  05 WS-MSGI-AREA-2392.                                                   
006500    10 WS-MSGI-IDTRANS-2392      PIC X(4)    VALUE '2392'.                
006600    10 WS-MSGI-IDARTNR-ENTER     PIC  9(9)   VALUE ZERO.                  
006700    10 WS-MSGI-ORDER-ENTER       PIC X       VALUE SPACE.                 
006800    10 WS-MSGI-IDARTNR-PF7       PIC  9(9)   VALUE ZERO.                  
006900    10 WS-MSGI-ORDER-PF7         PIC X       VALUE SPACE.                 
007000    10 WS-MSGI-IDTYPE            PIC X       VALUE SPACE.                 
007010    10 WS-MSGI-IDDC-REF          PIC X(2)    VALUE SPACE.                 
007100                                                                          
007200  05 WS-WDE301KY-ENTER.                                                   
007300    10 WS-IDDC-ENTER             PIC X(2)    VALUE '11'.                  
007400    10 WS-IDPERSON-BUY-ENTER     PIC S9(3)   VALUE ZERO COMP-3.           
007500    10 WS-KDREFTYP-ENTER         PIC X       VALUE SPACE.                 
007600    10 WS-IDARTNR-ENTER          PIC S9(9)   VALUE ZERO COMP-3.           
007700    10 WS-IDDISTR-ENTER          PIC S9(5)   VALUE ZERO COMP-3.           
007800  05 WS-WDE301KY-NEXT.                                                    
007900    10 WS-IDDC-NEXT              PIC X(2)    VALUE '11'.                  
008000    10 WS-IDPERSON-BUY-NEXT      PIC S9(3)   VALUE ZERO COMP-3.           
008100    10 WS-KDREFTYP-NEXT          PIC X       VALUE SPACE.                 
008200    10 WS-IDARTNR-NEXT           PIC S9(9)   VALUE ZERO COMP-3.           
008300    10 WS-IDDISTR-NEXT           PIC S9(5)   VALUE ZERO COMP-3.           
008400                                                                          
008500  05 WS-IDTYPE                   PIC X       VALUE SPACE.                 
008510  05 WS-IDDC-REF                 PIC X(2)    VALUE SPACE.                 
008600  05 WS-IDDC-CDC                 PIC X(2)    VALUE '11'.                  
008700  05 WS-BUY-TO-REVIEW            PIC S9(6)   VALUE ZERO.                  
008800  05 WS-TOT-TO-REVIEW            PIC S9(6)   VALUE ZERO.                  
008900  05 WS-RED-ANTAL                PIC Z(5)9.                               
009000  05 WS-SPARA-IDPERSON-BUY       PIC S9(3)   VALUE ZERO COMP-3.           
009100  05 WS-SPARA-KDREFTYP           PIC X       VALUE SPACE.                 
009200  05 WS-SPARA-IDARTNR            PIC S9(9)   VALUE ZERO COMP-3.           
009300                                                                          
009400*      --- VALID IDDC CODES                                               
009500*                                                                         
009600*01    -COPY WWDC99                                                       
009700       EJECT                                                              
009800                                                                          
009900*    --- WORKING STORAGE FOR KEY FIELDS FOR SCREEN                        
010000                                                                          
010100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010200     88  NYCKLAR-OK                          VALUE 'J'.                   
010300     88  NYCKLAR-FEL                         VALUE 'N'.                   
010400                                                                          
010500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010600     88  EGEN-MID                            VALUE '2391'.                
010700     88  GODK-MID                            VALUE '2353' '2354'          
010800                                                   '2105' '2107'          
010900                                                   '2341' '2359'.         
011000     88  HELP-MID                            VALUE '0551'.                
011100     EJECT                                                                
011200                                                                          
011300*    --- SUBPROGRAM                                                       
011400 01  GENERELLA-SUBPROGRAM.                                                
011500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012000     EJECT                                                                
012100                                                                          
012200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012300*01 -COPY WMEDAREA                                                        
012400     SKIP3                                                                
012500 01  MESSAGE-CODES.                                                       
012800     03  URVAL-SAKNAS            PIC X(3)    VALUE '005'.                 
012900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
013400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
013800     03  RAD-FINNS               PIC X(3)    VALUE '245'.                 
014200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014400                                                                          
014500 01  MEDDELANDE.                                                          
014600     03  MED-1                   PIC X(30)                                
014700         VALUE 'TYPE : A,B OR C               '.                          
014800     03  MED-2                   PIC X(30)                                
014900         VALUE 'TO VIEW PROPOSAL; PRESS PF14  '.                          
014910     03  MED-3                   PIC X(30)                                
014920         VALUE 'REFILLING DC WRONG            '.                          
015000     EJECT                                                                
015100                                                                          
015200*01  -COPY WDATAREA                                                       
015300     EJECT                                                                
015400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015500*                                                                         
015600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
015700     SKIP3                                                                
015800*01 -COPY WMSGINIT                                                        
015900     SKIP3                                                                
016000*    --- AREA FOR MFS AND SCREEN HANDLING                                 
016100*                                                                         
016200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016300     SKIP3                                                                
016400*01  MID -COPY W2I39101                                                   
016500     EJECT                                                                
016600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016700     SKIP3                                                                
016800*01  -COPY WMSGAREA                                                       
016900     EJECT                                                                
017000     03  MOD REDEFINES MSG-AREA.                                          
017100*      05  -COPY W2O39101                                                 
017200     EJECT                                                                
017300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017400     SKIP3                                                                
017500*01  -COPY WMFSAREA                                                       
017600     EJECT                                                                
017700*    --- WORKING AREA FOR IMS-SECTIONS                                    
017800*                                                                         
017900     EJECT                                                                
018000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018100     SKIP3                                                                
018200 01  NYCKLAR-TILL-DLI.                                                    
018300                                                                          
018400     03 W-WDE301KY-X.                                                     
018500         05  W-IDDC-301          PIC X(2)  VALUE '11'.                    
018600         05  W-IDPERSON-BUY      PIC S9(3) VALUE ZERO COMP-3.             
018700         05  W-KDREFTYP          PIC X     VALUE SPACE.                   
018800         05  W-IDARTNR-301       PIC S9(9) VALUE ZERO COMP-3.             
018900         05  W-IDDISTR           PIC S9(7) VALUE ZERO COMP-3.             
019000                                                                          
019100     03 W-WDE301KY-MIN-X.                                                 
019200         05  W-IDDC-MIN          PIC X(2)  VALUE '11'.                    
019300         05  W-IDPERSON-BUY-MIN  PIC S9(3) VALUE ZERO COMP-3.             
019400         05  W-KDREFTYP-MIN      PIC X     VALUE SPACE.                   
019500         05  W-IDARTNR-MIN       PIC S9(9) VALUE ZERO COMP-3.             
019600         05  W-IDDISTR-MIN       PIC S9(5) VALUE ZERO COMP-3.             
019700                                                                          
019800     03 W-WDE301KY-MAX-X.                                                 
019900         05  W-IDDC-MAX          PIC X(2)  VALUE '11'.                    
020000         05  W-IDPERSON-BUY-MAX  PIC S9(3) VALUE +999 COMP-3.             
020100         05  W-KDREFTYP-MAX      PIC X     VALUE HIGH-VALUE.              
020200         05  W-IDARTNR-MAX       PIC S9(9)                                
020300                                         VALUE +999999999 COMP-3.         
020400         05  W-IDDISTR-MAX       PIC S9(5) VALUE +99999 COMP-3.           
020500                                                                          
020600     03 W-IDDC-B6-X.                                                      
020700         05  W-IDDC-B6           PIC X(2)  VALUE SPACE.                   
020800                                                                          
020900     SKIP2                                                                
021000                                                                          
021100*    --- STATUS-KOD FRÅN IMS                                              
021200 01  STATUS-WS                   PIC XX.                                  
021300     88  SEGMENT-FINNS                     VALUE '  '.                    
021400     88  SEGMENT-FINNS-REDAN               VALUE 'II'.                    
021500     88  SEGMENT-SAKNAS                    VALUE 'GE'                     
021600                                                 'GB'.                    
021700     SKIP2                                                                
021800 01  GODK-STATUSKODER.                                                    
021900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022000     SKIP3                                                                
022100 01  SSA1                        PIC X(64).                               
022200 01  SSA2                        PIC X(64).                               
022300     EJECT                                                                
022400                                                                          
022500*    --- IMS FUNKTION CODES                                               
022600*01  -COPY W0003                                                          
022700     EJECT                                                                
022800                                                                          
022900*    ---  DLI INPUT-OUTPUT AREA                                           
023000 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDE301'.           
023100 01  DLI-IO-WDE301.                                                       
023200*        05  -COPY WDE301                                                 
023300     EJECT                                                                
023400     SKIP3                                                                
023500 01  FILLER                  PIC X(16)   VALUE 'WDB601 AREA'.             
023600 01   DLI-IO-AREA-B601.                                                   
023700*     03  -COPY WDB601                                                    
023800     EJECT                                                                
023900                                                                          
024000 LINKAGE SECTION.                                                         
024100*01  -COPY W0009   -PRE MSG-                                              
024200*01  -COPY W0008   -PRE USEA-                                             
024300     05  FILLER                  PIC X.                                   
024400     EJECT                                                                
024500*01  -COPY W0008   -PRE WDE3-                                             
024600     05  FILLER                  PIC X.                                   
024700     EJECT                                                                
024800*01  -COPY W0008   -PRE WDB6-                                             
024900     05  FILLER                  PIC X.                                   
025000     EJECT                                                                
025100                                                                          
025200 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDE3-PCB.                     
025300                                                                          
025400 MAIN SECTION.                                                            
025500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDE3-PCB.                     
025600                                                                          
025700                                                                          
025800     PERFORM IMS-GET-MSG                                                  
025900     IF SEGMENT-FINNS                                                     
026000       PERFORM A-INIT                                                     
026100       PERFORM B-KOLLA-NYCKLAR                                            
026200       IF NYCKLAR-OK                                                      
026300         IF MFS-FIRST                                                     
026400           PERFORM C-FIRST-PAGE                                           
026500         ELSE                                                             
026600           IF MFS-NEXT                                                    
026700             PERFORM D-NEXT-PAGE                                          
026800           ELSE                                                           
026900             PERFORM E-SAME-PAGE                                          
027000           END-IF                                                         
027100         END-IF                                                           
027200         PERFORM F-READ-SHOW-INFO                                         
027300                                                                          
027400         MOVE WS-MSGI-AREA-2391 TO MSGI-SPAR-AREA                         
027500         MOVE '002'             TO MSGI-KDCALL                            
027600         MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                      
027700         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
027800         MOVE '2391'            TO MSGI-IDTRANS                           
028000         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
028100                                                                          
028200       END-IF                                                             
028300       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O39101 + 4                      
028400       PERFORM IMS-INSERT-MSG                                             
028500     END-IF                                                               
028600                                                                          
028700     MOVE ZERO TO RETURN-CODE                                             
028800     GOBACK                                                               
028900     .                                                                    
029000     EJECT                                                                
029100                                                                          
029200 A-INIT SECTION.                                                          
029300                                                                          
029400     IF MSG-DUBBLA-TRANSKODER                                             
029500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I39101                 
029600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
029700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
029800     ELSE                                                                 
029900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I39101                  
030000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
030100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
030200     END-IF                                                               
030300                                                                          
030400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
030500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
030600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
030700                                                                          
030800     MOVE LOW-VALUE   TO MSG-AREA                                         
030900     MOVE 'W2O391N1'  TO MFS-IDMOD                                        
031000     MOVE '2391'      TO MOD-IDTRANS                                      
031100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
031200                                                                          
031300     IF EGEN-MID OR HELP-MID                                              
031400       CONTINUE                                                           
031500     ELSE                                                                 
031600       MOVE 'A'              TO MID-IDTYPE-2391-IN                        
031610       MOVE SPACE            TO MID-IDDC-REF-2391-IN                      
031700       MOVE SPACE TO MFS-KDTRTYP                                          
031800       MOVE '7' TO MFS-IDPFK                                              
031900     END-IF                                                               
032000     .                                                                    
032100     EJECT                                                                
032200                                                                          
032300 B-KOLLA-NYCKLAR SECTION.                                                 
032400                                                                          
032500******   UPPDATERING AV MSGI-BLÄDDRINGSNYCKLAR SKER                       
032600******   I SLUTET AV PROGRAMMET                                           
032700     MOVE ALL '+'               TO MSGI-WMSGINIT                          
032800     MOVE '001'                 TO MSGI-KDCALL                            
032900     MOVE MSG-LTERM-NAME        TO MSGI-IDLTERM-USER                      
033000     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
033100     MOVE '2391'                TO MSGI-IDTRANS                           
033200     IF EGEN-MID                                                          
033300       MOVE WS-IDDC-CDC         TO MSGI-IDDC-KEY                          
033400     END-IF                                                               
033500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033600                                                                          
033700     IF W-IDTRANS                = '2392'                                 
033800     AND MSGI-SPAR-AREA(1:4)     = '2392'                                 
033900*                                                                         
034000*      -- FÖR ATT HÄMTA IDTYPE                                            
034100*                                                                         
034200       MOVE MSGI-SPAR-AREA      TO WS-MSGI-AREA-2392                      
034300       MOVE WS-MSGI-IDTYPE      TO WS-IDTYPE                              
034320       MOVE WS-MSGI-IDDC-REF    TO WS-IDDC-REF                            
034400                                                                          
034500     ELSE                                                                 
034600                                                                          
034700       IF EGEN-MID                                                        
034800         IF MSGI-SPAR-AREA(1:4)  = '2391'                                 
034900           MOVE MSGI-SPAR-AREA                                            
035000                                TO WS-MSGI-AREA-2391                      
035100         END-IF                                                           
035200       END-IF                                                             
035300                                                                          
035400       IF MID-IDTYPE-2391-IN     = ALL '+'                                
035500         IF MID-IDTYPE-2391-UT   = 'BOAT '                                
035600           MOVE 'B'             TO WS-IDTYPE                              
035700         END-IF                                                           
035800         IF MID-IDTYPE-2391-UT   = 'AIR  '                                
035900           MOVE 'A'             TO WS-IDTYPE                              
036000         END-IF                                                           
036100         IF MID-IDTYPE-2391-UT   = 'AIRCR'                                
036200           MOVE 'C'             TO WS-IDTYPE                              
036300         END-IF                                                           
036400       ELSE                                                               
036500         MOVE MID-IDTYPE-2391-IN                                          
036600                                TO WS-IDTYPE                              
036700       END-IF                                                             
036701                                                                          
036710       IF MID-IDDC-REF-2391-IN   = ALL '+'                                
036711         IF MID-IDDC-REF-2391-UT NOT = ALL '+'                            
036712            MOVE MID-IDDC-REF-2391-UT                                     
036713                             TO WS-IDDC-REF                               
036714         END-IF                                                           
036720*         MOVE SPACE TO       WS-IDDC-REF                                 
036730       ELSE                                                               
036740          MOVE MID-IDDC-REF-2391-IN  TO WS-IDDC-REF                       
036750       END-IF                                                             
036800     END-IF                                                               
036900                                                                          
037000     MOVE JA                    TO NYCKLAR-SW                             
037100                                                                          
037300                                                                          
037400     IF    WS-IDTYPE = 'A'                                                
037500     OR    WS-IDTYPE = 'C'                                                
037600     OR    WS-IDTYPE = 'B'                                                
037700       IF WS-IDTYPE = 'A'                                                 
037800         MOVE 'AIR'          TO MOD-IDTYPE-UT                             
037900       END-IF                                                             
038000       IF WS-IDTYPE = 'C'                                                 
038100         MOVE 'AIRCR'        TO MOD-IDTYPE-UT                             
038200       END-IF                                                             
038300       IF WS-IDTYPE = 'B'                                                 
038400         MOVE 'BOAT'         TO MOD-IDTYPE-UT                             
038500       END-IF                                                             
038600     ELSE                                                                 
038700       MOVE NEJ TO NYCKLAR-SW                                             
038800       MOVE MED-1            TO MOD-TEMFSINF                              
038900     END-IF                                                               
039000                                                                          
039010     MOVE WS-IDDC-REF TO WS-IDDC                                          
039020     IF GOOD-DC OR WS-IDDC-REF = SPACE                                    
039030       MOVE WS-IDDC       TO  MOD-IDDC-REF-UT                             
039040     ELSE                                                                 
039050       MOVE NEJ TO NYCKLAR-SW                                             
039051       MOVE MED-3            TO MOD-TEMFSINF                              
039060     END-IF                                                               
039100                                                                          
039110     MOVE WS-IDDC-CDC           TO WS-IDDC                                
039120                                                                          
039200     IF NYCKLAR-FEL                                                       
039300       MOVE 'GB '         TO MED-IDSKYLT                                  
039400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
039500       CALL WMEDKONV USING MED-WMEDAREA                                   
039600       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
039700       PERFORM MFS-RENSA-FAELT-IN                                         
039800       PERFORM MFS-RENSA-FAELT-UT                                         
039900     END-IF                                                               
040000     .                                                                    
040100     EJECT                                                                
040200                                                                          
040300 C-FIRST-PAGE   SECTION.                                                  
040400                                                                          
040500     MOVE 'GB '           TO MED-IDSKYLT                                  
040600     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
040700     CALL WMEDKONV USING MED-WMEDAREA                                     
040800     MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                                  
040900                                                                          
041000*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
041100     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
041200                                WS-MSGI-SSA-KEY-NEXT                      
041210                                WS-MSGI-IDDC-REF-2391                     
041300     PERFORM MFS-RENSA-FAELT-IN                                           
041400     .                                                                    
041500     EJECT                                                                
041600                                                                          
041700 D-NEXT-PAGE   SECTION.                                                   
041800                                                                          
041900     IF WS-MSGI-SSA-KEY-NEXT NOT = SPACE                                  
042000       MOVE WS-MSGI-SSA-KEY-NEXT                                          
042100                             TO W-WDE301KY-MIN-X                          
042101       IF WS-MSGI-IDDC-REF-2391 NOT = SPACE                               
042102         MOVE WS-MSGI-IDDC-REF-2391  TO WS-IDDC-REF                       
042103       END-IF                                                             
042104     END-IF                                                               
042105*    CALL FELLOG                                                          
042300     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
042400                                WS-MSGI-SSA-KEY-NEXT                      
042410                                WS-MSGI-IDDC-REF-2391                     
042500     .                                                                    
042600     EJECT                                                                
042700                                                                          
042800 E-SAME-PAGE  SECTION.                                                    
042900                                                                          
043000     IF WS-MSGI-SSA-KEY-ENTER NOT = SPACE                                 
043100       MOVE WS-MSGI-SSA-KEY-ENTER                                         
043200                             TO W-WDE301KY-MIN-X                          
043340     END-IF                                                               
043400     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
043500                                WS-MSGI-SSA-KEY-NEXT                      
043600     MOVE MED-2              TO MOD-TEMFSINF                              
043700     .                                                                    
043800     EJECT                                                                
043900                                                                          
044170 F-READ-SHOW-INFO SECTION.                                                
044180                                                                          
044200     MOVE +1                 TO INDX                                      
044300     PERFORM IMS-GU-WDE3-MIN-MAX                                          
044400     IF SEGMENT-FINNS                                                     
044500       MOVE REF-IDDC         TO WS-IDDC-ENTER                             
044600       MOVE REF-IDPERSON-BUY TO WS-IDPERSON-BUY-ENTER                     
044700       MOVE REF-KDREFTYP     TO WS-KDREFTYP-ENTER                         
044800       MOVE REF-IDARTNR      TO WS-IDARTNR-ENTER                          
044900       MOVE REF-IDDISTR      TO WS-IDDISTR-ENTER                          
045000       MOVE WS-WDE301KY-ENTER                                             
045100                             TO WS-MSGI-SSA-KEY-ENTER                     
045110       MOVE WS-IDDC-REF      TO WS-MSGI-IDDC-REF-2391                     
045200                                                                          
045300       PERFORM UNTIL INDX    > MAX-INDX                                   
045400       OR SEGMENT-SAKNAS                                                  
045500           MOVE REF-IDPERSON-BUY                                          
045600                             TO WS-SPARA-IDPERSON-BUY                     
045700           MOVE REF-KDREFTYP TO WS-SPARA-KDREFTYP                         
045800           MOVE ZERO         TO WS-BUY-TO-REVIEW                          
045900           PERFORM UNTIL SEGMENT-SAKNAS                                   
046000           OR NOT (REF-IDPERSON-BUY = WS-SPARA-IDPERSON-BUY               
046100           AND REF-KDREFTYP  =  WS-SPARA-KDREFTYP)                        
046110             IF WS-IDDC-REF = SPACE                                       
046200               IF REF-KDREFORS = 'P'                                      
046300               AND WS-IDTYPE =  REF-KDREFTYP                              
046400               AND WS-IDDC-CDC = REF-IDDC                                 
046500                 ADD +1      TO WS-BUY-TO-REVIEW                          
046600                                  WS-TOT-TO-REVIEW                        
046700                 MOVE REF-IDARTNR                                         
046800                               TO WS-SPARA-IDARTNR                        
046900                 PERFORM UNTIL SEGMENT-SAKNAS                             
047000                 OR NOT (REF-IDPERSON-BUY = WS-SPARA-IDPERSON-BUY         
047100                 AND REF-KDREFTYP = WS-SPARA-KDREFTYP                     
047200                 AND REF-IDARTNR = WS-SPARA-IDARTNR)                      
047300                   PERFORM IMS-GN-WDE3-MIN-MAX                            
047400                 END-PERFORM                                              
047500               ELSE                                                       
047600                 PERFORM IMS-GN-WDE3-MIN-MAX                              
047700               END-IF                                                     
047710             ELSE                                                         
047711               IF REF-KDREFORS = 'P'                                      
047712               AND WS-IDTYPE =  REF-KDREFTYP                              
047713               AND WS-IDDC-CDC = REF-IDDC                                 
047714               AND WS-IDDC-REF = REF-IDDC-REF                             
047715                 ADD +1      TO WS-BUY-TO-REVIEW                          
047716                                  WS-TOT-TO-REVIEW                        
047717                 MOVE REF-IDARTNR                                         
047718                               TO WS-SPARA-IDARTNR                        
047719                 PERFORM UNTIL SEGMENT-SAKNAS                             
047720                 OR NOT (REF-IDPERSON-BUY = WS-SPARA-IDPERSON-BUY         
047721                 AND REF-KDREFTYP = WS-SPARA-KDREFTYP                     
047722                 AND REF-IDARTNR = WS-SPARA-IDARTNR)                      
047723                   PERFORM IMS-GN-WDE3-MIN-MAX                            
047724                 END-PERFORM                                              
047725               ELSE                                                       
047726                 PERFORM IMS-GN-WDE3-MIN-MAX                              
047727               END-IF                                                     
047730             END-IF                                                       
047800                                                                          
047900           END-PERFORM                                                    
048000                                                                          
048100           IF WS-BUY-TO-REVIEW > ZERO                                     
048200             MOVE WS-SPARA-IDPERSON-BUY                                   
048300                             TO MOD-IDPERSON-BUY (INDX)                   
048400             MOVE MFS-ADD-LAES-IN-FAELT                                   
048500                             TO MOD-IDPERSON-BUY-ATTR (INDX)              
048600             MOVE WS-BUY-TO-REVIEW                                        
048700                             TO WS-RED-ANTAL                              
048800             MOVE WS-RED-ANTAL                                            
048900                             TO MOD-TO-REVIEW (INDX)                      
049000             ADD 1           TO INDX                                      
049100           END-IF                                                         
049200       END-PERFORM                                                        
049300                                                                          
049400       IF SEGMENT-FINNS                                                   
049500         MOVE WS-IDDC-CDC    TO WS-IDDC-NEXT                              
049600         MOVE REF-IDPERSON-BUY                                            
049700                             TO WS-IDPERSON-BUY-NEXT                      
049800         MOVE REF-KDREFTYP   TO WS-KDREFTYP-NEXT                          
049900         MOVE REF-IDARTNR    TO WS-IDARTNR-NEXT                           
050000         MOVE REF-IDDISTR    TO WS-IDDISTR-NEXT                           
050100         MOVE WS-WDE301KY-NEXT                                            
050200                             TO WS-MSGI-SSA-KEY-NEXT                      
050210         MOVE WS-IDDC-REF    TO WS-MSGI-IDDC-REF-2391                     
050300         MOVE 'GB '          TO MED-IDSKYLT                               
050400         MOVE INF-MORE-INFO-EXISTS                                        
050500                             TO MED-IDMFSFEL                              
050600         CALL WMEDKONV USING MED-WMEDAREA                                 
050700         MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                              
050800       ELSE                                                               
050900         MOVE SPACE          TO WS-KDREFTYP-NEXT                          
051000         MOVE WS-IDDC-CDC    TO WS-IDDC-NEXT                              
051100         MOVE ZERO           TO WS-IDPERSON-BUY-NEXT                      
051200                                WS-IDARTNR-NEXT                           
051300                                WS-IDDISTR-NEXT                           
051400         MOVE ZERO           TO W-IDPERSON-BUY-MIN                        
051500                                W-IDARTNR-MIN                             
051600         MOVE SPACE          TO WS-MSGI-SSA-KEY-NEXT                      
051700         MOVE ZERO           TO WS-TOT-TO-REVIEW                          
051800         PERFORM IMS-GU-WDE3-MIN-MAX                                      
051900                                                                          
052000         PERFORM UNTIL SEGMENT-SAKNAS                                     
052100           IF REF-KDREFORS = 'P'                                          
052200           AND WS-IDTYPE   = REF-KDREFTYP                                 
052300           AND WS-IDDC-CDC = REF-IDDC                                     
052310             IF WS-IDDC-REF = SPACE                                       
052400               ADD +1        TO WS-TOT-TO-REVIEW                          
052410             ELSE                                                         
052420                IF WS-IDDC-REF = REF-IDDC-REF                             
052421                  ADD +1     TO WS-TOT-TO-REVIEW                          
052430                END-IF                                                    
052440             END-IF                                                       
052500             MOVE REF-IDPERSON-BUY                                        
052600                             TO WS-SPARA-IDPERSON-BUY                     
052700             MOVE REF-KDREFTYP                                            
052800                             TO WS-SPARA-KDREFTYP                         
052900             MOVE REF-IDARTNR                                             
053000                             TO WS-SPARA-IDARTNR                          
053100                                                                          
053200             PERFORM UNTIL SEGMENT-SAKNAS                                 
053300             OR NOT (REF-IDPERSON-BUY = WS-SPARA-IDPERSON-BUY             
053400             AND REF-KDREFTYP = WS-SPARA-KDREFTYP                         
053500             AND REF-IDARTNR = WS-SPARA-IDARTNR)                          
053600                                                                          
053700               PERFORM IMS-GN-WDE3-MIN-MAX                                
053800             END-PERFORM                                                  
053900           ELSE                                                           
054000             PERFORM IMS-GN-WDE3-MIN-MAX                                  
054100           END-IF                                                         
054200                                                                          
054300         END-PERFORM                                                      
054400         ADD +1              TO INDX                                      
054500         MOVE WS-TOT-TO-REVIEW                                            
054600                             TO WS-RED-ANTAL                              
054700         MOVE WS-RED-ANTAL   TO MOD-TO-REVIEW (INDX)                      
054800       END-IF                                                             
054900     ELSE                                                                 
055000       MOVE SPACE            TO WS-MSGI-SSA-KEY-ENTER                     
055100       IF MFS-FIRST                                                       
055200         MOVE 'GB '          TO MED-IDSKYLT                               
055300         MOVE URVAL-SAKNAS   TO MED-IDMFSFEL                              
055400         CALL WMEDKONV USING MED-WMEDAREA                                 
055500         MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                              
055600       END-IF                                                             
055700     END-IF                                                               
055800     .                                                                    
055900     EJECT                                                                
055901                                                                          
056110 MFS-RENSA-FAELT-UT SECTION.                                              
056200                                                                          
056300*    --- ALLA UTDATA-FÄLT                                                 
056400     MOVE MFS-RENSA-FAELT    TO MOD-IDTYPE-UT                             
056410                                MOD-IDDC-REF-UT                           
056500                                                                          
056600     MOVE 1                  TO INDX                                      
056700     PERFORM UNTIL INDX > 13                                              
056800       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
056900       ADD 1                 TO INDX                                      
057000     END-PERFORM                                                          
057100     .                                                                    
057200     SKIP3                                                                
057300                                                                          
057400 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
057500                                                                          
057600*    --- ALLA UTDATA-FÄLT                                                 
057700                                                                          
057800     MOVE MFS-RENSA-FAELT    TO MOD-SELECT (INDX)                         
057900                                MOD-IDPERSON-BUY (INDX)                   
058000                                MOD-TO-REVIEW (INDX)                      
058100     .                                                                    
058200     SKIP3                                                                
058300                                                                          
058400 MFS-RENSA-FAELT-IN SECTION.                                              
058500                                                                          
058600*    --- ALLA INDATA-FÄLT                                                 
058700     MOVE MFS-RENSA-FAELT    TO MOD-IDTYPE-IN                             
058710                                MOD-IDDC-REF-IN                           
058800                                                                          
058900     MOVE 1                  TO INDX                                      
059000     PERFORM UNTIL INDX > 13                                              
059100       MOVE MFS-RENSA-FAELT  TO MOD-SELECT (INDX)                         
059200       ADD 1                 TO INDX                                      
059300     END-PERFORM                                                          
059400     .                                                                    
059500     EJECT                                                                
059600                                                                          
059700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
059800                                                                          
059900*    --- ALLA UTDATA-FÄLT                                                 
060000     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDTYPE-UT                             
060010                                MOD-IDDC-REF-UT                           
060100                                MOD-TEMFSINF                              
060200     MOVE +1 TO INDX                                                      
060300     PERFORM UNTIL INDX > MAX-INDX                                        
060400       MOVE MFS-ROER-EJ-FAELT                                             
060500                             TO MOD-SELECT (INDX)                         
060600                                MOD-IDPERSON-BUY (INDX)                   
060700                                MOD-TO-REVIEW (INDX)                      
060800       ADD +1 TO INDX                                                     
060900     END-PERFORM                                                          
061000     .                                                                    
061100     SKIP2                                                                
061200                                                                          
061300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
061400                                                                          
061500*    --- ALLA INDATA-FÄLT                                                 
061600     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDTYPE-IN                             
061610                                MOD-IDDC-REF-IN                           
061700     .                                                                    
061800     EJECT                                                                
061900* --- IMS SEKTIONER ---                                                   
062000     SKIP3                                                                
062100                                                                          
062200 IMS-GET-MSG SECTION.                                                     
062300                                                                          
062400     MOVE '  QC' TO GODK-STATUSKODER                                      
062500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
062600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
062700     PERFORM IMS-STATUSKONTROLL                                           
062800     .                                                                    
062900     SKIP3                                                                
063000                                                                          
063100 IMS-INSERT-MSG SECTION.                                                  
063200                                                                          
063300     IF ENGLISH-TEXT                                                      
063400       MOVE 'N' TO MFS-KDHUVOMR                                           
063500     END-IF                                                               
063600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
063700     MOVE SPACE TO GODK-STATUSKODER                                       
063800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
063900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
064000     PERFORM IMS-STATUSKONTROLL                                           
064100     .                                                                    
064200     EJECT                                                                
064300                                                                          
064400 IMS-GU-WDE3-MIN-MAX SECTION.                                             
064500                                                                          
064510     STRING 'WDE301  (WDE301KY>=' W-WDE301KY-MIN-X                        
064700                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
064800          DELIMITED BY SIZE INTO SSA1                                     
064900     MOVE '  GE' TO GODK-STATUSKODER                                      
065000     CALL CBLTDLI USING GU WDE3-PCB DLI-IO-WDE301 SSA1                    
065100     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
065200     PERFORM IMS-STATUSKONTROLL                                           
065300     .                                                                    
065400     SKIP3                                                                
065500                                                                          
065600 IMS-GN-WDE3-MIN-MAX SECTION.                                             
065700                                                                          
065710     STRING 'WDE301  (WDE301KY>=' W-WDE301KY-MIN-X                        
065900                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
066000          DELIMITED BY SIZE INTO SSA1                                     
066100     MOVE '  GE' TO GODK-STATUSKODER                                      
066200     CALL CBLTDLI USING GN WDE3-PCB DLI-IO-WDE301 SSA1                    
066300     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
066400     PERFORM IMS-STATUSKONTROLL                                           
066500     .                                                                    
066600     EJECT                                                                
066700                                                                          
067800 IMS-STATUSKONTROLL SECTION.                                              
067900                                                                          
068000     SET STATUS-IX TO 1                                                   
068100     SEARCH GODK-STATUS                                                   
068200       AT END                                                             
068300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
068400         DELIMITED BY SIZE INTO FELTEXT                                   
068500         CALL FELLOG                                                      
068600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
068700         CONTINUE                                                         
068800     END-SEARCH                                                           
068900     .                                                                    
