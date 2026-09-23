000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2038100.                                                
000400 AUTHOR.         JOHAN NIHLBLAD                                           
000500 DATE-WRITTEN.   06/02/14.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        ÖVERSIKTSBILD SOM VISAR HUR MÅNGA ORDERFÖRSLAG                   
001000*        PER BUYER SOM ÅTERSTÅR ATT BEDÖMMA .                             
001100*        GENOM ATT ANGE ETT 'S' FRAMFÖR BUYER OCH TRYCKA PF14             
001200*        KOMMER MAN ÖVER TILL 2382 OCH FÅR UPP FÖRSTA ICKE                
001300*        BEDÖMDA ORDERFÖRSLAG FÖR ANGIVEN DC-GRUPP                        
001400*                                                                         
001500*        PROGRAMMET LÄSER                                                 
001600*                              WDE3B  (WDE3) SEK. INDEX                   
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W2T381                                              
002000*        MID:         W2I38101                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W2O38101                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W2038100'.            
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900                                                                          
004000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004200*    FÖR ATT GARANTERA PLATS FÖR TOTALEN                                  
004300*    SÄTTS MAX-INDX TILL 11                                               
004400 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
004500*    TOTALT ANTALET RADER PÅ BILDEN ÄR 13                                 
004600 77  MAX-INDX-SIDA               PIC S9(4)  VALUE +13   COMP SYNC.        
004700*                                                                         
004800 77  TAB-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
004900*                                                                         
005000 77  VISA-IX                     PIC 9(4).                                
005100*                                                                         
005200 77  DC-IX                       PIC S9(4)  VALUE +0    COMP SYNC.        
005300*                                                                         
005400 77  MAX-DC-IX                   PIC S9(4)  VALUE +6    COMP SYNC.        
005500                                                                          
005600 01  WS.                                                                  
005700  05 FILLER                      PIC X(16)   VALUE                        
005800                                             'WS-IMS-SEKTION'.            
005900  05 WS-IMS-SEKTION              PIC X(24)   VALUE SPACE.                 
006000  05 FILLER                      PIC X(16)   VALUE                        
006100                                             'WS-DB2-SEKTION'.            
006200  05 WS-DB2-SEKTION              PIC X(24)   VALUE SPACE.                 
006300*********************************************************                 
006400*    WS-MSGI-AREA-2381                                                    
006500*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
006600*           (I MSGI-SPAR-AREA)                                            
006700*********************************************************                 
006800  05 WS-MSGI-AREA-2381.                                                   
006900    10 WS-MSGI-IDTRANS-2381      PIC X(4)    VALUE '2381'.                
007000    10 WS-VISA-IX-ENTER          PIC 9(4)    VALUE ZERO.                  
007100    10 WS-VISA-IX-NEXT           PIC 9(4)    VALUE ZERO.                  
007200    10 WS-MSGI-ANTAL-FORSLAG-TOT PIC 9(7)    VALUE ZERO.                  
007300    10 WS-MSGI-GRP-TAB.                                                   
007400     12 FILLER                   OCCURS 50.                               
007500       15 WS-MSGI-IDLOPNR-DC-TAB PIC S9(7)   VALUE ZERO COMP-3.           
007600       15 WS-MSGI-FLAGGA-TRAEFF  PIC X       VALUE 'N'.                   
007700       15 WS-MSGI-ANTAL-FORSLAG  PIC 9(7)    VALUE ZERO.                  
007800       15 WS-MSGI-IDDC-TAB       OCCURS 6                                 
007900                                    PIC X(2)  VALUE SPACE.                
008000                                                                          
008100*********************************************************                 
008200*    WS-MSGI-AREA-2382                                                    
008300*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
008400*           (I MSGI-SPAR-AREA)                                            
008500*           WS-MSGI-IDTYPE ANVÄNDS FÖR ATT FÅ UPP RÄTT KÖ                 
008600*********************************************************                 
008700  05 WS-MSGI-AREA-2382.                                                   
008800    10 WS-MSGI-IDTRANS-2382      PIC X(4)    VALUE '2382'.                
008900    10 WS-MSGI-IDARTNR-ENTER     PIC  9(9)   VALUE ZERO.                  
009000    10 WS-MSGI-ORDER-ENTER       PIC X       VALUE SPACE.                 
009100    10 WS-MSGI-IDARTNR-PF7       PIC  9(9)   VALUE ZERO.                  
009200    10 WS-MSGI-ORDER-PF7         PIC X       VALUE SPACE.                 
009300    10 WS-MSGI-IDTYPE            PIC X       VALUE SPACE.                 
009400                                                                          
009500  05 WS-IDTYPE                   PIC X       VALUE SPACE.                 
009600  05 WS-RED-ANTAL                PIC Z(5)9.                               
009700  05 WS-SPARA-IDARTNR            PIC S9(9)   VALUE ZERO COMP-3.           
009800  05 WS-ANTAL-FORSLAG-TOT        PIC 9(7)    VALUE ZERO.                  
009900  05 WS-GRP-TAB.                                                          
010000   07 FILLER                     OCCURS 50.                               
010100     10 WS-IDLOPNR-DC-TAB        PIC S9(7)   VALUE ZERO COMP-3.           
010200     10 WS-FLAGGA-TRAEFF         PIC X       VALUE 'N'.                   
010300     10 WS-ANTAL-FORSLAG         PIC 9(7)    VALUE ZERO.                  
010400     10 WS-IDDC-TAB              OCCURS 6                                 
010500                                 PIC X(2)    VALUE SPACE.                 
010600 77  MAX-GRP-TAB-IX              PIC S9(4)  VALUE +0    COMP SYNC.        
010700*                                                                         
010800 77  MAX-TAB-IX                  PIC S9(4)  VALUE +50   COMP SYNC.        
010900*                                                                         
011000                                                                          
011100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
011200                                                                          
011300                                                                          
011400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
011500     88  NYCKLAR-OK                          VALUE 'J'.                   
011600     88  NYCKLAR-FEL                         VALUE 'N'.                   
011700                                                                          
011800 77  SW-TRAEFF                   PIC X       VALUE 'N'.                   
011900                                                                          
012000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012100     88  EGEN-MID                            VALUE '2381'.                
012200     88  GODK-MID                            VALUE '2381' '2382'.         
012300     88  HELP-MID                            VALUE '0551'.                
012400     EJECT                                                                
012500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012600 01  GENERELLA-SUBPROGRAM.                                                
012700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013300     EJECT                                                                
013400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013500*01 -COPY WMEDAREA                                                        
013600     SKIP3                                                                
013700 01  MESSAGE-CODES.                                                       
013800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
013900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014000     03  URVAL-SAKNAS            PIC X(3)    VALUE '005'.                 
014100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
014200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014300     03  INF-PART-MISSING        PIC X(3)    VALUE '017'.                 
014400     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
014500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
014700     03  INF-PRESS-PF9           PIC X(3)    VALUE '127'.                 
014800     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
014900     03  ARTIKEL-EJ-AKTIV        PIC X(3)    VALUE '244'.                 
015000     03  RAD-FINNS               PIC X(3)    VALUE '245'.                 
015100     03  QUEUED-PRINTER          PIC X(3)    VALUE '246'.                 
015200     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
015300     03  MARKERA-RAD             PIC X(3)    VALUE '309'.                 
015400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015500     03  INF-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
015600                                                                          
015700                                                                          
015800 01  MEDDELANDE.                                                          
015900     03  MED-1                  PIC X(30)                                 
016000         VALUE 'TYPE : A,B,C  OR L            '.                          
016100     03  MED-2                  PIC X(30)                                 
016200         VALUE 'TO CHANGE SCREEN; PRESS PF14  '.                          
016300     EJECT                                                                
016400*01  -COPY WDATAREA                                                       
016500     EJECT                                                                
016600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
016700*                                                                         
016800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
016900     SKIP3                                                                
017000*01 -COPY WMSGINIT                                                        
017100     SKIP3                                                                
017200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
017300*                                                                         
017400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017500     SKIP3                                                                
017600*01  MID -COPY W2I38101                                                   
017700     EJECT                                                                
017800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017900     SKIP3                                                                
018000*01  -COPY WMSGAREA                                                       
018100     EJECT                                                                
018200     03  MOD REDEFINES MSG-AREA.                                          
018300*      05  -COPY W2O38101                                                 
018400     EJECT                                                                
018500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018600     SKIP3                                                                
018700*01  -COPY WMFSAREA                                                       
018800     EJECT                                                                
018900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019000*                                                                         
019100     EJECT                                                                
019200 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
019300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
019400                                                                          
019500 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
019600 01  DB2-WS.                                                              
019700     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
019800         88  CURSOR-OK                      VALUE 000.                    
019900         88  LINES-FOUND                    VALUE 000.                    
020000         88  LINES-MISSING                  VALUE 100.                    
020100         88  RESOURCE-WRONG                 VALUE 904.                    
020200     03  GOOD-SQLCODECODES.                                               
020300         05  GOOD-SQLCODE OCCURS 5                                        
020400             INDEXED BY SQLCODE-IX PIC 9(3).                              
020500 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
020600     EJECT                                                                
020700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020800     SKIP3                                                                
020900 01  NYCKLAR-TILL-DLI.                                                    
021000                                                                          
021100     03 W-WDE3B1KY-MIN-X.                                                 
021200         05  W-KDREFTYP-MIN      PIC X     VALUE SPACE.                   
021300         05  W-IDARTNR-MIN       PIC S9(9) VALUE ZERO COMP-3.             
021400         05  W-IDDC-MIN          PIC X(2)  VALUE SPACE.                   
021410         05  FILLER              PIC X(2)  VALUE LOW-VALUE.               
021500                                                                          
021600     03 W-WDE3B1KY-MAX-X.                                                 
021700         05  W-KDREFTYP-MAX      PIC X     VALUE HIGH-VALUE.              
021800         05  W-IDARTNR-MAX       PIC S9(9)                                
021900                                          VALUE +999999999 COMP-3.        
022000         05  W-IDDC-MAX          PIC X(2)  VALUE HIGH-VALUE.              
022010         05  FILLER              PIC X(2)  VALUE HIGH-VALUE.              
022100                                                                          
022200     SKIP2                                                                
022300*    --- STATUS-KOD FRÅN IMS                                              
022400 01  STATUS-WS                   PIC XX.                                  
022500     88  SEGMENT-FINNS                       VALUE '  '.                  
022600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022700     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
022800                                                   'GB'.                  
022900     SKIP2                                                                
023000 01  GODK-STATUSKODER.                                                    
023100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023200     SKIP3                                                                
023300 01  SSA1                        PIC X(64).                               
023400 01  SSA2                        PIC X(64).                               
023500     EJECT                                                                
023600*    --- IMS FUNKTIONSKODER                                               
023700*01  -COPY W0003                                                          
023800     EJECT                                                                
023900*    ---  DLI INPUT-OUTPUT AREA                                           
024000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDE3B1'.             
024100     SKIP3                                                                
024200 01  DLI-IO-AREA-WDE3B1.                                                  
024300*        05  -COPY WDE3B1                                                 
024400     EJECT                                                                
024500 01  FILLER                      PIC X(16)  VALUE 'TP5IDDC-AREA'.         
024600                                                                          
024700*01  -COPY TP5IDDC -PRE TP5IDDC-                                          
024800     EJECT                                                                
024900     EJECT                                                                
025000     EXEC SQL INCLUDE TP5IDDC END-EXEC.                                   
025100                                                                          
025200 LINKAGE SECTION.                                                         
025300*01  -COPY W0009   -PRE MSG-                                              
025400*01  -COPY W0008   -PRE USEA-                                             
025500     05  FILLER                  PIC X.                                   
025600     EJECT                                                                
025700*01  -COPY W0008  -PRE WDE3B-                                             
025800     05  FILLER                  PIC X.                                   
025900     EJECT                                                                
026000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDE3B-PCB.                    
026100 MAIN SECTION.                                                            
026200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDE3B-PCB                     
026300                                                                          
026400     PERFORM IMS-GET-MSG                                                  
026500     IF SEGMENT-FINNS                                                     
026600       PERFORM A-INIT                                                     
026700       PERFORM B-KOLLA-NYCKLAR                                            
026800       IF NYCKLAR-OK                                                      
026900         IF MFS-FIRST                                                     
027000           PERFORM C-FOERSTA-SIDA                                         
027100           PERFORM F-LAES-VISA-INFO                                       
027200           PERFORM FC-VISA-TABELL                                         
027300         ELSE                                                             
027400           IF MFS-NEXT                                                    
027500             PERFORM D-NAESTA-SIDA                                        
027600             PERFORM FC-VISA-TABELL                                       
027700           ELSE                                                           
027800             IF MID-IDTYPE-2381-IN NOT = ALL '+'                          
027900               PERFORM F-LAES-VISA-INFO                                   
028000             ELSE                                                         
028100               PERFORM E-SAMMA-SIDA                                       
028200             END-IF                                                       
028300             PERFORM FC-VISA-TABELL                                       
028400           END-IF                                                         
028500         END-IF                                                           
028600                                                                          
028700         MOVE WS-MSGI-AREA-2381 TO MSGI-SPAR-AREA                         
028800         MOVE '002'             TO MSGI-KDCALL                            
028900         MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                      
029000         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
029100         MOVE '2381'            TO MSGI-IDTRANS                           
029200         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
029300                                                                          
029400       END-IF                                                             
029500       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O38101 + 4                      
029600       PERFORM IMS-INSERT-MSG                                             
029700     END-IF                                                               
029800                                                                          
029900     MOVE ZERO TO RETURN-CODE                                             
030000     GOBACK                                                               
030100     .                                                                    
030200     EJECT                                                                
030300 A-INIT SECTION.                                                          
030400                                                                          
030500     IF MSG-DUBBLA-TRANSKODER                                             
030600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I38101                 
030700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
030800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
030900     ELSE                                                                 
031000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I38101                  
031100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
031200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
031300     END-IF                                                               
031400                                                                          
031500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
031600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
031700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
031800                                                                          
031900     MOVE LOW-VALUE TO MSG-AREA                                           
032000     MOVE 'W2O381N1' TO MFS-IDMOD                                         
032100     MOVE '2381' TO MOD-IDTRANS                                           
032200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
032300                                                                          
032400     IF EGEN-MID OR HELP-MID                                              
032500       CONTINUE                                                           
032600     ELSE                                                                 
032700       MOVE 'C'              TO MID-IDTYPE-2381-IN                        
032800       MOVE SPACE TO MFS-KDTRTYP                                          
032900       MOVE '7' TO MFS-IDPFK                                              
033000     END-IF                                                               
033100     .                                                                    
033200     EJECT                                                                
033300 B-KOLLA-NYCKLAR SECTION.                                                 
033400                                                                          
033500******   UPPDATERING AV MSGI-BLÄDDRINGSNYCKLAR SKER                       
033600******   I SLUTET AV PROGRAMMET                                           
033700     MOVE ALL '+'               TO MSGI-WMSGINIT                          
033800     MOVE '001'                 TO MSGI-KDCALL                            
033900     MOVE MSG-LTERM-NAME        TO MSGI-IDLTERM-USER                      
034000     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
034100     MOVE '2381'                TO MSGI-IDTRANS                           
034200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
034300                                                                          
034400     IF W-IDTRANS            = '2382'                                     
034500     AND MSGI-SPAR-AREA(1:4) = '2382'                                     
034600*                                                                         
034700*      -- FÖR ATT HÄMTA IDTYPE                                            
034800*                                                                         
034900       MOVE MSGI-SPAR-AREA   TO WS-MSGI-AREA-2382                         
035000       MOVE WS-MSGI-IDTYPE   TO WS-IDTYPE                                 
035100                                                                          
035200     ELSE                                                                 
035300                                                                          
035400       IF EGEN-MID                                                        
035500       AND MSGI-SPAR-AREA(1:4) = '2381'                                   
035600         MOVE MSGI-SPAR-AREA TO WS-MSGI-AREA-2381                         
035700       END-IF                                                             
035800                                                                          
035900       IF MID-IDTYPE-2381-IN = ALL '+'                                    
036000         IF MID-IDTYPE-2381-UT = 'BOAT '                                  
036100           MOVE 'B'          TO WS-IDTYPE                                 
036200         END-IF                                                           
036300         IF MID-IDTYPE-2381-UT = 'AIR  '                                  
036400           MOVE 'A'          TO WS-IDTYPE                                 
036500         END-IF                                                           
036600         IF MID-IDTYPE-2381-UT = 'AIRCR'                                  
036700           MOVE 'C'          TO WS-IDTYPE                                 
036800         END-IF                                                           
036900         IF MID-IDTYPE-2381-UT = 'LOCAL'                                  
037000           MOVE 'L'          TO WS-IDTYPE                                 
037100         END-IF                                                           
037200       ELSE                                                               
037300         MOVE MID-IDTYPE-2381-IN TO WS-IDTYPE                             
037400       END-IF                                                             
037500     END-IF                                                               
037600                                                                          
037700*      -- KONTROLL AV TYP                                                 
037800     MOVE MFS-RENSA-FAELT    TO MOD-IDTYPE-IN                             
037900     MOVE JA TO NYCKLAR-SW                                                
038000                                                                          
038100     IF    WS-IDTYPE = 'A'                                                
038200     OR    WS-IDTYPE = 'C'                                                
038300     OR    WS-IDTYPE = 'B'                                                
038400     OR    WS-IDTYPE = 'L'                                                
038500       IF WS-IDTYPE = 'A'                                                 
038600         MOVE 'AIR'          TO MOD-IDTYPE-UT                             
038700       END-IF                                                             
038800       IF WS-IDTYPE = 'C'                                                 
038900         MOVE 'AIRCR'        TO MOD-IDTYPE-UT                             
039000       END-IF                                                             
039100       IF WS-IDTYPE = 'B'                                                 
039200         MOVE 'BOAT'         TO MOD-IDTYPE-UT                             
039300       END-IF                                                             
039400       IF WS-IDTYPE = 'L'                                                 
039500         MOVE 'LOCAL'        TO MOD-IDTYPE-UT                             
039600       END-IF                                                             
039700       MOVE WS-IDTYPE        TO W-KDREFTYP-MIN                            
039800                                W-KDREFTYP-MAX                            
039900     ELSE                                                                 
040000       MOVE NEJ TO NYCKLAR-SW                                             
040100       MOVE MED-1            TO MOD-TEMFSINF                              
040200     END-IF                                                               
040300                                                                          
040400     PERFORM DB2-OPEN-TP5IDDC-CRS                                         
040500     PERFORM DB2-FETCH-TP5IDDC-CRS                                        
040600     MOVE +1 TO   TAB-IX                                                  
040700     PERFORM UNTIL SQLCODE > ZERO                                         
040800       MOVE TP5IDDC-IDLOPNR-DC    TO WS-IDLOPNR-DC-TAB (TAB-IX)           
040900       MOVE +1 TO DC-IX                                                   
041000       PERFORM UNTIL SQLCODE > ZERO                                       
041100         OR TP5IDDC-IDLOPNR-DC NOT = WS-IDLOPNR-DC-TAB (TAB-IX)           
041200         OR DC-IX > MAX-DC-IX                                             
041300         MOVE TP5IDDC-IDDC        TO WS-IDDC-TAB (TAB-IX, DC-IX)          
041400         ADD +1   TO DC-IX                                                
041500         PERFORM DB2-FETCH-TP5IDDC-CRS                                    
041600       END-PERFORM                                                        
041700       ADD +1     TO TAB-IX                                               
041800       IF TAB-IX > MAX-TAB-IX                                             
041900         STRING ' TABELL MÅSTE ÖKAS (MAX-TAB-IX)'                         
042000         DELIMITED BY SIZE INTO FELTEXT                                   
042100         CALL FELLOG                                                      
042200       END-IF                                                             
042300     END-PERFORM                                                          
042400     PERFORM DB2-CLOSE-TP5IDDC-CRS                                        
042500     COMPUTE MAX-GRP-TAB-IX = TAB-IX - 1                                  
042600                                                                          
042700     MOVE +1 TO VISA-IX                                                   
042800     IF NYCKLAR-FEL                                                       
042900       MOVE 'GB '         TO MED-IDSKYLT                                  
043000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
043100       CALL WMEDKONV USING MED-WMEDAREA                                   
043200       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
043300       PERFORM MFS-RENSA-FAELT-IN                                         
043400       PERFORM MFS-RENSA-FAELT-UT                                         
043500     END-IF                                                               
043600     .                                                                    
043700     EJECT                                                                
043800 C-FOERSTA-SIDA SECTION.                                                  
043900                                                                          
044000     MOVE 'GB '           TO MED-IDSKYLT                                  
044100     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
044200     CALL WMEDKONV USING MED-WMEDAREA                                     
044300     MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                                  
044400                                                                          
044500     PERFORM MFS-RENSA-FAELT-IN                                           
044600     MOVE +1 TO VISA-IX                                                   
044700     .                                                                    
044800     EJECT                                                                
044900 D-NAESTA-SIDA SECTION.                                                   
045000                                                                          
045100     MOVE WS-MSGI-GRP-TAB TO WS-GRP-TAB                                   
045200     MOVE WS-MSGI-ANTAL-FORSLAG-TOT                                       
045300          TO WS-ANTAL-FORSLAG-TOT                                         
045400     MOVE WS-VISA-IX-NEXT TO VISA-IX                                      
045500     MOVE ZERO TO WS-VISA-IX-NEXT                                         
045600     .                                                                    
045700     EJECT                                                                
045800 E-SAMMA-SIDA SECTION.                                                    
045900                                                                          
046000     MOVE MED-2              TO MOD-TEMFSINF                              
046100     MOVE WS-MSGI-GRP-TAB TO WS-GRP-TAB                                   
046200     MOVE WS-MSGI-ANTAL-FORSLAG-TOT                                       
046300          TO WS-ANTAL-FORSLAG-TOT                                         
046400     MOVE WS-VISA-IX-ENTER TO VISA-IX                                     
046500     MOVE ZERO TO WS-VISA-IX-ENTER                                        
046600     .                                                                    
046700     EJECT                                                                
046800 F-LAES-VISA-INFO SECTION.                                                
046900                                                                          
047000     PERFORM FA-LAES-WDE3B1                                               
047100                                                                          
047200     PERFORM UNTIL SEGMENT-SAKNAS                                         
047300       MOVE SEQB-IDARTNR TO WS-SPARA-IDARTNR                              
047400       PERFORM FB-INIT-FLAGGOR                                            
047500       PERFORM UNTIL SEGMENT-SAKNAS                                       
047600       OR SEQB-IDARTNR NOT = WS-SPARA-IDARTNR                             
047700         MOVE NEJ TO SW-TRAEFF                                            
047800         MOVE ZERO TO TAB-IX                                              
047900         PERFORM UNTIL TAB-IX > MAX-GRP-TAB-IX                            
048000         OR SW-TRAEFF = JA                                                
048100           ADD +1 TO TAB-IX                                               
048200           MOVE +1 TO DC-IX                                               
048300           PERFORM UNTIL DC-IX > MAX-DC-IX                                
048400           OR SEQB-IDDC = WS-IDDC-TAB (TAB-IX, DC-IX)                     
048500             ADD +1 TO DC-IX                                              
048600           END-PERFORM                                                    
048700           IF DC-IX > MAX-DC-IX                                           
048800             CONTINUE                                                     
048900           ELSE                                                           
049000              MOVE JA TO SW-TRAEFF                                        
049100           END-IF                                                         
049200         END-PERFORM                                                      
049300         IF TAB-IX > MAX-GRP-TAB-IX                                       
049400           CONTINUE                                                       
049500         ELSE                                                             
049600           IF WS-FLAGGA-TRAEFF (TAB-IX) = NEJ                             
049700             ADD +1 TO WS-ANTAL-FORSLAG (TAB-IX)                          
049800             MOVE JA TO WS-FLAGGA-TRAEFF (TAB-IX)                         
049900           END-IF                                                         
050000         END-IF                                                           
050100         PERFORM FA-LAES-WDE3B1                                           
050200       END-PERFORM                                                        
050300     END-PERFORM                                                          
050400     MOVE +1 TO  INDX                                                     
050500     PERFORM UNTIL INDX > MAX-GRP-TAB-IX                                  
050600       COMPUTE WS-ANTAL-FORSLAG-TOT = WS-ANTAL-FORSLAG (INDX)             
050700                                    + WS-ANTAL-FORSLAG-TOT                
050800       ADD +1 TO INDX                                                     
050900     END-PERFORM                                                          
051000     PERFORM FD-RENSA-MSGI                                                
051100     MOVE WS-ANTAL-FORSLAG-TOT TO WS-MSGI-ANTAL-FORSLAG-TOT               
051200     MOVE WS-GRP-TAB TO WS-MSGI-GRP-TAB                                   
051300     .                                                                    
051400     EJECT                                                                
051500 FA-LAES-WDE3B1 SECTION.                                                  
051600     PERFORM IMS-GN-WDE3B1-MIN-MAX                                        
051700     PERFORM UNTIL SEGMENT-SAKNAS OR SEQB-KDREFORS = 'P'                  
051800       PERFORM IMS-GN-WDE3B1-MIN-MAX                                      
051900     END-PERFORM                                                          
052000     .                                                                    
052100     EJECT                                                                
052200 FB-INIT-FLAGGOR SECTION.                                                 
052300                                                                          
052400     MOVE +1   TO TAB-IX                                                  
052500     PERFORM UNTIL TAB-IX > MAX-GRP-TAB-IX                                
052600       MOVE NEJ TO WS-FLAGGA-TRAEFF (TAB-IX)                              
052700       ADD +1 TO TAB-IX                                                   
052800     END-PERFORM                                                          
052900     .                                                                    
053000     EJECT                                                                
053100 FC-VISA-TABELL SECTION.                                                  
053200                                                                          
053300     MOVE VISA-IX  TO WS-VISA-IX-ENTER                                    
053400     MOVE +1 TO INDX                                                      
053500     COMPUTE MAX-INDX = VISA-IX + 10                                      
053600     PERFORM UNTIL  VISA-IX > MAX-INDX                                    
053700     OR VISA-IX > MAX-GRP-TAB-IX                                          
053800       MOVE WS-ANTAL-FORSLAG (VISA-IX) TO MOD-TO-REVIEW (INDX)            
053900       MOVE +1 TO DC-IX                                                   
054000       PERFORM UNTIL DC-IX > MAX-DC-IX                                    
054100       OR WS-IDDC-TAB (VISA-IX, DC-IX) = SPACE                            
054200         MOVE WS-IDDC-TAB (VISA-IX, DC-IX)                                
054300                          TO MOD-IDDC-2381 (INDX, DC-IX)                  
054400         ADD +1 TO DC-IX                                                  
054500       END-PERFORM                                                        
054600       ADD +1 TO VISA-IX                                                  
054700       ADD +1 TO INDX                                                     
054800     END-PERFORM                                                          
054900     MOVE VISA-IX  TO WS-VISA-IX-NEXT                                     
055000     IF WS-IDDC-TAB (VISA-IX + 1, 1) NOT = SPACE                          
055100       MOVE 'GB '         TO MED-IDSKYLT                                  
055200       MOVE INF-MORE-INFO-EXISTS                                          
055300                           TO MED-IDMFSFEL                                
055400       CALL WMEDKONV USING MED-WMEDAREA                                   
055500       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
055600     END-IF                                                               
055700     MOVE WS-ANTAL-FORSLAG-TOT TO MOD-TO-REVIEW (INDX + 1)                
055800     .                                                                    
055900     EJECT                                                                
056000 FD-RENSA-MSGI SECTION.                                                   
056100                                                                          
056200     MOVE +1 TO INDX                                                      
056300     PERFORM UNTIL INDX > MAX-TAB-IX                                      
056400       MOVE ZERO       TO WS-MSGI-IDLOPNR-DC-TAB (INDX)                   
056500       MOVE SPACE      TO WS-MSGI-FLAGGA-TRAEFF (INDX)                    
056600       MOVE ZERO       TO WS-MSGI-ANTAL-FORSLAG (INDX)                    
056700       MOVE +1 TO DC-IX                                                   
056800       PERFORM UNTIL DC-IX > MAX-DC-IX                                    
056900         MOVE SPACE TO WS-MSGI-IDDC-TAB (INDX, DC-IX)                     
057000         ADD +1 TO DC-IX                                                  
057100       END-PERFORM                                                        
057200       ADD +1 TO INDX                                                     
057300     END-PERFORM                                                          
057400     MOVE ZERO TO WS-MSGI-ANTAL-FORSLAG-TOT                               
057500     .                                                                    
057600     SKIP3                                                                
057700 MFS-RENSA-FAELT-UT SECTION.                                              
057800                                                                          
057900*    --- ALLA UTDATA-FÄLT                                                 
058000     MOVE MFS-RENSA-FAELT    TO MOD-IDTYPE-UT                             
058100                                                                          
058200     MOVE 1                  TO INDX                                      
058300     PERFORM UNTIL INDX > 13                                              
058400       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
058500       ADD 1                 TO INDX                                      
058600     END-PERFORM                                                          
058700     .                                                                    
058800     SKIP3                                                                
058900 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
059000                                                                          
059100*    --- ALLA UTDATA-FÄLT                                                 
059200                                                                          
059300     MOVE MFS-RENSA-FAELT    TO MOD-SELECT (INDX)                         
059400                                MOD-TO-REVIEW (INDX)                      
059500     MOVE 1                  TO DC-IX                                     
059600     PERFORM UNTIL DC-IX > MAX-DC-IX                                      
059700       MOVE MFS-RENSA-FAELT  TO MOD-IDDC-2381 (INDX, DC-IX)               
059800       ADD 1                 TO DC-IX                                     
059900     END-PERFORM                                                          
060000     .                                                                    
060100     SKIP3                                                                
060200 MFS-RENSA-FAELT-IN SECTION.                                              
060300                                                                          
060400*    --- ALLA INDATA-FÄLT                                                 
060500     MOVE MFS-RENSA-FAELT    TO MOD-IDTYPE-IN                             
060600                                                                          
060700     MOVE 1                  TO INDX                                      
060800     PERFORM UNTIL INDX > 13                                              
060900       MOVE MFS-RENSA-FAELT  TO MOD-SELECT (INDX)                         
061000       ADD 1                 TO INDX                                      
061100     END-PERFORM                                                          
061200     .                                                                    
061300     EJECT                                                                
061400 IMS-GET-MSG SECTION.                                                     
061500                                                                          
061600     MOVE '  QC' TO GODK-STATUSKODER                                      
061700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
061800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
061900     PERFORM IMS-STATUSKONTROLL                                           
062000     .                                                                    
062100     SKIP3                                                                
062200 IMS-INSERT-MSG SECTION.                                                  
062300                                                                          
062400     IF ENGLISH-TEXT                                                      
062500       MOVE 'N' TO MFS-KDHUVOMR                                           
062600     END-IF                                                               
062700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
062800     MOVE SPACE TO GODK-STATUSKODER                                       
062900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
063000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
063100     PERFORM IMS-STATUSKONTROLL                                           
063200     .                                                                    
063300     EJECT                                                                
063400 IMS-GN-WDE3B1-MIN-MAX SECTION.                                           
063500     MOVE 'IMS-GN-WDE3B1-MIN-MAX '                                        
063600                                 TO WS-IMS-SEKTION                        
063700                                                                          
063800     STRING 'WDE3B1  (WDE3B1KY>=' W-WDE3B1KY-MIN-X                        
063900                    '&WDE3B1KY<=' W-WDE3B1KY-MAX-X ')'                    
064000          DELIMITED BY SIZE INTO SSA1                                     
064100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
064200     CALL CBLTDLI USING GN WDE3B-PCB DLI-IO-AREA-WDE3B1 SSA1              
064300     MOVE WDE3B-STATUS-CODE TO STATUS-WS                                  
064400     PERFORM IMS-STATUSKONTROLL                                           
064500     .                                                                    
064600     SKIP3                                                                
064700 IMS-STATUSKONTROLL SECTION.                                              
064800                                                                          
064900     SET STATUS-IX TO 1                                                   
065000     SEARCH GODK-STATUS                                                   
065100       AT END                                                             
065200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
065300         DELIMITED BY SIZE INTO FELTEXT                                   
065400         CALL FELLOG                                                      
065500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
065600         CONTINUE                                                         
065700     END-SEARCH                                                           
065800     .                                                                    
065900 DB2-OPEN-TP5IDDC-CRS SECTION.                                            
066000     MOVE 'DB2-OPEN-TP5IDDC-CRS' TO  WS-DB2-SEKTION                       
066100                                                                          
066200     EXEC SQL DECLARE TP5IDDC-CRS CURSOR FOR                              
066300         SELECT  IDLOPNR_DC                                               
066400                ,IDDC                                                     
066500                                                                          
066600         FROM    TP5IDDC                                                  
066700                                                                          
066800         ORDER BY IDLOPNR_DC                                              
066900                 ,IDDC                                                    
067000                                                                          
067100     END-EXEC                                                             
067200                                                                          
067300     MOVE SQLCODE TO SQLCODE-WS                                           
067400     MOVE 000     TO GOOD-SQLCODECODES                                    
067500     EXEC SQL OPEN TP5IDDC-CRS END-EXEC                                   
067600     PERFORM DB2-STATUS-CHECK                                             
067700     .                                                                    
067800     EJECT                                                                
067900 DB2-FETCH-TP5IDDC-CRS SECTION.                                           
068000     MOVE 'DB2-FETCH-TP5IDDC-CRS' TO  WS-DB2-SEKTION                      
068100     MOVE 000100  TO GOOD-SQLCODECODES                                    
068200     EXEC SQL FETCH TP5IDDC-CRS INTO                                      
068300                :TP5IDDC-IDLOPNR-DC                                       
068400               ,:TP5IDDC-IDDC                                             
068500                                                                          
068600     END-EXEC                                                             
068700                                                                          
068800     MOVE SQLCODE TO SQLCODE-WS                                           
068900     PERFORM DB2-STATUS-CHECK                                             
069000     .                                                                    
069100     SKIP3                                                                
069200                                                                          
069300 DB2-CLOSE-TP5IDDC-CRS SECTION.                                           
069400     MOVE 'DB2-CLOSE-TP5IDDC-CRS' TO  WS-DB2-SEKTION                      
069500                                                                          
069600     EXEC SQL CLOSE TP5IDDC-CRS END-EXEC                                  
069700     .                                                                    
069800     EJECT                                                                
069900                                                                          
070000 DB2-STATUS-CHECK  SECTION.                                               
070100                                                                          
070200     SET SQLCODE-IX TO 1                                                  
070300     SEARCH GOOD-SQLCODE                                                  
070400       AT END                                                             
070500*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
070600*         DELIMITED BY SIZE INTO ERROR-TEXT                               
070700          CALL ABEND USING RKOD-ABEND-DB2                                 
070800       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
070900     END-SEARCH                                                           
071000     .                                                                    
071100     EJECT                                                                
071200                                                                          
