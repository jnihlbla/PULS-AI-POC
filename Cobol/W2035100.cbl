000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2035100.                                                
000400 AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
000500 DATE-WRITTEN.   96/06/28.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        ÖVERSIKTSBILD SOM VISAR HUR MÅNGA ORDERFÖRSLAG                   
001000*        PER BUYER SOM ÅTERSTÅR ATT BEDÖMMA .                             
001100*        GENOM ATT ANGE ETT 'S' FRAMFÖR BUYER OCH TRYCKA PF14             
001200*        KOMMER MAN ÖVER TILL 2352 OCH FÅR UPP FÖRSTA ICKE                
001300*        BEDÖMDA ORDERFÖRSLAG FÖR ANGIVEN BUYER                           
001310*                                                                         
001311*    CHANGE LOG :                                                         
001312*        DATE : 20160607                                                  
001320*        CHANGED THE SCREEN TO ACCPET DC AS INPUT AND                     
001330*        DISPLAY PROPOSALS AS PER THE DC GROUP ON 2365                    
001400*                                                                         
001500*        PROGRAMMET LÄSER      WLORDW (WDE3)                              
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W2T351                                              
001900*        MID:         W2I35101                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W2O35101                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W2035100'.            
003200                                                                          
003300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003500                                                                          
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800                                                                          
003900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004100*    FÖR ATT GARANTERA PLATS FÖR TOTALEN                                  
004200*    SÄTTS MAX-INDX TILL 11                                               
004300 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
004400*    TOTALT ANTALET RADER PÅ BILDEN ÄR 13                                 
004500 77  MAX-INDX-SIDA               PIC S9(4)  VALUE +13   COMP SYNC.        
004600*    FOR ACCESS TO DB2 TABLE                                              
004700 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
004910 77  IX-DC                       PIC S9(3)  VALUE ZERO  COMP-3.           
004920 77  IX-DC-N                     PIC S9(3)  VALUE ZERO  COMP-3.           
004930 77  IX-DC-MAX                   PIC S9(3)  VALUE +6    COMP-3.           
005000*                                                                         
005100 01  WS-IDDC-MIN                 PIC X(2)   VALUE SPACE.                  
005200 01  WS-IDDC-MAX                 PIC X(2)   VALUE SPACE.                  
005300 01  WS-IDLOPNR-DC               PIC S9(7)  VALUE ZERO  COMP-3.           
005400 01  WS-IX                       PIC 9(2)   VALUE ZERO.                   
005500 01  WS-ANTAL-DB2                PIC S9(5)  VALUE ZERO COMP-3.            
005600*                                                                         
005700 01  WS.                                                                  
005800                                                                          
005900*********************************************************                 
006000*    WS-MSGI-AREA-2351                                                    
006100*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
006200*           (I MSGI-SPAR-AREA)                                            
006300*********************************************************                 
006400  05 WS-MSGI-AREA-2351.                                                   
006500    10 WS-MSGI-IDTRANS-2351      PIC X(4)    VALUE '2351'.                
006600    10 WS-MSGI-SSA-KEY-ENTER     PIC X(14)   VALUE SPACE.                 
006700    10 WS-MSGI-SSA-KEY-NEXT      PIC X(14)   VALUE SPACE.                 
006800    10 FILLER                    PIC X(168)  VALUE SPACE.                 
006900                                                                          
007000*********************************************************                 
007100*    WS-MSGI-AREA-2352                                                    
007200*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
007300*           (I MSGI-SPAR-AREA)                                            
007400*           WS-MSGI-IDTYPE ANVÄNDS FÖR ATT FÅ UPP RÄTT KÖ                 
007500*********************************************************                 
007600  05 WS-MSGI-AREA-2352.                                                   
007700    10 WS-MSGI-IDTRANS-2352      PIC X(4)    VALUE '2352'.                
007800    10 WS-MSGI-IDARTNR-ENTER     PIC  9(9)   VALUE ZERO.                  
007900    10 WS-MSGI-ORDER-ENTER       PIC X       VALUE SPACE.                 
008000    10 WS-MSGI-IDARTNR-PF7       PIC  9(9)   VALUE ZERO.                  
008100    10 WS-MSGI-ORDER-PF7         PIC X       VALUE SPACE.                 
008200    10 WS-MSGI-IDTYPE            PIC X       VALUE SPACE.                 
008300                                                                          
008400  05 WS-WDE3A1KY-ENTER.                                                   
008430    10 WS-KDREFTYP-ENTER         PIC X       VALUE SPACE.                 
008600    10 WS-IDPERSON-BUY-ENTER     PIC S9(3)   VALUE ZERO COMP-3.           
008700    10 WS-IDARTNR-ENTER          PIC S9(9)   VALUE ZERO COMP-3.           
008800    10 WS-IDDC-ENTER             PIC X(2)    VALUE SPACE.                 
008900  05 WS-WDE3A1KY-NEXT.                                                    
008930    10 WS-KDREFTYP-NEXT          PIC X       VALUE SPACE.                 
009100    10 WS-IDPERSON-BUY-NEXT      PIC S9(3)   VALUE ZERO COMP-3.           
009200    10 WS-IDARTNR-NEXT           PIC S9(9)   VALUE ZERO COMP-3.           
009300    10 WS-IDDC-NEXT              PIC X(2)    VALUE SPACE.                 
009400  05 WS-IDTYPE                   PIC X       VALUE SPACE.                 
009500  05 WS-BUY-TO-REVIEW            PIC S9(6)   VALUE ZERO.                  
009600  05 WS-TOT-TO-REVIEW            PIC S9(6)   VALUE ZERO.                  
009700  05 WS-RED-ANTAL                PIC Z(5)9.                               
009800  05 WS-SPARA-IDPERSON-BUY       PIC S9(3)   VALUE ZERO COMP-3.           
009900  05 WS-SPARA-KDREFTYP           PIC X       VALUE SPACE.                 
010000  05 WS-SPARA-IDARTNR            PIC S9(9)   VALUE ZERO COMP-3.           
010010*                                                                         
010020  05 WS-DC-TABELL.                                                        
010030   10  WS-DC-1                   PIC X(2)    VALUE SPACES.                
010040   10  WS-DC-2                   PIC X(2)    VALUE SPACES.                
010050   10  WS-DC-3                   PIC X(2)    VALUE SPACES.                
010060   10  WS-DC-4                   PIC X(2)    VALUE SPACES.                
010070   10  WS-DC-5                   PIC X(2)    VALUE SPACES.                
010080   10  WS-DC-6                   PIC X(2)    VALUE SPACES.                
010090  05 FILLER REDEFINES            WS-DC-TABELL.                            
010091   10  WS-DC-NR                  OCCURS 6                                 
010092                                 PIC X(2).                                
010093                                                                          
010094 77  VALID-IDDC-SW               PIC X       VALUE 'N'.                   
010095     88  VALID-IDDC-JA                       VALUE 'J'.                   
010096     88  VALID-IDDC-NEJ                      VALUE 'N'.                   
010097                                                                          
010100                                                                          
010200                                                                          
010300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
010400                                                                          
010500                                                                          
010600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010700     88  NYCKLAR-OK                          VALUE 'J'.                   
010800     88  NYCKLAR-FEL                         VALUE 'N'.                   
010900                                                                          
011000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011100     88  EGEN-MID                            VALUE '2351'.                
011200     88  GODK-MID                            VALUE '2351' '2352'          
011300                                                   '2353' '2354'          
011400                                                   '2355' '2356'          
011500                                                   '2357' '2358'          
011600                                                   '2359'.                
011700     88  HELP-MID                            VALUE '0551'.                
011800     EJECT                                                                
011900*      --- VALID IDDC CODES                                               
012000*                                                                         
012100*01    -COPY WWDC99                                                       
012200       EJECT                                                              
012300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012400 01  GENERELLA-SUBPROGRAM.                                                
012500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013100*    --- PARAMETRAR TILL ABEND                                            
013200                                                                          
013300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013500     EJECT                                                                
013600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013700*01 -COPY WMEDAREA                                                        
013800     SKIP3                                                                
013900 01  MESSAGE-CODES.                                                       
014000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014100     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014200     03  URVAL-SAKNAS            PIC X(3)    VALUE '005'.                 
014300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
014400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014500     03  INF-PART-MISSING        PIC X(3)    VALUE '017'.                 
014600     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
014700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
014900     03  INF-PRESS-PF9           PIC X(3)    VALUE '127'.                 
015000     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
015100     03  ARTIKEL-EJ-AKTIV        PIC X(3)    VALUE '244'.                 
015200     03  RAD-FINNS               PIC X(3)    VALUE '245'.                 
015300     03  QUEUED-PRINTER          PIC X(3)    VALUE '246'.                 
015400     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
015500     03  MARKERA-RAD             PIC X(3)    VALUE '309'.                 
015600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015700     03  INF-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
015800                                                                          
015900                                                                          
016000 01  MEDDELANDE.                                                          
016100     03  MED-1                  PIC X(30)                                 
016200         VALUE 'TYPE : A,B,C  OR L            '.                          
016300     03  MED-2                  PIC X(30)                                 
016400         VALUE 'TO CHANGE SCREEN; PRESS PF14  '.                          
016500     EJECT                                                                
016600*01  -COPY WDATAREA                                                       
016700     EJECT                                                                
016800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
016900*                                                                         
017000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
017100     SKIP3                                                                
017200*01 -COPY WMSGINIT                                                        
017300     SKIP3                                                                
017400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
017500*                                                                         
017600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017700     SKIP3                                                                
017800*01  MID -COPY W2I35101                                                   
017900     EJECT                                                                
018000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
018100     SKIP3                                                                
018200*01  -COPY WMSGAREA                                                       
018300     EJECT                                                                
018400     03  MOD REDEFINES MSG-AREA.                                          
018500*      05  -COPY W2O35101                                                 
018600     EJECT                                                                
018700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018800     SKIP3                                                                
018900*01  -COPY WMFSAREA                                                       
019000     EJECT                                                                
019100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019200*                                                                         
019300     EJECT                                                                
019400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019500     SKIP3                                                                
019600 01  NYCKLAR-TILL-DLI.                                                    
019700                                                                          
019800     03 W-WDE3A1KY-X.                                                     
019830         05  W-KDREFTYP          PIC X     VALUE SPACE.                   
020000         05  W-IDPERSON-BUY      PIC S9(3) VALUE ZERO COMP-3.             
020100         05  W-IDARTNR-301       PIC S9(9) VALUE ZERO COMP-3.             
020200         05  W-IDDC-301          PIC X(2)  VALUE SPACE.                   
020300                                                                          
020400     03 W-WDE3A1KY-MIN-X.                                                 
020430         05  W-KDREFTYP-MIN      PIC X     VALUE SPACE.                   
020600         05  W-IDPERSON-BUY-MIN  PIC S9(3) VALUE ZERO COMP-3.             
020700         05  W-IDARTNR-MIN       PIC S9(9) VALUE ZERO COMP-3.             
020800         05  W-IDDC-MIN          PIC X(2)  VALUE SPACE.                   
020900                                                                          
021000     03 W-WDE3A1KY-MAX-X.                                                 
021030         05  W-KDREFTYP-MAX      PIC X     VALUE HIGH-VALUE.              
021200         05  W-IDPERSON-BUY-MAX  PIC S9(3) VALUE +999 COMP-3.             
021300         05  W-IDARTNR-MAX       PIC S9(9)                                
021400                                          VALUE +999999999 COMP-3.        
021500         05  W-IDDC-MAX          PIC X(2)  VALUE HIGH-VALUE.              
021600                                                                          
021900     03  W-IDDC-X.                                                        
022000         05  W-IDDC-TP5          PIC X(2)    VALUE SPACE.                 
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
024000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ORDW01'.             
024100     SKIP3                                                                
024200 01  DLI-IO-AREA-ORDW01.                                                  
024300*        05  -COPY WDE3A1                                                 
024400     EJECT                                                                
025000                                                                          
025100 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
025200       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
025300                                                                          
025400 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
025500 01  DB2-WS.                                                              
025600     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
025700         88  CURSOR-OK                       VALUE 000.                   
025800         88  RADER-FINNS                     VALUE 000.                   
025900         88  RADER-SAKNAS                    VALUE 100.                   
026000         88  ATKOMST-FEL                     VALUE 904.                   
026100     03  GODK-SQLCODEKODER.                                               
026200         05  GODK-SQLCODE OCCURS 5                                        
026300             INDEXED BY SQLCODE-IX PIC 9(3).                              
026400     EJECT                                                                
026500 01  FILLER                      PIC X(16)  VALUE 'TP5IDDC-AREA'.         
026600                                                                          
026700*01  -COPY TP5IDDC -PRE TP5IDDC-                                          
026800     EJECT                                                                
026900     EXEC SQL INCLUDE TP5IDDC END-EXEC.                                   
027000 LINKAGE SECTION.                                                         
027100*01  -COPY W0009   -PRE MSG-                                              
027200*01  -COPY W0008   -PRE USEA-                                             
027300     05  FILLER                  PIC X.                                   
027400     EJECT                                                                
027500*01  -COPY W0008  -PRE ORDW-                                              
027600     05  FILLER                  PIC X.                                   
027700     EJECT                                                                
028100 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ORDW-PCB.                     
028300 MAIN SECTION.                                                            
028400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ORDW-PCB.                     
028600                                                                          
028700     PERFORM IMS-GET-MSG                                                  
028800     IF SEGMENT-FINNS                                                     
028900       PERFORM A-INIT                                                     
029000       PERFORM B-KOLLA-NYCKLAR                                            
029100       IF NYCKLAR-OK                                                      
029200         IF MFS-FIRST                                                     
029300           PERFORM C-FOERSTA-SIDA                                         
029400         ELSE                                                             
029500           IF MFS-NEXT                                                    
029600             PERFORM D-NAESTA-SIDA                                        
029700           ELSE                                                           
029800             PERFORM E-SAMMA-SIDA                                         
029900           END-IF                                                         
030000         END-IF                                                           
030100         PERFORM F-LAES-VISA-INFO                                         
030200                                                                          
030300         MOVE WS-MSGI-AREA-2351 TO MSGI-SPAR-AREA                         
030400         MOVE '002'             TO MSGI-KDCALL                            
030500         MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                      
030600         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
030700         MOVE '2351'            TO MSGI-IDTRANS                           
030800         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
030900                                                                          
031000       END-IF                                                             
031100       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O35101 + 4                      
031200       PERFORM IMS-INSERT-MSG                                             
031300     END-IF                                                               
031400                                                                          
031500     MOVE ZERO TO RETURN-CODE                                             
031600     GOBACK                                                               
031700     .                                                                    
031800     EJECT                                                                
031900 A-INIT SECTION.                                                          
032000                                                                          
032100     IF MSG-DUBBLA-TRANSKODER                                             
032200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I35101                 
032300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
032400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
032500     ELSE                                                                 
032600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I35101                  
032700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
032800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
032900     END-IF                                                               
033000                                                                          
033100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
033200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
033300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
033400                                                                          
033500     MOVE LOW-VALUE TO MSG-AREA                                           
033600     MOVE 'W2O351N1' TO MFS-IDMOD                                         
033700     MOVE '2351' TO MOD-IDTRANS                                           
033800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
033900                                                                          
034000     IF EGEN-MID OR HELP-MID                                              
034100       CONTINUE                                                           
034200     ELSE                                                                 
034300       MOVE 'C'              TO MID-IDTYPE-2351-IN                        
034400       MOVE SPACE TO MFS-KDTRTYP                                          
034500       MOVE '7' TO MFS-IDPFK                                              
034600     END-IF                                                               
034700                                                                          
034800     INITIALIZE GODK-SQLCODEKODER                                         
034900     .                                                                    
035000     EJECT                                                                
035100 B-KOLLA-NYCKLAR SECTION.                                                 
035200                                                                          
035300******   UPPDATERING AV MSGI-BLÄDDRINGSNYCKLAR SKER                       
035400******   I SLUTET AV PROGRAMMET                                           
035500     MOVE ALL '+'               TO MSGI-WMSGINIT                          
035600     MOVE '001'                 TO MSGI-KDCALL                            
035700     MOVE MSG-LTERM-NAME        TO MSGI-IDLTERM-USER                      
035800     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
035900     MOVE '2351'                TO MSGI-IDTRANS                           
036000     IF EGEN-MID                                                          
036100       MOVE MID-IDDC-2351-IN    TO MSGI-IDDC-KEY                          
036200     END-IF                                                               
036300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
036400                                                                          
036500     IF W-IDTRANS            = '2352'                                     
036600     AND MSGI-SPAR-AREA(1:4) = '2352'                                     
036700*                                                                         
036800*      -- FÖR ATT HÄMTA IDTYPE                                            
036900*                                                                         
037000       MOVE MSGI-SPAR-AREA   TO WS-MSGI-AREA-2352                         
037100       MOVE WS-MSGI-IDTYPE   TO WS-IDTYPE                                 
037200                                                                          
037300     ELSE                                                                 
037400                                                                          
037500       IF EGEN-MID                                                        
037600       AND MSGI-SPAR-AREA(1:4) = '2351'                                   
037700         MOVE MSGI-SPAR-AREA TO WS-MSGI-AREA-2351                         
037800       END-IF                                                             
037900                                                                          
038000       IF MID-IDTYPE-2351-IN = ALL '+'                                    
038100         IF MID-IDTYPE-2351-UT = 'BOAT '                                  
038200           MOVE 'B'          TO WS-IDTYPE                                 
038300         END-IF                                                           
038400         IF MID-IDTYPE-2351-UT = 'AIR  '                                  
038500           MOVE 'A'          TO WS-IDTYPE                                 
038600         END-IF                                                           
038700         IF MID-IDTYPE-2351-UT = 'AIRCR'                                  
038800           MOVE 'C'          TO WS-IDTYPE                                 
038900         END-IF                                                           
039000         IF MID-IDTYPE-2351-UT = 'LOCAL'                                  
039100           MOVE 'L'          TO WS-IDTYPE                                 
039200         END-IF                                                           
039300       ELSE                                                               
039400         MOVE MID-IDTYPE-2351-IN TO WS-IDTYPE                             
039500         MOVE SPACE          TO WS-MSGI-SSA-KEY-ENTER                     
039600       END-IF                                                             
039700     END-IF                                                               
039800                                                                          
039900     MOVE JA TO NYCKLAR-SW                                                
040000*    -- KONTROLL AV IDDC                                                  
040100     MOVE MFS-RENSA-FAELT    TO MOD-IDDC-IN                               
040200                                                                          
040210     IF MID-IDDC-2351-IN = ALL '+'                                        
040220        CONTINUE                                                          
040230     ELSE                                                                 
040240        MOVE SPACE           TO WS-MSGI-SSA-KEY-ENTER                     
040250     END-IF                                                               
040260*                                                                         
040300     MOVE MSGI-IDDC-KEY      TO W-IDDC-TP5                                
040400                                WS-IDDC                                   
040710     IF NDC-NA                                                            
040800         CONTINUE                                                         
040900     ELSE                                                                 
041000       MOVE NEJ              TO NYCKLAR-SW                                
041100     END-IF                                                               
041200     MOVE WS-IDDC            TO MOD-IDDC-UT                               
041300                                                                          
041400*      -- KONTROLL AV TYP                                                 
041500     MOVE MFS-RENSA-FAELT    TO MOD-IDTYPE-IN                             
041700                                                                          
041800     IF    WS-IDTYPE = 'A'                                                
041900     OR    WS-IDTYPE = 'C'                                                
042000     OR    WS-IDTYPE = 'B'                                                
042100     OR    WS-IDTYPE = 'L'                                                
042200       IF WS-IDTYPE = 'A'                                                 
042300         MOVE 'AIR'          TO MOD-IDTYPE-UT                             
042400       END-IF                                                             
042500       IF WS-IDTYPE = 'C'                                                 
042600         MOVE 'AIRCR'        TO MOD-IDTYPE-UT                             
042700       END-IF                                                             
042800       IF WS-IDTYPE = 'B'                                                 
042900         MOVE 'BOAT'         TO MOD-IDTYPE-UT                             
043000       END-IF                                                             
043100       IF WS-IDTYPE = 'L'                                                 
043200         MOVE 'LOCAL'        TO MOD-IDTYPE-UT                             
043300       END-IF                                                             
043400       MOVE WS-IDTYPE        TO W-KDREFTYP                                
043500                                W-KDREFTYP-MIN                            
043600                                W-KDREFTYP-MAX                            
043700     ELSE                                                                 
043800       MOVE NEJ TO NYCKLAR-SW                                             
043900       MOVE MED-1            TO MOD-TEMFSINF                              
044000     END-IF                                                               
044100                                                                          
044110     IF NYCKLAR-OK                                                        
044150        PERFORM S01-GET-DCGROUP-ALL-DC                                    
044160        IF WS-DC-NR (1)       > SPACES                                    
044170           MOVE WS-DC-NR (1) TO W-IDDC-MIN                                
044180           MOVE WS-DC-NR (IX-DC-MAX)                                      
044190                             TO W-IDDC-MAX                                
044191        ELSE                                                              
044192           MOVE NEJ          TO NYCKLAR-SW                                
044193        END-IF                                                            
044194     END-IF                                                               
044700                                                                          
044800     IF NYCKLAR-FEL                                                       
044900       MOVE 'GB '         TO MED-IDSKYLT                                  
045000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
045100       CALL WMEDKONV USING MED-WMEDAREA                                   
045200       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
045300       PERFORM MFS-RENSA-FAELT-IN                                         
045400       PERFORM MFS-RENSA-FAELT-UT                                         
045500     END-IF                                                               
045600     .                                                                    
045700     EJECT                                                                
045800 C-FOERSTA-SIDA SECTION.                                                  
045900                                                                          
046000     MOVE 'GB '           TO MED-IDSKYLT                                  
046100     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
046200     CALL WMEDKONV USING MED-WMEDAREA                                     
046300     MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                                  
046400                                                                          
046500*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
046600     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
046700                                WS-MSGI-SSA-KEY-NEXT                      
046800     PERFORM MFS-RENSA-FAELT-IN                                           
046900     .                                                                    
047000     EJECT                                                                
047100 D-NAESTA-SIDA SECTION.                                                   
047200                                                                          
047300     IF WS-MSGI-SSA-KEY-NEXT NOT = SPACE                                  
047400       MOVE WS-MSGI-SSA-KEY-NEXT                                          
047500                             TO W-WDE3A1KY-MIN-X                          
047600     END-IF                                                               
047700     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
047800                                WS-MSGI-SSA-KEY-NEXT                      
047900     .                                                                    
048000     EJECT                                                                
048100 E-SAMMA-SIDA SECTION.                                                    
048200                                                                          
048300     IF WS-MSGI-SSA-KEY-ENTER NOT = SPACE                                 
048400       MOVE WS-MSGI-SSA-KEY-ENTER                                         
048500                             TO W-WDE3A1KY-MIN-X                          
048600     END-IF                                                               
048700     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
048800                                WS-MSGI-SSA-KEY-NEXT                      
048900     MOVE MED-2              TO MOD-TEMFSINF                              
049000     .                                                                    
049100     EJECT                                                                
049200 F-LAES-VISA-INFO SECTION.                                                
049300                                                                          
049400     MOVE +1                 TO INDX                                      
049500     PERFORM IMS-GU-WDE3-ORDW01-MIN-MAX                                   
049600     MOVE SEQA-IDDC          TO WS-IDDC                                   
049700     IF SEGMENT-FINNS                                                     
049800       MOVE SEQA-KDREFTYP    TO WS-KDREFTYP-ENTER                         
049900       MOVE SEQA-IDPERSON-BUY                                             
050000                             TO WS-IDPERSON-BUY-ENTER                     
050100       MOVE SEQA-IDARTNR     TO WS-IDARTNR-ENTER                          
050200       MOVE SEQA-IDDC        TO WS-IDDC-ENTER                             
050300       MOVE WS-WDE3A1KY-ENTER                                             
050400                             TO WS-MSGI-SSA-KEY-ENTER                     
050500                                                                          
050600       PERFORM UNTIL INDX    > MAX-INDX                                   
050700       OR SEGMENT-SAKNAS                                                  
050800           MOVE SEQA-IDPERSON-BUY                                         
050900                             TO WS-SPARA-IDPERSON-BUY                     
051000           MOVE ZERO         TO WS-BUY-TO-REVIEW                          
051100           PERFORM UNTIL SEGMENT-SAKNAS                                   
051200           OR SEQA-IDPERSON-BUY NOT = WS-SPARA-IDPERSON-BUY               
051210             PERFORM S02-CHECK-IDDC                                       
051300             IF SEQA-KDREFORS = 'P'                                       
051400             AND VALID-IDDC-JA                                            
051500               ADD +1        TO WS-BUY-TO-REVIEW                          
051700               MOVE SEQA-IDARTNR                                          
051800                             TO WS-SPARA-IDARTNR                          
051900               PERFORM UNTIL SEGMENT-SAKNAS                               
052000               OR NOT (SEQA-IDPERSON-BUY = WS-SPARA-IDPERSON-BUY          
052100               AND SEQA-IDARTNR = WS-SPARA-IDARTNR                        
052200               AND VALID-IDDC-JA)                                         
052300                                                                          
052400                 PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                       
052500                 MOVE SEQA-IDDC                                           
052600                             TO WS-IDDC                                   
052610                 PERFORM S02-CHECK-IDDC                                   
052700               END-PERFORM                                                
052800             ELSE                                                         
052900               PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                         
053000               MOVE SEQA-IDDC                                             
053100                             TO WS-IDDC                                   
053200             END-IF                                                       
053300                                                                          
053400           END-PERFORM                                                    
053500                                                                          
053600           IF WS-BUY-TO-REVIEW > ZERO                                     
053700             MOVE WS-SPARA-IDPERSON-BUY                                   
053800                             TO MOD-IDPERSON-BUY (INDX)                   
053900             MOVE MFS-ADD-LAES-IN-FAELT                                   
054000                             TO MOD-IDPERSON-BUY-ATTR (INDX)              
054100             MOVE WS-BUY-TO-REVIEW                                        
054200                             TO WS-RED-ANTAL                              
054300             MOVE WS-RED-ANTAL                                            
054400                             TO MOD-TO-REVIEW (INDX)                      
054500             ADD 1           TO INDX                                      
054600           END-IF                                                         
054700       END-PERFORM                                                        
054800                                                                          
054900       IF SEGMENT-FINNS                                                   
055000         MOVE SEQA-KDREFTYP  TO WS-KDREFTYP-NEXT                          
055100         MOVE SEQA-IDPERSON-BUY                                           
055200                             TO WS-IDPERSON-BUY-NEXT                      
055300         MOVE SEQA-IDARTNR   TO WS-IDARTNR-NEXT                           
055400         MOVE SEQA-IDDC      TO WS-IDDC-NEXT                              
055500         MOVE WS-WDE3A1KY-NEXT                                            
055600                             TO WS-MSGI-SSA-KEY-NEXT                      
055700         MOVE 'GB '          TO MED-IDSKYLT                               
055800         MOVE INF-MORE-INFO-EXISTS                                        
055900                             TO MED-IDMFSFEL                              
056000         CALL WMEDKONV USING MED-WMEDAREA                                 
056100         MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                              
056200       ELSE                                                               
056300         MOVE SPACE          TO WS-KDREFTYP-NEXT                          
056400                                WS-IDDC-NEXT                              
056500         MOVE ZERO           TO WS-IDPERSON-BUY-NEXT                      
056600                                WS-IDARTNR-NEXT                           
056700         MOVE ZERO           TO W-IDPERSON-BUY-MIN                        
056800                                W-IDARTNR-MIN                             
056900         MOVE SPACE          TO WS-MSGI-SSA-KEY-NEXT                      
057010       END-IF                                                             
057011*                                                                         
057020       MOVE ZERO             TO WS-TOT-TO-REVIEW                          
057100       PERFORM IMS-GU-WDE3-ORDW01-MIN-MAX                                 
057200       MOVE SEQA-IDDC        TO WS-IDDC                                   
057210       PERFORM S02-CHECK-IDDC                                             
057300                                                                          
057400       PERFORM UNTIL SEGMENT-SAKNAS                                       
057500         IF SEQA-KDREFORS     = 'P'                                       
057600         AND VALID-IDDC-JA                                                
057700           ADD +1            TO WS-TOT-TO-REVIEW                          
057800           MOVE SEQA-IDPERSON-BUY                                         
057900                             TO WS-SPARA-IDPERSON-BUY                     
058000           MOVE SEQA-IDARTNR                                              
058100                             TO WS-SPARA-IDARTNR                          
058200                                                                          
058300           PERFORM UNTIL SEGMENT-SAKNAS                                   
058400           OR NOT (SEQA-IDPERSON-BUY = WS-SPARA-IDPERSON-BUY              
058500           AND SEQA-IDARTNR   = WS-SPARA-IDARTNR                          
058600           AND VALID-IDDC-JA)                                             
058700                                                                          
058800             PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                           
058900             MOVE SEQA-IDDC                                               
059000                             TO WS-IDDC                                   
059010             PERFORM S02-CHECK-IDDC                                       
059100           END-PERFORM                                                    
059200         ELSE                                                             
059300           PERFORM IMS-GN-WDE3-ORDW01-MIN-MAX                             
059400           MOVE SEQA-IDDC    TO WS-IDDC                                   
059410           PERFORM S02-CHECK-IDDC                                         
059500         END-IF                                                           
059600                                                                          
059700       END-PERFORM                                                        
059800       ADD +1                TO INDX                                      
059900       MOVE WS-TOT-TO-REVIEW                                              
060000                             TO WS-RED-ANTAL                              
060100       MOVE WS-RED-ANTAL     TO MOD-TO-REVIEW (INDX)                      
060300     ELSE                                                                 
060400       MOVE SPACE            TO WS-MSGI-SSA-KEY-ENTER                     
060500       IF MFS-FIRST                                                       
060600         MOVE 'GB '         TO MED-IDSKYLT                                
060700         MOVE URVAL-SAKNAS   TO MED-IDMFSFEL                              
060800         CALL WMEDKONV USING MED-WMEDAREA                                 
060900         MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                              
061000       END-IF                                                             
061100     END-IF                                                               
061200     .                                                                    
061300     EJECT                                                                
063312 S01-GET-DCGROUP-ALL-DC SECTION.                                          
063313                                                                          
063314     PERFORM DB2-DCL-OPN-TP5IDDC-CRS                                      
063315     PERFORM DB2-FETCH-TP5IDDC-CRS                                        
063316     MOVE +1                 TO IX-DC                                     
063317     PERFORM UNTIL IX-DC > IX-DC-MAX OR RADER-SAKNAS                      
063318       MOVE TP5IDDC-IDDC     TO WS-DC-NR (IX-DC)                          
063319       PERFORM DB2-FETCH-TP5IDDC-CRS                                      
063320       MOVE IX-DC            TO IX-DC-N                                   
063321       ADD +1                TO IX-DC                                     
063322     END-PERFORM                                                          
063323     MOVE IX-DC-N            TO IX-DC-MAX                                 
063324     PERFORM DB2-CLOSE-TP5IDDC-CRS                                        
063325     .                                                                    
063326     EJECT                                                                
063327 S02-CHECK-IDDC SECTION.                                                  
063328                                                                          
063329     MOVE NEJ                TO VALID-IDDC-SW                             
063330     MOVE +1                 TO IX-DC                                     
063331     PERFORM UNTIL IX-DC     >  IX-DC-MAX                                 
063332       IF WS-IDDC            =  WS-DC-NR (IX-DC)                          
063333          MOVE JA            TO VALID-IDDC-SW                             
063334          MOVE IX-DC-MAX     TO IX-DC                                     
063335       END-IF                                                             
063336       ADD +1                TO IX-DC                                     
063337     END-PERFORM                                                          
063338     .                                                                    
063339     EJECT                                                                
063340 MFS-RENSA-FAELT-UT SECTION.                                              
063400                                                                          
063500*    --- ALLA UTDATA-FÄLT                                                 
063600     MOVE MFS-RENSA-FAELT    TO MOD-IDTYPE-UT                             
063700                                                                          
063800     MOVE 1                  TO INDX                                      
063900     PERFORM UNTIL INDX > 13                                              
064000       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
064100       ADD 1                 TO INDX                                      
064200     END-PERFORM                                                          
064300     .                                                                    
064400     SKIP3                                                                
064500 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
064600                                                                          
064700*    --- ALLA UTDATA-FÄLT                                                 
064800                                                                          
064900     MOVE MFS-RENSA-FAELT    TO MOD-SELECT (INDX)                         
065000                                MOD-IDPERSON-BUY (INDX)                   
065100                                MOD-TO-REVIEW (INDX)                      
065200     .                                                                    
065300     SKIP3                                                                
065400 MFS-RENSA-FAELT-IN SECTION.                                              
065500                                                                          
065600*    --- ALLA INDATA-FÄLT                                                 
065700     MOVE MFS-RENSA-FAELT    TO MOD-IDTYPE-IN                             
065800                                                                          
065900     MOVE 1                  TO INDX                                      
066000     PERFORM UNTIL INDX > 13                                              
066100       MOVE MFS-RENSA-FAELT  TO MOD-SELECT (INDX)                         
066200       ADD 1                 TO INDX                                      
066300     END-PERFORM                                                          
066400     .                                                                    
066500     EJECT                                                                
066600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
066700                                                                          
066800*    --- ALLA UTDATA-FÄLT                                                 
066900     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDTYPE-UT                             
067000                                MOD-TEMFSINF                              
067100     MOVE +1 TO INDX                                                      
067200     PERFORM UNTIL INDX > MAX-INDX                                        
067300       MOVE MFS-ROER-EJ-FAELT                                             
067400                             TO MOD-SELECT (INDX)                         
067500                                MOD-IDPERSON-BUY (INDX)                   
067600                                MOD-TO-REVIEW (INDX)                      
067700       ADD +1 TO INDX                                                     
067800     END-PERFORM                                                          
067900     .                                                                    
068000     SKIP2                                                                
068100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
068200                                                                          
068300*    --- ALLA INDATA-FÄLT                                                 
068400     MOVE MFS-ROER-EJ-FAELT  TO MOD-IDTYPE-IN                             
068500     .                                                                    
068600     EJECT                                                                
068700* --- DB2 SEKTIONER ---                                                   
068800     SKIP3                                                                
068900 DB2-DCL-OPN-TP5IDDC-CRS  SECTION.                                        
069000                                                                          
069100     MOVE 000100  TO GODK-SQLCODEKODER                                    
069200                                                                          
069300     EXEC SQL                                                             
069400         DECLARE TP5IDDC-CRS CURSOR FOR                                   
069500                                                                          
069600           SELECT  IDDC, IDLOPNR_DC                                       
069700                                                                          
069800           FROM    TP5IDDC                                                
069900           WHERE   IDLOPNR_DC = (SELECT IDLOPNR_DC                        
070000                                 FROM TP5IDDC                             
070100                                 WHERE IDDC = :W-IDDC-TP5)                
070200           ORDER BY IDDC                                                  
070300     END-EXEC                                                             
070400                                                                          
070500     MOVE 000100  TO GODK-SQLCODEKODER                                    
070600     EXEC SQL OPEN TP5IDDC-CRS END-EXEC                                   
070700                                                                          
070800     .                                                                    
070900     SKIP3                                                                
071000 DB2-FETCH-TP5IDDC-CRS  SECTION.                                          
071100     SKIP2                                                                
071200     MOVE 000100  TO GODK-SQLCODEKODER                                    
071300     EXEC SQL                                                             
071400         FETCH TP5IDDC-CRS INTO :TP5IDDC-IDDC                             
071500                               ,:TP5IDDC-IDLOPNR-DC                       
071600     END-EXEC                                                             
071700                                                                          
071800     MOVE SQLCODE TO SQLCODE-WS                                           
071900     PERFORM DB2-STATUS-KONTROLL                                          
072000     .                                                                    
072100     SKIP3                                                                
072200 DB2-CLOSE-TP5IDDC-CRS  SECTION.                                          
072300                                                                          
072400     EXEC SQL CLOSE TP5IDDC-CRS END-EXEC                                  
072500     .                                                                    
072600     EJECT                                                                
072720 DB2-STATUS-KONTROLL  SECTION.                                            
072800                                                                          
072900     SET SQLCODE-IX TO 1                                                  
073000     SEARCH GODK-SQLCODE                                                  
073100       AT END                                                             
073200          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
073300          DELIMITED BY SIZE INTO FELTEXT                                  
073400          CALL ABEND USING RKOD-ABEND-DB2                                 
073500       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
073600     END-SEARCH                                                           
073700     .                                                                    
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
075000     IF ENGLISH-TEXT                                                      
075100       MOVE 'N' TO MFS-KDHUVOMR                                           
075200     END-IF                                                               
075300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
075400     MOVE SPACE TO GODK-STATUSKODER                                       
075500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
075600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075700     PERFORM IMS-STATUSKONTROLL                                           
075800     .                                                                    
075900     EJECT                                                                
076000 IMS-GU-WDE3-ORDW01-MIN-MAX SECTION.                                      
076100                                                                          
076200     STRING 'WLORDW01(WDE3A1KY>=' W-WDE3A1KY-MIN-X                        
076300                    '&WDE3A1KY<=' W-WDE3A1KY-MAX-X ')'                    
076400          DELIMITED BY SIZE INTO SSA1                                     
076500     MOVE '  GE' TO GODK-STATUSKODER                                      
076600     CALL CBLTDLI USING GU ORDW-PCB DLI-IO-AREA-ORDW01 SSA1               
076700     MOVE ORDW-STATUS-CODE TO STATUS-WS                                   
076800     PERFORM IMS-STATUSKONTROLL                                           
076900     .                                                                    
077000     SKIP3                                                                
077100 IMS-GN-WDE3-ORDW01-MIN-MAX SECTION.                                      
077200                                                                          
077300     STRING 'WLORDW01(WDE3A1KY>=' W-WDE3A1KY-MIN-X                        
077400                    '&WDE3A1KY<=' W-WDE3A1KY-MAX-X ')'                    
077500          DELIMITED BY SIZE INTO SSA1                                     
077600     MOVE '  GE' TO GODK-STATUSKODER                                      
077700     CALL CBLTDLI USING GN ORDW-PCB DLI-IO-AREA-ORDW01 SSA1               
077800     MOVE ORDW-STATUS-CODE TO STATUS-WS                                   
077900     PERFORM IMS-STATUSKONTROLL                                           
078000     .                                                                    
078100     SKIP3                                                                
078200 IMS-STATUSKONTROLL SECTION.                                              
078300                                                                          
078400     SET STATUS-IX TO 1                                                   
078500     SEARCH GODK-STATUS                                                   
078600       AT END                                                             
078700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
078800         DELIMITED BY SIZE INTO FELTEXT                                   
078900         CALL FELLOG                                                      
079000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
079100         CONTINUE                                                         
079200     END-SEARCH                                                           
079300     .                                                                    
