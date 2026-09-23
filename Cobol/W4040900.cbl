000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4040900.                                                
000300 AUTHOR.         ARUP DATTA.                                              
000400 DATE-WRITTEN.   16/11/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        POSSIBILITY TO CHANGE FC FACTOR                                  
000900*                                                                         
001000*        PROGRAM    READS      WDB6                                       
001100*        PROGRAM    UPDATES    WDB6                                       
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W4T409                                              
001500*                     W4T409U                                             
001600*                     W4T409V                                             
001700*        MID:         W4I40901                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W4O40901                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W4040900'.            
002900                                                                          
003000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500*                                                                         
003600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003700 01  WORK.                                                                
003800     03 IX-RAD                   PIC S9(3)  COMP-3  VALUE ZERO.           
003900     03 IX-OTH                   PIC S9(3)  COMP-3  VALUE ZERO.           
004000     03 IX-TAB                   PIC S9(3)  COMP-3  VALUE ZERO.           
004100     03 IX-RAD-MAX               PIC S9(3)  COMP-3  VALUE 12.             
004200     03 IX-TAB-MAX               PIC S9(3)  COMP-3  VALUE 12.             
004300     03 WS-TEMFSFEL              PIC X(40)   VALUE SPACE.                 
004400     03 WS-TEMFSINF              PIC X(40)   VALUE SPACE.                 
004500     03 WS-IDDC-DEFAULT          PIC X(2)    VALUE '99'.                  
004600     03 WS-TOT-FC-NORM           PIC S9V9(2) VALUE ZERO COMP-3.           
004700     03 WS-TOT-FC-SVAG           PIC S9V9(2) VALUE ZERO COMP-3.           
004800     03 WS-TOT-FC-STARK          PIC S9V9(2) VALUE ZERO COMP-3.           
004900     03 WS-GODK-TOT-FC           PIC S9V9(2) VALUE ZERO COMP-3.           
005000     03 W-IDUSER                 PIC X(8)    VALUE SPACES.                
005100     03 WS-IDDC                  PIC X(2)    VALUE SPACES.                
005200     03 WS-IDDC-COPY             PIC X(2)    VALUE SPACES.                
005300     03 WS-KDPROGOI              PIC X(1)    VALUE SPACES.                
005400     03 WS-KDPROGOI-COPY         PIC X(1)    VALUE SPACES.                
005500     03 WS-DEFAULT               PIC X(1)    VALUE SPACES.                
005600     03 WS-FC-FACT-MAX           PIC S9V9(2) VALUE 1.00 COMP-3.           
005700                                                                          
005800 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005900                                                                          
006000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006100     88  INDATA-OK                           VALUE 'J'.                   
006200     88  INDATA-FEL                          VALUE 'N'.                   
006300                                                                          
006400 77  INPUT-RAD-SW                PIC X       VALUE 'N'.                   
006500     88  INPUT-RAD-JA                        VALUE 'J'.                   
006600     88  INPUT-RAD-NEJ                       VALUE 'N'.                   
006700                                                                          
006800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006900     88  NYCKLAR-OK                          VALUE 'J'.                   
007000     88  NYCKLAR-FEL                         VALUE 'N'.                   
007100                                                                          
007200 77  UPD-DONE-SW                 PIC X       VALUE 'N'.                   
007300     88  UPD-DONE-JA                         VALUE 'J'.                   
007400     88  UPD-DONE-NEJ                        VALUE 'N'.                   
007500                                                                          
007600 77  UPD-PFKEY-SW                PIC X       VALUE 'N'.                   
007700     88  UPD-PFKEY-JA                        VALUE 'J'.                   
007800                                                                          
007900 77  UPD-RAD-SW                  PIC X       VALUE 'N'.                   
008000     88  UPD-RAD-JA                          VALUE 'J'.                   
008100     88  UPD-RAD-NEJ                         VALUE 'N'.                   
008200                                                                          
008300 77  UPD-DEF-FC-SW               PIC X       VALUE 'N'.                   
008400     88  UPD-DEF-FC-JA                       VALUE 'J'.                   
008500     88  UPD-DEF-FC-NEJ                      VALUE 'N'.                   
008600                                                                          
008700 77  TREND-NORM-SW               PIC X       VALUE 'N'.                   
008800     88  TREND-NORM-JA                       VALUE 'J'.                   
008900     88  TREND-NORM-NEJ                      VALUE 'N'.                   
009000                                                                          
009100 77  TREND-SVAG-SW               PIC X       VALUE 'N'.                   
009200     88  TREND-SVAG-JA                       VALUE 'J'.                   
009300     88  TREND-SVAG-NEJ                      VALUE 'N'.                   
009400                                                                          
009500 77  TREND-STARK-SW              PIC X       VALUE 'N'.                   
009600     88  TREND-STARK-JA                      VALUE 'J'.                   
009700     88  TREND-STARK-NEJ                     VALUE 'N'.                   
009800                                                                          
009900 77  UPD-FC-NORM-SW              PIC X       VALUE 'N'.                   
010000     88  UPD-FC-NORM-JA                      VALUE 'J'.                   
010100     88  UPD-FC-NORM-NEJ                     VALUE 'N'.                   
010200                                                                          
010300 77  UPD-FC-SVAG-SW              PIC X       VALUE 'N'.                   
010400     88  UPD-FC-SVAG-JA                      VALUE 'J'.                   
010500     88  UPD-FC-SVAG-NEJ                     VALUE 'N'.                   
010600                                                                          
010700 77  UPD-FC-STARK-SW             PIC X       VALUE 'N'.                   
010800     88  UPD-FC-STARK-JA                     VALUE 'J'.                   
010900     88  UPD-FC-STARK-NEJ                    VALUE 'N'.                   
011000                                                                          
011100 77  INPUT-DEF-SW                PIC X       VALUE 'N'.                   
011200     88  INPUT-DEF-JA                        VALUE 'J'.                   
011300     88  INPUT-DEF-NEJ                       VALUE 'N'.                   
011400                                                                          
011500 77  INPUT-COPY-SW               PIC X       VALUE 'N'.                   
011600     88  INPUT-COPY-JA                       VALUE 'J'.                   
011700     88  INPUT-COPY-NEJ                      VALUE 'N'.                   
011800                                                                          
011900 77  TREND-SW                    PIC X       VALUE ' '.                   
012000     88  TREND-NORM                          VALUE '1'.                   
012100     88  TREND-SVAG                          VALUE '2'.                   
012200     88  TREND-STARK                         VALUE '3'.                   
012300                                                                          
012400 01  WS-TAB-FC-IDDC.                                                      
012500     03 WS-TAB-PER-FC            OCCURS 12.                               
012600        05 WS-TAB-PER            PIC 9(2)    VALUE ZERO.                  
012700        05 WS-TAB-REFT-NORM      PIC S9V9(2) VALUE ZERO  COMP-3.          
012800        05 WS-TAB-REFT-SVAG      PIC S9V9(2) VALUE ZERO  COMP-3.          
012900        05 WS-TAB-REFT-STARK     PIC S9V9(2) VALUE ZERO  COMP-3.          
013000     EJECT                                                                
013100                                                                          
013200 01  WS-TMP-TREND                PIC X(3)    VALUE ZERO.                  
013300 01  FILLER REDEFINES WS-TMP-TREND.                                       
013400     03 WS-TREND-TAB             OCCURS 3.                                
013500        05 WS-TREND-E            PIC X.                                   
013600                                                                          
013700 01  WS-TREND-DECEDIT            PIC 9.9(2)  VALUE ZERO.                  
013800 01  WS-TMP-TREND-FORMAT         PIC 9(3)    VALUE ZERO.                  
013900 01  FILLER REDEFINES WS-TMP-TREND-FORMAT.                                
014000     03 WS-TREND-FORM-R          PIC 9V9(2).                              
014100                                                                          
014200 01  WS-TMP-REFT-NORM            PIC 9V9(2)  VALUE ZERO.                  
014300 01  FILLER REDEFINES WS-TMP-REFT-NORM.                                   
014400     03 WS-REFT-NORM-N           PIC 9(3).                                
014500*                                                                         
014600 01  WS-TMP-REFT-SVAG            PIC 9V9(2)  VALUE ZERO.                  
014700 01  FILLER REDEFINES WS-TMP-REFT-SVAG.                                   
014800     03 WS-REFT-SVAG-N           PIC 9(3).                                
014900*                                                                         
015000 01  WS-TMP-REFT-STARK           PIC 9V9(2)  VALUE ZERO.                  
015100 01  FILLER REDEFINES WS-TMP-REFT-STARK.                                  
015200     03 WS-REFT-STARK-N          PIC 9(3).                                
015300*                                                                         
015400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
015500     88  EGEN-MID                            VALUE '4409'.                
015600     88  GODK-MID                            VALUE '4409'.                
015700     88  HELP-MID                            VALUE '0551'.                
015800     EJECT                                                                
015900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
016000 01  GENERELLA-SUBPROGRAM.                                                
016100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016600     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
016700     EJECT                                                                
016800*    --- PARAMETRAR TILL SUBPROGRAM WDECAREA                              
016900 01  FILLER                      PIC X(16) VALUE 'WDECAREA'.              
017000*01 -COPY WDECAREA                                                        
017100     SKIP3                                                                
017200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
017300*01 -COPY WMEDAREA                                                        
017400     SKIP3                                                                
017500 01  MESSAGE-CODES.                                                       
017600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
017700     03  CONFLICT                PIC X(3)    VALUE '002'.                 
017800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
017900     03  INF-PRESS-PF23          PIC X(3)    VALUE '206'.                 
018000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
018100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
018200     EJECT                                                                
018300 01  FELTEXTER.                                                           
018400     03  MED-1                  PIC X(40)                                 
018500         VALUE 'DEFAULT VALUES                '.                          
018600     03  MED-2                  PIC X(40)                                 
018700         VALUE 'TOTAL SUM OF FACTORS HAS TO BE 100'.                      
018800     03  MED-3                  PIC X(40)                                 
018900         VALUE 'PRESS PF23 TO UPDATE           '.                         
019000     03  MED-4                  PIC X(40)                                 
019100         VALUE 'PRESS ENTER BEFORE PF11        '.                         
019200     03  MED-5                  PIC X(40)                                 
019300         VALUE 'USE SET DEFAULT FUNCTION INSTEAD'.                        
019400     03  MED-6                  PIC X(40)                                 
019500         VALUE 'FUNCTION NOT AVAILABLE FOR DC 99'.                        
019600     03  MED-7                  PIC X(40)                                 
019700         VALUE 'NO DC-SPECIFIC VALUES           '.                        
019800                                                                          
019900     EJECT                                                                
020000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
020100*                                                                         
020200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
020300     SKIP3                                                                
020400*01 -COPY WMSGINIT                                                        
020500     EJECT                                                                
020600 01  FILLER                      PIC X(16)   VALUE 'DAT-AREA'.            
020700     SKIP3                                                                
020800*01 -COPY WDATAREA                                                        
020900     EJECT                                                                
021000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
021100*                                                                         
021200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
021300     SKIP3                                                                
021400*01  MID -COPY W4I40901                                                   
021500     EJECT                                                                
021600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021700     SKIP3                                                                
021800*01  -COPY WMSGAREA                                                       
021900     EJECT                                                                
022000     03  MOD REDEFINES MSG-AREA.                                          
022100*      05  -COPY W4O40901                                                 
022200     EJECT                                                                
022300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
022400     SKIP3                                                                
022500*01  -COPY WMFSAREA                                                       
022600     EJECT                                                                
022700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022800*                                                                         
022900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023000     SKIP3                                                                
023100 01  NYCKLAR-TILL-DLI.                                                    
023200     03  W-IDDC-B6-X.                                                     
023300         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
023400*                                                                         
023500     03  W-IDTRANS-B6-X.                                                  
023600         05  W-IDTRANS-B6        PIC X(4)    VALUE '4409'.                
023700*                                                                         
023800     03  W-KDPROGOI-B6-X.                                                 
023900         05  W-KDPROGOI-B6       PIC X       VALUE SPACE.                 
024000     SKIP2                                                                
024100*    --- STATUS-KOD FRÅN IMS                                              
024200 01  STATUS-WS                   PIC XX.                                  
024300     88  SEGMENT-FINNS                       VALUE '  '.                  
024400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
024700     SKIP2                                                                
024800 01  GODK-STATUSKODER.                                                    
024900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025000     SKIP3                                                                
025100 01  SSA1                        PIC X(64).                               
025200 01  SSA2                        PIC X(64).                               
025300     EJECT                                                                
025400*    --- IMS FUNKTIONSKODER                                               
025500*01  -COPY W0003                                                          
025600     EJECT                                                                
025700*    ---  DLI INPUT-OUTPUT AREA                                           
025800                                                                          
025900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
026000 01  DLI-IO-WDB601.                                                       
026100*    03  -COPY WDB601                                                     
026200     EJECT                                                                
026300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB615'.                      
026400 01  DLI-IO-WDB615.                                                       
026500*    03  -COPY WDB615                                                     
026600     EJECT                                                                
026700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB618'.                      
026800 01  DLI-IO-WDB618.                                                       
026900*    03  -COPY WDB618                                                     
027000     EJECT                                                                
027100 LINKAGE SECTION.                                                         
027200*01  -COPY W0009   -PRE MSG-                                              
027300*01  -COPY W0008   -PRE USEA-                                             
027400     05  FILLER                  PIC X.                                   
027500                                                                          
027600*01  -COPY W0008  -PRE WDB6-                                              
027700     05  FILLER                  PIC X.                                   
027800     EJECT                                                                
027900 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDB6-PCB.                     
028000 MAIN SECTION.                                                            
028100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDB6-PCB.                     
028200                                                                          
028300     PERFORM IMS-GET-MSG                                                  
028400     IF SEGMENT-FINNS                                                     
028500       PERFORM A-INIT                                                     
028600       PERFORM B-KOLLA-NYCKLAR                                            
028700       IF NYCKLAR-OK                                                      
028800           IF MFS-UPDATE OR MFS-UPD-V                                     
028900              PERFORM G-CONTROL-INPUT                                     
029000              IF INDATA-OK                                                
029100                 PERFORM H-UPPDATERA                                      
029200              END-IF                                                      
029300           ELSE                                                           
029400             IF MFS-FIRST                                                 
029500                PERFORM C-FOERSTA-SIDA                                    
029600             ELSE                                                         
029700                PERFORM E-SAMMA-SIDA                                      
029800             END-IF                                                       
029900           END-IF                                                         
030000           PERFORM F-LAES-VISA-INFO                                       
030100       END-IF                                                             
030200       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O40901 + 4                      
030300       PERFORM IMS-INSERT-MSG                                             
030400     END-IF                                                               
030500                                                                          
030600     MOVE ZERO TO RETURN-CODE                                             
030700     GOBACK                                                               
030800     .                                                                    
030900     EJECT                                                                
031000 A-INIT SECTION.                                                          
031100                                                                          
031200     IF MSG-DUBBLA-TRANSKODER                                             
031300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I40901                 
031400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
031500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
031600     ELSE                                                                 
031700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I40901                  
031800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
031900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
032000     END-IF                                                               
032100                                                                          
032200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
032300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
032400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
032500                                                                          
032600     MOVE LOW-VALUE TO MSG-AREA                                           
032700     MOVE 'W4O409N1' TO MFS-IDMOD                                         
032800     MOVE '4409' TO MOD-IDTRANS                                           
032900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
033000                             MOD-TEMFSINF                                 
033100                                                                          
033200     ACCEPT DAGENS-DATUM FROM DATE                                        
033300                                                                          
033400     IF EGEN-MID OR HELP-MID                                              
033500       CONTINUE                                                           
033600     ELSE                                                                 
033700       MOVE SPACE TO MFS-KDTRTYP                                          
033800       MOVE '7' TO MFS-IDPFK                                              
033900     END-IF                                                               
034000*                                                                         
034100     MOVE 'GB '                  TO MED-IDSKYLT                           
034200     MOVE SPACE                  TO WS-TEMFSFEL                           
034300                                    WS-TEMFSINF                           
034400     .                                                                    
034500     EJECT                                                                
034600 B-KOLLA-NYCKLAR SECTION.                                                 
034700                                                                          
034800     MOVE ALL '+'                TO MSGI-WMSGINIT                         
034900     MOVE '001'                  TO MSGI-KDCALL                           
035000     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
035100     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
035200                                    W-IDUSER                              
035300     MOVE '4409'                 TO MSGI-IDTRANS                          
035400     IF EGEN-MID                                                          
035500         MOVE MID-IDDC-IN        TO MSGI-IDDC-KEY                         
035600     END-IF                                                               
035700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
035800                                                                          
035900*    -- SET LANGUAGE USING WMEDKONV                                       
036000     MOVE 'GB  '                 TO MED-IDSKYLT                           
036100                                                                          
036200     MOVE JA TO NYCKLAR-SW                                                
036300                                                                          
036400*    -- CONTROL OF IDDC                                                   
036500                                                                          
036600     MOVE MSGI-IDDC-KEY          TO WS-IDDC                               
036700                                                                          
036800     IF  MFS-UPDATE                                                       
036900     OR  MFS-UPD-V                                                        
037000         MOVE JA                 TO UPD-PFKEY-SW                          
037100     END-IF                                                               
037200                                                                          
037300     IF  MID-IDDC-IN              = ALL '+'                               
037400     AND MID-KDPROGOI-IN          = ALL '+'                               
037500         CONTINUE                                                         
037600     ELSE                                                                 
037700         MOVE '7'                TO MFS-IDPFK                             
037800         MOVE SPACE              TO MFS-KDTRTYP                           
037900     END-IF                                                               
038000                                                                          
038100*    -- ACCESS WDB6 TO VALIDATE DC                                        
038200     MOVE WS-IDDC                TO W-IDDC-B6                             
038300     PERFORM IMS-GU-WDB601                                                
038400     IF SEGMENT-FINNS                                                     
038500        MOVE WS-IDDC             TO MOD-IDDC-UT                           
038600     ELSE                                                                 
038700        MOVE NEJ                 TO NYCKLAR-SW                            
038800        MOVE MFS-RENSA-FAELT     TO MOD-IDDC-UT                           
038900        MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDDC-IN-ATTR                      
039000        MOVE WS-IDDC             TO MOD-IDDC-IN                           
039100     END-IF                                                               
039200                                                                          
039300     INSPECT MOD-IDDC-UT    REPLACING LEADING '+'  BY SPACE               
039400                                                                          
039500*    -- CONTROL OF KDPROGOI                                               
039600     MOVE SPACES                 TO WS-KDPROGOI                           
039700     IF  MID-IDDC-IN     = ALL '+'                                        
039800     AND MID-KDPROGOI-IN = ALL '+'                                        
039900        INSPECT MID-KDPROGOI-UT REPLACING LEADING '+' BY SPACE            
040000        IF MID-KDPROGOI-UT        = 'CUSTOMER ORDER FORECAST'             
040100              MOVE 'S'           TO WS-KDPROGOI                           
040200        ELSE                                                              
040300           IF MID-KDPROGOI-UT     = 'REFILL ORDER FORECAST'               
040400              MOVE 'R'           TO WS-KDPROGOI                           
040500           END-IF                                                         
040600        END-IF                                                            
040700     ELSE                                                                 
040800       IF MID-KDPROGOI-IN NOT = ALL '+'                                   
040900          MOVE MID-KDPROGOI-IN   TO WS-KDPROGOI                           
041000       END-IF                                                             
041100     END-IF                                                               
041200                                                                          
041300     IF WS-KDPROGOI           NOT > SPACES                                
041400       IF NOT UPD-PFKEY-JA                                                
041500          MOVE 'S'               TO WS-KDPROGOI                           
041600       ELSE                                                               
041700          MOVE NEJ               TO NYCKLAR-SW                            
041800          MOVE MFS-ALFA-FAELT-FEL                                         
041900                                 TO MOD-KDPROGOI-IN-ATTR                  
042000       END-IF                                                             
042100     ELSE                                                                 
042200        IF WS-KDPROGOI            = 'S' OR 'R'                            
042300           CONTINUE                                                       
042400        ELSE                                                              
042500           MOVE NEJ              TO NYCKLAR-SW                            
042600           MOVE MFS-RENSA-FAELT  TO MOD-KDPROGOI-UT                       
042700           MOVE MFS-ALFA-FAELT-FEL                                        
042800                                 TO MOD-KDPROGOI-IN-ATTR                  
042900           MOVE WS-KDPROGOI      TO MOD-KDPROGOI-IN                       
043000        END-IF                                                            
043100     END-IF                                                               
043200*                                                                         
043300     IF NYCKLAR-OK                                                        
043400       MOVE WS-IDDC              TO MOD-IDDC-UT                           
043500       IF WS-KDPROGOI       = 'S'                                         
043600          MOVE 'CUSTOMER ORDER FORECAST'                                  
043700                                 TO MOD-KDPROGOI-UT                       
043800       ELSE                                                               
043900          MOVE 'REFILL ORDER FORECAST'                                    
044000                                 TO MOD-KDPROGOI-UT                       
044100       END-IF                                                             
044200     END-IF                                                               
044300*                                                                         
044400     IF NYCKLAR-FEL                                                       
044500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
044600       CALL WMEDKONV USING MED-WMEDAREA                                   
044700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
044800       PERFORM MFS-RENSA-FAELT-UT                                         
044900       PERFORM MFS-RENSA-FAELT-IN                                         
045000     END-IF                                                               
045100     .                                                                    
045200     EJECT                                                                
045300 C-FOERSTA-SIDA SECTION.                                                  
045400                                                                          
045500     PERFORM MFS-RENSA-FAELT-IN                                           
045600     .                                                                    
045700     EJECT                                                                
045800 E-SAMMA-SIDA SECTION.                                                    
045900                                                                          
046000     PERFORM EA-CONTROL-HEADER-INP                                        
046100                                                                          
046200     IF INDATA-OK                                                         
046300        PERFORM EB-CONTROL-OTHER-INP                                      
046400     END-IF                                                               
046500     .                                                                    
046600     EJECT                                                                
046700 EA-CONTROL-HEADER-INP  SECTION.                                          
046800                                                                          
046900     IF MID-IDDC-COPY-IN      NOT = ALL '+'                               
047000        MOVE MID-IDDC-COPY-IN    TO WS-IDDC-COPY                          
047100     END-IF                                                               
047200                                                                          
047300     IF MID-KDPROGOI-COPY-IN  NOT = ALL '+'                               
047400        MOVE MID-KDPROGOI-COPY-IN                                         
047500                                 TO WS-KDPROGOI-COPY                      
047600     END-IF                                                               
047700                                                                          
047800     IF MID-DEFAULT-IN        NOT = ALL '+'                               
047900        MOVE MID-DEFAULT-IN      TO WS-DEFAULT                            
048000     END-IF                                                               
048100*                                                                         
048200     IF WS-IDDC-COPY > SPACES                                             
048300        MOVE WS-IDDC-COPY        TO W-IDDC-B6                             
048400        PERFORM IMS-GU-WDB601                                             
048500        IF SEGMENT-FINNS                                                  
048600           MOVE MFS-ALFA-FAELT-RAETT                                      
048700                                 TO MOD-IDDC-COPY-IN-ATTR                 
048800           MOVE WS-IDDC-COPY     TO MOD-IDDC-COPY-IN                      
048900        ELSE                                                              
049000           MOVE NEJ              TO INDATA-SW                             
049100           MOVE MFS-ALFA-FAELT-FEL                                        
049200                                 TO MOD-IDDC-COPY-IN-ATTR                 
049300           MOVE WS-IDDC-COPY     TO MOD-IDDC-COPY-IN                      
049400        END-IF                                                            
049500     END-IF                                                               
049600     IF WS-KDPROGOI-COPY      > SPACES                                    
049700        IF WS-KDPROGOI-COPY       = 'S' OR 'R'                            
049800           MOVE MFS-ALFA-FAELT-RAETT                                      
049900                                 TO MOD-KDPROGOI-COPY-IN-ATTR             
050000           MOVE WS-KDPROGOI-COPY TO MOD-KDPROGOI-COPY-IN                  
050100        ELSE                                                              
050200           MOVE NEJ              TO INDATA-SW                             
050300           MOVE MFS-ALFA-FAELT-FEL                                        
050400                                 TO MOD-KDPROGOI-COPY-IN-ATTR             
050500           MOVE WS-KDPROGOI-COPY TO MOD-KDPROGOI-COPY-IN                  
050600           MOVE WS-IDDC-COPY     TO MOD-IDDC-COPY-IN                      
050700        END-IF                                                            
050800     END-IF                                                               
050900     IF  WS-IDDC-COPY             > SPACES                                
051000     AND WS-KDPROGOI-COPY     NOT > SPACES                                
051100           MOVE NEJ              TO INDATA-SW                             
051200           MOVE MFS-ALFA-FAELT-FEL                                        
051300                                 TO MOD-KDPROGOI-COPY-IN-ATTR             
051400           MOVE WS-IDDC-COPY     TO MOD-IDDC-COPY-IN                      
051500     ELSE                                                                 
051600       IF  WS-IDDC-COPY       NOT > SPACES                                
051700       AND WS-KDPROGOI-COPY       > SPACES                                
051800           MOVE NEJ              TO INDATA-SW                             
051900           MOVE MFS-ALFA-FAELT-FEL                                        
052000                                 TO MOD-IDDC-COPY-IN-ATTR                 
052100           MOVE WS-KDPROGOI-COPY TO MOD-KDPROGOI-COPY-IN                  
052200       END-IF                                                             
052300     END-IF                                                               
052400     IF WS-IDDC-COPY              > SPACES                                
052500        MOVE SPACES              TO WS-DEFAULT                            
052600     END-IF                                                               
052700     IF WS-DEFAULT                > SPACES                                
052800        IF WS-DEFAULT             = 'J' OR 'Y'                            
052900           MOVE MFS-ALFA-FAELT-RAETT                                      
053000                                 TO MOD-DEFAULT-IN-ATTR                   
053100           MOVE WS-DEFAULT       TO MOD-DEFAULT-IN                        
053200        ELSE                                                              
053300           MOVE NEJ              TO INDATA-SW                             
053400           MOVE MFS-ALFA-FAELT-FEL                                        
053500                                 TO MOD-DEFAULT-IN-ATTR                   
053600           MOVE WS-DEFAULT       TO MOD-DEFAULT-IN                        
053700        END-IF                                                            
053800     END-IF                                                               
053900                                                                          
054000     IF INDATA-FEL                                                        
054100        MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                          
054200        CALL WMEDKONV         USING MED-WMEDAREA                          
054300        MOVE MED-MFSFEL          TO MOD-TEMFSFEL                          
054400     END-IF                                                               
054500     .                                                                    
054600     EJECT                                                                
054700 EB-CONTROL-OTHER-INP   SECTION.                                          
054800                                                                          
054900     PERFORM S01-CHECK-INPUT                                              
055000*                                                                         
055100     IF  INPUT-RAD-NEJ                                                    
055200     AND INPUT-DEF-NEJ                                                    
055300     AND INPUT-COPY-NEJ                                                   
055400         PERFORM MFS-RENSA-FAELT-IN                                       
055500     ELSE                                                                 
055600       IF EGEN-MID OR HELP-MID                                            
055700          IF WS-IDDC-COPY  = WS-IDDC-DEFAULT                              
055800             MOVE NEJ                TO INDATA-SW                         
055900             MOVE ERR-WRONG-KEY      TO MED-IDMFSFEL                      
056000             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-COPY-IN-ATTR             
056100             MOVE WS-IDDC-COPY       TO MOD-IDDC-COPY-IN                  
056200*                                                                         
056300             CALL WMEDKONV        USING MED-WMEDAREA                      
056400             MOVE MED-MFSFEL         TO MOD-TEMFSFEL                      
056500          ELSE                                                            
056600             IF INPUT-RAD-JA                                              
056700                PERFORM EC-MID-INDATA-TILL-MOD                            
056800             END-IF                                                       
056900                                                                          
057000             IF INPUT-COPY-JA                                             
057100                PERFORM ED-COPY-DC-DATA-TILL-MOD                          
057200             END-IF                                                       
057300*                                                                         
057400             IF INDATA-OK                                                 
057500                IF WS-IDDC    = WS-IDDC-DEFAULT                           
057600                   MOVE INF-PRESS-PF23                                    
057700                                     TO MED-IDMFSINF                      
057800                ELSE                                                      
057900                   MOVE INF-PRESS-PF11                                    
058000                                     TO MED-IDMFSINF                      
058100                END-IF                                                    
058200                CALL WMEDKONV     USING MED-WMEDAREA                      
058300                MOVE MED-TEMFSINF    TO MOD-TEMFSINF                      
058400             END-IF                                                       
058500          END-IF                                                          
058600       ELSE                                                               
058700           PERFORM MFS-RENSA-FAELT-IN                                     
058800       END-IF                                                             
058900     END-IF                                                               
059000     .                                                                    
059100     EJECT                                                                
059200 EC-MID-INDATA-TILL-MOD SECTION.                                          
059300                                                                          
059400     MOVE +1                     TO IX-RAD                                
059500     PERFORM UNTIL IX-RAD         > IX-RAD-MAX                            
059600       IF MID-REFTREND-NORM-IN (IX-RAD)     = ALL '+' OR SPACES           
059700         MOVE MFS-RENSA-FAELT                                             
059800                                 TO MOD-REFTREND-NORM-IN (IX-RAD)         
059900       ELSE                                                               
060000         MOVE '1'                TO TREND-SW                              
060100         PERFORM S04-EDIT-REFTREND                                        
060200         IF DEC-KDSVAR-OK                                                 
060300            MOVE DEC-IDEDITDATA  TO WS-TMP-REFT-NORM                      
060400            MOVE WS-REFT-NORM-N                                           
060500                                 TO MOD-REFTREND-NORM-IN (IX-RAD)         
060600         END-IF                                                           
060700       END-IF                                                             
060800       IF MID-REFTREND-SVAG-IN (IX-RAD)     = ALL '+' OR SPACES           
060900         MOVE MFS-RENSA-FAELT                                             
061000                                 TO MOD-REFTREND-SVAG-IN (IX-RAD)         
061100       ELSE                                                               
061200         MOVE '2'                TO TREND-SW                              
061300         PERFORM S04-EDIT-REFTREND                                        
061400         IF DEC-KDSVAR-OK                                                 
061500            MOVE DEC-IDEDITDATA  TO WS-TMP-REFT-SVAG                      
061600            MOVE WS-REFT-SVAG-N                                           
061700                                 TO MOD-REFTREND-SVAG-IN (IX-RAD)         
061800         END-IF                                                           
061900       END-IF                                                             
062000       IF MID-REFTREND-STARK-IN (IX-RAD)    = ALL '+' OR SPACES           
062100         MOVE MFS-RENSA-FAELT                                             
062200                                 TO MOD-REFTREND-STARK-IN (IX-RAD)        
062300       ELSE                                                               
062400         MOVE '3'                TO TREND-SW                              
062500         PERFORM S04-EDIT-REFTREND                                        
062600         IF DEC-KDSVAR-OK                                                 
062700            MOVE DEC-IDEDITDATA  TO WS-TMP-REFT-STARK                     
062800            MOVE WS-REFT-STARK-N                                          
062900                                 TO MOD-REFTREND-STARK-IN (IX-RAD)        
063000         END-IF                                                           
063100       END-IF                                                             
063200       ADD +1                    TO IX-RAD                                
063300     END-PERFORM                                                          
063400     MOVE MFS-RENSA-FAELT        TO MOD-IDDC-COPY-IN                      
063500                                    MOD-KDPROGOI-COPY-IN                  
063600     .                                                                    
063700     EJECT                                                                
063800 ED-COPY-DC-DATA-TILL-MOD SECTION.                                        
063900                                                                          
064000     MOVE WS-IDDC-COPY         TO W-IDDC-B6                               
064100     MOVE WS-KDPROGOI-COPY     TO W-KDPROGOI-B6                           
064200     PERFORM IMS-GU-WDB618                                                
064300     IF SEGMENT-FINNS                                                     
064400        MOVE +1                TO IX-RAD                                  
064500        PERFORM UNTIL IX-RAD    > IX-RAD-MAX                              
064600          MOVE PROG-REFTREND-NORM (IX-RAD)                                
064700                               TO WS-TMP-REFT-NORM                        
064800          MOVE WS-REFT-NORM-N  TO MOD-REFTREND-NORM-IN (IX-RAD)           
064900          MOVE PROG-REFTREND-SVAG (IX-RAD)                                
065000                               TO WS-TMP-REFT-SVAG                        
065100          MOVE WS-REFT-SVAG-N  TO MOD-REFTREND-SVAG-IN (IX-RAD)           
065200          MOVE PROG-REFTREND-STARK(IX-RAD)                                
065300                               TO WS-TMP-REFT-STARK                       
065400          MOVE WS-REFT-STARK-N TO MOD-REFTREND-STARK-IN(IX-RAD)           
065500          ADD +1               TO IX-RAD                                  
065600        END-PERFORM                                                       
065700        MOVE MFS-RENSA-FAELT   TO MOD-IDDC-COPY-IN                        
065800                                  MOD-KDPROGOI-COPY-IN                    
065900     ELSE                                                                 
066000       MOVE NEJ                TO INDATA-SW                               
066100       MOVE SPACES             TO WS-TEMFSFEL                             
066200       MOVE MED-7              TO WS-TEMFSFEL                             
066300       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-COPY-IN-ATTR                   
066400                                  MOD-KDPROGOI-COPY-IN-ATTR               
066500       MOVE WS-IDDC-COPY       TO MOD-IDDC-COPY-IN                        
066600       MOVE WS-KDPROGOI-COPY   TO MOD-KDPROGOI-COPY-IN                    
066700       MOVE WS-TEMFSFEL        TO MOD-TEMFSFEL                            
066800     END-IF                                                               
066900                                                                          
067000     .                                                                    
067100     EJECT                                                                
067200 F-LAES-VISA-INFO SECTION.                                                
067300                                                                          
067400     MOVE WS-IDDC                TO W-IDDC-B6                             
067500*                                                                         
067600     PERFORM IMS-GU-WDB601                                                
067700                                                                          
067800     IF SEGMENT-FINNS                                                     
067900        PERFORM IMS-GNP-WDB615                                            
068000        IF SEGMENT-FINNS                                                  
068100           MOVE LOGG-IDUSER      TO MOD-IDUSER-UT                         
068200           MOVE LOGG-TIUPPDAT    TO MOD-TIUPPDAT-UT                       
068300        ELSE                                                              
068400           MOVE MFS-RENSA-FAELT  TO MOD-IDUSER-UT                         
068500                                    MOD-TIUPPDAT-UT                       
068600        END-IF                                                            
068700                                                                          
068800        MOVE +1                  TO IX-RAD                                
068900        MOVE WS-KDPROGOI         TO W-KDPROGOI-B6                         
069000        PERFORM IMS-GU-WDB618                                             
069100*                                                                         
069200        IF SEGMENT-SAKNAS                                                 
069300           MOVE WS-IDDC-DEFAULT  TO W-IDDC-B6                             
069400           PERFORM IMS-GU-WDB618                                          
069500           MOVE MED-1            TO WS-TEMFSINF                           
069600           MOVE WS-TEMFSINF      TO MOD-TEMFSINF                          
069700        END-IF                                                            
069800*                                                                         
069900        IF SEGMENT-FINNS                                                  
070000           PERFORM UNTIL IX-RAD   > IX-RAD-MAX                            
070100             MOVE IX-RAD       TO MOD-PERIOD-UT         (IX-RAD)          
070200             MOVE PROG-REFTREND-NORM (IX-RAD)                             
070300                               TO WS-TMP-REFT-NORM                        
070400             MOVE WS-REFT-NORM-N                                          
070500                               TO MOD-REFTREND-NORM-UT  (IX-RAD)          
070600             MOVE PROG-REFTREND-SVAG (IX-RAD)                             
070700                               TO WS-TMP-REFT-SVAG                        
070800             MOVE WS-REFT-SVAG-N                                          
070900                               TO MOD-REFTREND-SVAG-UT  (IX-RAD)          
071000             MOVE PROG-REFTREND-STARK(IX-RAD)                             
071100                               TO WS-TMP-REFT-STARK                       
071200             MOVE WS-REFT-STARK-N                                         
071300                               TO MOD-REFTREND-STARK-UT (IX-RAD)          
071400             ADD +1            TO IX-RAD                                  
071500           END-PERFORM                                                    
071600        END-IF                                                            
071700     END-IF                                                               
071800     .                                                                    
071900     EJECT                                                                
072000 G-CONTROL-INPUT       SECTION.                                           
072100                                                                          
072200     MOVE JA                     TO INDATA-SW                             
072300     MOVE NEJ                    TO TREND-NORM-SW                         
072400                                    TREND-SVAG-SW                         
072500                                    TREND-STARK-SW                        
072600*                                                                         
072700     PERFORM GA-CONTROL-HEADER-INP                                        
072800*                                                                         
072900     PERFORM GB-CONTROL-OTHER-INP                                         
073000     .                                                                    
073100     EJECT                                                                
073200 GA-CONTROL-HEADER-INP SECTION.                                           
073300                                                                          
073400     IF MID-IDDC-COPY-IN      NOT = ALL '+'                               
073500        MOVE MID-IDDC-COPY-IN    TO WS-IDDC-COPY                          
073600     END-IF                                                               
073700                                                                          
073800     IF MID-KDPROGOI-COPY-IN  NOT = ALL '+'                               
073900        MOVE MID-KDPROGOI-COPY-IN                                         
074000                                 TO WS-KDPROGOI-COPY                      
074100     END-IF                                                               
074200                                                                          
074300     IF MID-DEFAULT-IN        NOT = ALL '+'                               
074400        MOVE MID-DEFAULT-IN      TO WS-DEFAULT                            
074500     END-IF                                                               
074600*                                                                         
074700     IF WS-DEFAULT                > SPACES                                
074800        IF WS-DEFAULT             = 'J' OR 'Y'                            
074900           MOVE MFS-ALFA-FAELT-RAETT                                      
075000                                 TO MOD-DEFAULT-IN-ATTR                   
075100        ELSE                                                              
075200           MOVE NEJ              TO INDATA-SW                             
075300           MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                          
075400           MOVE MFS-ALFA-FAELT-FEL                                        
075500                                 TO MOD-DEFAULT-IN-ATTR                   
075600           MOVE WS-DEFAULT       TO MOD-DEFAULT-IN                        
075700        END-IF                                                            
075800     END-IF                                                               
075900*                                                                         
076000     IF  WS-IDDC-COPY             > SPACES                                
076100     AND WS-KDPROGOI-COPY         > SPACES                                
076200     AND WS-DEFAULT               > SPACES                                
076300         MOVE NEJ                TO INDATA-SW                             
076400         MOVE SPACES             TO WS-DEFAULT                            
076500         MOVE ERR-WRONG-KEY      TO MED-IDMFSFEL                          
076600         MOVE MED-4              TO WS-TEMFSINF                           
076700         MOVE MFS-ALFA-FAELT-RAETT                                        
076800                                 TO MOD-IDDC-COPY-IN-ATTR                 
076900                                    MOD-KDPROGOI-COPY-IN-ATTR             
077000         MOVE WS-IDDC-COPY       TO MOD-IDDC-COPY-IN                      
077100         MOVE WS-KDPROGOI-COPY   TO MOD-KDPROGOI-COPY-IN                  
077200     END-IF                                                               
077300     .                                                                    
077400     EJECT                                                                
077500 GB-CONTROL-OTHER-INP  SECTION.                                           
077600                                                                          
077700     IF INDATA-OK                                                         
077800                                                                          
077900        PERFORM S01-CHECK-INPUT                                           
078000                                                                          
078100*       --- NO INPUT AND PF11 PRESSED                                     
078200        IF INPUT-RAD-NEJ AND INPUT-DEF-NEJ                                
078300           MOVE ERR-PF11-AND-NO-DATA                                      
078400                                 TO MED-IDMFSFEL                          
078500           MOVE NEJ              TO INDATA-SW                             
078600        ELSE                                                              
078700*       --- UPDATE FC FACTORS FOR DC 99 ONLY WITH PF23                    
078800          IF  WS-IDDC             = WS-IDDC-DEFAULT                       
078900          AND INPUT-RAD-JA                                                
079000          AND MFS-UPDATE                                                  
079100              MOVE MED-3         TO WS-TEMFSFEL                           
079200              MOVE NEJ           TO INDATA-SW                             
079300              PERFORM MFS-ROER-EJ-FAELT-IN                                
079400          END-IF                                                          
079500*       --- UPDATE FC FACTORS FOR DC NOT EQUAL TO 99 WITH PF11            
079600          IF  WS-IDDC         NOT = WS-IDDC-DEFAULT                       
079700          AND INPUT-RAD-JA                                                
079800          AND MFS-UPD-V                                                   
079900              MOVE INF-PRESS-PF11                                         
080000                                 TO MED-IDMFSINF                          
080100              MOVE NEJ           TO INDATA-SW                             
080200              PERFORM MFS-ROER-EJ-FAELT-IN                                
080300          END-IF                                                          
080400*         --- DC 99 IN INPUT AND ALSO SET DEFAULT IS 'Y'                  
080500          IF  WS-IDDC             = WS-IDDC-DEFAULT                       
080600          AND INPUT-DEF-JA                                                
080700              PERFORM MFS-ROER-EJ-FAELT-IN                                
080800              MOVE MFS-ALFA-FAELT-FEL                                     
080900                                 TO MOD-DEFAULT-IN-ATTR                   
081000              MOVE WS-DEFAULT    TO MOD-DEFAULT-IN                        
081100              MOVE MED-6         TO WS-TEMFSFEL                           
081200              MOVE NEJ           TO INDATA-SW                             
081300          END-IF                                                          
081400*         --- SET DEFAULT IS 'Y' AND ALSO FC FACTORS ENTERED              
081500          IF  INPUT-DEF-JA  AND INPUT-RAD-JA                              
081600              PERFORM MFS-ROER-EJ-FAELT-IN                                
081700              MOVE MFS-ALFA-FAELT-FEL                                     
081800                                 TO MOD-DEFAULT-IN-ATTR                   
081900              MOVE WS-DEFAULT    TO MOD-DEFAULT-IN                        
082000              MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                          
082100              MOVE NEJ           TO INDATA-SW                             
082200          END-IF                                                          
082300        END-IF                                                            
082400     END-IF                                                               
082500                                                                          
082600*    --- USER ENTERS FC FACOTRS FOR A DC                                  
082700     IF  INPUT-RAD-JA                                                     
082800     AND INDATA-OK                                                        
082900        PERFORM S02-GET-FC-FACTOR                                         
083000        MOVE +1                  TO IX-RAD                                
083100        PERFORM UNTIL IX-RAD      > IX-RAD-MAX                            
083200          IF MID-REFTREND-NORM-IN (IX-RAD)  = ALL '+' OR SPACES           
083300             MOVE MFS-RENSA-FAELT                                         
083400                                 TO MOD-REFTREND-NORM-IN (IX-RAD)         
083500          ELSE                                                            
083600             MOVE '1'            TO TREND-SW                              
083700             PERFORM S04-EDIT-REFTREND                                    
083800             IF DEC-KDSVAR-OK                                             
083900                MOVE DEC-IDEDITDATA                                       
084000                                 TO WS-TMP-REFT-NORM                      
084100                IF WS-TMP-REFT-NORM                                       
084200                             NOT  > WS-FC-FACT-MAX                        
084300                   MOVE WS-TMP-REFT-NORM                                  
084400                                 TO WS-TAB-REFT-NORM     (IX-RAD)         
084500                   MOVE WS-REFT-NORM-N                                    
084600                                 TO MOD-REFTREND-NORM-IN (IX-RAD)         
084700                   MOVE JA       TO TREND-NORM-SW                         
084800                ELSE                                                      
084900                   MOVE MFS-NUM-FAELT-FEL                                 
085000                            TO MOD-REFTREND-NORM-IN-ATTR (IX-RAD)         
085100                   MOVE WS-REFT-NORM-N                                    
085200                                 TO MOD-REFTREND-NORM-IN (IX-RAD)         
085300                   MOVE ERR-WRONG-KEY                                     
085400                                 TO MED-IDMFSFEL                          
085500                   MOVE NEJ      TO INDATA-SW                             
085600                END-IF                                                    
085700             ELSE                                                         
085800                MOVE MFS-NUM-FAELT-FEL                                    
085900                            TO MOD-REFTREND-NORM-IN-ATTR (IX-RAD)         
086000                MOVE MID-REFTREND-NORM-IN   (IX-RAD)                      
086100                                 TO MOD-REFTREND-NORM-IN (IX-RAD)         
086200                MOVE ERR-WRONG-KEY                                        
086300                                 TO MED-IDMFSFEL                          
086400                MOVE NEJ         TO INDATA-SW                             
086500                                                                          
086600             END-IF                                                       
086700          END-IF                                                          
086800*                                                                         
086900          IF MID-REFTREND-SVAG-IN (IX-RAD)  = ALL '+' OR SPACES           
087000             MOVE MFS-RENSA-FAELT                                         
087100                                 TO MOD-REFTREND-SVAG-IN (IX-RAD)         
087200          ELSE                                                            
087300             MOVE '2'            TO TREND-SW                              
087400             PERFORM S04-EDIT-REFTREND                                    
087500             IF DEC-KDSVAR-OK                                             
087600                MOVE DEC-IDEDITDATA                                       
087700                                 TO WS-TMP-REFT-SVAG                      
087800                IF WS-TMP-REFT-SVAG                                       
087900                             NOT  > WS-FC-FACT-MAX                        
088000                   MOVE WS-TMP-REFT-SVAG                                  
088100                                 TO WS-TAB-REFT-SVAG     (IX-RAD)         
088200                   MOVE WS-REFT-SVAG-N                                    
088300                                 TO MOD-REFTREND-SVAG-IN (IX-RAD)         
088400                   MOVE JA       TO TREND-SVAG-SW                         
088500                ELSE                                                      
088600                   MOVE MFS-NUM-FAELT-FEL                                 
088700                            TO MOD-REFTREND-SVAG-IN-ATTR (IX-RAD)         
088800                   MOVE WS-REFT-SVAG-N                                    
088900                                 TO MOD-REFTREND-SVAG-IN (IX-RAD)         
089000                   MOVE ERR-WRONG-KEY                                     
089100                                 TO MED-IDMFSFEL                          
089200                   MOVE NEJ      TO INDATA-SW                             
089300                END-IF                                                    
089400             ELSE                                                         
089500                MOVE MFS-NUM-FAELT-FEL                                    
089600                            TO MOD-REFTREND-SVAG-IN-ATTR (IX-RAD)         
089700                MOVE MID-REFTREND-SVAG-IN   (IX-RAD)                      
089800                                 TO MOD-REFTREND-SVAG-IN (IX-RAD)         
089900                MOVE ERR-WRONG-KEY                                        
090000                                 TO MED-IDMFSFEL                          
090100                MOVE NEJ         TO INDATA-SW                             
090200                                                                          
090300             END-IF                                                       
090400          END-IF                                                          
090500*                                                                         
090600          IF MID-REFTREND-STARK-IN(IX-RAD)  = ALL '+' OR SPACES           
090700             MOVE MFS-RENSA-FAELT                                         
090800                                 TO MOD-REFTREND-STARK-IN(IX-RAD)         
090900          ELSE                                                            
091000             MOVE '3'            TO TREND-SW                              
091100             PERFORM S04-EDIT-REFTREND                                    
091200             IF DEC-KDSVAR-OK                                             
091300                MOVE DEC-IDEDITDATA                                       
091400                                 TO WS-TMP-REFT-STARK                     
091500                IF WS-TMP-REFT-STARK                                      
091600                             NOT  > WS-FC-FACT-MAX                        
091700                   MOVE WS-TMP-REFT-STARK                                 
091800                                 TO WS-TAB-REFT-STARK    (IX-RAD)         
091900                   MOVE WS-REFT-STARK-N                                   
092000                                 TO MOD-REFTREND-STARK-IN(IX-RAD)         
092100                   MOVE JA       TO TREND-STARK-SW                        
092200                ELSE                                                      
092300                   MOVE MFS-NUM-FAELT-FEL                                 
092400                            TO MOD-REFTREND-STARK-IN-ATTR(IX-RAD)         
092500                   MOVE WS-REFT-STARK-N                                   
092600                                 TO MOD-REFTREND-STARK-IN(IX-RAD)         
092700                   MOVE ERR-WRONG-KEY                                     
092800                                 TO MED-IDMFSFEL                          
092900                   MOVE NEJ      TO INDATA-SW                             
093000                END-IF                                                    
093100             ELSE                                                         
093200                MOVE MFS-NUM-FAELT-FEL                                    
093300                            TO MOD-REFTREND-STARK-IN-ATTR(IX-RAD)         
093400                MOVE MID-REFTREND-STARK-IN  (IX-RAD)                      
093500                                 TO MOD-REFTREND-STARK-IN(IX-RAD)         
093600                MOVE ERR-WRONG-KEY                                        
093700                                 TO MED-IDMFSFEL                          
093800                MOVE NEJ         TO INDATA-SW                             
093900                                                                          
094000             END-IF                                                       
094100          END-IF                                                          
094200          ADD +1                 TO IX-RAD                                
094300        END-PERFORM                                                       
094400*                                                                         
094500*---    TOTAL FC FOR ALL 12 PERIODS SHOULD BE 100                         
094600*                                                                         
094700        IF   INDATA-OK                                                    
094800        AND (TREND-NORM-JA                                                
094900        OR   TREND-SVAG-JA                                                
095000        OR   TREND-STARK-JA)                                              
095100                                                                          
095200           PERFORM S03-CNTRL-INPUT-FC                                     
095300           IF TREND-NORM-JA                                               
095400              IF UPD-FC-NORM-NEJ                                          
095500                 MOVE +1              TO IX-RAD                           
095600                 PERFORM UNTIL IX-RAD  > IX-RAD-MAX                       
095700                   IF MID-REFTREND-NORM-IN (IX-RAD) NOT = ALL '+'         
095800                      MOVE MFS-NUM-FAELT-FEL                              
095900                            TO MOD-REFTREND-NORM-IN-ATTR (IX-RAD)         
096000                      MOVE WS-TAB-REFT-NORM     (IX-RAD)                  
096100                                 TO WS-TMP-REFT-NORM                      
096200                      MOVE WS-REFT-NORM-N                                 
096300                                 TO MOD-REFTREND-NORM-IN (IX-RAD)         
096400                      MOVE MED-2      TO WS-TEMFSFEL                      
096500                      MOVE NEJ        TO INDATA-SW                        
096600                   END-IF                                                 
096700                   ADD +1             TO IX-RAD                           
096800                 END-PERFORM                                              
096900              ELSE                                                        
097000                 MOVE +1              TO IX-RAD                           
097100                 PERFORM UNTIL IX-RAD  > IX-RAD-MAX                       
097200                   IF MID-REFTREND-NORM-IN (IX-RAD) NOT = ALL '+'         
097300                      MOVE MFS-NUM-FAELT-RAETT                            
097400                            TO MOD-REFTREND-NORM-IN-ATTR (IX-RAD)         
097500                      MOVE WS-TAB-REFT-NORM     (IX-RAD)                  
097600                                 TO WS-TMP-REFT-NORM                      
097700                      MOVE WS-REFT-NORM-N                                 
097800                                 TO MOD-REFTREND-NORM-IN (IX-RAD)         
097900                      MOVE JA         TO UPD-RAD-SW                       
098000                   END-IF                                                 
098100                   ADD +1             TO IX-RAD                           
098200                 END-PERFORM                                              
098300              END-IF                                                      
098400           END-IF                                                         
098500           IF TREND-SVAG-JA                                               
098600              IF UPD-FC-SVAG-NEJ                                          
098700                 MOVE +1              TO IX-RAD                           
098800                 PERFORM UNTIL IX-RAD  > IX-RAD-MAX                       
098900                   IF MID-REFTREND-SVAG-IN (IX-RAD) NOT = ALL '+'         
099000                      MOVE MFS-NUM-FAELT-FEL                              
099100                            TO MOD-REFTREND-SVAG-IN-ATTR (IX-RAD)         
099200                      MOVE WS-TAB-REFT-SVAG     (IX-RAD)                  
099300                                 TO WS-TMP-REFT-SVAG                      
099400                      MOVE WS-REFT-SVAG-N                                 
099500                                 TO MOD-REFTREND-SVAG-IN (IX-RAD)         
099600                      MOVE MED-2      TO WS-TEMFSFEL                      
099700                      MOVE NEJ        TO INDATA-SW                        
099800                   END-IF                                                 
099900                   ADD +1             TO IX-RAD                           
100000                 END-PERFORM                                              
100100              ELSE                                                        
100200                 MOVE +1              TO IX-RAD                           
100300                 PERFORM UNTIL IX-RAD  > IX-RAD-MAX                       
100400                   IF MID-REFTREND-SVAG-IN (IX-RAD) NOT = ALL '+'         
100500                      MOVE MFS-NUM-FAELT-RAETT                            
100600                            TO MOD-REFTREND-SVAG-IN-ATTR (IX-RAD)         
100700                      MOVE WS-TAB-REFT-SVAG     (IX-RAD)                  
100800                                 TO WS-TMP-REFT-SVAG                      
100900                      MOVE WS-REFT-SVAG-N                                 
101000                                 TO MOD-REFTREND-SVAG-IN (IX-RAD)         
101100                      MOVE JA         TO UPD-RAD-SW                       
101200                   END-IF                                                 
101300                   ADD +1             TO IX-RAD                           
101400                 END-PERFORM                                              
101500              END-IF                                                      
101600           END-IF                                                         
101700           IF TREND-STARK-JA                                              
101800              IF UPD-FC-STARK-NEJ                                         
101900                 MOVE +1              TO IX-RAD                           
102000                 PERFORM UNTIL IX-RAD  > IX-RAD-MAX                       
102100                   IF MID-REFTREND-STARK-IN(IX-RAD) NOT = ALL '+'         
102200                      MOVE MFS-NUM-FAELT-FEL                              
102300                            TO MOD-REFTREND-STARK-IN-ATTR(IX-RAD)         
102400                      MOVE WS-TAB-REFT-STARK    (IX-RAD)                  
102500                                 TO WS-TMP-REFT-STARK                     
102600                      MOVE WS-REFT-STARK-N                                
102700                                 TO MOD-REFTREND-STARK-IN(IX-RAD)         
102800                      MOVE MED-2      TO WS-TEMFSFEL                      
102900                      MOVE NEJ        TO INDATA-SW                        
103000                   END-IF                                                 
103100                   ADD +1             TO IX-RAD                           
103200                 END-PERFORM                                              
103300              ELSE                                                        
103400                 MOVE +1              TO IX-RAD                           
103500                 PERFORM UNTIL IX-RAD  > IX-RAD-MAX                       
103600                   IF MID-REFTREND-STARK-IN(IX-RAD) NOT = ALL '+'         
103700                      MOVE MFS-NUM-FAELT-RAETT                            
103800                            TO MOD-REFTREND-STARK-IN-ATTR(IX-RAD)         
103900                      MOVE WS-TAB-REFT-STARK    (IX-RAD)                  
104000                                 TO WS-TMP-REFT-STARK                     
104100                      MOVE WS-REFT-STARK-N                                
104200                                 TO MOD-REFTREND-STARK-IN(IX-RAD)         
104300                      MOVE JA         TO UPD-RAD-SW                       
104400                   END-IF                                                 
104500                   ADD +1             TO IX-RAD                           
104600                 END-PERFORM                                              
104700              END-IF                                                      
104800           END-IF                                                         
104900        END-IF                                                            
105000     END-IF                                                               
105100*                                                                         
105200*    --- COPY DC ENTERED AND ALSO SET DEFAULT MARKED                      
105300     IF  INPUT-COPY-JA                                                    
105400     AND INPUT-DEF-JA                                                     
105500         MOVE NEJ                TO INDATA-SW                             
105600         MOVE SPACES             TO WS-TEMFSFEL                           
105700         MOVE ERR-WRONG-KEY      TO MED-IDMFSFEL                          
105800         MOVE MFS-ALFA-FAELT-FEL                                          
105900                                 TO MOD-IDDC-COPY-IN-ATTR                 
106000                                    MOD-KDPROGOI-COPY-IN-ATTR             
106100         MOVE WS-IDDC-COPY       TO MOD-IDDC-COPY-IN                      
106200         MOVE WS-KDPROGOI-COPY   TO MOD-KDPROGOI-COPY-IN                  
106300         MOVE WS-DEFAULT         TO MOD-DEFAULT-IN                        
106400     END-IF                                                               
106500                                                                          
106600*    --- SET DEFAULT IS SET TO YES                                        
106700     IF  INPUT-DEF-JA                                                     
106800     AND INDATA-OK                                                        
106900         MOVE JA                 TO UPD-DEF-FC-SW                         
107000     ELSE                                                                 
107100      IF  INPUT-RAD-JA                                                    
107200      AND INPUT-COPY-JA                                                   
107300          PERFORM MFS-ROER-EJ-FAELT-IN                                    
107400          MOVE NEJ               TO INDATA-SW                             
107500          MOVE SPACES            TO WS-TEMFSFEL                           
107600          MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                          
107700          MOVE MFS-ALFA-FAELT-FEL                                         
107800                                 TO MOD-IDDC-COPY-IN-ATTR                 
107900                                    MOD-KDPROGOI-COPY-IN-ATTR             
108000          MOVE WS-IDDC-COPY      TO MOD-IDDC-COPY-IN                      
108100          MOVE WS-KDPROGOI-COPY  TO MOD-KDPROGOI-COPY-IN                  
108200      ELSE                                                                
108300       IF INPUT-COPY-JA                                                   
108400          MOVE NEJ               TO INDATA-SW                             
108500          MOVE MED-4             TO WS-TEMFSINF                           
108600          MOVE MFS-ALFA-FAELT-RAETT                                       
108700                                 TO MOD-IDDC-COPY-IN-ATTR                 
108800                                    MOD-KDPROGOI-COPY-IN-ATTR             
108900          MOVE WS-IDDC-COPY      TO MOD-IDDC-COPY-IN                      
109000          MOVE WS-KDPROGOI-COPY  TO MOD-KDPROGOI-COPY-IN                  
109100       END-IF                                                             
109200      END-IF                                                              
109300     END-IF                                                               
109400                                                                          
109500     IF INDATA-FEL                                                        
109600        IF WS-TEMFSFEL            = SPACE                                 
109700          CALL WMEDKONV       USING MED-WMEDAREA                          
109800          MOVE MED-MFSFEL        TO MOD-TEMFSFEL                          
109900        ELSE                                                              
110000          MOVE WS-TEMFSFEL       TO MOD-TEMFSFEL                          
110100        END-IF                                                            
110200*                                                                         
110300        IF WS-TEMFSINF            > SPACES                                
110400          MOVE WS-TEMFSINF       TO MOD-TEMFSINF                          
110500        ELSE                                                              
110600          CALL WMEDKONV       USING MED-WMEDAREA                          
110700          MOVE MED-TEMFSINF      TO MOD-TEMFSINF                          
110800        END-IF                                                            
110900     END-IF                                                               
111000     .                                                                    
111100     EJECT                                                                
111200 H-UPPDATERA SECTION.                                                     
111300                                                                          
111400     IF UPD-RAD-JA                                                        
111500        IF  (WS-IDDC          NOT = WS-IDDC-DEFAULT                       
111600        AND  MFS-UPDATE )                                                 
111700        OR  (WS-IDDC              = WS-IDDC-DEFAULT                       
111800        AND  MFS-UPD-V)                                                   
111900          MOVE +1                TO IX-RAD                                
112000          MOVE WS-IDDC           TO W-IDDC-B6                             
112100          MOVE WS-KDPROGOI       TO W-KDPROGOI-B6                         
112200          PERFORM IMS-GHU-WDB618                                          
112300          IF SEGMENT-FINNS                                                
112400             PERFORM UNTIL IX-RAD > IX-RAD-MAX                            
112500              MOVE WS-TAB-REFT-NORM  (IX-RAD)                             
112600                                 TO PROG-REFTREND-NORM  (IX-RAD)          
112700              MOVE WS-TAB-REFT-SVAG  (IX-RAD)                             
112800                                 TO PROG-REFTREND-SVAG  (IX-RAD)          
112900              MOVE WS-TAB-REFT-STARK (IX-RAD)                             
113000                                 TO PROG-REFTREND-STARK (IX-RAD)          
113100              ADD +1             TO IX-RAD                                
113200             END-PERFORM                                                  
113300             PERFORM IMS-REPL-WDB618                                      
113400             MOVE JA             TO UPD-DONE-SW                           
113500          ELSE                                                            
113600             MOVE WS-KDPROGOI    TO PROG-KDPROGOI                         
113700             PERFORM UNTIL IX-RAD > IX-RAD-MAX                            
113800              MOVE WS-TAB-REFT-NORM  (IX-RAD)                             
113900                                 TO PROG-REFTREND-NORM  (IX-RAD)          
114000              MOVE WS-TAB-REFT-SVAG  (IX-RAD)                             
114100                                 TO PROG-REFTREND-SVAG  (IX-RAD)          
114200              MOVE WS-TAB-REFT-STARK (IX-RAD)                             
114300                                 TO PROG-REFTREND-STARK (IX-RAD)          
114400              ADD +1             TO IX-RAD                                
114500             END-PERFORM                                                  
114600             PERFORM IMS-ISRT-WDB618                                      
114700             MOVE JA             TO UPD-DONE-SW                           
114800          END-IF                                                          
114900        END-IF                                                            
115000*                                                                         
115100     ELSE                                                                 
115200*      --- SET DEFAULT                                                    
115300       IF UPD-DEF-FC-JA                                                   
115400          MOVE WS-IDDC           TO W-IDDC-B6                             
115500          MOVE WS-KDPROGOI       TO W-KDPROGOI-B6                         
115600          PERFORM IMS-GHU-WDB618                                          
115700          IF SEGMENT-FINNS                                                
115800             PERFORM IMS-DLET-WDB618                                      
115900             MOVE JA             TO UPD-DONE-SW                           
116000          END-IF                                                          
116100       END-IF                                                             
116200     END-IF                                                               
116300*                                                                         
116400     IF UPD-DONE-JA                                                       
116500*---    INSERT USER INFO IN WDB615                                        
116600        PERFORM IMS-GU-WDB601                                             
116700        IF SEGMENT-FINNS                                                  
116800           PERFORM IMS-GHU-WDB615                                         
116900           IF SEGMENT-FINNS                                               
117000              MOVE W-IDUSER      TO LOGG-IDUSER                           
117100              MOVE DAGENS-DATUM  TO LOGG-TIUPPDAT                         
117200              PERFORM IMS-REPL-WDB615                                     
117300           ELSE                                                           
117400              MOVE W-IDTRANS     TO LOGG-IDTRANS                          
117500              MOVE SPACE         TO LOGG-IDDC-REF                         
117600              MOVE W-IDUSER      TO LOGG-IDUSER                           
117700              MOVE DAGENS-DATUM  TO LOGG-TIUPPDAT                         
117800              PERFORM IMS-ISRT-WDB615                                     
117900           END-IF                                                         
118000        END-IF                                                            
118100                                                                          
118200        MOVE INF-UPDATE-DONE     TO MED-IDMFSINF                          
118300        CALL WMEDKONV         USING MED-WMEDAREA                          
118400        MOVE MED-TEMFSINF        TO MOD-TEMFSINF                          
118500        PERFORM MFS-RENSA-FAELT-IN                                        
118600     END-IF                                                               
118700     .                                                                    
118800     EJECT                                                                
118900 S01-CHECK-INPUT    SECTION.                                              
119000                                                                          
119100     MOVE NEJ                    TO INPUT-RAD-SW                          
119200                                    INPUT-DEF-SW                          
119300                                    INPUT-COPY-SW                         
119400*                                                                         
119500     IF MID-INPUT             NOT = ALL '+'                               
119600        MOVE +1                  TO IX-OTH                                
119700        PERFORM UNTIL IX-OTH      > IX-RAD-MAX                            
119800         IF MID-REFTREND-NORM-IN (IX-OTH)                                 
119900                              NOT = (ALL '+' AND SPACES)                  
120000            MOVE JA              TO INPUT-RAD-SW                          
120100         ELSE                                                             
120200           IF MID-REFTREND-SVAG-IN (IX-OTH)                               
120300                              NOT = (ALL '+' AND SPACES)                  
120400              MOVE JA            TO INPUT-RAD-SW                          
120500           ELSE                                                           
120600             IF MID-REFTREND-STARK-IN(IX-OTH)                             
120700                              NOT = (ALL '+' AND SPACES)                  
120800                MOVE JA          TO INPUT-RAD-SW                          
120900             END-IF                                                       
121000           END-IF                                                         
121100         END-IF                                                           
121200         ADD +1                  TO IX-OTH                                
121300        END-PERFORM                                                       
121400     END-IF                                                               
121500*                                                                         
121600     IF WS-DEFAULT                > SPACES                                
121700        MOVE JA                  TO INPUT-DEF-SW                          
121800     END-IF                                                               
121900*                                                                         
122000     IF WS-IDDC-COPY              > SPACES                                
122100        MOVE JA                  TO INPUT-COPY-SW                         
122200     END-IF                                                               
122300     .                                                                    
122400     EJECT                                                                
122500 S02-GET-FC-FACTOR  SECTION.                                              
122600                                                                          
122700     PERFORM S02A-INIT-FC-TAB                                             
122800                                                                          
122900     MOVE +1                  TO IX-TAB                                   
123000     MOVE WS-IDDC             TO W-IDDC-B6                                
123100     MOVE WS-KDPROGOI         TO W-KDPROGOI-B6                            
123200     PERFORM IMS-GU-WDB618                                                
123300*                                                                         
123400     IF SEGMENT-SAKNAS                                                    
123500        MOVE WS-IDDC-DEFAULT  TO W-IDDC-B6                                
123600        PERFORM IMS-GU-WDB618                                             
123700     END-IF                                                               
123800*                                                                         
123900     IF SEGMENT-FINNS                                                     
124000        PERFORM UNTIL IX-TAB   > IX-TAB-MAX                               
124100          MOVE IX-TAB         TO WS-TAB-PER        (IX-TAB)               
124200          MOVE PROG-REFTREND-NORM (IX-TAB)                                
124300                              TO WS-TAB-REFT-NORM  (IX-TAB)               
124400          MOVE PROG-REFTREND-SVAG (IX-TAB)                                
124500                              TO WS-TAB-REFT-SVAG  (IX-TAB)               
124600          MOVE PROG-REFTREND-STARK(IX-TAB)                                
124700                              TO WS-TAB-REFT-STARK (IX-TAB)               
124800          ADD +1              TO IX-TAB                                   
124900        END-PERFORM                                                       
125000     END-IF                                                               
125100     .                                                                    
125200     EJECT                                                                
125300 S02A-INIT-FC-TAB   SECTION.                                              
125400                                                                          
125500     MOVE +1                     TO IX-TAB                                
125600     PERFORM UNTIL IX-TAB         > IX-TAB-MAX                            
125700       MOVE ALL ZERO             TO WS-TAB-PER        (IX-TAB)            
125800                                    WS-TAB-REFT-NORM  (IX-TAB)            
125900                                    WS-TAB-REFT-SVAG  (IX-TAB)            
126000                                    WS-TAB-REFT-STARK (IX-TAB)            
126100       ADD +1                    TO IX-TAB                                
126200     END-PERFORM                                                          
126300     .                                                                    
126400     EJECT                                                                
126500 S03-CNTRL-INPUT-FC SECTION.                                              
126600                                                                          
126700     MOVE ALL ZERO               TO WS-TOT-FC-NORM                        
126800                                    WS-TOT-FC-SVAG                        
126900                                    WS-TOT-FC-STARK                       
127000     MOVE +1                     TO WS-GODK-TOT-FC                        
127100                                                                          
127200*                                                                         
127300     IF TREND-NORM-JA                                                     
127400        MOVE JA                  TO UPD-FC-NORM-SW                        
127500        MOVE +1                  TO IX-TAB                                
127600        PERFORM UNTIL IX-TAB      > IX-TAB-MAX                            
127700          COMPUTE WS-TOT-FC-NORM  = WS-TOT-FC-NORM                        
127800                                  + WS-TAB-REFT-NORM  (IX-TAB)            
127900          ADD +1                 TO IX-TAB                                
128000        END-PERFORM                                                       
128100        IF WS-TOT-FC-NORM     NOT = WS-GODK-TOT-FC                        
128200           MOVE NEJ              TO UPD-FC-NORM-SW                        
128300        END-IF                                                            
128400     END-IF                                                               
128500     IF TREND-SVAG-JA                                                     
128600        MOVE JA                  TO UPD-FC-SVAG-SW                        
128700        MOVE +1                  TO IX-TAB                                
128800        PERFORM UNTIL IX-TAB      > IX-TAB-MAX                            
128900          COMPUTE WS-TOT-FC-SVAG  = WS-TOT-FC-SVAG                        
129000                                  + WS-TAB-REFT-SVAG  (IX-TAB)            
129100          ADD +1                 TO IX-TAB                                
129200        END-PERFORM                                                       
129300        IF WS-TOT-FC-SVAG     NOT = WS-GODK-TOT-FC                        
129400           MOVE NEJ              TO UPD-FC-SVAG-SW                        
129500        END-IF                                                            
129600     END-IF                                                               
129700     IF TREND-STARK-JA                                                    
129800        MOVE JA                  TO UPD-FC-STARK-SW                       
129900        MOVE +1                  TO IX-TAB                                
130000        PERFORM UNTIL IX-TAB      > IX-TAB-MAX                            
130100          COMPUTE WS-TOT-FC-STARK = WS-TOT-FC-STARK                       
130200                                  + WS-TAB-REFT-STARK (IX-TAB)            
130300          ADD +1                 TO IX-TAB                                
130400        END-PERFORM                                                       
130500        IF WS-TOT-FC-STARK    NOT = WS-GODK-TOT-FC                        
130600           MOVE NEJ              TO UPD-FC-STARK-SW                       
130700        END-IF                                                            
130800     END-IF                                                               
130900     .                                                                    
131000     EJECT                                                                
131100 S04-EDIT-REFTREND     SECTION.                                           
131200                                                                          
131300     MOVE ZERO                   TO WS-TMP-REFT-NORM                      
131400                                    TALLY                                 
131500     MOVE SPACES                 TO WS-TMP-TREND                          
131600                                                                          
131700     IF TREND-NORM                                                        
131800        MOVE MID-REFTREND-NORM-IN   (IX-RAD)                              
131900                                 TO WS-TMP-TREND                          
132000     ELSE                                                                 
132100       IF TREND-SVAG                                                      
132200          MOVE MID-REFTREND-SVAG-IN (IX-RAD)                              
132300                                 TO WS-TMP-TREND                          
132400       ELSE                                                               
132500         IF TREND-STARK                                                   
132600            MOVE MID-REFTREND-STARK-IN(IX-RAD)                            
132700                                 TO WS-TMP-TREND                          
132800         END-IF                                                           
132900       END-IF                                                             
133000     END-IF                                                               
133100*                                                                         
133200     INSPECT WS-TMP-TREND  TALLYING TALLY                                 
133300             FOR CHARACTERS BEFORE INITIAL SPACE                          
133400     MOVE WS-TMP-TREND(1:TALLY)                                           
133500                                 TO WS-TMP-TREND-FORMAT                   
133600     MOVE WS-TREND-FORM-R        TO WS-TREND-DECEDIT                      
133700*                                                                         
133800     MOVE WS-TREND-DECEDIT       TO DEC-IDFRIDATA                         
133900     MOVE 1                      TO DEC-KVHELTAL                          
134000     MOVE 2                      TO DEC-KVDECIMAL                         
134100                                                                          
134200     CALL WDECEDIT USING DEC-WDECAREA                                     
134300     .                                                                    
134400     EJECT                                                                
134500 MFS-RENSA-FAELT-UT SECTION.                                              
134600                                                                          
134700*    --- ALLA UTDATA-FÄLT                                                 
134800     MOVE +1                     TO IX-OTH                                
134900     PERFORM UNTIL IX-OTH         > IX-RAD-MAX                            
135000        MOVE MFS-RENSA-FAELT     TO MOD-PERIOD-UT        (IX-OTH)         
135100                                    MOD-REFTREND-NORM-UT (IX-OTH)         
135200                                    MOD-REFTREND-SVAG-UT (IX-OTH)         
135300                                    MOD-REFTREND-STARK-UT(IX-OTH)         
135400        ADD +1                   TO IX-OTH                                
135500     END-PERFORM                                                          
135600     MOVE MFS-RENSA-FAELT        TO MOD-IDUSER-UT                         
135700                                    MOD-TIUPPDAT-UT                       
135800     .                                                                    
135900     EJECT                                                                
136000 MFS-RENSA-FAELT-IN SECTION.                                              
136100                                                                          
136200*    --- ALLA INDATA-FÄLT                                                 
136300     MOVE +1                     TO IX-OTH                                
136400     PERFORM UNTIL IX-OTH         > IX-RAD-MAX                            
136500       MOVE MFS-RENSA-FAELT      TO MOD-REFTREND-NORM-IN (IX-OTH)         
136600                                    MOD-REFTREND-SVAG-IN (IX-OTH)         
136700                                    MOD-REFTREND-STARK-IN(IX-OTH)         
136800       ADD +1                    TO IX-OTH                                
136900     END-PERFORM                                                          
137000     MOVE MFS-RENSA-FAELT        TO MOD-IDDC-COPY-IN                      
137100                                    MOD-KDPROGOI-COPY-IN                  
137200                                    MOD-DEFAULT-IN                        
137300     .                                                                    
137400     EJECT                                                                
137500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
137600                                                                          
137700*    --- ALLA UTDATA-FÄLT                                                 
137800     MOVE +1                     TO IX-OTH                                
137900     PERFORM UNTIL IX-OTH         > IX-RAD-MAX                            
138000       MOVE MFS-ROER-EJ-FAELT                                             
138100                            TO MOD-REFTREND-NORM-IN-ATTR (IX-OTH)         
138200                               MOD-REFTREND-SVAG-IN-ATTR (IX-OTH)         
138300                               MOD-REFTREND-STARK-IN-ATTR(IX-OTH)         
138400       ADD +1                    TO IX-OTH                                
138500     END-PERFORM                                                          
138600     MOVE MFS-ROER-EJ-FAELT      TO MOD-IDDC-COPY-IN-ATTR                 
138700                                    MOD-KDPROGOI-COPY-IN-ATTR             
138800                                    MOD-DEFAULT-IN-ATTR                   
138900     .                                                                    
139000     EJECT                                                                
139100 MFS-LAES-IN-IGEN SECTION.                                                
139200                                                                          
139300*    --- ALLA INDATA-FÄLT                                                 
139400     MOVE +1                     TO IX-OTH                                
139500     PERFORM UNTIL IX-OTH         > IX-RAD-MAX                            
139600       MOVE MFS-ADD-LAES-IN-FAELT                                         
139700                            TO MOD-REFTREND-NORM-IN-ATTR (IX-OTH)         
139800                               MOD-REFTREND-SVAG-IN-ATTR (IX-OTH)         
139900                               MOD-REFTREND-STARK-IN-ATTR(IX-OTH)         
140000       ADD +1                    TO IX-OTH                                
140100     END-PERFORM                                                          
140200     MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDDC-COPY-IN-ATTR                 
140300                                    MOD-KDPROGOI-COPY-IN-ATTR             
140400                                    MOD-DEFAULT-IN-ATTR                   
140500                                                                          
140600     .                                                                    
140700     EJECT                                                                
140800 MFS-STAENG-FAELT-IN    SECTION.                                          
140900                                                                          
141000*    --- PROTECT INPUT FIELDS                                             
141100     MOVE +1                     TO IX-OTH                                
141200     PERFORM UNTIL IX-OTH         > IX-RAD-MAX                            
141300       MOVE MFS-STAENG-FAELT                                              
141400                            TO MOD-REFTREND-NORM-IN-ATTR (IX-OTH)         
141500                               MOD-REFTREND-SVAG-IN-ATTR (IX-OTH)         
141600                               MOD-REFTREND-STARK-IN-ATTR(IX-OTH)         
141700                                                                          
141800       ADD +1                    TO IX-OTH                                
141900     END-PERFORM                                                          
142000     MOVE MFS-STAENG-FAELT       TO MOD-IDDC-COPY-IN-ATTR                 
142100                                    MOD-KDPROGOI-COPY-IN-ATTR             
142200                                    MOD-DEFAULT-IN-ATTR                   
142300     .                                                                    
142400     EJECT                                                                
142500* --- IMS SEKTIONER ---                                                   
142600     SKIP3                                                                
142700 IMS-GET-MSG SECTION.                                                     
142800                                                                          
142900     MOVE '  QC' TO GODK-STATUSKODER                                      
143000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
143100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
143200     PERFORM IMS-STATUSKONTROLL                                           
143300     .                                                                    
143400     SKIP3                                                                
143500 IMS-INSERT-MSG SECTION.                                                  
143600                                                                          
143700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
143800     MOVE SPACE TO GODK-STATUSKODER                                       
143900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
144000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
144100     PERFORM IMS-STATUSKONTROLL                                           
144200     .                                                                    
144300     EJECT                                                                
144400 IMS-GU-WDB601    SECTION.                                                
144500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
144600          DELIMITED BY SIZE INTO SSA1                                     
144700     MOVE '  GE'              TO GODK-STATUSKODER                         
144800     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-WDB601 SSA1                   
144900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
145000     PERFORM IMS-STATUSKONTROLL                                           
145100     .                                                                    
145200     EJECT                                                                
145300 IMS-GNP-WDB615 SECTION.                                                  
145400                                                                          
145500     STRING 'WDB615  (IDTRANS  =' W-IDTRANS-B6-X ')'                      
145600     DELIMITED BY SIZE INTO SSA1                                          
145700     MOVE '  GE'              TO GODK-STATUSKODER                         
145800     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB615 SSA1                   
145900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
146000     PERFORM IMS-STATUSKONTROLL                                           
146100     .                                                                    
146200     EJECT                                                                
146300 IMS-GHU-WDB615 SECTION.                                                  
146400                                                                          
146500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
146600          DELIMITED BY SIZE INTO SSA1                                     
146700     STRING 'WDB615  (IDTRANS  =' W-IDTRANS-B6-X ')'                      
146800          DELIMITED BY SIZE INTO SSA2                                     
146900     MOVE '  GE'              TO GODK-STATUSKODER                         
147000     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB615 SSA1 SSA2              
147100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
147200     PERFORM IMS-STATUSKONTROLL                                           
147300     .                                                                    
147400     EJECT                                                                
147500 IMS-REPL-WDB615 SECTION.                                                 
147600                                                                          
147700     MOVE '  '                TO GODK-STATUSKODER                         
147800     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB615                       
147900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
148000     PERFORM IMS-STATUSKONTROLL                                           
148100     .                                                                    
148200     EJECT                                                                
148300 IMS-ISRT-WDB615 SECTION.                                                 
148400                                                                          
148500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
148600          DELIMITED BY SIZE INTO SSA1                                     
148700     MOVE 'WDB615  '          TO SSA2                                     
148800     MOVE '    '              TO GODK-STATUSKODER                         
148900     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB615 SSA1 SSA2             
149000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
149100     PERFORM IMS-STATUSKONTROLL                                           
149200     .                                                                    
149300     EJECT                                                                
149400 IMS-GU-WDB618 SECTION.                                                   
149500                                                                          
149600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
149700          DELIMITED BY SIZE INTO SSA1                                     
149800     STRING 'WDB618  (KDPROGOI =' W-KDPROGOI-B6-X ')'                     
149900          DELIMITED BY SIZE INTO SSA2                                     
150000     MOVE '  GE'              TO GODK-STATUSKODER                         
150100     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-WDB618 SSA1 SSA2              
150200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
150300     PERFORM IMS-STATUSKONTROLL                                           
150400     .                                                                    
150500     EJECT                                                                
150600 IMS-GHU-WDB618 SECTION.                                                  
150700                                                                          
150800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
150900          DELIMITED BY SIZE INTO SSA1                                     
151000     STRING 'WDB618  (KDPROGOI =' W-KDPROGOI-B6-X ')'                     
151100          DELIMITED BY SIZE INTO SSA2                                     
151200     MOVE '  GE'              TO GODK-STATUSKODER                         
151300     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB618 SSA1 SSA2              
151400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
151500     PERFORM IMS-STATUSKONTROLL                                           
151600     .                                                                    
151700     EJECT                                                                
151800 IMS-REPL-WDB618 SECTION.                                                 
151900                                                                          
152000     MOVE '  '                TO GODK-STATUSKODER                         
152100     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB618                       
152200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
152300     PERFORM IMS-STATUSKONTROLL                                           
152400     .                                                                    
152500     EJECT                                                                
152600 IMS-ISRT-WDB618 SECTION.                                                 
152700                                                                          
152800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
152900          DELIMITED BY SIZE INTO SSA1                                     
153000     MOVE 'WDB618  '          TO SSA2                                     
153100     MOVE '    '              TO GODK-STATUSKODER                         
153200     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB618 SSA1 SSA2             
153300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
153400     PERFORM IMS-STATUSKONTROLL                                           
153500     .                                                                    
153600     EJECT                                                                
153700 IMS-DLET-WDB618 SECTION.                                                 
153800                                                                          
153900     MOVE '  '                TO GODK-STATUSKODER                         
154000     CALL CBLTDLI USING DLET WDB6-PCB DLI-IO-WDB618                       
154100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
154200     PERFORM IMS-STATUSKONTROLL                                           
154300     .                                                                    
154400     EJECT                                                                
154500 IMS-STATUSKONTROLL SECTION.                                              
154600                                                                          
154700     SET STATUS-IX TO 1                                                   
154800     SEARCH GODK-STATUS                                                   
154900       AT END                                                             
155000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
155100         DELIMITED BY SIZE INTO FELTEXT                                   
155200         CALL FELLOG                                                      
155300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
155400         CONTINUE                                                         
155500     END-SEARCH                                                           
155600     .                                                                    
