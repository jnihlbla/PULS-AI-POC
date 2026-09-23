000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6015500.                                                
000300 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000400 DATE-WRITTEN.   02/02/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        HISTORISKA FÖRFLYTTNINGAR MELLAN CDC OCH SVS                     
000900*        OCH VICE VERSA                                                   
001000*                                                                         
001100*        KOPIERAT PRINTNING FRÅN W6019800                                 
001200*                 SORTERING FRÅN W2035300                                 
001300*                                                                         
001400*                                                                         
001500*                                                                         
001600*        PROGRAMMET LÄSER      WDM9 (HISTORIKBAS)                         
001700*                              WDP7 (USERDATABASEN)                       
001800*                                                                         
001900*        PROGRAMMET UPPDATERAR WDP7 (USERDATABASEN)                       
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W6T155                                              
002300*        MID:         W6I15501                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W6O15501                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W6015500'.            
003600                                                                          
003700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 77  FOERSTA-RADEN               PIC X       VALUE 'N'.                   
004300                                                                          
004400 77  DAGENS-DATUM                PIC X(6)    VALUE SPACE.                 
004500                                                                          
004600 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004700 77  RAD-MAX                     PIC S9(4)  VALUE +13   COMP SYNC.        
004800 77  RAD-IX                      PIC S9(3)  VALUE ZERO  COMP-3.           
004900 77  IDSID-RAKN                  PIC S9(3)  VALUE ZERO.                   
005000                                                                          
005100     EJECT                                                                
005200 01  FILLER                      PIC X(16)   VALUE 'TABELL'.              
005300     SKIP3                                                                
005400 01  TAB-MAX                     PIC S9(9) COMP VALUE 200.                
005500     EJECT                                                                
005600*    --- TABELL SOM SORTERAS AV WINTSOR                                   
005700 01  TABELL.                                                              
005800     03  TAB-POST  OCCURS 200.                                            
005900       04  TAB-RAD.                                                       
006000         05  TAB-IDARTNR         PIC 9(9)    BLANK WHEN ZERO.             
006100         05  TAB-KVANTAL         PIC 9(6)    BLANK WHEN ZERO.             
006200         05  TAB-ADART.                                                   
006300           07  TAB-ADLAGOMR      PIC 9(2)    BLANK WHEN ZERO.             
006400           07  TAB-ADGANG        PIC 9(2)    BLANK WHEN ZERO.             
006500           07  TAB-ADPLATS       PIC 9(5)    BLANK WHEN ZERO.             
006600         05  TAB-TETRPMED        PIC X(20).                               
006700       04  TAB-SORT.                                                      
006800           07  TAB-ADLAGOMR-SORT PIC 9(2)    BLANK WHEN ZERO.             
006900           07  TAB-ADGANG-SORT   PIC 9(2)    BLANK WHEN ZERO.             
007000           07  TAB-ADPLATS-SORT  PIC 9(5)    BLANK WHEN ZERO.             
007100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007200     88  INDATA-OK                           VALUE 'J'.                   
007300     88  INDATA-FEL                          VALUE 'N'.                   
007400                                                                          
007500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007600     88  NYCKLAR-OK                          VALUE 'J'.                   
007700     88  NYCKLAR-FEL                         VALUE 'N'.                   
007800                                                                          
007900                                                                          
008000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008100     88  EGEN-MID                            VALUE '6155'.                
008200     88  GODK-MID                            VALUE '6155'.                
008300     88  HELP-MID                            VALUE '0551'.                
008400                                                                          
008500*    --- GENERELLA ARBETSAREAOR.                                          
008600                                                                          
008700     EJECT                                                                
008800 01  FILLER                      PIC X(16)   VALUE 'WS'.                  
008900     SKIP3                                                                
009000 01  WS.                                                                  
009100*********************************************************                 
009200*    WS-MSGI-AREA-6155                                                    
009300*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
009400*           (I MSGI-SPAR-AREA)                                            
009500*********************************************************                 
009600  05 WS-MSGI-AREA-6155.                                                   
009700    10 WS-MSGI-IDTRANS-6155      PIC X(4)    VALUE '6155'.                
009800    10 WS-MSGI-SSA-KEY-ENTER.                                             
009900      15  WS-ENTER-IDARTNR       PIC S9(9)   COMP-3 VALUE ZERO.           
010000      15  WS-ENTER-DADATTID-9KOMPL                                        
010100                                 PIC 9(14)   VALUE ZERO.                  
010200    10 WS-MSGI-SSA-KEY-NEXT.                                              
010300      15  WS-NEXT-IDARTNR        PIC S9(9)   COMP-3 VALUE ZERO.           
010400      15  WS-NEXT-DADATTID-9KOMPL                                         
010500                                 PIC 9(14)   VALUE ZERO.                  
010600    10 FILLER                    PIC X(958)  VALUE SPACE.                 
010700                                                                          
010800  05 FILLER                      PIC X(16)   VALUE 'WS-IDARTNR'.          
010900  05 WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
011000  05 WS-AVSDAT                   PIC X(6)    VALUE SPACE.                 
011100  05 WS-ADTRDEST                 PIC X(3)    VALUE SPACE.                 
011200  05 FILLER                      PIC X(16)   VALUE 'WS-IDTRPTNR'.         
011300  05 WS-IDTRPTNR                 PIC X(5)    VALUE SPACE.                 
011400  05 WS-DATUM-14                 PIC 9(14)   VALUE ZERO.                  
011500  05 WS-DATUM-14-9KOMPL          PIC 9(14)   VALUE ZERO.                  
011510  05 WS-TITRPMOT                 PIC 9(6)    VALUE ZERO.                  
011600                                                                          
015817                                                                          
015820                                                                          
015900       EJECT                                                              
016000     EJECT                                                                
016100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
016200 01  GENERELLA-SUBPROGRAM.                                                
016300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
016400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016800     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
016900     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
017000     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
017100     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
017200     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
017300     EJECT                                                                
017400*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
017500 01  TABENTRY-PARM.                                                       
017600     03  STEGLANGD               PIC S9(9) COMP  VALUE 88.                
017700     03  ANTAL                   PIC S9(9) COMP.                          
017800     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 10.                
017900                                                                          
018000     EJECT                                                                
018100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
018200*01 -COPY WMSGINIT                                                        
018300     EJECT                                                                
018400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
018500*01 -COPY WMEDAREA                                                        
018600     SKIP3                                                                
018700 01  MESSAGE-CODES.                                                       
018800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
018900     03  CONFLICT                PIC X(3)    VALUE '002'.                 
019000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
019100     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
019200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
019300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
019400     03  TOM-RAD                 PIC X(3)    VALUE '080'.                 
019500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
019600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
019700     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '115'.                 
019800     03  INF-PRINT-BEGAERD       PIC X(3)    VALUE '118'.                 
019900     03  INF-PRINT-START         PIC X(3)    VALUE '202'.                 
020000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
020100     03  AREA-MISSING            PIC X(3)    VALUE '705'.                 
020200     03  ERR-FEL-PRINTER         PIC X(3)    VALUE '772'.                 
020300     03  UPDATING-NOT-ALLOWED    PIC X(3)    VALUE '777'.                 
020301     SKIP3                                                                
020310 01  FELTEXTER.                                                           
020320     03  MED-1.                                                           
020321       05 FILLER                 PIC X(28)                                
020330         VALUE 'VAL: ARTNR EL. (ARTNR+DEST) '.                            
020340       05 FILLER                 PIC X(27)                                
020350         VALUE 'EL. (AVSDAT+DEST) EL. TRPID'.                             
020400     EJECT                                                                
020500 01  FILLER                      PIC X(16)   VALUE 'DAT-AREA'.            
020600     SKIP3                                                                
020700 01  DAT-IO-AREA.                                                         
020800*    03  -COPY WDATAREA                                                   
020900     EJECT                                                                
021000                                                                          
021100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
021200*                                                                         
021300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
021400     SKIP3                                                                
021500*01  MID -COPY W6I15501                                                   
021600     EJECT                                                                
021700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021800     SKIP3                                                                
021900*01  -COPY WMSGAREA                                                       
022000     EJECT                                                                
022100     03  MOD REDEFINES MSG-AREA.                                          
022200*      05  -COPY W6O15501                                                 
022300     EJECT                                                                
022400*    --- AREOR FÖR W006KOM SUBMODUL                                       
022500*                                                                         
022600 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
022700*01  -COPY WMSGKOM                                                        
022800     EJECT                                                                
022900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
023000     SKIP3                                                                
023100*01  -COPY WMFSAREA                                                       
023200     EJECT                                                                
023300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
023400*                                                                         
023500     EJECT                                                                
023600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023700     SKIP3                                                                
023800 01  NYCKLAR-TILL-DLI.                                                    
023900     03  W-IDARTNR-X.                                                     
024000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
024100     03  W-DADATTID-9KOMPL-X.                                             
024200         05  W-DADATTID-9KOMPL   PIC 9(14)   VALUE ZERO.                  
024300     03  W-ADTRDEST              PIC X(3)    VALUE SPACE.                 
024400     03  W-IDTRPTNR-X.                                                    
024500         05  W-IDTRPTNR          PIC S9(5)   VALUE ZERO COMP-3.           
024600   03    W-WDM9A1KY-MIN-X.                                                
024800     05    W-ADTRDEST-MIN        PIC X(3)    VALUE SPACE.                 
024810     05    W-DADATTID-9KOMPL-MIN PIC 9(14)   VALUE ZERO.                  
024900     05    FILLER                PIC X(5)    VALUE LOW-VALUE.             
025000   03    W-WDM9A1KY-MAX-X.                                                
025200     05    W-ADTRDEST-MAX        PIC X(3)    VALUE SPACE.                 
025210     05    W-DADATTID-9KOMPL-MAX PIC 9(14)   VALUE ZERO.                  
025300     05    FILLER                PIC X(5)    VALUE HIGH-VALUE.            
025400   03    W-WDM9B1KY-MIN-X.                                                
025500     05    W-IDTRPTNR-MIN        PIC S9(5)   VALUE ZERO  COMP-3.          
025600     05    FILLER                PIC X(19)   VALUE LOW-VALUE.             
025700   03    W-WDM9B1KY-MAX-X.                                                
025800     05    W-IDTRPTNR-MAX        PIC S9(5)   VALUE ZERO  COMP-3.          
025900     05    FILLER                PIC X(19)   VALUE HIGH-VALUE.            
026000                                                                          
026100                                                                          
026200                                                                          
026300     EJECT                                                                
026400*    --- STATUS-KOD FRÅN IMS                                              
026500 01  FILLER                      PIC X(16)   VALUE 'STATUS-WS'.           
026600 01  STATUS-WS                   PIC XX.                                  
026700     88  SEGMENT-FINNS                       VALUE '  '.                  
026800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026900     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
027000                                                   'GB'.                  
027100     SKIP2                                                                
027200 01  GODK-STATUSKODER.                                                    
027300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027400     SKIP3                                                                
027500     EJECT                                                                
027600 01  FILLER                      PIC X(16)   VALUE 'SSA'.                 
027700     SKIP3                                                                
027800 01  SSA1                        PIC X(160).                              
027900 01  SSA2                        PIC X(128).                              
028000 01  SSA3                        PIC X(128).                              
028100     EJECT                                                                
028200                                                                          
028300*    --- IMS FUNKTIONSKODER                                               
028400*01  -COPY W0003                                                          
028500     EJECT                                                                
028600*    ---  DLI INPUT-OUTPUT AREA                                           
028700     EJECT                                                                
028800                                                                          
028900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM901'.                      
029000 01  DLI-IO-WDM901.                                                       
029100*    03  -COPY WDM901                                                     
029200     EJECT                                                                
029300                                                                          
029400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM911'.                      
029500 01  DLI-IO-WDM911.                                                       
029600*    03  -COPY WDM911                                                     
029700     EJECT                                                                
029710                                                                          
029720 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM9A1'.                      
029730 01  DLI-IO-WDM9A1.                                                       
029740*    03  -COPY WDM9A1                                                     
029750     EJECT                                                                
029760                                                                          
029770 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM9B1'.                      
029780 01  DLI-IO-WDM9B1.                                                       
029790*    03  -COPY WDM9B1                                                     
029791     EJECT                                                                
029800                                                                          
029900 LINKAGE SECTION.                                                         
030000                                                                          
030100*01  -COPY W0009   -PRE MSG-                                              
030200     EJECT                                                                
030300*01  -COPY W0009   -PRE ALT-                                              
030400     EJECT                                                                
030500*01  -COPY W0008   -PRE WDP7-                                             
030600     05  FILLER                  PIC X.                                   
030700     EJECT                                                                
030800*01  -COPY W0008  -PRE WDM9-                                              
030900     05  FILLER                  PIC X.                                   
031000     EJECT                                                                
031100*01  -COPY W0008  -PRE WDM9A-                                             
031200     05  FILLER                  PIC X.                                   
031300     EJECT                                                                
031400*01  -COPY W0008  -PRE WDM9B-                                             
031500     05  FILLER                  PIC X.                                   
031600     EJECT                                                                
031700 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP7-PCB WDM9-PCB              
031800                           WDM9A-PCB WDM9B-PCB.                           
031900 MAIN SECTION.                                                            
032000     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP7-PCB WDM9-PCB              
032100                           WDM9A-PCB WDM9B-PCB.                           
032200                                                                          
032300     PERFORM IMS-GET-MSG                                                  
032400                                                                          
032500     IF SEGMENT-FINNS                                                     
032600        PERFORM A-INIT                                                    
032700        PERFORM B-KOLLA-NYCKLAR                                           
032800                                                                          
032900        IF NYCKLAR-OK                                                     
033000                                                                          
033100           IF MFS-FIRST                                                   
033200              PERFORM C-FOERSTA-SIDA                                      
033300           ELSE                                                           
033400              IF MFS-NEXT                                                 
033500                 PERFORM D-NAESTA-SIDA                                    
033600              ELSE                                                        
033700                 PERFORM E-SAMMA-SIDA                                     
033800              END-IF                                                      
033900           END-IF                                                         
034000                                                                          
034100           PERFORM F-LAES-VISA-INFO                                       
034200                                                                          
034300* ---    UPPDATERA MSGI-SPAR-AREA                                         
034400           MOVE '002'           TO MSGI-KDCALL                            
034500           MOVE MSG-LTERM-NAME  TO MSGI-IDLTERM-USER                      
034600           MOVE MSG-SIGNON-USERID                                         
034700                                TO MSGI-IDUSER                            
034800           MOVE '6155'          TO MSGI-IDTRANS                           
034900           MOVE WS-MSGI-AREA-6155                                         
035000                                TO MSGI-SPAR-AREA                         
035100           CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                     
035200                                                                          
035300        END-IF                                                            
035400                                                                          
037100        COMPUTE MSG-KVLL = LENGTH OF MOD-W6O15501 + 4                     
037200        PERFORM IMS-INSERT-MSG                                            
037300     END-IF                                                               
037400                                                                          
037500     MOVE ZERO TO RETURN-CODE                                             
037600     GOBACK                                                               
037700     .                                                                    
037800     EJECT                                                                
037900 A-INIT SECTION.                                                          
038000                                                                          
038100     IF MSG-DUBBLA-TRANSKODER                                             
038200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I15501                 
038300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
038400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
038500     ELSE                                                                 
038600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I15501                  
038700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
038800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
038900     END-IF                                                               
039000                                                                          
039100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
039200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
039300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
039400                                                                          
039500     MOVE LOW-VALUE TO MSG-AREA                                           
039600     MOVE 'W6O15501' TO MFS-IDMOD                                         
039700     MOVE '6155' TO MOD-IDTRANS                                           
039800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
039900                                                                          
040000     IF EGEN-MID OR HELP-MID                                              
040100        CONTINUE                                                          
040200     ELSE                                                                 
040300        MOVE SPACE TO MFS-KDTRTYP                                         
040400        MOVE '7'   TO MFS-IDPFK                                           
040500     END-IF                                                               
040600     ACCEPT DAGENS-DATUM FROM DATE                                        
040610     MOVE 'SE '           TO MED-IDSKYLT                                  
040700     .                                                                    
040800     EJECT                                                                
040900 B-KOLLA-NYCKLAR SECTION.                                                 
041000                                                                          
041100     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
041200                                WS-MSGI-SSA-KEY-NEXT                      
041300                                                                          
041400******   UPPDATERING AV MSGI-BLÄDDRINGSNYCKLAR SKER                       
041500******   I SLUTET AV PROGRAMMET                                           
041600     MOVE ALL '+'            TO MSGI-WMSGINIT                             
041700     MOVE '001'              TO MSGI-KDCALL                               
041800     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
041900     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
042000     MOVE '6155'             TO MSGI-IDTRANS                              
042100     IF EGEN-MID                                                          
042200       MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                              
042300     END-IF                                                               
042400     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
042500                                                                          
042600     IF EGEN-MID                                                          
042700     AND MSGI-SPAR-AREA(1:4) = '6155'                                     
042800       MOVE MSGI-SPAR-AREA   TO WS-MSGI-AREA-6155                         
042900     ELSE                                                                 
043000       MOVE SPACE            TO MID-AVSDAT-IN                             
043100                                MID-ADTRDEST-IN                           
043200                                MID-IDTRPTNR-IN                           
043300     END-IF                                                               
043400                                                                          
043500                                                                          
043600     MOVE JA TO NYCKLAR-SW                                                
043700     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-IN                            
043800                                                                          
043801     IF MID-IDARTNR-IN = ALL '+'                                          
043810       IF (MID-AVSDAT-IN = ALL '+' OR SPACE) AND                          
043820          (MID-ADTRDEST-IN = ALL '+' OR SPACE) AND                        
043830          (MID-IDTRPTNR-IN = ALL '+' OR SPACE)                            
043900          MOVE MSGI-IDARTNR TO WS-IDARTNR                                 
044000                               MOD-IDARTNR-UT                             
044010       END-IF                                                             
044020     ELSE                                                                 
044021       MOVE MSGI-IDARTNR TO WS-IDARTNR                                    
044022                            MOD-IDARTNR-UT                                
044030     END-IF                                                               
044100     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
044200     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
044300                                                                          
044400     IF MID-IDARTNR-IN = ALL '+'                                          
044500       CONTINUE                                                           
044600     ELSE                                                                 
044700       MOVE '7'              TO MFS-IDPFK                                 
044800       MOVE SPACE            TO MFS-KDTRTYP                               
044900     END-IF                                                               
045000                                                                          
045100     IF WS-IDARTNR NUMERIC                                                
045200       CONTINUE                                                           
045300     ELSE                                                                 
045400       MOVE NEJ              TO NYCKLAR-SW                                
045600     END-IF                                                               
045700                                                                          
045800     MOVE MFS-RENSA-FAELT    TO MOD-AVSDAT-IN                             
045900                                                                          
046000     IF MID-AVSDAT-IN = ALL '+'                                           
046011        IF MID-ADTRDEST-IN = ALL '+' AND                                  
046012           MID-IDTRPTNR-IN = ALL '+' AND MID-IDARTNR-IN = ALL '+'         
046020                                                                          
046100           INSPECT MID-AVSDAT-UT   REPLACING LEADING '+' BY SPACE         
046200           MOVE MID-AVSDAT-UT    TO WS-AVSDAT                             
046310        END-IF                                                            
046400     ELSE                                                                 
046500       MOVE MID-AVSDAT-IN    TO WS-AVSDAT                                 
046700       MOVE '7'              TO MFS-IDPFK                                 
046800       MOVE SPACE            TO MFS-KDTRTYP                               
046900     END-IF                                                               
047000     INSPECT WS-AVSDAT REPLACING LEADING SPACE BY ZERO                    
047100     MOVE WS-AVSDAT          TO MOD-AVSDAT-UT                             
047110     IF WS-AVSDAT = ZERO                                                  
047200       INSPECT MOD-AVSDAT-UT REPLACING LEADING ZERO BY SPACE              
047210     END-IF                                                               
047400                                                                          
047500     IF WS-AVSDAT NUMERIC                                                 
047600       CONTINUE                                                           
047700     ELSE                                                                 
047900       MOVE NEJ              TO NYCKLAR-SW                                
048000     END-IF                                                               
048100                                                                          
048200     IF MID-ADTRDEST-IN = ALL '+'                                         
048210       IF MID-IDARTNR-IN = ALL '+' AND                                    
048220          MID-IDTRPTNR-IN = ALL '+' AND MID-AVSDAT-IN = ALL '+'           
048300          INSPECT MID-ADTRDEST-UT REPLACING LEADING '+' BY SPACE          
048400          MOVE MID-ADTRDEST-UT  TO WS-ADTRDEST                            
048410       END-IF                                                             
048500     ELSE                                                                 
048600       MOVE MID-ADTRDEST-IN  TO WS-ADTRDEST                               
048700       MOVE '7'              TO MFS-IDPFK                                 
048800       MOVE SPACE            TO MFS-KDTRTYP                               
048900     END-IF                                                               
049000     MOVE WS-ADTRDEST        TO MOD-ADTRDEST-UT                           
049100                                                                          
049200     IF WS-ADTRDEST = 'SVS'                                               
049300     OR WS-ADTRDEST = 'CDC'                                               
049400     OR WS-ADTRDEST = SPACE                                               
049500       MOVE WS-ADTRDEST      TO MOD-ADTRDEST-UT                           
049600     ELSE                                                                 
049800       MOVE NEJ TO NYCKLAR-SW                                             
049900     END-IF                                                               
050000                                                                          
050100     MOVE MFS-RENSA-FAELT    TO MOD-IDTRPTNR-IN                           
050200                                                                          
050300     IF MID-IDTRPTNR-IN = ALL '+'                                         
050310       IF MID-IDARTNR-IN = ALL '+' AND                                    
050320          MID-ADTRDEST-IN = ALL '+' AND MID-AVSDAT-IN = ALL '+'           
050500          INSPECT MID-IDTRPTNR-UT REPLACING LEADING '+' BY SPACE          
050600          MOVE MID-IDTRPTNR-UT  TO WS-IDTRPTNR                            
050710       END-IF                                                             
050800     ELSE                                                                 
051000       MOVE MID-IDTRPTNR-IN  TO WS-IDTRPTNR                               
051200       MOVE '7'              TO MFS-IDPFK                                 
051300       MOVE SPACE            TO MFS-KDTRTYP                               
051400     END-IF                                                               
051500     INSPECT WS-IDTRPTNR REPLACING LEADING SPACE BY ZERO                  
051600     MOVE WS-IDTRPTNR        TO MOD-IDTRPTNR-UT                           
051700     INSPECT MOD-IDTRPTNR-UT REPLACING LEADING ZERO BY SPACE              
051800                                                                          
051900     IF WS-IDTRPTNR NUMERIC                                               
052000       CONTINUE                                                           
052100     ELSE                                                                 
052300       MOVE NEJ              TO NYCKLAR-SW                                
052400     END-IF                                                               
052500                                                                          
052600                                                                          
052700                                                                          
052800*                                                                         
052900*        TRE GILTIGA VAL FINNS                                            
053000*                                                                         
053100*        1) ARTNR                                                         
053200*        2) AVSDAT + ADTRDEST                                             
053300*        3) IDTRPTNR                                                      
053310*        4) ARTNR  + ADTRDEST                                             
053400*                                                                         
053500                                                                          
053600                                                                          
053700     IF (WS-IDARTNR  NOT = ZERO                                           
053800     AND WS-AVSDAT       = ZERO                                           
053900     AND WS-ADTRDEST     = SPACE                                          
054000     AND WS-IDTRPTNR     = ZERO)                                          
054100                                                                          
054200     OR (WS-IDARTNR      = ZERO                                           
054300     AND WS-AVSDAT   NOT = ZERO                                           
054400     AND WS-ADTRDEST NOT = SPACE                                          
054500     AND WS-IDTRPTNR     = ZERO)                                          
054600                                                                          
054700     OR (WS-IDARTNR      = ZERO                                           
054800     AND WS-AVSDAT       = ZERO                                           
054900     AND WS-ADTRDEST     = SPACE                                          
055000     AND WS-IDTRPTNR NOT = ZERO)                                          
055100                                                                          
055110     OR (WS-IDARTNR  NOT = ZERO                                           
055120     AND WS-AVSDAT       = ZERO                                           
055130     AND WS-ADTRDEST NOT = SPACE                                          
055140     AND WS-IDTRPTNR     = ZERO)                                          
055150                                                                          
055200       CONTINUE                                                           
055300     ELSE                                                                 
055500       MOVE NEJ              TO NYCKLAR-SW                                
055600     END-IF                                                               
055700                                                                          
055800     IF NYCKLAR-FEL                                                       
055910        MOVE MED-1           TO MOD-TEMFSINF                              
056000        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
056100        CALL WMEDKONV USING MED-WMEDAREA                                  
056200                                                                          
056300        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
056400        PERFORM MFS-RENSA-FAELT-IN                                        
056500        PERFORM MFS-RENSA-FAELT-UT                                        
056600     END-IF                                                               
056700     .                                                                    
056800     EJECT                                                                
056900 C-FOERSTA-SIDA SECTION.                                                  
057000                                                                          
057100     MOVE 'SE '           TO MED-IDSKYLT                                  
057200     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
057300     CALL WMEDKONV USING MED-WMEDAREA                                     
057400     MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                                  
057500                                                                          
057600*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
057700     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
057800                                WS-MSGI-SSA-KEY-NEXT                      
057900     PERFORM MFS-RENSA-FAELT-IN                                           
058000     .                                                                    
058100     EJECT                                                                
058200 D-NAESTA-SIDA SECTION.                                                   
058300                                                                          
059000     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
059200     .                                                                    
059300     EJECT                                                                
059400 E-SAMMA-SIDA SECTION.                                                    
059500                                                                          
059600     PERFORM MFS-RENSA-FAELT-IN                                           
060600     .                                                                    
060700     EJECT                                                                
060800 F-LAES-VISA-INFO SECTION.                                                
060900                                                                          
061000     IF WS-IDARTNR NOT = ZERO                                             
061100                                                                          
061200*                                                                         
061300*       WS-IDARTNR ÄR VALT SOM URVAL                                      
061400*       OCH EVENTUELLT WS-ADTRDEST                                        
061410*                                                                         
061500                                                                          
061600                                                                          
061700       IF WS-MSGI-SSA-KEY-ENTER NOT = SPACE                               
061800         MOVE WS-ENTER-IDARTNR                                            
061900                             TO W-IDARTNR                                 
061930         PERFORM IMS-GU-M901                                              
061940                                                                          
061950         IF SEGMENT-FINNS                                                 
062000           MOVE WS-ENTER-DADATTID-9KOMPL                                  
062100                             TO W-DADATTID-9KOMPL                         
062200           PERFORM IMS-GNP-M911-KVAL                                      
062210         END-IF                                                           
062300                                                                          
062400       ELSE                                                               
062500         IF WS-MSGI-SSA-KEY-NEXT NOT = SPACE                              
062600           MOVE WS-NEXT-IDARTNR                                           
062700                             TO W-IDARTNR                                 
062710           PERFORM IMS-GU-M901                                            
062720                                                                          
062730           IF SEGMENT-FINNS                                               
062800             MOVE WS-NEXT-DADATTID-9KOMPL                                 
062900                             TO W-DADATTID-9KOMPL                         
062910             PERFORM IMS-GNP-M911-KVAL                                    
062920           END-IF                                                         
063100         ELSE                                                             
063200                                                                          
063300           MOVE WS-IDARTNR   TO W-IDARTNR                                 
063500           PERFORM IMS-GU-M901                                            
063600                                                                          
063700           IF SEGMENT-FINNS                                               
063800             PERFORM IMS-GNP-M911                                         
063900           END-IF                                                         
064000         END-IF                                                           
064100       END-IF                                                             
064200       MOVE SPACE            TO WS-MSGI-SSA-KEY-ENTER                     
064300                                WS-MSGI-SSA-KEY-NEXT                      
064400                                                                          
064500       MOVE 1                TO INDX                                      
064600       PERFORM UNTIL INDX > RAD-MAX                                       
064700       OR SEGMENT-SAKNAS                                                  
064800                                                                          
064810         IF WS-ADTRDEST = SPACE                                           
064820         OR WS-ADTRDEST = TRP-ADTRDEST                                    
064900            MOVE ART-IDARTNR TO MOD-IDARTNR (INDX)                        
065000            MOVE TRP-KVANTAL TO MOD-KVANTAL (INDX)                        
065100            MOVE TRP-ADTRDEST                                             
065110                             TO MOD-ADTRDEST (INDX)                       
065200            COMPUTE WS-DATUM-14 = 99999999999999                          
065300                              - TRP-DADATTID-9KOMPL                       
065400            MOVE WS-DATUM-14 (3:6)                                        
065500                             TO MOD-AVSDAT (INDX)                         
065600            MOVE TRP-TITRPMOT                                             
065601                             TO WS-TITRPMOT                               
065610            IF WS-TITRPMOT = ZERO                                         
065620              MOVE SPACE     TO MOD-TITRPMOT (INDX)                       
065630            ELSE                                                          
065640              MOVE WS-TITRPMOT                                            
065641                             TO MOD-TITRPMOT (INDX)                       
065650            END-IF                                                        
065700            MOVE TRP-IDTRPTNR                                             
065710                             TO MOD-IDTRPTNR (INDX)                       
065800            MOVE TRP-ADINLOMR-LPL                                         
065900                             TO MOD-ADINLOMR-LPL (INDX)                   
066000            MOVE TRP-IDUSER-TRP                                           
066100                             TO MOD-IDUSER-TRP (INDX)                     
066200            MOVE TRP-TETRPMED                                             
066210                             TO MOD-TETRPMED (INDX)                       
066300                                                                          
066400            IF INDX = 1                                                   
066500              MOVE ART-IDARTNR                                            
066510                             TO WS-ENTER-IDARTNR                          
066600              MOVE TRP-DADATTID-9KOMPL                                    
066700                             TO WS-ENTER-DADATTID-9KOMPL                  
066800            END-IF                                                        
066900                                                                          
067000            ADD 1            TO INDX                                      
067001                                                                          
067010          END-IF                                                          
067100          PERFORM IMS-GNP-M911                                            
067200                                                                          
067300       END-PERFORM                                                        
067400                                                                          
067500       IF SEGMENT-FINNS                                                   
067600          MOVE ART-IDARTNR   TO WS-NEXT-IDARTNR                           
067700          MOVE TRP-DADATTID-9KOMPL                                        
067800                             TO WS-NEXT-DADATTID-9KOMPL                   
067900          MOVE 'SE '         TO MED-IDSKYLT                               
068000          MOVE INF-MORE-INFO-EXISTS                                       
068100                             TO MED-IDMFSFEL                              
068200          CALL WMEDKONV USING MED-WMEDAREA                                
068300          MOVE MED-TEMFSFEL  TO MOD-TEMFSFEL                              
068400       ELSE                                                               
068500          MOVE SPACE         TO WS-MSGI-SSA-KEY-NEXT                      
068600       END-IF                                                             
068700                                                                          
068800       PERFORM UNTIL INDX > RAD-MAX                                       
068900                                                                          
069000          MOVE MFS-RENSA-FAELT TO MOD-IDARTNR (INDX)                      
069100                                  MOD-KVANTAL (INDX)                      
069200                                  MOD-ADTRDEST (INDX)                     
069300                                  MOD-AVSDAT (INDX)                       
069400                                  MOD-TITRPMOT (INDX)                     
069500                                  MOD-IDTRPTNR (INDX)                     
069600                                  MOD-ADINLOMR-LPL (INDX)                 
069700                                  MOD-IDUSER-TRP (INDX)                   
069800                                  MOD-TETRPMED (INDX)                     
069900                                                                          
070000         ADD 1               TO INDX                                      
070100                                                                          
070200       END-PERFORM                                                        
070300     ELSE                                                                 
070400                                                                          
070500       IF  WS-AVSDAT NOT = ZERO                                           
070600       AND WS-ADTRDEST NOT = SPACE                                        
070700                                                                          
070800*                                                                         
070900*       WS-AVSDAT OCH WS-ADTRDEST ÄR VALT SOM URVAL                       
071000*                                                                         
071100                                                                          
071200         IF WS-MSGI-SSA-KEY-ENTER NOT = SPACE                             
071300           MOVE WS-ENTER-DADATTID-9KOMPL                                  
071400                               TO W-DADATTID-9KOMPL-MIN                   
071500                                                                          
071600         ELSE                                                             
071700           IF WS-MSGI-SSA-KEY-NEXT NOT = SPACE                            
071800             MOVE WS-NEXT-DADATTID-9KOMPL                                 
071900                               TO W-DADATTID-9KOMPL-MIN                   
072000           ELSE                                                           
072100                                                                          
072101             MOVE 20         TO WS-DATUM-14 (1:2)                         
072102             MOVE WS-AVSDAT  TO WS-DATUM-14 (3:6)                         
072103             MOVE 999999     TO WS-DATUM-14 (9:6)                         
072104             COMPUTE WS-DATUM-14-9KOMPL = 99999999999999                  
072105                                        - WS-DATUM-14                     
072106             MOVE WS-DATUM-14-9KOMPL                                      
072107                             TO W-DADATTID-9KOMPL-MIN                     
072110                                                                          
072800           END-IF                                                         
072900         END-IF                                                           
073000         MOVE SPACE          TO WS-MSGI-SSA-KEY-ENTER                     
073100                                WS-MSGI-SSA-KEY-NEXT                      
073110         MOVE 20             TO WS-DATUM-14 (1:2)                         
073120         MOVE WS-AVSDAT      TO WS-DATUM-14 (3:6)                         
073130         MOVE ZERO           TO WS-DATUM-14 (9:6)                         
073140         COMPUTE WS-DATUM-14-9KOMPL = 99999999999999                      
073150                                    - WS-DATUM-14                         
073160         MOVE WS-DATUM-14-9KOMPL                                          
073170                             TO W-DADATTID-9KOMPL-MAX                     
073200                                                                          
074000         MOVE WS-ADTRDEST    TO W-ADTRDEST-MIN                            
074100                                W-ADTRDEST-MAX                            
074200                                                                          
074300         PERFORM IMS-GU-WDM9A                                             
074410                                                                          
074500         MOVE 1              TO INDX                                      
074600         PERFORM UNTIL INDX > RAD-MAX                                     
074700         OR SEGMENT-SAKNAS                                                
074800                                                                          
074900           IF WS-ADTRDEST = SEQA-ADTRDEST                                 
075100                                                                          
075200              MOVE SEQA-IDARTNR                                           
075210                             TO MOD-IDARTNR (INDX)                        
075220                                W-IDARTNR                                 
075221              MOVE SEQA-DADATTID-9KOMPL                                   
075222                             TO W-DADATTID-9KOMPL                         
075230              PERFORM IMS-GU-M911                                         
075300              MOVE TRP-KVANTAL                                            
075310                             TO MOD-KVANTAL (INDX)                        
075400              MOVE TRP-ADTRDEST                                           
075410                             TO MOD-ADTRDEST (INDX)                       
075500              COMPUTE WS-DATUM-14 = 99999999999999                        
075600                                  - TRP-DADATTID-9KOMPL                   
075700              MOVE WS-DATUM-14 (3:6)                                      
075800                             TO MOD-AVSDAT (INDX)                         
075920              MOVE TRP-TITRPMOT                                           
075921                             TO WS-TITRPMOT                               
075930              IF WS-TITRPMOT = ZERO                                       
075940                MOVE SPACE   TO MOD-TITRPMOT (INDX)                       
075950              ELSE                                                        
075960                MOVE WS-TITRPMOT                                          
075961                             TO MOD-TITRPMOT (INDX)                       
075970              END-IF                                                      
076000              MOVE TRP-IDTRPTNR                                           
076010                             TO MOD-IDTRPTNR (INDX)                       
076100              MOVE TRP-ADINLOMR-LPL                                       
076200                             TO MOD-ADINLOMR-LPL (INDX)                   
076300              MOVE TRP-IDUSER-TRP                                         
076400                             TO MOD-IDUSER-TRP (INDX)                     
076500              MOVE TRP-TETRPMED                                           
076510                             TO MOD-TETRPMED (INDX)                       
076600                                                                          
076700              IF INDX = 1                                                 
076800                MOVE SEQA-IDARTNR                                         
076810                             TO WS-ENTER-IDARTNR                          
076900                MOVE SEQA-DADATTID-9KOMPL                                 
077000                             TO WS-ENTER-DADATTID-9KOMPL                  
077100              END-IF                                                      
077200                                                                          
077300              ADD 1          TO INDX                                      
077400           END-IF                                                         
077500           PERFORM IMS-GN-WDM9A                                           
077600                                                                          
077700         END-PERFORM                                                      
077800                                                                          
077900         IF SEGMENT-FINNS                                                 
077910            MOVE SEQA-IDARTNR                                             
078000                             TO WS-NEXT-IDARTNR                           
078010            MOVE SEQA-DADATTID-9KOMPL                                     
078200                             TO WS-NEXT-DADATTID-9KOMPL                   
078300            MOVE 'SE '       TO MED-IDSKYLT                               
078400            MOVE INF-MORE-INFO-EXISTS                                     
078500                             TO MED-IDMFSFEL                              
078600            CALL WMEDKONV USING MED-WMEDAREA                              
078700            MOVE MED-TEMFSFEL                                             
078800                             TO MOD-TEMFSFEL                              
078900         ELSE                                                             
079000            MOVE SPACE       TO WS-MSGI-SSA-KEY-NEXT                      
079100         END-IF                                                           
079200                                                                          
079300         PERFORM UNTIL INDX > RAD-MAX                                     
079400                                                                          
079500            MOVE MFS-RENSA-FAELT TO MOD-IDARTNR (INDX)                    
079600                                    MOD-KVANTAL (INDX)                    
079700                                    MOD-ADTRDEST (INDX)                   
079800                                    MOD-AVSDAT (INDX)                     
079900                                    MOD-TITRPMOT (INDX)                   
080000                                    MOD-IDTRPTNR (INDX)                   
080100                                    MOD-ADINLOMR-LPL (INDX)               
080200                                    MOD-IDUSER-TRP (INDX)                 
080300                                    MOD-TETRPMED (INDX)                   
080400                                                                          
080500           ADD 1             TO INDX                                      
080600                                                                          
080700         END-PERFORM                                                      
080800       ELSE                                                               
080900                                                                          
081000         IF WS-IDTRPTNR NOT = ZERO                                        
081100                                                                          
081200*                                                                         
081300*       WS-IDTRPTNR ÄR VALT SOM URVAL                                     
081400*                                                                         
081500                                                                          
081600           MOVE WS-IDTRPTNR  TO W-IDTRPTNR-MIN                            
081700                                W-IDTRPTNR-MAX                            
081800                                                                          
081900           PERFORM IMS-GU-WDM9B                                           
082000                                                                          
082100           IF WS-MSGI-SSA-KEY-ENTER NOT = SPACE                           
082200             PERFORM UNTIL SEGMENT-SAKNAS                                 
082300             OR WS-ENTER-DADATTID-9KOMPL = SEQB-DADATTID-9KOMPL           
082400                PERFORM IMS-GN-WDM9B                                      
082500             END-PERFORM                                                  
082600                                                                          
082700           ELSE                                                           
082800                                                                          
082900             IF WS-MSGI-SSA-KEY-NEXT NOT = SPACE                          
083000               PERFORM UNTIL SEGMENT-SAKNAS                               
083100               OR WS-NEXT-DADATTID-9KOMPL = SEQB-DADATTID-9KOMPL          
083200                  PERFORM IMS-GN-WDM9B                                    
083300               END-PERFORM                                                
083400                                                                          
083500             END-IF                                                       
083600           END-IF                                                         
083700           MOVE SPACE        TO WS-MSGI-SSA-KEY-ENTER                     
083800                                WS-MSGI-SSA-KEY-NEXT                      
083900                                                                          
084000           MOVE 1            TO INDX                                      
084100           PERFORM UNTIL INDX > RAD-MAX                                   
084200           OR SEGMENT-SAKNAS                                              
084300                                                                          
084510              MOVE SEQB-IDARTNR                                           
084600                             TO MOD-IDARTNR (INDX)                        
084610                                W-IDARTNR                                 
084620              MOVE SEQB-DADATTID-9KOMPL                                   
084630                             TO W-DADATTID-9KOMPL                         
084640              PERFORM IMS-GU-M911                                         
084650                                                                          
084700              MOVE TRP-KVANTAL                                            
084710                             TO MOD-KVANTAL (INDX)                        
084800              MOVE TRP-ADTRDEST                                           
084810                             TO MOD-ADTRDEST (INDX)                       
084900              COMPUTE WS-DATUM-14 = 99999999999999                        
085000                                  - TRP-DADATTID-9KOMPL                   
085100              MOVE WS-DATUM-14 (3:6)                                      
085200                             TO MOD-AVSDAT (INDX)                         
085320              MOVE TRP-TITRPMOT                                           
085330                             TO WS-TITRPMOT                               
085340              IF WS-TITRPMOT = ZERO                                       
085350                MOVE SPACE   TO MOD-TITRPMOT (INDX)                       
085360              ELSE                                                        
085370                MOVE WS-TITRPMOT                                          
085380                             TO MOD-TITRPMOT (INDX)                       
085390              END-IF                                                      
085400              MOVE TRP-IDTRPTNR                                           
085410                             TO MOD-IDTRPTNR (INDX)                       
085500              MOVE TRP-ADINLOMR-LPL                                       
085600                             TO MOD-ADINLOMR-LPL (INDX)                   
085700              MOVE TRP-IDUSER-TRP                                         
085800                             TO MOD-IDUSER-TRP (INDX)                     
085900              MOVE TRP-TETRPMED                                           
085910                             TO MOD-TETRPMED (INDX)                       
086000                                                                          
086100              IF INDX = 1                                                 
086200                MOVE SEQB-IDARTNR                                         
086210                             TO WS-ENTER-IDARTNR                          
086300                MOVE SEQB-DADATTID-9KOMPL                                 
086400                             TO WS-ENTER-DADATTID-9KOMPL                  
086500              END-IF                                                      
086600                                                                          
086700              ADD 1          TO INDX                                      
086800              PERFORM IMS-GN-WDM9B                                        
086900                                                                          
087000           END-PERFORM                                                    
087100                                                                          
087200           IF SEGMENT-FINNS                                               
087210              MOVE SEQB-IDARTNR                                           
087300                             TO WS-NEXT-IDARTNR                           
087310              MOVE SEQB-DADATTID-9KOMPL                                   
087500                             TO WS-NEXT-DADATTID-9KOMPL                   
087600              MOVE 'SE '     TO MED-IDSKYLT                               
087700              MOVE INF-MORE-INFO-EXISTS                                   
087800                             TO MED-IDMFSFEL                              
087900              CALL WMEDKONV USING MED-WMEDAREA                            
088000              MOVE MED-TEMFSFEL                                           
088100                             TO MOD-TEMFSFEL                              
088200           ELSE                                                           
088300              MOVE SPACE     TO WS-MSGI-SSA-KEY-NEXT                      
088400           END-IF                                                         
088500                                                                          
088600           PERFORM UNTIL INDX > RAD-MAX                                   
088700                                                                          
088800              MOVE MFS-RENSA-FAELT TO MOD-IDARTNR (INDX)                  
088900                                      MOD-KVANTAL (INDX)                  
089000                                      MOD-ADTRDEST (INDX)                 
089100                                      MOD-AVSDAT (INDX)                   
089200                                      MOD-TITRPMOT (INDX)                 
089300                                      MOD-IDTRPTNR (INDX)                 
089400                                      MOD-ADINLOMR-LPL (INDX)             
089500                                      MOD-IDUSER-TRP (INDX)               
089600                                      MOD-TETRPMED (INDX)                 
089700                                                                          
089800             ADD 1           TO INDX                                      
089900                                                                          
090000           END-PERFORM                                                    
090100         END-IF                                                           
090200       END-IF                                                             
090300     END-IF                                                               
090400     .                                                                    
090500     EJECT                                                                
090600 MFS-RENSA-FAELT-UT SECTION.                                              
090700                                                                          
091400     MOVE +1 TO INDX                                                      
091500     PERFORM UNTIL INDX > RAD-MAX                                         
091600       MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR (INDX)                      
091700                                  MOD-KVANTAL (INDX)                      
091800                                  MOD-ADTRDEST (INDX)                     
091900                                  MOD-AVSDAT (INDX)                       
092000                                  MOD-TITRPMOT (INDX)                     
092100                                  MOD-IDTRPTNR (INDX)                     
092200                                  MOD-ADINLOMR-LPL (INDX)                 
092300                                  MOD-IDUSER-TRP (INDX)                   
092400                                  MOD-TETRPMED (INDX)                     
092500       ADD +1 TO INDX                                                     
092600     END-PERFORM                                                          
092700     .                                                                    
092800     EJECT                                                                
092900 MFS-RENSA-FAELT-IN SECTION.                                              
093000                                                                          
093100*    --- ALLA INDATA-FÄLT                                                 
093200     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-IN                            
093300                                MOD-AVSDAT-IN                             
093400                                MOD-ADTRDEST-IN                           
093500                                MOD-IDTRPTNR-IN                           
093600     .                                                                    
093700     EJECT                                                                
096900* --- IMS SEKTIONER ---                                                   
097000     SKIP3                                                                
097100 IMS-GET-MSG SECTION.                                                     
097200     MOVE '  QC' TO GODK-STATUSKODER                                      
097300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
097400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
097500     PERFORM IMS-STATUSKONTROLL                                           
097600     .                                                                    
097700     SKIP2                                                                
097800 IMS-INSERT-MSG SECTION.                                                  
097900     IF SWEDISH-TEXT                                                      
098000        IF MSGI-IDLAND-SPR NOT = 'GB'                                     
098100           MOVE '0' TO MFS-KDHUVOMR                                       
098200        END-IF                                                            
098300     END-IF                                                               
098400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
098500     MOVE SPACE TO GODK-STATUSKODER                                       
098600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
098700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
098800     PERFORM IMS-STATUSKONTROLL                                           
098900     .                                                                    
099000     EJECT                                                                
099100 IMS-GU-M901    SECTION.                                                  
099200                                                                          
099300     STRING 'WDM901  (IDARTNR  =' W-IDARTNR-X ')'                         
099400          DELIMITED BY SIZE INTO SSA1                                     
099500     MOVE '  GE' TO GODK-STATUSKODER                                      
099600     CALL CBLTDLI USING GU  WDM9-PCB DLI-IO-WDM901 SSA1                   
099700     MOVE WDM9-STATUS-CODE TO STATUS-WS                                   
099800     PERFORM IMS-STATUSKONTROLL                                           
099900     .                                                                    
100000     EJECT                                                                
100100 IMS-GU-M911 SECTION.                                                     
100200                                                                          
100300     STRING 'WDM901  (IDARTNR  =' W-IDARTNR-X ')'                         
100400          DELIMITED BY SIZE INTO SSA1                                     
100500     STRING 'WDM911  (DADATTI9 =' W-DADATTID-9KOMPL-X ')'                 
100600          DELIMITED BY SIZE INTO SSA2                                     
100700     MOVE '  ' TO GODK-STATUSKODER                                        
100800     CALL CBLTDLI USING GU WDM9-PCB                                       
100900                             DLI-IO-WDM911 SSA1 SSA2                      
101000     MOVE WDM9-STATUS-CODE TO STATUS-WS                                   
101100     PERFORM IMS-STATUSKONTROLL                                           
101200     .                                                                    
101300     EJECT                                                                
101400 IMS-GNP-M911 SECTION.                                                    
101500                                                                          
101600     STRING 'WDM911    '                                                  
101700          DELIMITED BY SIZE INTO SSA1                                     
101800     MOVE '  GE' TO GODK-STATUSKODER                                      
101900     CALL CBLTDLI USING GNP WDM9-PCB DLI-IO-WDM911 SSA1                   
102000     MOVE WDM9-STATUS-CODE TO STATUS-WS                                   
102100     PERFORM IMS-STATUSKONTROLL                                           
102200     .                                                                    
102300     EJECT                                                                
102320 IMS-GNP-M911-KVAL SECTION.                                               
102330                                                                          
102351     STRING 'WDM911  (DADATTI9 =' W-DADATTID-9KOMPL-X ')'                 
102352          DELIMITED BY SIZE INTO SSA1                                     
102360     MOVE '  GE' TO GODK-STATUSKODER                                      
102370     CALL CBLTDLI USING GNP WDM9-PCB DLI-IO-WDM911 SSA1                   
102380     MOVE WDM9-STATUS-CODE TO STATUS-WS                                   
102390     PERFORM IMS-STATUSKONTROLL                                           
102391     .                                                                    
102392     EJECT                                                                
102400 IMS-GU-WDM9A SECTION.                                                    
102500     STRING 'WDM9A1  (WDM9A1KY>=' W-WDM9A1KY-MIN-X                        
102600                    '&WDM9A1KY<=' W-WDM9A1KY-MAX-X ')'                    
102700            DELIMITED BY SIZE INTO SSA1                                   
102800     MOVE '  GE' TO GODK-STATUSKODER                                      
102900     CALL CBLTDLI USING GU WDM9A-PCB DLI-IO-WDM9A1 SSA1                   
103000     MOVE WDM9A-STATUS-CODE TO STATUS-WS                                  
103100     PERFORM IMS-STATUSKONTROLL                                           
103200     .                                                                    
103300     EJECT                                                                
103400 IMS-GN-WDM9A SECTION.                                                    
103500     STRING 'WDM9A1  (WDM9A1KY>=' W-WDM9A1KY-MIN-X                        
103600                    '&WDM9A1KY<=' W-WDM9A1KY-MAX-X ')'                    
103700            DELIMITED BY SIZE INTO SSA1                                   
103800     MOVE '  GE' TO GODK-STATUSKODER                                      
103900     CALL CBLTDLI USING GN WDM9A-PCB DLI-IO-WDM9A1 SSA1                   
104000     MOVE WDM9A-STATUS-CODE TO STATUS-WS                                  
104100     PERFORM IMS-STATUSKONTROLL                                           
104200     .                                                                    
104300     EJECT                                                                
104400 IMS-GU-WDM9B SECTION.                                                    
104500     STRING 'WDM9B1  (WDM9B1KY>=' W-WDM9B1KY-MIN-X                        
104600                    '&WDM9B1KY<=' W-WDM9B1KY-MAX-X ')'                    
104700            DELIMITED BY SIZE INTO SSA1                                   
104800     MOVE '  GE' TO GODK-STATUSKODER                                      
104900     CALL CBLTDLI USING GU WDM9B-PCB DLI-IO-WDM9B1 SSA1                   
105000     MOVE WDM9B-STATUS-CODE TO STATUS-WS                                  
105100     PERFORM IMS-STATUSKONTROLL                                           
105200     .                                                                    
105300     EJECT                                                                
105400 IMS-GN-WDM9B SECTION.                                                    
105500     STRING 'WDM9B1  (WDM9B1KY>=' W-WDM9B1KY-MIN-X                        
105600                    '&WDM9B1KY<=' W-WDM9B1KY-MAX-X ')'                    
105700            DELIMITED BY SIZE INTO SSA1                                   
105800     MOVE '  GE' TO GODK-STATUSKODER                                      
105900     CALL CBLTDLI USING GN WDM9B-PCB DLI-IO-WDM9B1 SSA1                   
106000     MOVE WDM9B-STATUS-CODE TO STATUS-WS                                  
106100     PERFORM IMS-STATUSKONTROLL                                           
106200     .                                                                    
106300     EJECT                                                                
107400 IMS-STATUSKONTROLL SECTION.                                              
107500     SET STATUS-IX TO 1                                                   
107600     SEARCH GODK-STATUS                                                   
107700       AT END                                                             
107800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
107900         DELIMITED BY SIZE INTO FELTEXT                                   
108000         CALL FELLOG                                                      
108100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
108200         CONTINUE                                                         
108300     END-SEARCH                                                           
108400     .                                                                    
