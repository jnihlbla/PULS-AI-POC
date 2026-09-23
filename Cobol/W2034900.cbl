000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2034900.                                                
000400 AUTHOR.         ÅSGÅRDEN STEFAN.                                         
000500 DATE-WRITTEN.   05/05/20.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        HANTERAR KUNDNR OCH DISTRNR GÄLLANDE TRANSFERS                   
001000*                                                                         
001100*        PROGRAMMET LÄSER      TABELL TP4TRAN                             
001200*        PROGRAMMET LÄSER      WDP7                                       
001300*        PROGRAMMET LÄSER      WDB2                                       
001400*                                                                         
001500******************************************************************        
001600*                                                                         
001700* OBS UPPDATERA COPYTEXTER                                                
001800*     (MED ALLA DISTRIKT SOM REGISTRERAS VIA DENNA BILD)                  
001900*                                                                         
002000*               WWDIST35 (OM DET ÄR EN EU-TRANSFER)                       
002100*               WWDIST57                                                  
002200*               WWDIST79 (OM DET ÄR EN EU-TRANSFER)                       
002300*               WWDC03                                                    
002400*                                                                         
002500******************************************************************        
002600*                                                                         
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSAKTION: W2T349                                              
003000*        MID:         W2I34901                                            
003100*                                                                         
003200*    UTDATA.                                                              
003300*        MOD:         W2O34901                                            
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700                                                                          
003800 DATA DIVISION.                                                           
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100 77  IDPGM                       PIC X(08)   VALUE 'W2034900'.            
004200                                                                          
004300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  IX                          PIC 9(9)    VALUE ZERO.                  
004900 77  RAD-IX                      PIC 9(9)    VALUE ZERO.                  
005000 77  RAD-MAX                     PIC 9(9)    VALUE 22.                    
005100 77  IX-SISTA-POST               PIC 9(9)    VALUE ZERO.                  
005200                                                                          
005300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005400                                                                          
005500                                                                          
005600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005700     88  INDATA-OK                           VALUE 'J'.                   
005800     88  INDATA-FEL                          VALUE 'N'.                   
005900                                                                          
006000 77  UPD-RAD-SW                  PIC X       VALUE 'N'.                   
006100     88  UPD-RAD-JA                          VALUE 'J'.                   
006200     88  UPD-RAD-NEJ                         VALUE 'N'.                   
006300                                                                          
006400 77  REG-NY-RAD-SW               PIC X       VALUE 'N'.                   
006500     88  REG-NY-RAD-JA                       VALUE 'J'.                   
006600     88  REG-NY-RAD-NEJ                      VALUE 'N'.                   
006700                                                                          
006800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006900     88  NYCKLAR-OK                          VALUE 'J'.                   
007000     88  NYCKLAR-FEL                         VALUE 'N'.                   
007100                                                                          
007200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007300     88  EGEN-MID                            VALUE '2349'.                
007400     88  GODK-MID                            VALUE '2341' '2342'          
007500                                                   '2343' '2344'          
007600                                                   '2345' '2346'          
007700                                                   '2347' '2348'          
007800                                                   '2349'.                
007900     88  HELP-MID                            VALUE '0551'.                
008000     EJECT                                                                
008100*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
008200 01  TABENTRY-PARM.                                                       
008300     03  STEGLANGD               PIC S9(9) COMP  VALUE 15.                
008400     03  ANTAL                   PIC S9(9) COMP.                          
008500     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 3.                 
008600                                                                          
008700                                                                          
008800 01  WS.                                                                  
008900                                                                          
009000*********************************************************                 
009100*    WS-MSGI-AREA ANVÄNDS FÖR ATT SPARA DET SOM LIGGER                    
009200*                 I MOD FÖR DE FÄLT DÄR SAMMA FÄLT ANVÄNDS                
009300*                 FÖR MID OCH MOD                                         
009400*    WS-MSGI-AREA SPARAS UNDAN PÅ MSGI-SPAR-AREA I                        
009500*                 NYCKELDATABASEN                                         
009600*********************************************************                 
009700     03 WS-MSGI-AREA.                                                     
009800       05 WS-MSGI-IDTRANS-2349   PIC X(4)    VALUE '2349'.                
009900       05 WS-TABELL.                                                      
010000        06 WS-TAB-POST  OCCURS 22.                                        
010100         07 WS-TAB-RAD.                                                   
010200          08 WS-IDDC-TAB         PIC X(2).                                
010300          08 WS-IDDISTR-TAB      PIC 9(4).                                
010400          08 WS-IDKUNDNR-TAB     PIC 9(6).                                
010500         07 WS-TAB-SORT.                                                  
010600           08 WS-IDDC-1          PIC X(1).                                
010700           08 WS-SORTFAELT       PIC X(1).                                
010800           08 WS-IDDC-2          PIC X(1).                                
010900     03 WS-IDDC-SEND             PIC X(2)    VALUE SPACE.                 
011000     03 WS-IDDC-REC              PIC X(2)    VALUE SPACE.                 
011100     03 WS-KDARBTYP              PIC X(8)    VALUE SPACE.                 
011200     03 WS-IDDISTR               PIC 9(4)    VALUE ZERO.                  
011300     03 WS-IDKUNDNR              PIC 9(6)    VALUE ZERO.                  
011400     03 WS-TEMFSFEL              PIC X(40)   VALUE SPACE.                 
011500     03  WS-TABELL-MAX           PIC 9(3)    VALUE 22.                    
011600                                                                          
011700     03 WS-SECTION               PIC X(24)   VALUE SPACE.                 
011800     03 FILLER                   PIC X(16)   VALUE                        
011900                                             'WS-DB2-SEKTION'.            
012000     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
012100                                                                          
012200 77  VAL-DC                      PIC X       VALUE ' '.                   
012300     88  VAL-DC-SEND                         VALUE 'S'.                   
012400     88  VAL-DC-REC                          VALUE 'R'.                   
012500                                                                          
012600     EJECT                                                                
012700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012800 01  GENERELLA-SUBPROGRAM.                                                
012900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013500     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
013600     EJECT                                                                
013700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013800*01 -COPY WMEDAREA                                                        
013900     SKIP3                                                                
014000 01  MESSAGE-CODES.                                                       
014100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014200     03  CONFLICT                PIC X(3)    VALUE '002'.                 
014300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014400     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
014500     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
014600     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014700     03  TOM-RAD                 PIC X(3)    VALUE '080'.                 
014800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
015000     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '115'.                 
015100     03  INF-PRINT-BEGAERD       PIC X(3)    VALUE '118'.                 
015200     03  INF-PRINT-START         PIC X(3)    VALUE '202'.                 
015300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015400     03  AREA-MISSING            PIC X(3)    VALUE '705'.                 
015500     03  ERR-FEL-PRINTER         PIC X(3)    VALUE '772'.                 
015600     03  UPDATING-NOT-ALLOWED    PIC X(3)    VALUE '777'.                 
015700 01  FELTEXTER.                                                           
015800     03  MED-1                  PIC X(40)                                 
015900         VALUE 'DISTRICT/CUSTOMER DOES NOT EXIST     '.                   
016000     03  MED-2                  PIC X(40)                                 
016100         VALUE 'DISTRICT/CUSTOMER NOT VALID          '.                   
016200     03  MED-3                  PIC X(40)                                 
016300         VALUE 'ROW ALREADY EXIST                    '.                   
016400     03  MED-4                  PIC X(40)                                 
016500         VALUE 'PART DO NOT EXIST IN PULS             '.                  
016600     03  MED-5                  PIC X(40)                                 
016700         VALUE 'PRODUCT GROUP = 18                   '.                   
016800     03  MED-6                  PIC X(40)                                 
016900         VALUE 'DISTR EXIST (FROM DC)              '.                     
017000     03  MED-7                  PIC X(40)                                 
017100         VALUE 'DISTR IS NOT UNIQUE (TO DC)     '.                        
017200                                                                          
017300     EJECT                                                                
017400*01  -COPY WDATAREA                                                       
017500     EJECT                                                                
017600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
017700*                                                                         
017800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
017900     SKIP3                                                                
018000*01 -COPY WMSGINIT                                                        
018100     EJECT                                                                
018200*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
018300*                                                                         
018400 01  SPAR-AREA.                                                           
018500     03  SPAR-IDTRANS           PIC X(4)    VALUE '2349'.                 
018600     EJECT                                                                
018700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
018800*                                                                         
018900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019000     SKIP3                                                                
019100*01  MID -COPY W2I34901                                                   
019200     EJECT                                                                
019300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019400     SKIP3                                                                
019500*01  -COPY WMSGAREA                                                       
019600     EJECT                                                                
019700     03  MOD REDEFINES MSG-AREA.                                          
019800*      05  -COPY W2O34901                                                 
019900     EJECT                                                                
020000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020100     SKIP3                                                                
020200*01  -COPY WMFSAREA                                                       
020300     EJECT                                                                
020400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020500*                                                                         
020600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020700     SKIP3                                                                
020800 01  NYCKLAR-TILL-DLI.                                                    
020900     03  W-IDARTNR-X.                                                     
021000         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
021100                                                                          
021200     03  W-IDSKYLT-X.                                                     
021300         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
021400                                                                          
021500     03  W-KDSEGKEY-X.                                                    
021600         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
021700                                                                          
021800     03  W-IDGMT-X.                                                       
021900         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
022000         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
022100                                                                          
022200     SKIP2                                                                
022300*    --- STATUS-KOD FRÅN IMS                                              
022400 01  STATUS-WS                   PIC XX.                                  
022500     88  SEGMENT-FINNS                       VALUE '  '.                  
022600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022800     SKIP2                                                                
022900 01  GODK-STATUSKODER.                                                    
023000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023100     SKIP3                                                                
023200 01  SSA1                        PIC X(64).                               
023300 01  SSA2                        PIC X(64).                               
023400     EJECT                                                                
023500*    --- IMS FUNKTIONSKODER                                               
023600*01  -COPY W0003                                                          
023700     EJECT                                                                
023800 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
023900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
024000                                                                          
024100 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
024200 01  DB2-WS.                                                              
024300     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
024400         88  CURSOR-OK                       VALUE 000.                   
024500         88  RADER-FINNS                     VALUE 000.                   
024600         88  RADER-SAKNAS                    VALUE 100.                   
024700         88  ATKOMST-FEL                     VALUE 904.                   
024800     03  GODK-SQLCODEKODER.                                               
024900         05  GODK-SQLCODE OCCURS 5                                        
025000             INDEXED BY SQLCODE-IX PIC 9(3).                              
025100 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
025200     EJECT                                                                
025300*    ---  DLI INPUT-OUTPUT AREA                                           
025400                                                                          
025500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
025600 01  DLI-IO-WDB201.                                                       
025700*    03  -COPY WDB201                                                     
025800     EJECT                                                                
025900 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
026000                                                                          
026100*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
026200     EJECT                                                                
026300     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
026400     EJECT                                                                
026500 LINKAGE SECTION.                                                         
026600*01  -COPY W0009   -PRE MSG-                                              
026700*01  -COPY W0008   -PRE WDP7-                                             
026800     05  FILLER                  PIC X.                                   
026900                                                                          
027000*01  -COPY W0008  -PRE WDB2-                                              
027100     05  FILLER                  PIC X.                                   
027200     EJECT                                                                
027300 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB2-PCB.                     
027400 MAIN SECTION.                                                            
027500     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB2-PCB.                     
027600                                                                          
027700     PERFORM IMS-GET-MSG                                                  
027800     IF SEGMENT-FINNS                                                     
027900       PERFORM A-INIT                                                     
028000       PERFORM B-KOLLA-NYCKLAR                                            
028100       IF NYCKLAR-OK                                                      
028200         IF MFS-UPDATE                                                    
028300           PERFORM G-KOLLA-INPUT                                          
028400           IF INDATA-OK                                                   
028500             PERFORM H-UPPDATERA                                          
028600           END-IF                                                         
028700         ELSE                                                             
028800           PERFORM MFS-RENSA-FAELT-IN                                     
028900         END-IF                                                           
029000         IF INDATA-OK                                                     
029100           PERFORM F-LAES-VISA-INFO                                       
029200         END-IF                                                           
029300                                                                          
029400         MOVE '002'             TO MSGI-KDCALL                            
029500         MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                      
029600         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
029700         MOVE '2349'            TO MSGI-IDTRANS                           
029800         MOVE WS-MSGI-AREA      TO MSGI-SPAR-AREA                         
029900         CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                       
030000       END-IF                                                             
030300       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O34901 + 4                      
030400       PERFORM IMS-INSERT-MSG                                             
030500     END-IF                                                               
030600                                                                          
030700     MOVE ZERO TO RETURN-CODE                                             
030800     GOBACK                                                               
030900     .                                                                    
031000     EJECT                                                                
031100 A-INIT SECTION.                                                          
031200                                                                          
031300     IF MSG-DUBBLA-TRANSKODER                                             
031400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I34901                 
031500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
031600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
031700     ELSE                                                                 
031800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I34901                  
031900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
032000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
032100     END-IF                                                               
032200                                                                          
032300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
032400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
032500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
032600                                                                          
032700     MOVE LOW-VALUE TO MSG-AREA                                           
032800     MOVE 'W2O349N1' TO MFS-IDMOD                                         
032900     MOVE '2349' TO MOD-IDTRANS                                           
033000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
033100                                                                          
033200     IF EGEN-MID OR HELP-MID                                              
033300       CONTINUE                                                           
033400     ELSE                                                                 
033500       MOVE SPACE TO MFS-KDTRTYP                                          
033600       MOVE '7' TO MFS-IDPFK                                              
033700     END-IF                                                               
033800     MOVE 'GB '              TO MED-IDSKYLT                               
033900                                                                          
034000     INITIALIZE GODK-SQLCODEKODER                                         
034100     .                                                                    
034200     EJECT                                                                
034300 B-KOLLA-NYCKLAR SECTION.                                                 
034400                                                                          
034500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
034600     MOVE '001'             TO MSGI-KDCALL                                
034700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
034800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
034900     MOVE '2349'            TO MSGI-IDTRANS                               
035000     IF EGEN-MID                                                          
035100         MOVE MID-IDDC-SEND-IN                                            
035200                            TO MSGI-IDDC-SEND                             
035300         MOVE MID-IDDC-REC-IN                                             
035400                            TO MSGI-IDDC-REC                              
035500         MOVE MID-KDARBTYP-IN                                             
035600                            TO MSGI-KDARBTYP                              
035700     END-IF                                                               
035800     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
035900     IF MSGI-SPAR-AREA (1:4) = '2349'                                     
036000       MOVE MSGI-SPAR-AREA  TO WS-MSGI-AREA                               
036100     END-IF                                                               
036200                                                                          
036300*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
036400     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
036500                                                                          
036600     MOVE JA TO NYCKLAR-SW                                                
036700                                                                          
036800                                                                          
036900*    --                                                                   
037000     MOVE MFS-RENSA-FAELT TO MOD-IDDC-SEND-IN                             
037100                                                                          
037200     IF MID-IDDC-SEND-IN NOT = ALL '+'                                    
037300       MOVE '7'         TO MFS-IDPFK                                      
037400       MOVE SPACE       TO MFS-KDTRTYP                                    
037500     END-IF                                                               
037600     MOVE MSGI-IDDC-SEND                                                  
037700                        TO WS-IDDC-SEND                                   
037800                           MOD-IDDC-SEND-UT                               
037900                                                                          
038000*    --                                                                   
038100     MOVE MFS-RENSA-FAELT TO MOD-IDDC-REC-IN                              
038200                                                                          
038300     IF MID-IDDC-REC-IN NOT = ALL '+'                                     
038400       MOVE '7'         TO MFS-IDPFK                                      
038500       MOVE SPACE       TO MFS-KDTRTYP                                    
038600     END-IF                                                               
038700     MOVE MSGI-IDDC-REC                                                   
038800                        TO WS-IDDC-REC                                    
038900                           MOD-IDDC-REC-UT                                
039000                                                                          
039100     MOVE SPACE         TO VAL-DC                                         
039200     IF  (WS-IDDC-SEND NOT = SPACE                                        
039300     AND WS-IDDC-REC  NOT = SPACE)                                        
039500     OR WS-IDDC-REC  = '3H'                                               
039600     OR WS-IDDC-SEND = '3J'                                               
039700     OR WS-IDDC-REC  = '3J'                                               
039710     OR WS-IDDC-SEND = '67'                                               
039720     OR WS-IDDC-REC  = '67'                                               
039730     OR WS-IDDC-SEND = '65'                                               
039740     OR WS-IDDC-REC  = '65'                                               
039800       MOVE NEJ              TO NYCKLAR-SW                                
039900     ELSE                                                                 
040000       IF WS-IDDC-SEND NOT = SPACE                                        
040100          MOVE 'S'      TO VAL-DC                                         
040200          MOVE '  TODC' TO MOD-DCRUBR1                                    
040300                           MOD-DCRUBR2                                    
040400       END-IF                                                             
040500       IF WS-IDDC-REC  NOT = SPACE                                        
040600          MOVE 'R'      TO VAL-DC                                         
040700          MOVE 'FROMDC' TO MOD-DCRUBR1                                    
040800                           MOD-DCRUBR2                                    
040900       END-IF                                                             
041000     END-IF                                                               
041100                                                                          
041200                                                                          
041300*    --                                                                   
041400     MOVE MFS-RENSA-FAELT TO MOD-KDARBTYP-IN                              
041500                                                                          
041600     IF MID-KDARBTYP-IN NOT = ALL '+'                                     
041700       MOVE '7'         TO MFS-IDPFK                                      
041800       MOVE SPACE       TO MFS-KDTRTYP                                    
041900     END-IF                                                               
042000     MOVE MSGI-KDARBTYP TO WS-KDARBTYP                                    
042100                           MOD-KDARBTYP-UT                                
042200     IF WS-KDARBTYP = 'QUAL'                                              
042300     OR WS-KDARBTYP = 'ESC '                                              
042400       CONTINUE                                                           
042500     ELSE                                                                 
042600       MOVE NEJ              TO NYCKLAR-SW                                
042700     END-IF                                                               
042800                                                                          
042900     IF NYCKLAR-FEL                                                       
043000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
043100       CALL WMEDKONV USING MED-WMEDAREA                                   
043200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
043300       PERFORM MFS-RENSA-FAELT-IN                                         
043400       PERFORM MFS-RENSA-FAELT-UT                                         
043500     END-IF                                                               
043600     .                                                                    
043700     EJECT                                                                
043800 F-LAES-VISA-INFO SECTION.                                                
043900                                                                          
044000     MOVE ZERO               TO IX-SISTA-POST                             
044100     IF VAL-DC-SEND                                                       
044200       PERFORM FA-DC-SEND                                                 
044300     ELSE                                                                 
044400       PERFORM FB-DC-REC                                                  
044500     END-IF                                                               
044600     .                                                                    
044700     EJECT                                                                
044800 FA-DC-SEND SECTION.                                                      
044900                                                                          
045000     PERFORM DB2-DCL-OPN-CRS-TP4TRAN-SEND                                 
045100                                                                          
045200     IF SQLCODE > ZERO                                                    
045300       MOVE INF-URVAL-SAKNAS                                              
045400                             TO MED-IDMFSFEL                              
045500       CALL WMEDKONV USING MED-WMEDAREA                                   
045600       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
045700                                                                          
045800     ELSE                                                                 
045900                                                                          
046000       PERFORM DB2-FETCH-TP4TRAN-SEND                                     
046100       IF SQLCODE > ZERO                                                  
046200         MOVE INF-URVAL-SAKNAS                                            
046300                           TO MED-IDMFSFEL                                
046400         CALL WMEDKONV USING MED-WMEDAREA                                 
046500         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
046600                                                                          
046700       ELSE                                                               
046800         MOVE 1              TO RAD-IX                                    
046900         PERFORM UNTIL SQLCODE > ZERO                                     
047000         OR RAD-IX > RAD-MAX                                              
047100            MOVE TP4TRAN-IDDC-REC                                         
047200                             TO WS-IDDC-TAB     (RAD-IX)                  
047300            MOVE WS-IDDC-TAB (RAD-IX) (1:1)                               
047400                             TO WS-IDDC-1       (RAD-IX)                  
047500            MOVE WS-IDDC-TAB (RAD-IX) (2:1)                               
047600                             TO WS-IDDC-2       (RAD-IX)                  
047700            IF WS-IDDC-2 (RAD-IX) NUMERIC                                 
047800              MOVE 'A'       TO WS-SORTFAELT    (RAD-IX)                  
047900            ELSE                                                          
048000              MOVE 'B'       TO WS-SORTFAELT    (RAD-IX)                  
048100            END-IF                                                        
048200            MOVE TP4TRAN-IDDISTR                                          
048300                             TO WS-IDDISTR-TAB  (RAD-IX)                  
048400            MOVE TP4TRAN-IDKUNDNR                                         
048500                             TO WS-IDKUNDNR-TAB (RAD-IX)                  
048600            MOVE RAD-IX      TO IX-SISTA-POST                             
048700            ADD 1            TO RAD-IX                                    
048800                                                                          
048900            PERFORM DB2-FETCH-TP4TRAN-SEND                                
049000         END-PERFORM                                                      
049100                                                                          
049200         PERFORM FC-SORTERA-PLATSER                                       
049300                                                                          
049400         MOVE 1              TO RAD-IX                                    
049500         PERFORM UNTIL RAD-IX > RAD-MAX                                   
049600         OR RAD-IX > IX-SISTA-POST                                        
049700            MOVE WS-IDDC-TAB     (RAD-IX)                                 
049800                             TO MOD-IDDC     (RAD-IX)                     
049900            MOVE WS-IDDISTR-TAB  (RAD-IX)                                 
050000                             TO MOD-IDDISTR  (RAD-IX)                     
050100            MOVE WS-IDKUNDNR-TAB (RAD-IX)                                 
050200                             TO MOD-IDKUNDNR (RAD-IX)                     
050300            ADD 1            TO RAD-IX                                    
050400                                                                          
050500         END-PERFORM                                                      
050600                                                                          
050700         PERFORM UNTIL RAD-IX > RAD-MAX                                   
050800           MOVE MFS-STAENG-FAELT                                          
050900                             TO MOD-CMD-ATTR      (RAD-IX)                
051000           MOVE MFS-RENSA-FAELT                                           
051100                             TO MOD-CMD           (RAD-IX)                
051200                                MOD-IDDC        (RAD-IX)                  
051300                                MOD-IDDISTR       (RAD-IX)                
051400                                MOD-IDKUNDNR      (RAD-IX)                
051500                                                                          
051600           ADD 1             TO RAD-IX                                    
051700         END-PERFORM                                                      
051800       END-IF                                                             
051900                                                                          
052000     END-IF                                                               
052100                                                                          
052200     PERFORM DB2-CLOSE-TP4TRAN-SEND-CRS                                   
052300     .                                                                    
052400     EJECT                                                                
052500 FB-DC-REC SECTION.                                                       
052600                                                                          
052700     PERFORM DB2-DCL-OPN-CRS-TP4TRAN-REC                                  
052800                                                                          
052900     IF SQLCODE > ZERO                                                    
053000       MOVE INF-URVAL-SAKNAS                                              
053100                             TO MED-IDMFSFEL                              
053200       CALL WMEDKONV USING MED-WMEDAREA                                   
053300       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
053400                                                                          
053500     ELSE                                                                 
053600                                                                          
053700       PERFORM DB2-FETCH-TP4TRAN-REC                                      
053800       IF SQLCODE > ZERO                                                  
053900         MOVE INF-URVAL-SAKNAS                                            
054000                           TO MED-IDMFSFEL                                
054100         CALL WMEDKONV USING MED-WMEDAREA                                 
054200         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
054300                                                                          
054400       ELSE                                                               
054500                                                                          
054600         MOVE 1              TO RAD-IX                                    
054700         PERFORM UNTIL SQLCODE > ZERO                                     
054800         OR RAD-IX > RAD-MAX                                              
054900            MOVE TP4TRAN-IDDC-SEND                                        
055000                             TO WS-IDDC-TAB     (RAD-IX)                  
055100            MOVE WS-IDDC-TAB (RAD-IX) (1:1)                               
055200                             TO WS-IDDC-1       (RAD-IX)                  
055300            MOVE WS-IDDC-TAB (RAD-IX) (2:1)                               
055400                             TO WS-IDDC-2       (RAD-IX)                  
055500            IF WS-IDDC-2 (RAD-IX) NUMERIC                                 
055600              MOVE 'A'       TO WS-SORTFAELT    (RAD-IX)                  
055700            ELSE                                                          
055800              MOVE 'B'       TO WS-SORTFAELT    (RAD-IX)                  
055900            END-IF                                                        
056000            MOVE TP4TRAN-IDDISTR                                          
056100                             TO WS-IDDISTR-TAB  (RAD-IX)                  
056200            MOVE TP4TRAN-IDKUNDNR                                         
056300                             TO WS-IDKUNDNR-TAB (RAD-IX)                  
056400            MOVE RAD-IX      TO IX-SISTA-POST                             
056500            ADD 1            TO RAD-IX                                    
056600                                                                          
056700            PERFORM DB2-FETCH-TP4TRAN-REC                                 
056800         END-PERFORM                                                      
056900                                                                          
057000         PERFORM FC-SORTERA-PLATSER                                       
057100                                                                          
057200         MOVE 1              TO RAD-IX                                    
057300         PERFORM UNTIL RAD-IX > RAD-MAX                                   
057400         OR RAD-IX > IX-SISTA-POST                                        
057500            MOVE WS-IDDC-TAB     (RAD-IX)                                 
057600                             TO MOD-IDDC     (RAD-IX)                     
057700            MOVE WS-IDDISTR-TAB  (RAD-IX)                                 
057800                             TO MOD-IDDISTR  (RAD-IX)                     
057900            MOVE WS-IDKUNDNR-TAB (RAD-IX)                                 
058000                             TO MOD-IDKUNDNR (RAD-IX)                     
058100            ADD 1            TO RAD-IX                                    
058200                                                                          
058300         END-PERFORM                                                      
058400         PERFORM UNTIL RAD-IX > RAD-MAX                                   
058500           MOVE MFS-STAENG-FAELT                                          
058600                             TO MOD-CMD-ATTR      (RAD-IX)                
058700           MOVE MFS-RENSA-FAELT                                           
058800                             TO MOD-CMD           (RAD-IX)                
058900                                MOD-IDDC        (RAD-IX)                  
059000                                MOD-IDDISTR       (RAD-IX)                
059100                                MOD-IDKUNDNR      (RAD-IX)                
059200                                                                          
059300           ADD 1             TO RAD-IX                                    
059400         END-PERFORM                                                      
059500       END-IF                                                             
059600                                                                          
059700     END-IF                                                               
059800                                                                          
059900     PERFORM DB2-CLOSE-TP4TRAN-REC-CRS                                    
060000     .                                                                    
060100     EJECT                                                                
060200 FC-SORTERA-PLATSER SECTION.                                              
060300                                                                          
060400     MOVE IX-SISTA-POST      TO ANTAL                                     
060500                                                                          
060600     CALL WINTSOR USING WS-TABELL STEGLANGD ANTAL                         
060700                  WS-TAB-SORT (1) NYCKELLANGD                             
060800     .                                                                    
060900     EJECT                                                                
061000 G-KOLLA-INPUT SECTION.                                                   
061100     MOVE 'G-KOLLA-INPUT      ' TO WS-SECTION                             
061200                                                                          
061300     MOVE JA                 TO INDATA-SW                                 
061400     MOVE NEJ                TO UPD-RAD-SW                                
061500                                REG-NY-RAD-SW                             
061600                                                                          
061700     IF  MID-INPUT      = ALL '+'                                         
061800        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
061900        MOVE NEJ TO INDATA-SW                                             
062000     ELSE                                                                 
062100                                                                          
062200       MOVE +1               TO RAD-IX                                    
062300       PERFORM UNTIL RAD-IX > RAD-MAX                                     
062400                                                                          
062500         IF MID-CMD             (RAD-IX) NOT = ALL '+'                    
062600           MOVE JA           TO UPD-RAD-SW                                
062700                                                                          
062800           IF MID-CMD (RAD-IX) = 'D'                                      
062900                                                                          
063000             MOVE JA         TO UPD-RAD-SW                                
063100             MOVE MFS-ALFA-FAELT-RAETT                                    
063200                        TO MOD-CMD-ATTR(RAD-IX)                           
063300           ELSE                                                           
063400               MOVE MFS-ALFA-FAELT-FEL                                    
063500                        TO MOD-CMD-ATTR (RAD-IX)                          
063600               MOVE ERR-CORR-HILITE-FLDS                                  
063700                        TO MED-IDMFSFEL                                   
063800               MOVE NEJ TO INDATA-SW                                      
063900           END-IF                                                         
064000         END-IF                                                           
064100                                                                          
064200         ADD 1 TO RAD-IX                                                  
064300       END-PERFORM                                                        
064400                                                                          
064500       IF  MID-NY-IDDC         = ALL '+'                                  
064600       AND MID-NY-IDDISTR      = ALL '+'                                  
064700       AND MID-NY-IDKUNDNR     = ALL '+'                                  
064800           IF UPD-RAD-NEJ                                                 
064900              MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                   
065000              MOVE NEJ         TO INDATA-SW                               
065100           END-IF                                                         
065200       ELSE                                                               
065300         IF MID-NY-IDDC = '11'                                            
065400           IF WS-IDDC-REC = 21                                            
065500             CONTINUE                                                     
065600           ELSE                                                           
065700             MOVE MFS-ALFA-FAELT-FEL                                      
065800                               TO MOD-NY-IDDC-ATTR                        
065900             MOVE MFS-NUM-FAELT-FEL                                       
066000                               TO MOD-NY-IDDISTR-ATTR                     
066100                                  MOD-NY-IDKUNDNR-ATTR                    
066200             MOVE ERR-WRONG-KEY                                           
066300                               TO MED-IDMFSFEL                            
066400             MOVE NEJ TO INDATA-SW                                        
066500           END-IF                                                         
066600         END-IF                                                           
066700                                                                          
066800         IF MID-NY-IDDC = '3H'                                            
066810           IF MID-IDDC-REC-UT = '21'                                      
066820             CONTINUE                                                     
066830           ELSE                                                           
066900             MOVE MFS-ALFA-FAELT-FEL                                      
067000                             TO MOD-NY-IDDC-ATTR                          
067100             MOVE MFS-NUM-FAELT-FEL                                       
067200                             TO MOD-NY-IDDISTR-ATTR                       
067300                                MOD-NY-IDKUNDNR-ATTR                      
067400             MOVE ERR-WRONG-KEY                                           
067500                             TO MED-IDMFSFEL                              
067600             MOVE NEJ TO INDATA-SW                                        
067610           END-IF                                                         
067700         END-IF                                                           
067800                                                                          
067900         MOVE JA           TO REG-NY-RAD-SW                               
068000                                                                          
068100         IF MID-NY-IDDC = ALL '+'                                         
068200           MOVE MFS-ALFA-FAELT-FEL                                        
068300                           TO MOD-NY-IDDC-ATTR                            
068400           MOVE ERR-CORR-HILITE-FLDS                                      
068500                           TO MED-IDMFSFEL                                
068600           MOVE NEJ        TO INDATA-SW                                   
068700         END-IF                                                           
068800                                                                          
068900         INSPECT MID-NY-IDDISTR                                           
069000                           REPLACING LEADING SPACE BY ZERO                
069100         IF MID-NY-IDDISTR NUMERIC                                        
069200           MOVE MFS-NUM-FAELT-RAETT                                       
069300                           TO MOD-NY-IDDISTR-ATTR                         
069400         ELSE                                                             
069500           MOVE MFS-NUM-FAELT-FEL                                         
069600                           TO MOD-NY-IDDISTR-ATTR                         
069700           MOVE ERR-CORR-HILITE-FLDS                                      
069800                           TO MED-IDMFSFEL                                
069900           MOVE NEJ        TO INDATA-SW                                   
070000         END-IF                                                           
070100                                                                          
070200         INSPECT MID-NY-IDKUNDNR                                          
070300                           REPLACING LEADING SPACE BY ZERO                
070400         IF MID-NY-IDKUNDNR NUMERIC                                       
070500           MOVE MFS-NUM-FAELT-RAETT                                       
070600                           TO MOD-NY-IDKUNDNR-ATTR                        
070700         ELSE                                                             
070800           MOVE MFS-NUM-FAELT-FEL                                         
070900                           TO MOD-NY-IDKUNDNR-ATTR                        
071000           MOVE ERR-CORR-HILITE-FLDS                                      
071100                           TO MED-IDMFSFEL                                
071200           MOVE NEJ        TO INDATA-SW                                   
071300         END-IF                                                           
071400                                                                          
071500         IF INDATA-OK                                                     
071600******************************************************************        
071700*   KONTROLLERA MOT KUNDREGISTRET                                         
071800******************************************************************        
071900           MOVE MID-NY-IDDISTR                                            
072000                             TO WS-IDDISTR                                
072100           MOVE WS-IDDISTR TO W-IDDISTR                                   
072200           MOVE MID-NY-IDKUNDNR                                           
072300                             TO WS-IDKUNDNR                               
072400           MOVE WS-IDKUNDNR TO W-IDKUNDNR                                 
072500           PERFORM IMS-GU-B201                                            
072600           IF SEGMENT-SAKNAS                                              
072700              MOVE MED-1   TO WS-TEMFSFEL                                 
072800              MOVE NEJ     TO INDATA-SW                                   
072900           ELSE                                                           
073000              IF GMT-TISTADAT > ZERO                                      
073100              AND GMT-TISTODAT = ZERO                                     
073200                CONTINUE                                                  
073300              ELSE                                                        
073400                MOVE MED-2 TO WS-TEMFSFEL                                 
073500                MOVE NEJ   TO INDATA-SW                                   
073600              END-IF                                                      
073700           END-IF                                                         
073800                                                                          
073900           IF VAL-DC-SEND                                                 
074000             MOVE MID-NY-IDDC                                             
074100                           TO WS-IDDC-REC                                 
074200           ELSE                                                           
074300             MOVE MID-NY-IDDC                                             
074400                           TO WS-IDDC-SEND                                
074500           END-IF                                                         
074600                                                                          
074700           PERFORM DB2-SELECT-TP4TRAN                                     
074800           IF RADER-SAKNAS                                                
074900                                                                          
075000******************************************************************        
075100*   KONTROLLERA VID NYUPPLÄGG ATT                                         
075200*   DISTR INTE FINNS SOM SÄNDANDE DC                                      
075300******************************************************************        
075400             PERFORM DB2-SELECT-TP4TRAN-2                                 
075500             IF RADER-FINNS                                               
075600               MOVE MFS-ALFA-FAELT-FEL                                    
075700                           TO MOD-NY-IDDC-ATTR                            
075800               MOVE MFS-NUM-FAELT-FEL                                     
075900                           TO MOD-NY-IDDISTR-ATTR                         
076000                              MOD-NY-IDKUNDNR-ATTR                        
076100               MOVE ERR-CORR-HILITE-FLDS                                  
076200                           TO MED-IDMFSFEL                                
076300               MOVE MED-6  TO WS-TEMFSFEL                                 
076400               MOVE NEJ    TO INDATA-SW                                   
076500             ELSE                                                         
076600                                                                          
076700******************************************************************        
076800*   KONTROLLERA VID NYUPPLÄGG ATT                                         
076900*   'DISTR/MOTTAGANDE DC' ÄR UNIK                                         
077000******************************************************************        
077100               PERFORM DB2-SELECT-TP4TRAN-3                               
077200               IF RADER-FINNS                                             
077300                 IF WS-IDDC-REC NOT = TP4TRAN-IDDC-REC                    
077400                   MOVE MFS-ALFA-FAELT-FEL                                
077500                             TO MOD-NY-IDDC-ATTR                          
077600                   MOVE MFS-NUM-FAELT-FEL                                 
077700                             TO MOD-NY-IDDISTR-ATTR                       
077800                                MOD-NY-IDKUNDNR-ATTR                      
077900                   MOVE ERR-CORR-HILITE-FLDS                              
078000                             TO MED-IDMFSFEL                              
078100                   MOVE MED-7 TO WS-TEMFSFEL                              
078200                   MOVE NEJ TO INDATA-SW                                  
078300                 END-IF                                                   
078400               END-IF                                                     
078410             END-IF                                                       
078500           END-IF                                                         
078600         END-IF                                                           
078700       END-IF                                                             
078800     END-IF                                                               
078900                                                                          
079000     IF INDATA-FEL                                                        
079100        IF WS-TEMFSFEL = SPACE                                            
079200          CALL WMEDKONV USING MED-WMEDAREA                                
079300          MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                               
079400        ELSE                                                              
079500          MOVE WS-TEMFSFEL  TO MOD-TEMFSFEL                               
079600        END-IF                                                            
079700        PERFORM MFS-ROER-EJ-FAELT-UT                                      
079800        PERFORM MFS-ROER-EJ-FAELT-IN                                      
081600     END-IF                                                               
081700     .                                                                    
081800     EJECT                                                                
081900                                                                          
082000 H-UPPDATERA SECTION.                                                     
082100     MOVE 'H-UPPDATERA'      TO WS-SECTION                                
082200                                                                          
082300     IF UPD-RAD-JA                                                        
082400                                                                          
082500       MOVE +1 TO IX                                                      
082600       PERFORM UNTIL IX > RAD-MAX                                         
082700          IF MID-CMD            (IX) NOT = ALL '+'                        
082800             PERFORM HA-UPD-RAD                                           
082900          END-IF                                                          
083000          ADD 1 TO IX                                                     
083100       END-PERFORM                                                        
083200     END-IF                                                               
083300                                                                          
083400     IF REG-NY-RAD-JA                                                     
083500       PERFORM HB-NY-RAD                                                  
083600     END-IF                                                               
083700                                                                          
083800     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
083900     CALL WMEDKONV USING MED-WMEDAREA                                     
084000     MOVE MED-TEMFSINF TO MOD-TEMFSINF                                    
084100     PERFORM MFS-RENSA-FAELT-IN                                           
084200     .                                                                    
084300     EJECT                                                                
084400 HA-UPD-RAD SECTION.                                                      
084500     MOVE 'HA-UPD-RAD'       TO WS-SECTION                                
084600                                                                          
084700     IF MID-CMD (IX) = 'D'                                                
084800                                                                          
084900       IF VAL-DC-SEND                                                     
085000         MOVE WS-IDDC-TAB (IX)                                            
085100                             TO WS-IDDC-REC                               
085200       ELSE                                                               
085300         MOVE WS-IDDC-TAB (IX)                                            
085400                             TO WS-IDDC-SEND                              
085500       END-IF                                                             
085600                                                                          
085700       PERFORM DB2-DELETE-TP4TRAN                                         
085800                                                                          
085900     END-IF                                                               
086000     .                                                                    
086100     EJECT                                                                
086200 HB-NY-RAD SECTION.                                                       
086300     MOVE 'HB-NY-RAD'        TO WS-SECTION                                
086400     IF VAL-DC-SEND                                                       
086500       MOVE MID-NY-IDDC      TO WS-IDDC-REC                               
086600     ELSE                                                                 
086700       MOVE MID-NY-IDDC      TO WS-IDDC-SEND                              
086800     END-IF                                                               
086900     PERFORM DB2-SELECT-TP4TRAN                                           
087000     IF RADER-FINNS                                                       
087100       MOVE MID-NY-IDDISTR   TO TP4TRAN-IDDISTR                           
087200       MOVE MID-NY-IDKUNDNR  TO TP4TRAN-IDKUNDNR                          
087300       PERFORM DB2-UPDATE-TP4TRAN                                         
087400     ELSE                                                                 
087500       MOVE WS-KDARBTYP      TO TP4TRAN-KDARBTYP                          
087600       MOVE WS-IDDC-SEND     TO TP4TRAN-IDDC-SEND                         
087700       MOVE WS-IDDC-REC      TO TP4TRAN-IDDC-REC                          
087800       MOVE MID-NY-IDDISTR   TO TP4TRAN-IDDISTR                           
087900       MOVE MID-NY-IDKUNDNR  TO TP4TRAN-IDKUNDNR                          
088000       PERFORM DB2-INSERT-TP4TRAN                                         
088100     END-IF                                                               
088200     .                                                                    
088300     EJECT                                                                
088400 MFS-RENSA-FAELT-UT SECTION.                                              
088500                                                                          
088600*    --- ALLA UTDATA-FÄLT                                                 
088700     MOVE +1 TO IX                                                        
088800     PERFORM UNTIL IX > RAD-MAX                                           
088900       MOVE MFS-RENSA-FAELT  TO MOD-IDDC      (IX)                        
089000                                MOD-IDDISTR   (IX)                        
089100                                MOD-IDKUNDNR  (IX)                        
089200       ADD +1 TO IX                                                       
089300     END-PERFORM                                                          
089400     MOVE MFS-RENSA-FAELT    TO MOD-TEMFSINF                              
089500     .                                                                    
089600     SKIP3                                                                
089700 MFS-RENSA-FAELT-IN SECTION.                                              
089800                                                                          
089900*    --- ALLA INDATA-FÄLT                                                 
090000     MOVE +1 TO IX                                                        
090100     PERFORM UNTIL IX > RAD-MAX                                           
090200       MOVE MFS-RENSA-FAELT  TO MOD-CMD (IX)                              
090300       ADD +1 TO IX                                                       
090400     END-PERFORM                                                          
090500     MOVE MFS-RENSA-FAELT    TO MOD-NY-IDDC                               
090600                                MOD-NY-IDDISTR                            
090700                                MOD-NY-IDKUNDNR                           
090800     .                                                                    
090900     EJECT                                                                
091000 MFS-ROER-EJ-FAELT-UT SECTION.                                            
091100                                                                          
091200*    --- ALLA UTDATA-FÄLT                                                 
091300     MOVE +1 TO IX                                                        
091400     PERFORM UNTIL IX > RAD-MAX                                           
091500       MOVE MFS-ROER-EJ-FAELT                                             
091600                             TO MOD-IDDC     (IX)                         
091700                                MOD-IDDISTR   (IX)                        
091800                                MOD-IDKUNDNR  (IX)                        
091900       ADD +1 TO IX                                                       
092000     END-PERFORM                                                          
092100     MOVE MFS-ROER-EJ-FAELT  TO MOD-TEMFSINF                              
092200     .                                                                    
092300     SKIP3                                                                
092400 MFS-ROER-EJ-FAELT-IN SECTION.                                            
092500                                                                          
092600*    --- ALLA INDATA-FÄLT                                                 
092700     MOVE +1 TO IX                                                        
092800     PERFORM UNTIL IX > RAD-MAX                                           
092900       MOVE MFS-ROER-EJ-FAELT TO MOD-CMD (IX)                             
093000       ADD +1 TO IX                                                       
093100     END-PERFORM                                                          
093200     MOVE MFS-ROER-EJ-FAELT  TO MOD-NY-IDDC                               
093300                                MOD-NY-IDDISTR                            
093400                                MOD-NY-IDKUNDNR                           
093500     .                                                                    
093600     EJECT                                                                
093700     SKIP3                                                                
093800 MFS-LAES-IN-IGEN SECTION.                                                
093900                                                                          
094000*    --- ALLA INDATA-FÄLT                                                 
094100     MOVE +1 TO IX                                                        
094200     PERFORM UNTIL IX > RAD-MAX                                           
094300       MOVE MFS-ADD-LAES-IN-FAELT                                         
094400                             TO MOD-CMD (IX)                              
094500       ADD +1 TO IX                                                       
094600     END-PERFORM                                                          
094700     MOVE MFS-ADD-LAES-IN-FAELT                                           
094800                             TO MOD-NY-IDDC                               
094900                                MOD-NY-IDDISTR                            
095000                                MOD-NY-IDKUNDNR                           
095100     .                                                                    
095200     EJECT                                                                
095300* --- IMS SEKTIONER ---                                                   
095400     SKIP3                                                                
095500 IMS-GET-MSG SECTION.                                                     
095600                                                                          
095700     MOVE '  QC' TO GODK-STATUSKODER                                      
095800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
095900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
096000     PERFORM IMS-STATUSKONTROLL                                           
096100     .                                                                    
096200     SKIP3                                                                
096300 IMS-INSERT-MSG SECTION.                                                  
096400                                                                          
096500     MOVE 'N'              TO MFS-KDHUVOMR                                
096600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
096700     MOVE SPACE TO GODK-STATUSKODER                                       
096800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
096900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
097000     PERFORM IMS-STATUSKONTROLL                                           
097100     .                                                                    
097200     EJECT                                                                
097300 IMS-GU-B201 SECTION.                                                     
097400                                                                          
097500     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
097600          DELIMITED BY SIZE INTO SSA1                                     
097700     MOVE '  GE' TO GODK-STATUSKODER                                      
097800     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
097900     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
098000     PERFORM IMS-STATUSKONTROLL                                           
098100     .                                                                    
098200     SKIP3                                                                
098300 IMS-STATUSKONTROLL SECTION.                                              
098400                                                                          
098500     SET STATUS-IX TO 1                                                   
098600     SEARCH GODK-STATUS                                                   
098700       AT END                                                             
098800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
098900         DELIMITED BY SIZE INTO FELTEXT                                   
099000         CALL FELLOG                                                      
099100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
099200         CONTINUE                                                         
099300     END-SEARCH                                                           
099400     .                                                                    
099500     EJECT                                                                
099600 DB2-DCL-OPN-CRS-TP4TRAN-SEND SECTION.                                    
099700     MOVE 'DB2-DCL-OPN-CRS-TP4KAMP-SEND'                                  
099800                            TO WS-DB2-SEKTION                             
099900*    DISPLAY WS-DB2-SEKTION                                               
100000* OBS!!! DECLARE GER INGEN SQLCODE I RETUR                                
100100     EXEC SQL DECLARE TP4TRAN-SEND-CRS CURSOR FOR                         
100200              SELECT KDARBTYP,                                            
100300                     IDDC_SEND,                                           
100400                     IDDC_REC,                                            
100500                     IDDISTR,                                             
100600                     IDKUNDNR                                             
100700              FROM TP4TRAN                                                
100800              WHERE KDARBTYP  = :WS-KDARBTYP                              
100900              AND   IDDC_SEND = :WS-IDDC-SEND                             
101000     END-EXEC                                                             
101100     MOVE 000               TO GODK-SQLCODEKODER                          
101200     EXEC SQL OPEN TP4TRAN-SEND-CRS END-EXEC                              
101300     MOVE SQLCODE           TO SQLCODE-WS                                 
101400     PERFORM DB2-STATUSKONTROLL                                           
101500     .                                                                    
101600     EJECT                                                                
101700 DB2-FETCH-TP4TRAN-SEND SECTION.                                          
101800     MOVE 'DB2-FETCH-TP4TRAN-SEND'                                        
101900                            TO WS-DB2-SEKTION                             
102000*    DISPLAY WS-DB2-SEKTION                                               
102100     MOVE 000100            TO GODK-SQLCODEKODER                          
102200     EXEC SQL FETCH TP4TRAN-SEND-CRS INTO                                 
102300            :TP4TRAN-KDARBTYP                                             
102400           ,:TP4TRAN-IDDC-SEND                                            
102500           ,:TP4TRAN-IDDC-REC                                             
102600           ,:TP4TRAN-IDDISTR                                              
102700           ,:TP4TRAN-IDKUNDNR                                             
102800     END-EXEC                                                             
102900     MOVE SQLCODE           TO SQLCODE-WS                                 
103000     PERFORM DB2-STATUSKONTROLL                                           
103100     .                                                                    
103200     EJECT                                                                
103300 DB2-CLOSE-TP4TRAN-SEND-CRS SECTION.                                      
103400     MOVE 'DB2-CLOSE-TP4TRAN-SEND-CRS'                                    
103500                            TO WS-DB2-SEKTION                             
103600*    DISPLAY WS-DB2-SEKTION                                               
103700     SKIP2                                                                
103800     EXEC SQL CLOSE TP4TRAN-SEND-CRS END-EXEC                             
103900     .                                                                    
104000     EJECT                                                                
104100 DB2-DCL-OPN-CRS-TP4TRAN-REC SECTION.                                     
104200     MOVE 'DB2-DCL-OPN-CRS-TP4KAMP-REC'                                   
104300                            TO WS-DB2-SEKTION                             
104400*    DISPLAY WS-DB2-SEKTION                                               
104500* OBS!!! DECLARE GER INGEN SQLCODE I RETUR                                
104600     EXEC SQL DECLARE TP4TRAN-REC-CRS CURSOR FOR                          
104700              SELECT KDARBTYP,                                            
104800                     IDDC_SEND,                                           
104900                     IDDC_REC,                                            
105000                     IDDISTR,                                             
105100                     IDKUNDNR                                             
105200              FROM TP4TRAN                                                
105300              WHERE KDARBTYP = :WS-KDARBTYP                               
105400              AND   IDDC_REC = :WS-IDDC-REC                               
105500     END-EXEC                                                             
105600     MOVE 000               TO GODK-SQLCODEKODER                          
105700     EXEC SQL OPEN TP4TRAN-REC-CRS END-EXEC                               
105800     MOVE SQLCODE           TO SQLCODE-WS                                 
105900     PERFORM DB2-STATUSKONTROLL                                           
106000     .                                                                    
106100     EJECT                                                                
106200 DB2-FETCH-TP4TRAN-REC SECTION.                                           
106300     MOVE 'DB2-FETCH-TP4TRAN-REC'                                         
106400                            TO WS-DB2-SEKTION                             
106500*    DISPLAY WS-DB2-SEKTION                                               
106600     MOVE 000100            TO GODK-SQLCODEKODER                          
106700     EXEC SQL FETCH TP4TRAN-REC-CRS INTO                                  
106800            :TP4TRAN-KDARBTYP                                             
106900           ,:TP4TRAN-IDDC-SEND                                            
107000           ,:TP4TRAN-IDDC-REC                                             
107100           ,:TP4TRAN-IDDISTR                                              
107200           ,:TP4TRAN-IDKUNDNR                                             
107300     END-EXEC                                                             
107400     MOVE SQLCODE           TO SQLCODE-WS                                 
107500     PERFORM DB2-STATUSKONTROLL                                           
107600     .                                                                    
107700     EJECT                                                                
107800 DB2-CLOSE-TP4TRAN-REC-CRS SECTION.                                       
107900     MOVE 'DB2-CLOSE-TP4TRAN-REC-CRS'                                     
108000                            TO WS-DB2-SEKTION                             
108100*    DISPLAY WS-DB2-SEKTION                                               
108200     SKIP2                                                                
108300     EXEC SQL CLOSE TP4TRAN-REC-CRS END-EXEC                              
108400     .                                                                    
108500     EJECT                                                                
108600 DB2-SELECT-TP4TRAN     SECTION.                                          
108700     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
108800                                                                          
108900     MOVE 000100 TO GODK-SQLCODEKODER                                     
109000                                                                          
109100     EXEC SQL                                                             
109200           SELECT  KDARBTYP                                               
109300                  ,IDDC_SEND                                              
109400                  ,IDDC_REC                                               
109500                  ,IDDISTR                                                
109600                  ,IDKUNDNR                                               
109700                                                                          
109800           INTO   :TP4TRAN-KDARBTYP                                       
109900                 ,:TP4TRAN-IDDC-SEND                                      
110000                 ,:TP4TRAN-IDDC-REC                                       
110100                 ,:TP4TRAN-IDDISTR                                        
110200                 ,:TP4TRAN-IDKUNDNR                                       
110300                                                                          
110400           FROM    TP4TRAN                                                
110500                                                                          
110600           WHERE KDARBTYP  = :WS-KDARBTYP                                 
110700           AND   IDDC_SEND = :WS-IDDC-SEND                                
110800           AND   IDDC_REC  = :WS-IDDC-REC                                 
110900     END-EXEC                                                             
111000                                                                          
111100     MOVE SQLCODE TO SQLCODE-WS                                           
111200     PERFORM DB2-STATUSKONTROLL                                           
111300     .                                                                    
111400     EJECT                                                                
111500 DB2-SELECT-TP4TRAN-2   SECTION.                                          
111600     MOVE 'DB2-SELECT-TP4TRAN-2 ' TO  WS-DB2-SEKTION                      
111700                                                                          
111800     MOVE 000100 TO GODK-SQLCODEKODER                                     
111900                                                                          
112000     EXEC SQL                                                             
112100           SELECT  KDARBTYP                                               
112200                  ,IDDC_SEND                                              
112300                  ,IDDC_REC                                               
112400                  ,IDDISTR                                                
112500                  ,IDKUNDNR                                               
112600                                                                          
112700           INTO   :TP4TRAN-KDARBTYP                                       
112800                 ,:TP4TRAN-IDDC-SEND                                      
112900                 ,:TP4TRAN-IDDC-REC                                       
113000                 ,:TP4TRAN-IDDISTR                                        
113100                 ,:TP4TRAN-IDKUNDNR                                       
113200                                                                          
113300           FROM    TP4TRAN                                                
113400                                                                          
113500           WHERE IDDC_SEND = :WS-IDDC-SEND                                
113600           AND   IDDISTR   = :W-IDDISTR                                   
113700     END-EXEC                                                             
113800                                                                          
113900     MOVE SQLCODE TO SQLCODE-WS                                           
114000     PERFORM DB2-STATUSKONTROLL                                           
114100     .                                                                    
114200     EJECT                                                                
114300 DB2-SELECT-TP4TRAN-3   SECTION.                                          
114400     MOVE 'DB2-SELECT-TP4TRAN-3 ' TO  WS-DB2-SEKTION                      
114500                                                                          
114600     MOVE 000100 TO GODK-SQLCODEKODER                                     
114700                                                                          
114800     EXEC SQL                                                             
114900           SELECT  DISTINCT                                               
115000                   IDDC_REC                                               
115100                                                                          
115200           INTO   :TP4TRAN-IDDC-REC                                       
115300                                                                          
115400           FROM    TP4TRAN                                                
115500                                                                          
115600           WHERE KDARBTYP = :WS-KDARBTYP                                  
115700           AND   IDDISTR  = :W-IDDISTR                                    
115800     END-EXEC                                                             
115900                                                                          
116000     MOVE SQLCODE TO SQLCODE-WS                                           
116100     PERFORM DB2-STATUSKONTROLL                                           
116200     .                                                                    
116300     EJECT                                                                
116400 DB2-DELETE-TP4TRAN SECTION.                                              
116500     MOVE 'DB2-DELETE-TP4TRAN   ' TO  WS-DB2-SEKTION                      
116600                                                                          
116700     MOVE 000   TO GODK-SQLCODEKODER                                      
116800                                                                          
116900     EXEC SQL                                                             
117000         DELETE FROM TP4TRAN                                              
117100                                                                          
117200         WHERE KDARBTYP  = :WS-KDARBTYP                                   
117300         AND   IDDC_SEND = :WS-IDDC-SEND                                  
117400         AND   IDDC_REC  = :WS-IDDC-REC                                   
117500     END-EXEC                                                             
117600                                                                          
117700     MOVE SQLCODE TO SQLCODE-WS                                           
117800     PERFORM DB2-STATUSKONTROLL                                           
117900     .                                                                    
118000     EJECT                                                                
118100 DB2-INSERT-TP4TRAN  SECTION.                                             
118200     MOVE 'DB2-INSERT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
118300     SKIP2                                                                
118400     MOVE 000   TO GODK-SQLCODEKODER                                      
118500     EXEC SQL                                                             
118600         INSERT INTO TP4TRAN                                              
118700            (KDARBTYP,IDDC_SEND,IDDC_REC                                  
118800            ,IDDISTR,IDKUNDNR)                                            
118900         VALUES                                                           
119000            (:TP4TRAN-KDARBTYP,:TP4TRAN-IDDC-SEND                         
119100            ,:TP4TRAN-IDDC-REC                                            
119200            ,:TP4TRAN-IDDISTR,:TP4TRAN-IDKUNDNR)                          
119300     END-EXEC                                                             
119400                                                                          
119500     MOVE SQLCODE TO SQLCODE-WS                                           
119600     PERFORM DB2-STATUSKONTROLL                                           
119700     .                                                                    
119800     EJECT                                                                
119900 DB2-UPDATE-TP4TRAN  SECTION.                                             
120000     MOVE 'DB2-UPDATE-TP4TRAN   ' TO  WS-DB2-SEKTION                      
120100                                                                          
120200     MOVE 000     TO GODK-SQLCODEKODER                                    
120300     EXEC SQL                                                             
120400         UPDATE TP4TRAN                                                   
120500             SET IDDISTR  = :TP4TRAN-IDDISTR                              
120600               , IDKUNDNR = :TP4TRAN-IDKUNDNR                             
120700                                                                          
120800         WHERE KDARBTYP  = :WS-KDARBTYP                                   
120900         AND   IDDC_SEND = :WS-IDDC-SEND                                  
121000         AND   IDDC_REC  = :WS-IDDC-REC                                   
121100     END-EXEC                                                             
121200                                                                          
121300     MOVE SQLCODE TO SQLCODE-WS                                           
121400     PERFORM DB2-STATUSKONTROLL                                           
121500     .                                                                    
121600     EJECT                                                                
121700 DB2-STATUSKONTROLL  SECTION.                                             
121800                                                                          
121900     SET SQLCODE-IX TO 1                                                  
122000     SEARCH GODK-SQLCODE                                                  
122100       AT END                                                             
122200          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
122300          DELIMITED BY SIZE INTO FELTEXT                                  
122400          CALL ABEND USING RKOD-ABEND-DB2                                 
122500       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
122600     END-SEARCH                                                           
122700     .                                                                    
