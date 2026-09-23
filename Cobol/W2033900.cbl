000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2033900.                                                
000300 AUTHOR.         BO HAMMARIN, GDC-GROUP.                                  
000400 DATE-WRITTEN.   FEBR-1999.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        DETTA PROGRAM HANTERAR UPPDATERING OCH FRÅGOR PÅ                 
000900*        LEVERANSSPÄRRAR, VILKA DISTRIKT OCH DEALERS ÄR                   
001000*        SPÄRRADE FÖR EN SPECIFIK LEVERANTÖRS-GRUPPART.                   
001100*        BILDEN VISAR DISTRIKT-, KUNDINTERVALL MED                        
001200*        LEVERANSSPÄRR OCH STARTDATUM PER KLASS.                          
001300*        DET FINNS 3 OLIKA INMATNINGSKANTKODER:                           
001400*        -NYUPPLÄGG AV RAD (N), TILLÅTEN PÅ RAD 1                         
001500*        -EDITERING AV RAD (E), TILLÅTEN PÅ RAD 1                         
001600*        -BORTTAG   AV RAD (D), TILLÅTEN PÅ ÖVRIGA RADER                  
001700*                                                                         
001800*        PROGRAMMET UPPDATERAR        (WDF8)                              
001900*        PROGRAMMET LÄSER      WLGMTA (WDB2)                              
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W2T339 W2T339U W2T339X                              
002300*        MID:         W2I33901                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W2O33901                                            
002700                                                                          
002800                                                                          
002900 ENVIRONMENT DIVISION.                                                    
003000                                                                          
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500*    -COPY WY2000W1                                                       
003600                                                                          
003700                                                                          
003800 77  IDPGM                       PIC X(08)   VALUE 'W2033900'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004410                                                                          
004420 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004430 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004460                                                                          
004470 01  W-IDDISTR-IN                PIC 9(5)    VALUE ZERO.                  
004480 01  W-IDKUNDNR-IN               PIC 9(7)    VALUE ZERO.                  
004490                                                                          
004500 77  SW-ENTRY                    PIC X       VALUE 'N'.                   
004600     88 NO-ENTRY                             VALUE 'N'.                   
004700 77  SW-OVERLAPPING              PIC X       VALUE 'N'.                   
004800     88 OVERLAPPING                          VALUE 'J'.                   
004801 77  SW-SEGMENT-OK               PIC X       VALUE 'N'.                   
004802     88 SEGMENT-OK                           VALUE 'J'.                   
004810                                                                          
004820 77  RAD-VALD-SW                 PIC X       VALUE 'N'.                   
004830     88  RAD-VALD                            VALUE 'J'.                   
004840     88  RAD-EJ-VALD                         VALUE 'N'.                   
004850                                                                          
004860 77  E-RAD-FEL-SW                PIC X       VALUE 'N'.                   
004870     88  E-RAD-FEL                           VALUE 'J'.                   
004900                                                                          
004910 77  E-DIST-FOM-FEL-SW           PIC X       VALUE 'N'.                   
004920     88  E-DIST-FOM-FEL                      VALUE 'J'.                   
004921 77  E-DIST-TOM-FEL-SW           PIC X       VALUE 'N'.                   
004922     88  E-DIST-TOM-FEL                      VALUE 'J'.                   
004923 77  E-KUND-FOM-FEL-SW           PIC X       VALUE 'N'.                   
004924     88  E-KUND-FOM-FEL                      VALUE 'J'.                   
004925 77  E-KUND-TOM-FEL-SW           PIC X       VALUE 'N'.                   
004926     88  E-KUND-TOM-FEL                      VALUE 'J'.                   
004927 77  E-DAT1-FEL-SW               PIC X       VALUE 'N'.                   
004928     88  E-DAT1-FEL                          VALUE 'J'.                   
004929 77  E-DAT2-FEL-SW               PIC X       VALUE 'N'.                   
004930     88  E-DAT2-FEL                          VALUE 'J'.                   
004931 77  E-DAT3-FEL-SW               PIC X       VALUE 'N'.                   
004932     88  E-DAT3-FEL                          VALUE 'J'.                   
004933 77  E-DAT4-FEL-SW               PIC X       VALUE 'N'.                   
004934     88  E-DAT4-FEL                          VALUE 'J'.                   
004935 77  E-DAT5-FEL-SW               PIC X       VALUE 'N'.                   
004936     88  E-DAT5-FEL                          VALUE 'J'.                   
004940                                                                          
005000*    --- GENERELLA ARBETSFÄLT                                             
005100 77  KUNDNR-FOM-NOLL             PIC X      VALUE 'N'.                    
005200                                                                          
005300 01  DAGENS-DATUM                PIC 9(6).                                
005400                                                                          
006120 01  WS-TISTADAT                 PIC 9(6).                                
006200                                                                          
006300                                                                          
006400*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006510 77  MFS-INDX                    PIC S9(4)  VALUE +0    COMP SYNC.        
006600 77  SPAR-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
006700 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
006800 77  KV-IX                       PIC S9(4)  VALUE +0    COMP SYNC.        
006900 77  MAX-KV-IX                   PIC S9(4)  VALUE +5    COMP SYNC.        
007000 77  DAT-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
007100 77  MAX-DAT-IX                  PIC S9(4)  VALUE +5    COMP SYNC.        
007100 77  W-FL-N-CNT                  PIC S9     VALUE +0    COMP SYNC.        
007200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007300                                                                          
007400 77  DAT-SW                      PIC X       VALUE 'J'.                   
007500     88  DAT-OK                              VALUE 'J'.                   
007600                                                                          
007610 77  RADER-FINNS-SW              PIC X       VALUE 'J'.                   
007620     88  RADER-FINNS                         VALUE 'J'.                   
007630                                                                          
007700 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007800     88  ALLT-OK                             VALUE 'J'.                   
007900                                                                          
008000 77  CMD-SW                      PIC X       VALUE 'N'.                   
008100     88  CMD-OK                              VALUE 'J'.                   
008200                                                                          
008300 77  NY-UPPD-POST-SW             PIC X       VALUE 'N'.                   
008400     88  NY-UPPD-POST                        VALUE 'J'.                   
008500                                                                          
008600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008700     88  INDATA-OK                           VALUE 'J'.                   
008800     88  INDATA-FEL                          VALUE 'N'.                   
008900                                                                          
009000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009100     88  NYCKLAR-OK                          VALUE 'J'.                   
009200     88  NYCKLAR-FEL                         VALUE 'N'.                   
009300                                                                          
009400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009500     88  EGEN-MID                            VALUE '2339'.                
009600     88  GODK-MID                            VALUE '2331' '2332'          
009700                                                   '2333' '2334'          
009800                                                   '2335' '2336'          
009900                                                   '2337' '2338'          
010000                                                   '2339'.                
010100     88  HELP-MID                            VALUE '0551'.                
010200                                                                          
010210                                                                          
010300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010400 01  GENERELLA-SUBPROGRAM.                                                
010500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011000                                                                          
011010                                                                          
011100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011200*01 -COPY WMEDAREA                                                        
011300                                                                          
011400*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
011500*01 -COPY WDATAREA                                                        
011600                                                                          
011700 01  MESSAGE-CODES.                                                       
011800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011900     03  ERR-CONFLICT            PIC X(3)    VALUE '002'.                 
012000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
012100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
012200     03  ERR-MISSING-REG         PIC X(3)    VALUE '010'.                 
012300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
012400     03  ERR-DIST-CUST-MISSING   PIC X(3)    VALUE '040'.                 
012500     03  DISTR-EXISTS            PIC X(3)    VALUE '070'.                 
012600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
012700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
012800     03  GROUP-MISSING           PIC X(3)    VALUE '275'.                 
012900     03  PART-EXISTS             PIC X(3)    VALUE '278'.                 
013000     03  PART-MISSING            PIC X(3)    VALUE '279'.                 
013100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013200     03  ERR-WRONG-INTERVAL      PIC X(3)    VALUE '738'.                 
013300                                                                          
013310                                                                          
013400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013500*                                                                         
013600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013700                                                                          
013800*01 -COPY WMSGINIT                                                        
013900                                                                          
013910                                                                          
014000*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
014100*                                                                         
014200 01  SPAR-AREA.                                                           
014300     03  SPAR-IDTRANS            PIC X(4)    VALUE '2339'.                
014400     03  SPAR-IDDISTR-KEY        PIC 9(4)    VALUE ZERO.                  
014401     03  SPAR-IDKUNDNR-KEY       PIC 9(6)    VALUE ZERO.                  
014410     03  SPAR-IDDISTR-FOM-ENTER  PIC 9(4)    VALUE ZERO.                  
014500     03  SPAR-IDDISTR-TOM-ENTER  PIC 9(4)    VALUE ZERO.                  
014600     03  SPAR-IDKUNDNR-FOM-ENTER PIC 9(6)    VALUE ZERO.                  
014700     03  SPAR-IDKUNDNR-TOM-ENTER PIC 9(6)    VALUE ZERO.                  
014800     03  SPAR-IDDISTR-FOM-NEXT   PIC 9(4)    VALUE ZERO.                  
014900     03  SPAR-IDDISTR-TOM-NEXT   PIC 9(4)    VALUE ZERO.                  
015000     03  SPAR-IDKUNDNR-FOM-NEXT  PIC 9(6)    VALUE ZERO.                  
015100     03  SPAR-IDKUNDNR-TOM-NEXT  PIC 9(6)    VALUE ZERO.                  
015200     03  SPAR-TABELL.                                                     
015300       05  SPAR-WDF811   OCCURS 11.                                       
015400           07  SPAR-WDF811-TAB.                                           
015500               09  SPAR-IDDISTR-FOM    PIC 9(4)    VALUE ZERO.            
015600               09  SPAR-IDDISTR-TOM    PIC 9(4)    VALUE ZERO.            
015700               09  SPAR-IDKUNDNR-FOM   PIC 9(6)    VALUE ZERO.            
015800               09  SPAR-IDKUNDNR-TOM   PIC 9(6)    VALUE ZERO.            
015900                                                                          
015910                                                                          
016000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016100*                                                                         
016200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016300                                                                          
016400*01  MID -COPY W2I33901                                                   
016500                                                                          
016510                                                                          
016600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016700                                                                          
016800*01  -COPY WMSGAREA                                                       
016900                                                                          
016910                                                                          
017000     03  MOD REDEFINES MSG-AREA.                                          
017100*      05  -COPY W2O33901                                                 
017200                                                                          
017210                                                                          
017300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017400                                                                          
017500*01  -COPY WMFSAREA                                                       
017600                                                                          
017610                                                                          
017700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017800*                                                                         
017900                                                                          
018000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018100                                                                          
018200 01  IMS-WS-4.                                                            
018300     03  FILLER                  PIC X(16)  VALUE 'MSG-KOM-AREA'.         
018400*      --- GENERELL IO-KOMMUNIKATIONSAREA FÖR DISPATCHER                  
018500*01  -COPY WMSGKOM                                                        
018600                                                                          
018700 01  NYCKLAR-TILL-DLI.                                                    
018800*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
018900     03  W-IDSPRGRP-X.                                                    
019100         05  W-IDSPRGRP          PIC X(10)   VALUE SPACE.                 
019200                                                                          
019300     03  W-WDF811KY-X.                                                    
019400         05  W-IDDISTR-FOM-X.                                             
019500             07  W-IDDISTR-FOM   PIC S9(5)   VALUE ZERO COMP-3.           
019600         05  W-IDDISTR-TOM-X.                                             
019700             07  W-IDDISTR-TOM   PIC S9(5)   VALUE ZERO COMP-3.           
019800         05  W-IDKUNDNR-FOM-X.                                            
019900             07  W-IDKUNDNR-FOM  PIC S9(7)   VALUE ZERO COMP-3.           
020000         05  W-IDKUNDNR-TOM-X.                                            
020100             07  W-IDKUNDNR-TOM  PIC S9(7)   VALUE ZERO COMP-3.           
020200                                                                          
020210     03  W-WDF811KY-F8-X.                                                 
020220         05  W-IDDISTR-FOM-F8-X.                                          
020230             07  W-IDDISTR-FOM-F8  PIC S9(5) VALUE ZERO COMP-3.           
020240         05  W-IDDISTR-TOM-F8-X.                                          
020250             07  W-IDDISTR-TOM-F8  PIC S9(5) VALUE ZERO COMP-3.           
020260         05  W-IDKUNDNR-FOM-F8-X.                                         
020270             07  W-IDKUNDNR-FOM-F8 PIC S9(7) VALUE ZERO COMP-3.           
020280         05  W-IDKUNDNR-TOM-F8-X.                                         
020290             07  W-IDKUNDNR-TOM-F8 PIC S9(7) VALUE ZERO COMP-3.           
020291                                                                          
020300     03  W-IDGMT-X.                                                       
020400         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
020500         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
020600                                                                          
020700                                                                          
020800*    --- STATUS-KOD FRÅN IMS                                              
020900 01  STATUS-WS                   PIC XX.                                  
021000     88  SEGMENT-FINNS                       VALUE '  '.                  
021100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021300                                                                          
021400 01  GODK-STATUSKODER.                                                    
021500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021600                                                                          
021700 01  ALL-SSA.                                                             
021710     03 SSA1                     PIC X(64).                               
021800     03 SSA2                     PIC X(150).                              
021900                                                                          
021910                                                                          
022000*    --- IMS FUNKTIONSKODER                                               
022100*01  -COPY W0003                                                          
022200                                                                          
022210                                                                          
022300*    ---  DLI INPUT-OUTPUT AREA                                           
022400                                                                          
022500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF801'.                      
022600 01  DLI-IO-WDF801.                                                       
022700*    03  -COPY WDF801                                                     
022800                                                                          
022900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF811'.                      
023000 01  DLI-IO-WDF811.                                                       
023100*    03  -COPY WDF811                                                     
023200                                                                          
023210                                                                          
023300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
023400 01  DLI-IO-WDB201.                                                       
023500*    03  -COPY WDB201                                                     
023600                                                                          
023610                                                                          
023700 LINKAGE SECTION.                                                         
023800*01  -COPY W0009   -PRE MSG-                                              
023900                                                                          
024000*01  -COPY W0009   -PRE MSGKOM-                                           
024100                                                                          
024200*01  -COPY W0008   -PRE USEA-                                             
024300     05  FILLER                  PIC X.                                   
024400                                                                          
024500*01  -COPY W0008  -PRE WDF8-                                              
024600     05  FILLER                  PIC X.                                   
024700                                                                          
024800*01  -COPY W0008  -PRE WDB2-                                              
024900     05  FILLER                  PIC X.                                   
025000                                                                          
025010                                                                          
025020                                                                          
025100 PROCEDURE DIVISION  USING MSG-PCB  MSGKOM-PCB USEA-PCB                   
025200                           WDF8-PCB WDB2-PCB.                             
025300 MAIN SECTION.                                                            
025400     ENTRY 'DLITCBL' USING MSG-PCB  MSGKOM-PCB USEA-PCB                   
025500                           WDF8-PCB WDB2-PCB.                             
025600                                                                          
025700     PERFORM IMS-GET-MSG                                                  
025800     IF SEGMENT-FINNS                                                     
025900       PERFORM IMS-GET-WMSGKOM-MSG                                        
026000       PERFORM A-INIT                                                     
026100       PERFORM B-KOLLA-NYCKLAR                                            
026110                                                                          
026200       IF NYCKLAR-OK                                                      
026300         IF MFS-UPDATE OR MFS-UPD-X                                       
026400           PERFORM G-KOLLA-INPUT                                          
026500           IF INDATA-OK                                                   
026600             PERFORM H-UPPDATERA                                          
026700           END-IF                                                         
026800         ELSE                                                             
026900           IF MFS-FIRST                                                   
027000             PERFORM C-FOERSTA-SIDA                                       
027100           ELSE                                                           
027200             IF MFS-NEXT                                                  
027300               PERFORM D-NAESTA-SIDA                                      
027400             ELSE                                                         
027500               PERFORM E-SAMMA-SIDA                                       
027600             END-IF                                                       
027700           END-IF                                                         
027800         END-IF                                                           
027810                                                                          
027900         IF ALLT-OK                                                       
028000           PERFORM F-LAES-VISA-INFO                                       
028010         ELSE                                                             
028020           IF E-RAD-FEL                                                   
028021           PERFORM F-LAES-VISA-INFO                                       
028030              PERFORM S03-E-RAD-TO-MOD                                    
028040           END-IF                                                         
028100         END-IF                                                           
028200       END-IF                                                             
028300       IF MFS-UPD-X                                                       
028400         COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM + 17            
028500         PERFORM IMS-INSERT-WMSGKOM-MSG                                   
028600       ELSE                                                               
028700         COMPUTE MSG-KVLL = LENGTH OF MOD-W2O33901 + 4                    
028800         PERFORM IMS-INSERT-MSG                                           
028900       END-IF                                                             
029000     END-IF                                                               
029100                                                                          
029200     MOVE ZERO TO RETURN-CODE                                             
029300     GOBACK                                                               
029400     .                                                                    
029500                                                                          
029510                                                                          
029600 A-INIT SECTION.                                                          
029610     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
029700                                                                          
029800     MOVE FUNCTION CURRENT-DATE(3:6) TO DAGENS-DATUM                      
029900                                                                          
030000     IF MSG-DUBBLA-TRANSKODER                                             
030100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I33901                 
030200       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
030300       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
030400     ELSE                                                                 
030500       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W2I33901                 
030600       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
030700       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
030800     END-IF                                                               
030900                                                                          
031000     MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                  
031100     MOVE MSG-IDPFK       TO MFS-IDPFK                                    
031200     MOVE MFS-IDTRANS     TO W-IDTRANS                                    
031300                                                                          
031400     MOVE LOW-VALUE       TO MSG-AREA                                     
031500     MOVE 'W2O339N1'      TO MFS-IDMOD                                    
031600     MOVE '2339'          TO MOD-IDTRANS                                  
031700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
031800                                                                          
031900     IF EGEN-MID OR HELP-MID                                              
031910       IF MID-IDSPRGRP-IN NOT = ALL '+'                                   
031920       OR MID-IDDISTR-IN  NOT = ALL '+'                                   
031930       OR MID-IDKUNDNR-IN NOT = ALL '+'                                   
031940                                                                          
031950          MOVE SPACE TO MFS-KDTRTYP                                       
031960          MOVE '7' TO MFS-IDPFK                                           
031970       ELSE                                                               
032000          CONTINUE                                                        
032010       END-IF                                                             
032100     ELSE                                                                 
032110       MOVE ALL '+' TO MID-IDSPRGRP-IN                                    
032120                       MID-IDDISTR-IN                                     
032130                       MID-IDKUNDNR-IN                                    
032140                                                                          
032200       MOVE SPACE TO MFS-KDTRTYP                                          
032300       MOVE '7'   TO MFS-IDPFK                                            
032400     END-IF                                                               
032500     .                                                                    
032600                                                                          
032610                                                                          
032700 B-KOLLA-NYCKLAR SECTION.                                                 
032710     MOVE 'B-KOLLA-NYCKLAR ' TO CURRENT-SECTION                           
032800                                                                          
032900     MOVE ALL '+'               TO MSGI-WMSGINIT                          
033000     MOVE '001'                 TO MSGI-KDCALL                            
033100     MOVE MSG-LTERM-NAME        TO MSGI-IDLTERM-USER                      
033200     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
033300     MOVE '2339'                TO MSGI-IDTRANS                           
033400                                                                          
033500     IF EGEN-MID                                                          
033600       MOVE MID-IDSPRGRP-IN     TO MSGI-IDDIRGRP                          
033700       IF MID-IDDISTR-IN NOT = ALL '+'                                    
033710         INSPECT MID-IDDISTR-IN REPLACING LEADING SPACE BY ZERO           
033800         MOVE MID-IDDISTR-IN    TO MSGI-IDDISTR                           
033900       ELSE                                                               
034000         MOVE MID-IDDISTR-UT    TO MID-IDDISTR-IN                         
034100                                   MSGI-IDDISTR                           
034200       END-IF                                                             
034300       IF MID-IDKUNDNR-IN NOT = ALL '+'                                   
034310         INSPECT MID-IDKUNDNR-IN REPLACING LEADING SPACE BY ZERO          
034400         MOVE MID-IDKUNDNR-IN   TO MSGI-IDKUNDNR                          
034500       ELSE                                                               
034600         MOVE MID-IDKUNDNR-UT   TO MID-IDKUNDNR-IN                        
034700                                   MSGI-IDKUNDNR                          
034800       END-IF                                                             
034900     END-IF                                                               
035000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
035100     MOVE MSGI-SPAR-AREA        TO SPAR-AREA                              
035101                                                                          
035110**INITIALIZE NUMERIC FIELDS INCASE OF INVALID DATA                        
035111     IF SPAR-IDDISTR-FOM-ENTER     NOT NUMERIC                            
035114        MOVE ZERO TO SPAR-IDDISTR-FOM-ENTER                               
035116     END-IF                                                               
035117     IF SPAR-IDDISTR-TOM-ENTER     NOT NUMERIC                            
035118        MOVE ZERO TO SPAR-IDDISTR-TOM-ENTER                               
035119     END-IF                                                               
035120     IF SPAR-IDKUNDNR-KEY          NOT NUMERIC                            
035121        MOVE ZERO TO SPAR-IDKUNDNR-KEY                                    
035122     END-IF                                                               
035123     IF SPAR-IDKUNDNR-FOM-ENTER    NOT NUMERIC                            
035124        MOVE ZERO TO SPAR-IDKUNDNR-FOM-ENTER                              
035125     END-IF                                                               
035126     IF SPAR-IDKUNDNR-TOM-ENTER    NOT NUMERIC                            
035127        MOVE ZERO TO SPAR-IDKUNDNR-TOM-ENTER                              
035128     END-IF                                                               
035129     IF SPAR-IDDISTR-FOM-NEXT      NOT NUMERIC                            
035130        MOVE ZERO TO SPAR-IDDISTR-FOM-NEXT                                
035131     END-IF                                                               
035132     IF SPAR-IDDISTR-TOM-NEXT      NOT NUMERIC                            
035133        MOVE ZERO TO SPAR-IDDISTR-TOM-NEXT                                
035134     END-IF                                                               
035135     IF SPAR-IDKUNDNR-FOM-NEXT     NOT NUMERIC                            
035136        MOVE ZERO TO SPAR-IDKUNDNR-FOM-NEXT                               
035137     END-IF                                                               
035138     IF SPAR-IDKUNDNR-TOM-NEXT     NOT NUMERIC                            
035139        MOVE ZERO TO SPAR-IDKUNDNR-TOM-NEXT                               
035140     END-IF                                                               
035150**                                                                        
035200                                                                          
035300     IF MSGI-IDLAND-SPR = 'GB'                                            
035400       MOVE 'GB'                TO MED-IDSKYLT                            
035500     ELSE                                                                 
035600       MOVE 'S'                 TO MED-IDSKYLT                            
035700     END-IF                                                               
035800                                                                          
035900     MOVE JA                    TO ALLT-SW                                
036000     MOVE JA                    TO NYCKLAR-SW                             
036100     MOVE SPACE                 TO MED-IDMFSFEL                           
036200     MOVE SPACE                 TO MED-IDMFSINF                           
036300                                                                          
036400*    -- KONTROLL AV IDSPRGRP                                              
036500     MOVE MFS-RENSA-FAELT       TO MOD-IDSPRGRP-IN                        
036600                                                                          
036700     MOVE MSGI-IDDIRGRP         TO W-IDSPRGRP                             
036800                                                                          
036900*    -- KONTROLL AV IDDISTR                                               
037000     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-IN                         
037100                                                                          
037110     IF EGEN-MID                                                          
037200        IF MID-IDDISTR-IN NOT = ALL '+'                                   
037300          INSPECT MID-IDDISTR-IN REPLACING LEADING SPACE BY ZERO          
037400          IF MID-IDDISTR-IN NUMERIC                                       
037500            IF MID-IDDISTR-IN >= 1                                        
037600              MOVE MID-IDDISTR-IN TO W-IDDISTR-FOM                        
037700                                      W-IDDISTR-TOM                       
037800            END-IF                                                        
037900          ELSE                                                            
038000            MOVE NEJ              TO NYCKLAR-SW                           
038100          END-IF                                                          
038210          MOVE MID-IDDISTR-IN     TO MOD-IDDISTR-UT                       
038220                                      SPAR-IDDISTR-KEY                    
038300          INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE          
038400        ELSE                                                              
038500          MOVE MFS-RENSA-FAELT    TO MOD-IDDISTR-UT                       
038510          MOVE ZERO               TO SPAR-IDDISTR-KEY                     
038600        END-IF                                                            
038700                                                                          
038800*    -- KONTROLL AV IDKUNDNR                                              
038900        MOVE MFS-RENSA-FAELT      TO MOD-IDKUNDNR-IN                      
039000                                                                          
039100        IF MID-IDKUNDNR-IN NOT = ALL '+'                                  
039200          INSPECT MID-IDKUNDNR-IN REPLACING LEADING SPACE BY ZERO         
039300          IF MID-IDKUNDNR-IN NUMERIC                                      
039400            IF MSGI-IDKUNDNR >= 1                                         
039500              MOVE MID-IDKUNDNR-IN TO W-IDKUNDNR-FOM                      
039600              MOVE MID-IDKUNDNR-IN TO W-IDKUNDNR-TOM                      
039700            END-IF                                                        
039800          ELSE                                                            
039900            MOVE NEJ               TO NYCKLAR-SW                          
040000          END-IF                                                          
040110          MOVE MSGI-IDKUNDNR       TO MOD-IDKUNDNR-UT                     
040200          INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE         
040300        ELSE                                                              
040400          MOVE MFS-RENSA-FAELT     TO MOD-IDKUNDNR-UT                     
040500        END-IF                                                            
040501     ELSE                                                                 
040502        MOVE ZERO TO SPAR-IDDISTR-KEY                                     
040503                     SPAR-IDKUNDNR-KEY                                    
040510     END-IF                                                               
040600                                                                          
040700     IF GODK-MID OR NYCKLAR-OK                                            
040800       MOVE MSGI-IDDIRGRP       TO MOD-IDSPRGRP-UT                        
040900     ELSE                                                                 
041000       MOVE MFS-RENSA-FAELT     TO MOD-IDSPRGRP-UT                        
041100     END-IF                                                               
041200                                                                          
041300     IF NYCKLAR-FEL                                                       
041400       MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                           
041500                                   MSG-KOM-IDMFSMED                       
041600       CALL WMEDKONV USING MED-WMEDAREA                                   
041700       MOVE MED-MFSFEL          TO MOD-TEMFSFEL                           
041800       PERFORM MFS-RENSA-FAELT-IN                                         
041900       PERFORM MFS-RENSA-FAELT-UT                                         
042000     END-IF                                                               
042100     .                                                                    
042200                                                                          
042210                                                                          
042300 C-FOERSTA-SIDA SECTION.                                                  
042310     MOVE 'C-FOERSTA-SIDA  ' TO CURRENT-SECTION                           
042400                                                                          
042410     IF MID-IDDISTR-IN NOT = ALL '+'                                      
042411        INSPECT MID-IDDISTR-IN REPLACING LEADING SPACE BY ZERO            
042420        MOVE MID-IDDISTR-IN  TO SPAR-IDDISTR-KEY                          
042430*    ELSE                                                                 
042440*       MOVE MSGI-IDDISTR    TO SPAR-IDDISTR-KEY                          
042450     END-IF                                                               
042460     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
042461        INSPECT MID-IDKUNDNR-IN REPLACING LEADING SPACE BY ZERO           
042470        MOVE MID-IDKUNDNR-IN TO SPAR-IDKUNDNR-KEY                         
042480*    ELSE                                                                 
042490*       MOVE MSGI-IDKUNDNR   TO SPAR-IDKUNDNR-KEY                         
042492     END-IF                                                               
042500     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
042600     CALL WMEDKONV USING MED-WMEDAREA                                     
042700     MOVE MED-MFSFEL     TO MOD-TEMFSFEL                                  
042800                                                                          
042900     PERFORM MFS-RENSA-FAELT-IN                                           
043000     .                                                                    
043100                                                                          
043110                                                                          
043200 D-NAESTA-SIDA SECTION.                                                   
043210     MOVE 'D-NAESTA-SIDA   ' TO CURRENT-SECTION                           
043300                                                                          
043400     IF SPAR-IDTRANS = '2339'                                             
043500       MOVE SPAR-IDDISTR-FOM-NEXT  TO W-IDDISTR-FOM                       
043600       MOVE SPAR-IDDISTR-TOM-NEXT  TO W-IDDISTR-TOM                       
043700       MOVE SPAR-IDKUNDNR-FOM-NEXT TO W-IDKUNDNR-FOM                      
043800       MOVE SPAR-IDKUNDNR-TOM-NEXT TO W-IDKUNDNR-TOM                      
043900     ELSE                                                                 
044000       PERFORM MFS-RENSA-FAELT-IN                                         
044100     END-IF                                                               
044200     .                                                                    
044300                                                                          
044310                                                                          
044400 E-SAMMA-SIDA SECTION.                                                    
044410     MOVE 'E-SAMMA-SIDA    ' TO CURRENT-SECTION                           
044500                                                                          
044600     IF SPAR-IDTRANS = '2339'                                             
044601        MOVE SPAR-IDDISTR-FOM-ENTER  TO W-IDDISTR-FOM                     
044603        MOVE SPAR-IDDISTR-TOM-ENTER  TO W-IDDISTR-TOM                     
044604        MOVE SPAR-IDKUNDNR-FOM-ENTER TO W-IDKUNDNR-FOM                    
044605        MOVE SPAR-IDKUNDNR-TOM-ENTER TO W-IDKUNDNR-TOM                    
044606     END-IF                                                               
044607     MOVE JA  TO INDATA-SW                                                
044608     MOVE NEJ TO RAD-VALD-SW                                              
044609                                                                          
044610     MOVE 1 TO INDX                                                       
044611     PERFORM UNTIL INDX > MAX-INDX                                        
044612        IF MID-CMD (INDX) NOT = '+' AND                                   
044613           MID-CMD (INDX) NOT = ' '                                       
044614           IF MID-CMD (INDX) NOT = 'C' OR                                 
044615              RAD-VALD                                                    
044616              IF MID-CMD (INDX) = 'D'                                     
044617                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR (INDX)         
044618                 CONTINUE                                                 
044619              ELSE                                                        
044621                 PERFORM MFS-ROER-EJ-FAELT-UT                             
044622                 PERFORM MFS-ROER-EJ-FAELT-IN                             
044624                 PERFORM MFS-LAES-IN-IGEN                                 
044625                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-CMD-ATTR (INDX)         
044626                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
044627                 CALL WMEDKONV USING MED-WMEDAREA                         
044628                 MOVE MED-MFSFEL           TO MOD-TEMFSFEL                
044629                 MOVE NEJ TO INDATA-SW                                    
044630              END-IF                                                      
044631           ELSE                                                           
044632              PERFORM EA-FLYTTA-VALD-RAD                                  
044633              MOVE JA TO RAD-VALD-SW                                      
044634           END-IF                                                         
044635        END-IF                                                            
044636        ADD 1 TO INDX                                                     
044637     END-PERFORM                                                          
044638                                                                          
044639     IF INDATA-OK AND                                                     
044640        SPAR-IDTRANS = '2339' OR '0551'                                   
044650        IF RAD-VALD                                                       
044800           PERFORM MFS-LAES-IN-IGEN-E                                     
044810           MOVE MFS-STAENG-FAELT TO MOD-CMD-E-ATTR                        
044820                                    MOD-IDDISTR-FOM-E-ATTR                
044830                                    MOD-IDDISTR-TOM-E-ATTR                
044840                                    MOD-IDKUNDNR-FOM-E-ATTR               
044850                                    MOD-IDKUNDNR-TOM-E-ATTR               
044900           MOVE 1 TO INDX                                                 
045000           PERFORM UNTIL INDX > MAX-INDX                                  
045010              MOVE MFS-STAENG-FAELT TO MOD-CMD-ATTR (INDX)                
045020              ADD +1 TO INDX                                              
045030           END-PERFORM                                                    
045100        ELSE                                                              
045110           PERFORM MFS-ROER-EJ-FAELT-UT                                   
045120           PERFORM MFS-ROER-EJ-FAELT-IN                                   
045130           PERFORM MFS-ROER-EJ-FAELT-IN-E                                 
045140           PERFORM MFS-LAES-IN-IGEN-E                                     
045143        END-IF                                                            
045150        MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                               
045160        CALL WMEDKONV USING MED-WMEDAREA                                  
045170        MOVE MED-MFSFEL     TO MOD-TEMFSFEL                               
045190        PERFORM EB-MID-INDATA-TILL-MOD                                    
047310     END-IF                                                               
047400     .                                                                    
047500                                                                          
047510                                                                          
047600 EA-FLYTTA-VALD-RAD SECTION.                                              
047610     MOVE 'EA-FLYTTA-RAD   ' TO CURRENT-SECTION                           
047700                                                                          
047701     MOVE MFS-RENSA-FAELT    TO MOD-CMD (INDX)                            
047702     INSPECT MID-IDDISTR-FOM (INDX)                                       
047703             REPLACING LEADING SPACE BY ZERO                              
047704     INSPECT MID-IDDISTR-TOM (INDX)                                       
047705             REPLACING LEADING SPACE BY ZERO                              
047706     INSPECT MID-IDKUNDNR-FOM (INDX)                                      
047707             REPLACING LEADING SPACE BY ZERO                              
047708     INSPECT MID-IDKUNDNR-TOM (INDX)                                      
047709             REPLACING LEADING SPACE BY ZERO                              
047710                                                                          
047711     MOVE MID-IDDISTR-FOM (INDX)  TO W-IDDISTR-FOM-F8                     
047712     MOVE MID-IDDISTR-TOM (INDX)  TO W-IDDISTR-TOM-F8                     
047713     MOVE MID-IDKUNDNR-FOM (INDX) TO W-IDKUNDNR-FOM-F8                    
047714     MOVE MID-IDKUNDNR-TOM (INDX) TO W-IDKUNDNR-TOM-F8                    
047715                                                                          
047716     PERFORM IMS-GHU-WDF811M                                              
047717                                                                          
047718     MOVE 'C'          TO MOD-CMD-E                                       
047719                          MID-CMD-E                                       
047720     MOVE DSPR-IDDISTR-FOM  TO MOD-IDDISTR-FOM-E                          
047721     MOVE DSPR-IDDISTR-TOM  TO MOD-IDDISTR-TOM-E                          
047722     MOVE DSPR-IDKUNDNR-FOM TO MOD-IDKUNDNR-FOM-E                         
047723     MOVE DSPR-IDKUNDNR-TOM TO MOD-IDKUNDNR-TOM-E                         
047725     MOVE MFS-STAENG-FAELT  TO MOD-CMD-E-ATTR                             
047726                               MOD-IDDISTR-FOM-E-ATTR                     
047727                               MOD-IDDISTR-TOM-E-ATTR                     
047728                               MOD-IDKUNDNR-FOM-E-ATTR                    
047729                               MOD-IDKUNDNR-TOM-E-ATTR                    
047730                                                                          
047731     MOVE 1 TO KV-IX                                                      
047732     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
047733        IF DSPR-FLMARKSP (KV-IX) = JA                                     
047734           MOVE DSPR-TISTADAT (KV-IX) TO MOD-TISTADAT-E (KV-IX)           
047735        ELSE                                                              
047736           MOVE ZERO                  TO MOD-TISTADAT-E (KV-IX)           
047737        END-IF                                                            
047738        INSPECT MOD-TISTADAT-E (KV-IX)                                    
047739                REPLACING LEADING ZERO BY SPACE                           
047740        ADD 1 TO KV-IX                                                    
047741     END-PERFORM                                                          
047742     .                                                                    
047743                                                                          
047744                                                                          
047745 EB-MID-INDATA-TILL-MOD SECTION.                                          
047750     MOVE 'EB-MID-TILL-MOD ' TO CURRENT-SECTION                           
047751                                                                          
047752     IF RAD-EJ-VALD                                                       
047755        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-E-ATTR                      
047762                                      MOD-IDDISTR-FOM-E-ATTR              
047768                                      MOD-IDDISTR-TOM-E-ATTR              
047774                                      MOD-IDKUNDNR-FOM-E-ATTR             
047780                                      MOD-IDKUNDNR-FOM-E-ATTR             
047784     END-IF                                                               
047785                                                                          
047786     MOVE 1 TO KV-IX                                                      
047787     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
047789        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TISTADAT-E-ATTR (KV-IX)         
047793        ADD 1 TO KV-IX                                                    
047794     END-PERFORM                                                          
047795                                                                          
047796     .                                                                    
047797                                                                          
047798                                                                          
047799 F-LAES-VISA-INFO SECTION.                                                
047800     MOVE 'F-LAES-VISA-INFO' TO CURRENT-SECTION                           
047801                                                                          
047810     PERFORM IMS-GU-WDF801                                                
047900                                                                          
048000     IF SEGMENT-SAKNAS                                                    
048100        MOVE GROUP-MISSING     TO MED-IDMFSFEL                            
048300        CALL WMEDKONV USING MED-WMEDAREA                                  
048400        MOVE MED-MFSFEL        TO MOD-TEMFSFEL                            
048500        PERFORM MFS-RENSA-FAELT-UT                                        
048600     ELSE                                                                 
048610        MOVE GSPR-TISTADAT     TO WS-TISTADAT                             
048620        MOVE WS-TISTADAT       TO MOD-TISTADAT-GRP                        
048630        IF GSPR-TISTADAT >= DAGENS-DATUM                                  
048640           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTADAT-GRP-ATTR            
048650        END-IF                                                            
048660        IF GSPR-FLAUTUPD = 'N'                                            
048670           MOVE 'NO'           TO MOD-FLAUTUPD                            
048680        ELSE                                                              
048690           MOVE 'YES'          TO MOD-FLAUTUPD                            
048691        END-IF                                                            
048700                                                                          
048710        MOVE NEJ TO RADER-FINNS-SW                                        
048800        PERFORM FA-LAES-NAESTA-WDF811                                     
048900                                                                          
049000        MOVE +1 TO INDX                                                   
049100        PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                   
049200           IF SEGMENT-FINNS                                               
049300             IF INDX = 1                                                  
049400                MOVE DSPR-IDDISTR-FOM   TO SPAR-IDDISTR-FOM-ENTER         
049500                MOVE DSPR-IDDISTR-TOM   TO SPAR-IDDISTR-TOM-ENTER         
049600                MOVE DSPR-IDKUNDNR-FOM  TO SPAR-IDKUNDNR-FOM-ENTER        
049700                MOVE DSPR-IDKUNDNR-TOM  TO SPAR-IDKUNDNR-TOM-ENTER        
049710                MOVE JA                 TO RADER-FINNS-SW                 
049800             END-IF                                                       
049900             MOVE DSPR-IDDISTR-FOM     TO MOD-IDDISTR-FOM   (INDX)        
050000                                          SPAR-IDDISTR-FOM  (INDX)        
050100             MOVE DSPR-IDDISTR-TOM     TO MOD-IDDISTR-TOM   (INDX)        
050200                                          SPAR-IDDISTR-TOM  (INDX)        
050300             MOVE DSPR-IDKUNDNR-FOM    TO MOD-IDKUNDNR-FOM  (INDX)        
050400                                          SPAR-IDKUNDNR-FOM (INDX)        
050500             MOVE DSPR-IDKUNDNR-TOM    TO MOD-IDKUNDNR-TOM  (INDX)        
050600                                          SPAR-IDKUNDNR-TOM (INDX)        
050700                                                                          
050800             MOVE +1 TO KV-IX                                             
050900             PERFORM UNTIL KV-IX > MAX-KV-IX                              
051000                                                                          
051100*---OM MARKNADSSPÄRR ÄR NEJ LÄGGS EJ DET UT                               
051200*---MAN BLANKAR UT DATUMFÄLTET ISTÄLLET                                   
051300              MOVE DAGENS-DATUM             TO WS-TISTADAT                
051400              IF DSPR-FLMARKSP (KV-IX) = JA                               
052100                MOVE DSPR-TISTADAT (KV-IX) TO WS-TISTADAT                 
052300                MOVE WS-TISTADAT  TO MOD-TISTADAT (INDX, KV-IX)           
052310                IF DSPR-TISTADAT (KV-IX) > DAGENS-DATUM                   
052320                   MOVE MFS-ADD-LYS-UPP-FAELT TO                          
052321                        MOD-TISTADAT-ATTR (INDX, KV-IX)                   
052322                ELSE                                                      
052400                                                                          
052700*---FÖR ATT TA BORT NOLLOR DÄR DET INTE FINNS DATUM UTLAGT                
052800*---KLARAR DATUM SOM BÖRJAR PÅ 00MMDD                                     
052900                  IF MOD-TISTADAT (INDX, KV-IX) = '000000'                
053000                    INSPECT MOD-TISTADAT (INDX, KV-IX)                    
053100                                  REPLACING LEADING ZERO BY SPACE         
053200                  END-IF                                                  
053210                END-IF                                                    
053300              ELSE                                                        
053400               MOVE '000000'      TO MOD-TISTADAT (INDX, KV-IX)           
053500               INSPECT MOD-TISTADAT (INDX, KV-IX)                         
053600                                REPLACING LEADING ZERO BY SPACE           
053700              END-IF                                                      
053800              ADD +1 TO KV-IX                                             
053900             END-PERFORM                                                  
055100           END-IF                                                         
055200           ADD +1 TO INDX                                                 
055210           PERFORM FA-LAES-NAESTA-WDF811                                  
055400        END-PERFORM                                                       
055500                                                                          
055600        IF SEGMENT-FINNS                                                  
055700           MOVE DSPR-IDDISTR-FOM     TO SPAR-IDDISTR-FOM-NEXT             
055800           MOVE DSPR-IDDISTR-TOM     TO SPAR-IDDISTR-TOM-NEXT             
055900           MOVE DSPR-IDKUNDNR-FOM    TO SPAR-IDKUNDNR-FOM-NEXT            
056000           MOVE DSPR-IDKUNDNR-TOM    TO SPAR-IDKUNDNR-TOM-NEXT            
056100           IF MED-IDMFSINF NOT = INF-UPDATE-DONE                          
056200              MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                   
056300              CALL WMEDKONV USING MED-WMEDAREA                            
056400              MOVE MED-TEMFSINF         TO MOD-TEMFSINF                   
056500           END-IF                                                         
056510        ELSE                                                              
056520           MOVE SPAR-IDDISTR-FOM-ENTER  TO SPAR-IDDISTR-FOM-NEXT          
056530           MOVE SPAR-IDDISTR-TOM-ENTER  TO SPAR-IDDISTR-TOM-NEXT          
056540           MOVE SPAR-IDKUNDNR-FOM-ENTER TO SPAR-IDKUNDNR-FOM-NEXT         
056550           MOVE SPAR-IDKUNDNR-TOM-ENTER TO SPAR-IDKUNDNR-TOM-NEXT         
056560           PERFORM UNTIL INDX > MAX-INDX                                  
056570              MOVE MFS-RENSA-FAELT  TO MOD-IDDISTR-FOM  (INDX)            
056580                                       MOD-IDDISTR-TOM  (INDX)            
056590                                       MOD-IDKUNDNR-FOM (INDX)            
056591                                       MOD-IDKUNDNR-TOM (INDX)            
056592              MOVE MFS-STAENG-FAELT TO MOD-CMD-ATTR (INDX)                
056593                                                                          
056594              MOVE +1 TO KV-IX                                            
056595              PERFORM UNTIL KV-IX > MAX-KV-IX                             
056596               MOVE MFS-RENSA-FAELT TO MOD-TISTADAT (INDX, KV-IX)         
056597               ADD +1 TO KV-IX                                            
056598              END-PERFORM                                                 
056599              ADD 1 TO  INDX                                              
056600           END-PERFORM                                                    
056610        END-IF                                                            
056700     END-IF                                                               
056800                                                                          
056810     IF RADER-FINNS                                                       
056900       MOVE '002'                  TO MSGI-KDCALL                         
057000       MOVE '2339'                 TO SPAR-IDTRANS                        
057100       MOVE SPAR-AREA              TO MSGI-SPAR-AREA                      
057200       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
057210     END-IF                                                               
057300     .                                                                    
057400                                                                          
057410                                                                          
057500 FA-LAES-NAESTA-WDF811 SECTION.                                           
057510     MOVE 'FA-NAESTA-WDF811' TO CURRENT-SECTION                           
057520                                                                          
057602     MOVE NEJ TO SW-SEGMENT-OK                                            
057603     PERFORM IMS-GNP-WDF811                                               
057604                                                                          
057605     IF SPAR-IDTRANS = '2339'                                             
057606      PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-OK                          
057609        MOVE JA  TO SW-SEGMENT-OK                                         
057610        IF  SPAR-IDDISTR-KEY  = ZERO                                      
057611        AND SPAR-IDKUNDNR-KEY = ZERO                                      
057612            CONTINUE                                                      
057613        ELSE                                                              
057614            IF SPAR-IDDISTR-KEY  > ZERO                                   
057615               IF  SPAR-IDDISTR-KEY  NOT < DSPR-IDDISTR-FOM               
057616               AND SPAR-IDDISTR-KEY  NOT > DSPR-IDDISTR-TOM               
057618                   CONTINUE                                               
057619               ELSE                                                       
057621                   MOVE NEJ TO SW-SEGMENT-OK                              
057622               END-IF                                                     
057623            END-IF                                                        
057624            IF SPAR-IDKUNDNR-KEY  > ZERO                                  
057626               IF  SPAR-IDKUNDNR-KEY  NOT < DSPR-IDKUNDNR-FOM             
057627               AND SPAR-IDKUNDNR-KEY  NOT > DSPR-IDKUNDNR-TOM             
057628                   CONTINUE                                               
057629               ELSE                                                       
057631                   MOVE NEJ TO SW-SEGMENT-OK                              
057632               END-IF                                                     
057633            END-IF                                                        
057634        END-IF                                                            
057635                                                                          
057637        IF NOT SEGMENT-OK                                                 
057638           PERFORM IMS-GNP-WDF811                                         
057639        END-IF                                                            
057640      END-PERFORM                                                         
057641     END-IF                                                               
057642     .                                                                    
057643                                                                          
057644                                                                          
057645 G-KOLLA-INPUT SECTION.                                                   
057650     MOVE 'G-KOLLA-INPUT   ' TO CURRENT-SECTION                           
057660                                                                          
057700     MOVE JA           TO INDATA-SW                                       
057800                          NY-UPPD-POST-SW                                 
057900     MOVE NEJ          TO CMD-SW                                          
057910     MOVE NEJ          TO E-RAD-FEL-SW                                    
057920     MOVE NEJ          TO RAD-VALD-SW                                     
057930     MOVE NEJ          TO E-DIST-FOM-FEL-SW                               
057931     MOVE NEJ          TO E-DIST-TOM-FEL-SW                               
057940     MOVE NEJ          TO E-KUND-FOM-FEL-SW                               
057941     MOVE NEJ          TO E-KUND-TOM-FEL-SW                               
057950     MOVE NEJ          TO E-DAT1-FEL-SW                                   
057951     MOVE NEJ          TO E-DAT2-FEL-SW                                   
057952     MOVE NEJ          TO E-DAT3-FEL-SW                                   
057953     MOVE NEJ          TO E-DAT4-FEL-SW                                   
057954     MOVE NEJ          TO E-DAT5-FEL-SW                                   
058000                                                                          
058001     IF SPAR-IDTRANS = '2339'                                             
058002       IF SPAR-IDDISTR-FOM-ENTER NUMERIC                                  
058010         MOVE SPAR-IDDISTR-FOM-ENTER  TO W-IDDISTR-FOM                    
058011       END-IF                                                             
058012       IF SPAR-IDDISTR-TOM-ENTER NUMERIC                                  
058020         MOVE SPAR-IDDISTR-TOM-ENTER  TO W-IDDISTR-TOM                    
058021       END-IF                                                             
058022       IF SPAR-IDKUNDNR-FOM-ENTER NUMERIC                                 
058030         MOVE SPAR-IDKUNDNR-FOM-ENTER TO W-IDKUNDNR-FOM                   
058031       END-IF                                                             
058032       IF SPAR-IDKUNDNR-TOM-ENTER NUMERIC                                 
058040         MOVE SPAR-IDKUNDNR-TOM-ENTER TO W-IDKUNDNR-TOM                   
058041       END-IF                                                             
058042     END-IF                                                               
058050                                                                          
058100     MOVE +1           TO INDX                                            
058200     PERFORM UNTIL INDX > MAX-INDX                                        
058300       IF MID-CMD (INDX) NOT = '+' AND ' '                                
058310          IF MID-CMD (INDX) = 'D'                                         
058320             MOVE JA   TO RAD-VALD-SW                                     
058330             MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR (INDX)             
058340          ELSE                                                            
058350             MOVE MFS-ALFA-FAELT-FEL   TO MOD-CMD-ATTR (INDX)             
058360             CALL WMEDKONV USING MED-WMEDAREA                             
058370             MOVE MED-MFSFEL       TO MOD-TEMFSFEL                        
058380             PERFORM MFS-ROER-EJ-FAELT-UT                                 
058390             PERFORM MFS-ROER-EJ-FAELT-IN                                 
058391             PERFORM MFS-ROER-EJ-FAELT-IN-E                               
058392             MOVE NEJ TO INDATA-SW                                        
058393          END-IF                                                          
058600       END-IF                                                             
058700       ADD +1          TO INDX                                            
058800     END-PERFORM                                                          
058900                                                                          
059000     IF INDATA-OK                                                         
059100        IF  MID-CMD-E = '+' OR ' '                                        
059200        AND MID-IDDISTR-FOM-E  = ALL '+'                                  
059300        AND MID-IDDISTR-TOM-E  = ALL '+'                                  
059400        AND MID-IDKUNDNR-FOM-E = ALL '+'                                  
059500        AND MID-IDKUNDNR-TOM-E = ALL '+'                                  
059600        AND MID-TISTADAT-E (1) = ALL '+'                                  
059700        AND MID-TISTADAT-E (2) = ALL '+'                                  
059800        AND MID-TISTADAT-E (3) = ALL '+'                                  
059900        AND MID-TISTADAT-E (4) = ALL '+'                                  
060000        AND MID-TISTADAT-E (5) = ALL '+'                                  
060100           IF RAD-EJ-VALD                                                 
060120              MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                   
060130              CALL WMEDKONV USING MED-WMEDAREA                            
060140              MOVE MED-MFSFEL        TO MOD-TEMFSFEL                      
060150              MOVE NEJ TO INDATA-SW                                       
060160              PERFORM MFS-ROER-EJ-FAELT-UT                                
060180              PERFORM MFS-ROER-EJ-FAELT-IN-E                              
060190              MOVE NEJ TO INDATA-SW                                       
060200           END-IF                                                         
060300        END-IF                                                            
060310     END-IF                                                               
060320                                                                          
060330     IF INDATA-OK                                                         
060340        IF MID-CMD-E = 'N' OR 'C'                                         
060341           PERFORM IMS-GU-WDF801                                          
060350           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-CMD-E-ATTR                   
060360           IF MID-CMD-E = 'N'                                             
060370              PERFORM GA-KOLLA-NYPOST                                     
060380              PERFORM S01-KOLLA-DATUM-N                                   
060390           ELSE                                                           
060392              PERFORM GB-KOLLA-AENDRING                                   
060393              PERFORM S02-KOLLA-DATUM-E                                   
060395           END-IF                                                         
060397           IF INDATA-FEL                                                  
060398              CALL WMEDKONV USING MED-WMEDAREA                            
060399              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
060400              MOVE NEJ       TO ALLT-SW                                   
060401              PERFORM MFS-LAES-IN-IGEN-E                                  
060402           END-IF                                                         
060403        ELSE                                                              
060404           IF RAD-EJ-VALD                                                 
060414              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
060415              CALL WMEDKONV USING MED-WMEDAREA                            
060416              MOVE MED-MFSFEL           TO MOD-TEMFSFEL                   
060420              MOVE JA                   TO E-RAD-FEL-SW                   
060421              MOVE NEJ                  TO ALLT-SW                        
060430              MOVE MFS-ALFA-FAELT-FEL   TO MOD-CMD-E-ATTR                 
060440           END-IF                                                         
060450        END-IF                                                            
060460     END-IF                                                               
060500     .                                                                    
061000                                                                          
068300 GA-KOLLA-NYPOST SECTION.                                                 
068310     MOVE 'GA-KOLLA-NYPOST ' TO CURRENT-SECTION                           
068400                                                                          
068500     INSPECT MID-IDDISTR-FOM-E REPLACING LEADING SPACE BY ZERO            
068600     INSPECT MID-IDDISTR-TOM-E REPLACING LEADING SPACE BY ZERO            
068700                                                                          
068800     IF MID-IDDISTR-FOM-E NOT = ALL '+'                                   
068900       IF MID-IDDISTR-FOM-E NUMERIC                                       
069000         IF MID-IDDISTR-FOM-E > 0                                         
069100           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-FOM-E-ATTR             
069200         ELSE                                                             
069300           MOVE NEJ                 TO INDATA-SW                          
069400           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-FOM-E-ATTR             
069410           MOVE JA                  TO E-DIST-FOM-FEL-SW                  
069420           MOVE JA                  TO E-RAD-FEL-SW                       
069500         END-IF                                                           
069600       ELSE                                                               
069700         MOVE NEJ                   TO INDATA-SW                          
069800         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-FOM-E-ATTR             
069900       END-IF                                                             
070000     ELSE                                                                 
070100       MOVE NEJ                     TO INDATA-SW                          
070200       MOVE MFS-NUM-FAELT-FEL       TO MOD-IDDISTR-FOM-E-ATTR             
070300     END-IF                                                               
070400                                                                          
070500     IF INDATA-OK                                                         
070600       IF MID-IDDISTR-TOM-E NOT = ALL '+'                                 
070700         IF MID-IDDISTR-FOM-E NOT = ALL '+'                               
070800           IF MID-IDDISTR-TOM-E NUMERIC                                   
070900             IF MID-IDDISTR-TOM-E NOT <  MID-IDDISTR-FOM-E                
071000                MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-TOM-E-ATTR        
071100             ELSE                                                         
071200               MOVE NEJ TO INDATA-SW                                      
071300               MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-TOM-E-ATTR         
071310               MOVE JA                  TO E-DIST-TOM-FEL-SW              
071320               MOVE JA                  TO E-RAD-FEL-SW                   
071400             END-IF                                                       
071500           ELSE                                                           
071600             MOVE NEJ TO INDATA-SW                                        
071700             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-TOM-E-ATTR           
071710             MOVE JA                  TO E-DIST-TOM-FEL-SW                
071720             MOVE JA                  TO E-RAD-FEL-SW                     
071800           END-IF                                                         
071900         ELSE                                                             
072000           MOVE NEJ TO INDATA-SW                                          
072100           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-TOM-E-ATTR           
072110           MOVE JA                  TO E-DIST-TOM-FEL-SW                  
072120           MOVE JA                  TO E-RAD-FEL-SW                       
072200         END-IF                                                           
072300       ELSE                                                               
072400         MOVE MID-IDDISTR-FOM-E       TO MID-IDDISTR-TOM-E                
072500         MOVE MFS-NUM-FAELT-RAETT     TO MOD-IDDISTR-TOM-E-ATTR           
072510         MOVE JA                  TO E-DIST-TOM-FEL-SW                    
072520         MOVE JA                  TO E-RAD-FEL-SW                         
072600       END-IF                                                             
072700     END-IF                                                               
072800                                                                          
072900     IF INDATA-OK                                                         
073000       IF MID-IDKUNDNR-FOM-E NOT = ALL '+'                                
073100         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-FOM-E-ATTR              
073200       ELSE                                                               
073300         MOVE JA                  TO KUNDNR-FOM-NOLL                      
073400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-FOM-E-ATTR              
073500       END-IF                                                             
073600     END-IF                                                               
073700                                                                          
073800     IF INDATA-OK                                                         
073900       IF MID-IDKUNDNR-TOM-E NOT = ALL '+'                                
074000         IF MID-IDKUNDNR-FOM-E NOT = ALL '+'                              
074100           IF MID-IDKUNDNR-TOM-E NOT <  MID-IDKUNDNR-FOM-E                
074200             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-TOM-E-ATTR          
074300           ELSE                                                           
074400             MOVE NEJ                 TO INDATA-SW                        
074500             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-TOM-E-ATTR          
074510             MOVE JA                  TO E-KUND-TOM-FEL-SW                
074520             MOVE JA                  TO E-RAD-FEL-SW                     
074600           END-IF                                                         
074700         ELSE                                                             
074800           MOVE NEJ                   TO INDATA-SW                        
074900           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKUNDNR-TOM-E-ATTR          
074910           MOVE JA                    TO E-KUND-TOM-FEL-SW                
074920           MOVE JA                    TO E-RAD-FEL-SW                     
075000         END-IF                                                           
075100       ELSE                                                               
075200         IF MID-IDKUNDNR-FOM-E NOT = ALL '+'                              
075300           MOVE MID-IDKUNDNR-FOM-E  TO MID-IDKUNDNR-TOM-E                 
075400           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-TOM-E-ATTR            
075500         ELSE                                                             
075600           MOVE 999999                TO MID-IDKUNDNR-TOM-E               
075700           MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDKUNDNR-TOM-E-ATTR          
075800         END-IF                                                           
075900       END-IF                                                             
076000     END-IF                                                               
076100                                                                          
076200     IF KUNDNR-FOM-NOLL = JA                                              
076300       MOVE ZERO TO MID-IDKUNDNR-FOM-E                                    
076400     END-IF                                                               
076500                                                                          
076600     IF INDATA-OK                                                         
076700       IF MID-IDDISTR-FOM-E  = MID-IDDISTR-TOM-E AND                      
076800          MID-IDKUNDNR-FOM-E = MID-IDKUNDNR-TOM-E                         
076900         MOVE MID-IDDISTR-FOM-E       TO W-IDDISTR                        
077000         MOVE MID-IDKUNDNR-FOM-E      TO W-IDKUNDNR                       
077100         PERFORM IMS-GU-WDB201                                            
077200         IF SEGMENT-SAKNAS                                                
077300           MOVE NEJ TO INDATA-SW                                          
077400           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKUNDNR-FOM-E-ATTR          
077500                                         MOD-IDKUNDNR-TOM-E-ATTR          
077510           MOVE JA                    TO E-KUND-FOM-FEL-SW                
077520           MOVE JA                    TO E-KUND-TOM-FEL-SW                
077530           MOVE JA                    TO E-RAD-FEL-SW                     
077600           MOVE ERR-DIST-CUST-MISSING TO MED-IDMFSFEL                     
077700                                         MSG-KOM-IDMFSMED                 
077800           CALL WMEDKONV USING MED-WMEDAREA                               
077900           MOVE MED-TEMFSFEL          TO MOD-TEMFSFEL                     
078000         END-IF                                                           
078100       END-IF                                                             
078200     END-IF                                                               
078300                                                                          
078400     IF INDATA-OK                                                         
078500       PERFORM IMS-GU-WDF801                                              
078600       MOVE MID-IDDISTR-FOM-E          TO W-IDDISTR-FOM                   
078700       MOVE MID-IDDISTR-TOM-E          TO W-IDDISTR-TOM                   
078800       MOVE MID-IDKUNDNR-FOM-E         TO W-IDKUNDNR-FOM                  
078900       MOVE MID-IDKUNDNR-TOM-E         TO W-IDKUNDNR-TOM                  
079000       PERFORM  IMS-GNP-WDF811-OKVAL                                      
079100                                                                          
079200       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
079300                     INDATA-FEL                                           
079400         IF SEGMENT-FINNS                                                 
079500           IF MID-IDDISTR-FOM-E NOT = MID-IDDISTR-TOM-E                   
079600             IF (DSPR-IDDISTR-FOM >= W-IDDISTR-FOM AND                    
079700                 DSPR-IDDISTR-FOM <= W-IDDISTR-TOM) OR                    
079800                (DSPR-IDDISTR-TOM >= W-IDDISTR-FOM AND                    
079900                 DSPR-IDDISTR-TOM <= W-IDDISTR-TOM)                       
080000               MOVE NEJ TO INDATA-SW                                      
080100               MOVE MFS-NUM-FAELT-FEL  TO MOD-IDDISTR-FOM-E-ATTR          
080200                                          MOD-IDDISTR-TOM-E-ATTR          
080210               MOVE JA                 TO E-DIST-FOM-FEL-SW               
080220               MOVE JA                 TO E-DIST-TOM-FEL-SW               
080230               MOVE JA                 TO E-RAD-FEL-SW                    
080300               MOVE ERR-WRONG-INTERVAL TO MED-IDMFSFEL                    
080400                                          MSG-KOM-IDMFSMED                
080500             END-IF                                                       
080600           ELSE                                                           
080700             IF (DSPR-IDDISTR-FOM  = W-IDDISTR-FOM   AND                  
080800                 DSPR-IDKUNDNR-FOM >= W-IDKUNDNR-FOM AND                  
080900                 DSPR-IDKUNDNR-FOM <= W-IDKUNDNR-TOM) OR                  
081000                (DSPR-IDDISTR-FOM  = W-IDDISTR-FOM   AND                  
081100                 DSPR-IDKUNDNR-TOM >= W-IDKUNDNR-FOM AND                  
081200                 DSPR-IDKUNDNR-TOM <= W-IDKUNDNR-TOM)                     
081300               MOVE NEJ TO INDATA-SW                                      
081400               MOVE MFS-NUM-FAELT-FEL  TO MOD-IDDISTR-FOM-E-ATTR          
081500                                          MOD-IDDISTR-TOM-E-ATTR          
081600                                          MOD-IDKUNDNR-TOM-E-ATTR         
081700                                          MOD-IDKUNDNR-FOM-E-ATTR         
081710               MOVE JA                 TO E-DIST-FOM-FEL-SW               
081720               MOVE JA                 TO E-DIST-TOM-FEL-SW               
081730               MOVE JA                 TO E-KUND-FOM-FEL-SW               
081740               MOVE JA                 TO E-KUND-TOM-FEL-SW               
081750               MOVE JA                 TO E-RAD-FEL-SW                    
081800               MOVE ERR-WRONG-INTERVAL TO MED-IDMFSFEL                    
081900                                          MSG-KOM-IDMFSMED                
082000             END-IF                                                       
082100           END-IF                                                         
082200           PERFORM IMS-GNP-WDF811-OKVAL                                   
082300         END-IF                                                           
082400       END-PERFORM                                                        
082500     END-IF                                                               
082510                                                                          
082520     IF INDATA-FEL                                                        
082521        CALL WMEDKONV USING MED-WMEDAREA                                  
082522        MOVE MED-MFSFEL      TO MOD-TEMFSFEL                              
082523        MOVE NEJ             TO ALLT-SW                                   
082530     END-IF                                                               
082600     .                                                                    
082700                                                                          
082710                                                                          
082800 GB-KOLLA-AENDRING SECTION.                                               
082810     MOVE 'GB-KOLLA-AENDR  ' TO CURRENT-SECTION                           
082820                                                                          
083000     INSPECT MID-IDDISTR-FOM-E REPLACING LEADING SPACE BY ZERO            
083100     INSPECT MID-IDDISTR-TOM-E REPLACING LEADING SPACE BY ZERO            
083200                                                                          
083600     MOVE MID-IDDISTR-FOM-E   TO W-IDDISTR-FOM                            
084900     MOVE MID-IDDISTR-TOM-E   TO W-IDDISTR-TOM                            
086200     MOVE MID-IDKUNDNR-FOM-E  TO W-IDKUNDNR-FOM                           
087500     MOVE MID-IDKUNDNR-TOM-E  TO W-IDKUNDNR-TOM                           
088600     PERFORM IMS-GHU-WDF811                                               
088700     IF SEGMENT-SAKNAS                                                    
088800        MOVE ERR-MISSING-REG  TO MED-IDMFSFEL                             
089000        CALL WMEDKONV USING MED-WMEDAREA                                  
089100        MOVE MED-MFSFEL       TO MOD-TEMFSFEL                             
089200        MOVE NEJ              TO INDATA-SW                                
089300        PERFORM MFS-RENSA-FAELT-UT                                        
089400     END-IF                                                               
090200     .                                                                    
092000                                                                          
092010                                                                          
092100 H-UPPDATERA SECTION.                                                     
092110     MOVE 'H-UPPDATERA     ' TO CURRENT-SECTION                           
092200                                                                          
092600     IF INDATA-OK                                                         
092700                                                                          
092800        IF MID-CMD-E = 'N'                                                
093000           PERFORM HA-SKAPA-NYPOST                                        
093100        ELSE                                                              
093200           PERFORM HB-UPPDATERA-POST                                      
093300        END-IF                                                            
093310                                                                          
093400        PERFORM HC-TA-BORT-POSTER                                         
093700     END-IF                                                               
093800                                                                          
093900*---FÖR ATT POSITIONERA SIG VID LÄSNING AV 1:A POST                       
093910     IF SPAR-IDTRANS = '2339'                                             
094000       MOVE SPAR-IDDISTR-FOM-ENTER  TO W-IDDISTR-FOM                      
094100       MOVE SPAR-IDDISTR-TOM-ENTER  TO W-IDDISTR-TOM                      
094200       MOVE SPAR-IDKUNDNR-FOM-ENTER TO W-IDKUNDNR-FOM                     
094300       MOVE SPAR-IDKUNDNR-TOM-ENTER TO W-IDKUNDNR-TOM                     
094310     END-IF                                                               
094400                                                                          
094500     MOVE INF-UPDATE-DONE         TO MED-IDMFSINF                         
094600                                     MSG-KOM-IDMFSMED                     
094700     CALL WMEDKONV USING MED-WMEDAREA                                     
094800     MOVE MED-MFSINF              TO MOD-TEMFSINF                         
094900     PERFORM MFS-FORM-ATTR                                                
095000     PERFORM MFS-RENSA-FAELT-IN                                           
095100     .                                                                    
095200                                                                          
095210                                                                          
095300 HA-SKAPA-NYPOST SECTION.                                                 
095310     MOVE 'HA-SKAPA-NYPOST ' TO CURRENT-SECTION                           
095400                                                                          
095710     MOVE W-IDDISTR-FOM        TO DSPR-IDDISTR-FOM                        
095720     MOVE W-IDDISTR-TOM        TO DSPR-IDDISTR-TOM                        
095800     IF W-IDDISTR-FOM NOT = W-IDDISTR-TOM                                 
095900       MOVE 0                  TO DSPR-IDKUNDNR-FOM                       
096000       MOVE 999999             TO DSPR-IDKUNDNR-TOM                       
096100     ELSE                                                                 
096200       MOVE W-IDKUNDNR-FOM     TO DSPR-IDKUNDNR-FOM                       
096300       MOVE W-IDKUNDNR-TOM     TO DSPR-IDKUNDNR-TOM                       
096400     END-IF                                                               
096500                                                                          
096600     MOVE +1 TO KV-IX                                                     
096700     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
096900       IF MID-TISTADAT-E (KV-IX) > ZERO AND                               
097000          MID-TISTADAT-E (KV-IX) NOT = 990101                             
097100         MOVE JA                     TO DSPR-FLMARKSP (KV-IX)             
097200         MOVE MID-TISTADAT-E (KV-IX) TO DSPR-TISTADAT (KV-IX)             
097300       ELSE                                                               
097400         MOVE NEJ                    TO DSPR-FLMARKSP (KV-IX)             
097500         MOVE ZERO                   TO DSPR-TISTADAT (KV-IX)             
097600       END-IF                                                             
097800       ADD +1 TO KV-IX                                                    
097900     END-PERFORM                                                          
098000                                                                          
098100     PERFORM IMS-ISRT-WDF811                                              
098200     .                                                                    
098300                                                                          
098310                                                                          
098400 HB-UPPDATERA-POST SECTION.                                               
098410     MOVE 'HB-UPPDATERA    ' TO CURRENT-SECTION                           
098500                                                                          
098600     PERFORM IMS-GHU-WDF811                                               
098700     IF SEGMENT-FINNS                                                     
099200       MOVE +1 TO KV-IX                                                   
099300       PERFORM UNTIL KV-IX > MAX-KV-IX                                    
099400         IF MID-TISTADAT-E (KV-IX) NOT = ALL '+'                          
099500           MOVE MID-TISTADAT-E (KV-IX)   TO TMP1-YYMMDD                   
099600           MOVE DAGENS-DATUM             TO TMP2-YYMMDD                   
099700           PERFORM WY2000P1                                               
099800           IF TMP1-YYMMDD >= TMP2-YYMMDD                                  
100000             MOVE JA                     TO DSPR-FLMARKSP (KV-IX)         
100100             MOVE MID-TISTADAT-E (KV-IX) TO DSPR-TISTADAT (KV-IX)         
100200           ELSE                                                           
100300             IF TMP1-YYMMDD = +0 OR                                       
100300               (TMP1-YYMMDD < TMP2-YYMMDD AND                             
100300               MID-TISTADAT-E (KV-IX) NOT = DSPR-TISTADAT (KV-IX))        
100300               MOVE NEJ                  TO DSPR-FLMARKSP (KV-IX)         
100400               MOVE +0                   TO DSPR-TISTADAT (KV-IX)         
100400               ADD +1                    TO W-FL-N-CNT                    
100400             END-IF                                                       
100400           END-IF                                                         
100600         ELSE                                                             
100700           IF DSPR-FLMARKSP (KV-IX) = NEJ AND                             
100700              DSPR-TISTADAT (KV-IX) > +0                                  
100800             MOVE +0                     TO DSPR-TISTADAT (KV-IX)         
100800             ADD +1                      TO W-FL-N-CNT                    
100800           ELSE                                                           
100800             IF DSPR-TISTADAT (KV-IX) = +0 AND                            
100800                DSPR-FLMARKSP (KV-IX) = JA                                
100800               MOVE NEJ                  TO DSPR-FLMARKSP (KV-IX)         
100800               ADD +1                    TO W-FL-N-CNT                    
100800             END-IF                                                       
100900           END-IF                                                         
101000         END-IF                                                           
101100                                                                          
101200         ADD +1 TO KV-IX                                                  
101300       END-PERFORM                                                        
101400                                                                          
101400       IF W-FL-N-CNT = MAX-KV-IX                                          
101400         PERFORM IMS-DLET-WDF811                                          
101400       ELSE                                                               
101400         PERFORM IMS-REPL-WDF811                                          
101400       END-IF                                                             
101400                                                                          
101400       MOVE +0               TO W-FL-N-CNT                                
101500                                                                          
101510     END-IF                                                               
101600     .                                                                    
101700                                                                          
101710                                                                          
101800 HC-TA-BORT-POSTER SECTION.                                               
101810     MOVE 'HC-TA-BORT-POST ' TO CURRENT-SECTION                           
101900                                                                          
102000     MOVE +1 TO INDX                                                      
102100     PERFORM UNTIL INDX > MAX-INDX                                        
102200       IF MID-CMD (INDX)  = 'D'                                           
102300         MOVE MID-IDDISTR-FOM  (INDX) TO W-IDDISTR-FOM                    
102400         MOVE MID-IDDISTR-TOM  (INDX) TO W-IDDISTR-TOM                    
102500         MOVE MID-IDKUNDNR-FOM (INDX) TO W-IDKUNDNR-FOM                   
102600         MOVE MID-IDKUNDNR-TOM (INDX) TO W-IDKUNDNR-TOM                   
102700         PERFORM IMS-GHU-WDF811                                           
102800         IF SEGMENT-FINNS                                                 
102900           PERFORM IMS-DLET-WDF811                                        
103000         END-IF                                                           
103100       END-IF                                                             
103200       ADD +1 TO INDX                                                     
103300     END-PERFORM                                                          
103400     .                                                                    
103500                                                                          
103510                                                                          
103600 S01-KOLLA-DATUM-N SECTION.                                               
103610     MOVE 'S01-KOLLA-DATUM ' TO CURRENT-SECTION                           
103700                                                                          
103800     MOVE +1 TO DAT-IX                                                    
103900     PERFORM UNTIL DAT-IX > MAX-DAT-IX                                    
104000       MOVE JA                      TO DAT-SW                             
104100       IF MID-TISTADAT-E (DAT-IX) = ALL '+' OR ' '                        
104200*---OM MAN EJ MATAT IN DATUM TAS DUMMY-DATUM                              
104300         MOVE 990101                TO MID-TISTADAT-E (DAT-IX)            
104310       ELSE                                                               
104320          INSPECT MID-TISTADAT-E (DAT-IX)                                 
104330                  REPLACING LEADING SPACE BY ZERO                         
104400       END-IF                                                             
104500                                                                          
104600*---KOLLAR SÅ ATT DATUM ÄR STÖRRE ELLER LIKA MED DAGENS DATUM             
104700       MOVE MID-TISTADAT-E (DAT-IX) TO WS-TISTADAT                        
104800                                                                          
105400       IF (WS-TISTADAT >= DAGENS-DATUM                                    
105410       AND WS-TISTADAT >= GSPR-TISTADAT) OR                               
105500           WS-TISTADAT  = 990101                                          
105600         MOVE MFS-ALFA-FAELT-RAETT                                        
105700                                   TO MOD-TISTADAT-E-ATTR (DAT-IX)        
105800       ELSE                                                               
105900         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
106000                                      MSG-KOM-IDMFSMED                    
106100         MOVE MFS-ALFA-FAELT-FEL                                          
106200                                   TO MOD-TISTADAT-E-ATTR (DAT-IX)        
106201         EVALUATE DAT-IX                                                  
106202           WHEN 1                                                         
106210               MOVE JA                 TO E-DAT1-FEL-SW                   
106220           WHEN 2                                                         
106230               MOVE JA                 TO E-DAT2-FEL-SW                   
106240           WHEN 3                                                         
106250               MOVE JA                 TO E-DAT3-FEL-SW                   
106260           WHEN 4                                                         
106270               MOVE JA                 TO E-DAT4-FEL-SW                   
106280           WHEN 5                                                         
106290               MOVE JA                 TO E-DAT5-FEL-SW                   
106291         END-EVALUATE                                                     
106292         MOVE JA                       TO E-RAD-FEL-SW                    
106300         MOVE NEJ                  TO INDATA-SW                           
106310             MOVE NEJ             TO ALLT-SW                              
106400       END-IF                                                             
106500                                                                          
106600       IF DAT-OK                                                          
106700*---VALIDERING AV DATUM                                                   
106800         MOVE 'AAMMDD'                TO DAT-KDDATFORM                    
106900         MOVE MID-TISTADAT-E (DAT-IX) TO DAT-I-TIDATUM                    
107000                                                                          
107100         CALL WDATKONV USING          DAT-KDDATFORM                       
107200                                      DAT-I-TIDATUM                       
107300                                      DAT-O-TIDATUM                       
107400                                      DAT-KDSVAR                          
107500         IF DAT-KDSVAR-FEL                                                
107600           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
107700           MOVE MFS-ALFA-FAELT-FEL TO MOD-TISTADAT-E-ATTR (DAT-IX)        
107800           MOVE NEJ                TO INDATA-SW                           
107900           MOVE NEJ                TO ALLT-SW                             
108000         END-IF                                                           
108100       END-IF                                                             
108200       ADD +1 TO DAT-IX                                                   
108300     END-PERFORM                                                          
108400     .                                                                    
108500                                                                          
108510                                                                          
108600 S02-KOLLA-DATUM-E SECTION.                                               
108610     MOVE 'S02-KOLLA-DATUME' TO CURRENT-SECTION                           
108700                                                                          
108800     MOVE +1 TO DAT-IX                                                    
108900     PERFORM UNTIL DAT-IX > MAX-DAT-IX                                    
109000       MOVE JA                  TO DAT-SW                                 
109100                                                                          
109200       IF MID-TISTADAT-E (DAT-IX) NOT = ALL '+'                           
109220         INSPECT MID-TISTADAT-E (DAT-IX)                                  
109230                 REPLACING LEADING SPACE BY ZERO                          
109300         IF MID-TISTADAT-E (DAT-IX) > 0                                   
109400           MOVE MID-TISTADAT-E (DAT-IX) TO WS-TISTADAT                    
109500                                                                          
110100           IF  WS-TISTADAT >= DAGENS-DATUM                                
110110           AND WS-TISTADAT >= GSPR-TISTADAT                               
110200             MOVE MFS-NUM-FAELT-RAETT                                     
110300                                  TO MOD-TISTADAT-E-ATTR (DAT-IX)         
110400           ELSE                                                           
110500             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
110600                                          MSG-KOM-IDMFSMED                
110700             MOVE MFS-NUM-FAELT-FEL                                       
110800                                  TO MOD-TISTADAT-E-ATTR (DAT-IX)         
110900             MOVE NEJ             TO INDATA-SW                            
110910             MOVE NEJ             TO ALLT-SW                              
111000             MOVE NEJ             TO DAT-SW                               
111010             EVALUATE DAT-IX                                              
111020               WHEN 1                                                     
111030                   MOVE JA             TO E-DAT1-FEL-SW                   
111040               WHEN 2                                                     
111050                   MOVE JA             TO E-DAT2-FEL-SW                   
111060               WHEN 3                                                     
111070                   MOVE JA             TO E-DAT3-FEL-SW                   
111080               WHEN 4                                                     
111090                   MOVE JA             TO E-DAT4-FEL-SW                   
111091               WHEN 5                                                     
111092                   MOVE JA             TO E-DAT5-FEL-SW                   
111093             END-EVALUATE                                                 
111094             MOVE JA                   TO E-RAD-FEL-SW                    
111100           END-IF                                                         
111200                                                                          
111300           IF DAT-OK                                                      
111400*---VALIDERING AV DATUM                                                   
111500             MOVE 'AAMMDD'                TO DAT-KDDATFORM                
111600             MOVE MID-TISTADAT-E (DAT-IX) TO DAT-I-TIDATUM                
111700                                                                          
111800             CALL WDATKONV USING          DAT-KDDATFORM                   
111900                                          DAT-I-TIDATUM                   
112000                                          DAT-O-TIDATUM                   
112100                                          DAT-KDSVAR                      
112200             IF DAT-KDSVAR-FEL                                            
112300               MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                 
112400               MOVE MFS-NUM-FAELT-FEL     TO                              
112500                                      MOD-TISTADAT-E-ATTR (DAT-IX)        
112600               MOVE NEJ                   TO INDATA-SW                    
112700               MOVE NEJ                   TO ALLT-SW                      
112710               EVALUATE DAT-IX                                            
112720                 WHEN 1                                                   
112730                     MOVE JA           TO E-DAT1-FEL-SW                   
112740                 WHEN 2                                                   
112750                     MOVE JA           TO E-DAT2-FEL-SW                   
112760                 WHEN 3                                                   
112770                     MOVE JA           TO E-DAT3-FEL-SW                   
112780                 WHEN 4                                                   
112790                     MOVE JA           TO E-DAT4-FEL-SW                   
112791                 WHEN 5                                                   
112792                     MOVE JA           TO E-DAT5-FEL-SW                   
112793               END-EVALUATE                                               
112794               MOVE JA                 TO E-RAD-FEL-SW                    
112800             END-IF                                                       
112900           END-IF                                                         
113000         END-IF                                                           
113100       END-IF                                                             
113200       ADD +1 TO DAT-IX                                                   
113300     END-PERFORM                                                          
113400     .                                                                    
113500                                                                          
113510                                                                          
113600 S03-E-RAD-TO-MOD   SECTION.                                              
113610     MOVE 'S03-E-RAD-TO-MOD' TO CURRENT-SECTION                           
113700                                                                          
113701     MOVE MID-CMD-E            TO MOD-CMD-E                               
113702                                                                          
113703     INSPECT MID-IDDISTR-FOM-E REPLACING LEADING SPACE BY ZERO            
113704     INSPECT MID-IDDISTR-FOM-E REPLACING LEADING '+'   BY ZERO            
113705     MOVE MID-IDDISTR-FOM-E    TO MOD-IDDISTR-FOM-E                       
113706                                                                          
113707     INSPECT MID-IDDISTR-TOM-E REPLACING LEADING SPACE BY ZERO            
113708     INSPECT MID-IDDISTR-TOM-E REPLACING LEADING '+'   BY ZERO            
113709     MOVE MID-IDDISTR-TOM-E    TO MOD-IDDISTR-TOM-E                       
113710                                                                          
113711     INSPECT MID-IDKUNDNR-FOM-E REPLACING LEADING SPACE BY ZERO           
113712     INSPECT MID-IDKUNDNR-FOM-E REPLACING LEADING '+'   BY ZERO           
113713     MOVE MID-IDKUNDNR-FOM-E   TO MOD-IDKUNDNR-FOM-E                      
113714                                                                          
113715     INSPECT MID-IDKUNDNR-TOM-E REPLACING LEADING SPACE BY ZERO           
113716     INSPECT MID-IDKUNDNR-TOM-E REPLACING LEADING '+'   BY ZERO           
113717     MOVE MID-IDKUNDNR-TOM-E   TO MOD-IDKUNDNR-TOM-E                      
113718                                                                          
113719     INSPECT MID-TISTADAT-E (1) REPLACING LEADING SPACE BY ZERO           
113720     INSPECT MID-TISTADAT-E (1) REPLACING LEADING '+'   BY ZERO           
113721     IF MID-TISTADAT-E (1) > ZERO AND < 990101                            
113722       MOVE MID-TISTADAT-E (1) TO MOD-TISTADAT-E (1)                      
113723     END-IF                                                               
113724                                                                          
113725     INSPECT MID-TISTADAT-E (2) REPLACING LEADING SPACE BY ZERO           
113726     INSPECT MID-TISTADAT-E (2) REPLACING LEADING '+'   BY ZERO           
113730     IF MID-TISTADAT-E (2) > ZERO AND < 990101                            
113731       MOVE MID-TISTADAT-E (2) TO MOD-TISTADAT-E (2)                      
113732     END-IF                                                               
113733                                                                          
113734     INSPECT MID-TISTADAT-E (3) REPLACING LEADING SPACE BY ZERO           
113735     INSPECT MID-TISTADAT-E (3) REPLACING LEADING '+'   BY ZERO           
113736     IF MID-TISTADAT-E (3) > ZERO AND < 990101                            
113737       MOVE MID-TISTADAT-E (3) TO MOD-TISTADAT-E (3)                      
113738     END-IF                                                               
113739                                                                          
113740     INSPECT MID-TISTADAT-E (4) REPLACING LEADING SPACE BY ZERO           
113741     INSPECT MID-TISTADAT-E (4) REPLACING LEADING '+'   BY ZERO           
113742     IF MID-TISTADAT-E (4) > ZERO AND < 990101                            
113743       MOVE MID-TISTADAT-E (4) TO MOD-TISTADAT-E (4)                      
113744     END-IF                                                               
113745                                                                          
113746     INSPECT MID-TISTADAT-E (5) REPLACING LEADING SPACE BY ZERO           
113747     INSPECT MID-TISTADAT-E (5) REPLACING LEADING '+'   BY ZERO           
113748     IF MID-TISTADAT-E (5) > ZERO AND < 990101                            
113749       MOVE MID-TISTADAT-E (5) TO MOD-TISTADAT-E (5)                      
113750     END-IF                                                               
113751                                                                          
113752     IF E-DIST-FOM-FEL                                                    
113753        MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-FOM-E-ATTR              
113754     END-IF                                                               
113755     IF E-DIST-TOM-FEL                                                    
113756        MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-TOM-E-ATTR              
113758     END-IF                                                               
113759     IF E-KUND-FOM-FEL                                                    
113760        MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKUNDNR-FOM-E-ATTR             
113763     END-IF                                                               
113764     IF E-KUND-TOM-FEL                                                    
113765        MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKUNDNR-TOM-E-ATTR             
113768     END-IF                                                               
113769     IF E-DAT1-FEL                                                        
113770        MOVE MFS-NUM-FAELT-FEL     TO MOD-TISTADAT-E-ATTR (1)             
113773     END-IF                                                               
113774     IF E-DAT2-FEL                                                        
113775        MOVE MFS-NUM-FAELT-FEL     TO MOD-TISTADAT-E-ATTR (2)             
113778     END-IF                                                               
113779     IF E-DAT3-FEL                                                        
113780        MOVE MFS-NUM-FAELT-FEL     TO MOD-TISTADAT-E-ATTR (3)             
113783     END-IF                                                               
113784     IF E-DAT4-FEL                                                        
113785        MOVE MFS-NUM-FAELT-FEL     TO MOD-TISTADAT-E-ATTR (4)             
113788     END-IF                                                               
113789     IF E-DAT5-FEL                                                        
113790        MOVE MFS-NUM-FAELT-FEL     TO MOD-TISTADAT-E-ATTR (5)             
113793     END-IF                                                               
113794     .                                                                    
113795                                                                          
113796                                                                          
113797 MFS-RENSA-FAELT-UT SECTION.                                              
113798                                                                          
113800*    --- ALLA UTDATA-FÄLT                                                 
113900*    --- INKL. BLÄDDRINGSNYCKLAR                                          
114000     MOVE MFS-RENSA-FAELT TO MOD-CMD-E                                    
114100                             MOD-IDDISTR-FOM-E                            
114200                             MOD-IDDISTR-TOM-E                            
114300                             MOD-IDKUNDNR-FOM-E                           
114400                             MOD-IDKUNDNR-TOM-E                           
114500                                                                          
114600     MOVE +1 TO KV-IX                                                     
114700     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
114800     MOVE MFS-RENSA-FAELT TO MOD-TISTADAT-E (KV-IX)                       
114900       ADD +1 TO KV-IX                                                    
115000     END-PERFORM                                                          
115100                                                                          
115200     MOVE +1 TO MFS-INDX                                                  
115300     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
115400       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
115500       ADD +1 TO MFS-INDX                                                 
115600     END-PERFORM                                                          
115700     .                                                                    
115800                                                                          
115900 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
116000                                                                          
116100*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
116200     MOVE MFS-RENSA-FAELT TO MOD-CMD            (MFS-INDX)                
116300                             MOD-IDDISTR-FOM    (MFS-INDX)                
116400                             MOD-IDDISTR-TOM    (MFS-INDX)                
116500                             MOD-IDKUNDNR-FOM   (MFS-INDX)                
116600                             MOD-IDKUNDNR-TOM   (MFS-INDX)                
116601                                                                          
116610     MOVE +1 TO KV-IX                                                     
116700     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
116800       MOVE MFS-RENSA-FAELT TO MOD-TISTADAT (MFS-INDX, KV-IX)             
116900       ADD +1 TO KV-IX                                                    
117000     END-PERFORM                                                          
117100     .                                                                    
117200                                                                          
117300 MFS-RENSA-FAELT-IN SECTION.                                              
117400                                                                          
117900     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-FOM-E                            
118000                             MOD-IDDISTR-TOM-E                            
118100                             MOD-IDKUNDNR-FOM-E                           
118200                             MOD-IDKUNDNR-TOM-E                           
118300     MOVE +1 TO KV-IX                                                     
118400     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
118500       MOVE MFS-RENSA-FAELT TO MOD-TISTADAT-E (KV-IX)                     
118600       ADD +1 TO KV-IX                                                    
118700     END-PERFORM                                                          
118800     MOVE +1 TO MFS-INDX                                                  
118900     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
119000       MOVE MFS-RENSA-FAELT TO MOD-CMD (MFS-INDX)                         
119100       ADD +1 TO MFS-INDX                                                 
119200     END-PERFORM                                                          
119300     .                                                                    
119400                                                                          
119410                                                                          
119500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
119600                                                                          
119700*    --- ALLA UTDATA-FÄLT                                                 
119800*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
119900     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-E                                  
120000                               MOD-IDDISTR-FOM-E                          
120100                               MOD-IDDISTR-TOM-E                          
120200                               MOD-IDKUNDNR-FOM-E                         
120300                               MOD-IDKUNDNR-TOM-E                         
120400                                                                          
120500     MOVE +1 TO KV-IX                                                     
120600     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
120700       MOVE MFS-ROER-EJ-FAELT TO MOD-TISTADAT-E (KV-IX)                   
120800       ADD +1 TO KV-IX                                                    
120900     END-PERFORM                                                          
121000     MOVE +1 TO MFS-INDX                                                  
121100     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
121200       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
121300       ADD +1 TO MFS-INDX                                                 
121400     END-PERFORM                                                          
121500                                                                          
121600     .                                                                    
121700 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
121800                                                                          
121900*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
122000     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD            (MFS-INDX)              
122100                               MOD-IDDISTR-FOM    (MFS-INDX)              
122200                               MOD-IDDISTR-TOM    (MFS-INDX)              
122300                               MOD-IDKUNDNR-FOM   (MFS-INDX)              
122400                               MOD-IDKUNDNR-TOM   (MFS-INDX)              
122500     MOVE +1 TO KV-IX                                                     
122600     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
122700       MOVE MFS-ROER-EJ-FAELT TO MOD-TISTADAT (MFS-INDX, KV-IX)           
122800       ADD +1 TO KV-IX                                                    
122900     END-PERFORM                                                          
123000     .                                                                    
123100                                                                          
123200 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
123300                                                                          
124500     MOVE +1 TO MFS-INDX                                                  
124600     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
124700       MOVE MFS-ROER-EJ-FAELT TO MOD-CMD (MFS-INDX)                       
124710       MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-FOM  (MFS-INDX)              
124711       MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-TOM  (MFS-INDX)              
124720       MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR-FOM (MFS-INDX)              
124730       MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR-TOM (MFS-INDX)              
124800       ADD +1 TO MFS-INDX                                                 
124900     END-PERFORM                                                          
125000     .                                                                    
125100                                                                          
125110                                                                          
125120 MFS-ROER-EJ-FAELT-IN-E  SECTION.                                         
125130                                                                          
125140*    --- ALLA E-FÄLT                                                      
125150     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-E                                  
125160                               MOD-IDDISTR-FOM-E                          
125170                               MOD-IDDISTR-TOM-E                          
125180                               MOD-IDKUNDNR-FOM-E                         
125190                               MOD-IDKUNDNR-TOM-E                         
125191     MOVE +1 TO KV-IX                                                     
125192     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
125193       MOVE MFS-ROER-EJ-FAELT TO MOD-TISTADAT-E (KV-IX)                   
125194       ADD +1 TO KV-IX                                                    
125195     END-PERFORM                                                          
125201     .                                                                    
125202                                                                          
125203                                                                          
125210 MFS-FORM-ATTR SECTION.                                                   
125300                                                                          
125400*    --- ALLA INDATA-FÄLT                                                 
125500     MOVE MFS-FORMATETS-ATTR TO MOD-CMD-E-ATTR                            
125600                                MOD-IDDISTR-FOM-E-ATTR                    
125700                                MOD-IDDISTR-TOM-E-ATTR                    
125800                                MOD-IDKUNDNR-FOM-E-ATTR                   
125900                                MOD-IDKUNDNR-TOM-E-ATTR                   
126000                                                                          
126100     MOVE +1 TO KV-IX                                                     
126200     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
126300       MOVE MFS-FORMATETS-ATTR TO MOD-TISTADAT-E-ATTR (KV-IX)             
126400       ADD +1 TO KV-IX                                                    
126500     END-PERFORM                                                          
126600     MOVE +1 TO MFS-INDX                                                  
126700     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
126800       MOVE MFS-FORMATETS-ATTR TO MOD-CMD-ATTR (MFS-INDX)                 
126900       ADD +1 TO MFS-INDX                                                 
127000     END-PERFORM                                                          
127100     .                                                                    
127200                                                                          
127300 MFS-LAES-IN-IGEN   SECTION.                                              
127400                                                                          
128600     MOVE +1 TO MFS-INDX                                                  
128700     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
128800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR (MFS-INDX)              
128900       ADD +1 TO MFS-INDX                                                 
129000     END-PERFORM                                                          
129100     .                                                                    
129200                                                                          
129210                                                                          
129220 MFS-LAES-IN-IGEN-E SECTION.                                              
129230                                                                          
129240*    --- ALLA INDATA-FÄLT                                                 
129250     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-E-ATTR                         
129260                                   MOD-IDDISTR-FOM-E-ATTR                 
129270                                   MOD-IDDISTR-TOM-E-ATTR                 
129280                                   MOD-IDKUNDNR-FOM-E-ATTR                
129290                                   MOD-IDKUNDNR-TOM-E-ATTR                
129291     MOVE +1 TO KV-IX                                                     
129292     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
129293       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TISTADAT-E-ATTR (KV-IX)          
129294       ADD +1 TO KV-IX                                                    
129295     END-PERFORM                                                          
129302     .                                                                    
129303                                                                          
129304                                                                          
129310* --- IMS SEKTIONER ---                                                   
129400                                                                          
129500 IMS-GET-MSG SECTION.                                                     
129600                                                                          
129700     MOVE '  QC' TO GODK-STATUSKODER                                      
129800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
129900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
130000     PERFORM IMS-STATUSKONTROLL                                           
130100     .                                                                    
130200                                                                          
130300 IMS-INSERT-MSG SECTION.                                                  
130400                                                                          
130500     MOVE LOW-VALUE       TO MSG-KDZ1 MSG-KDZ2                            
130600     MOVE SPACE           TO GODK-STATUSKODER                             
130700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
130800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
130900     PERFORM IMS-STATUSKONTROLL                                           
131000     .                                                                    
131100                                                                          
131110                                                                          
131200 IMS-GET-WMSGKOM-MSG SECTION.                                             
131300                                                                          
131400     MOVE '  QD'          TO GODK-STATUSKODER                             
131500     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
131600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
131700     PERFORM IMS-STATUSKONTROLL                                           
131800     .                                                                    
131900                                                                          
132000                                                                          
132100 IMS-INSERT-WMSGKOM-MSG SECTION.                                          
132200                                                                          
132300     MOVE '  '               TO GODK-STATUSKODER                          
132400     CALL CBLTDLI USING ISRT MSGKOM-PCB MSG-KOM-WMSGKOM                   
132500     MOVE MSGKOM-STATUS-CODE TO STATUS-WS                                 
132600     PERFORM IMS-STATUSKONTROLL                                           
132700     .                                                                    
132800                                                                          
132810                                                                          
132900 IMS-GU-WDF801 SECTION.                                                   
132910     MOVE 'IMS-GU-WDF801  ' TO CURRENT-IMS-SECTION                        
133000                                                                          
133010     MOVE SPACE              TO ALL-SSA                                   
133100     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
133200         DELIMITED BY SIZE INTO SSA1                                      
133300     MOVE '  GE'             TO GODK-STATUSKODER                          
133400     CALL CBLTDLI USING GU WDF8-PCB DLI-IO-WDF801 SSA1                    
133500     MOVE WDF8-STATUS-CODE   TO STATUS-WS                                 
133600     PERFORM IMS-STATUSKONTROLL                                           
133700     .                                                                    
133800                                                                          
133900 IMS-GHU-WDF811 SECTION.                                                  
133910     MOVE 'IMS-GHU-WDF811  ' TO CURRENT-IMS-SECTION                       
134000                                                                          
134010     MOVE SPACE              TO ALL-SSA                                   
134100     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
134200         DELIMITED BY SIZE INTO SSA1                                      
134300     STRING 'WDF811  (WDF811KY =' W-WDF811KY-X ')'                        
134400         DELIMITED BY SIZE INTO SSA2                                      
134500     MOVE '  GE'             TO GODK-STATUSKODER                          
134600     CALL CBLTDLI USING GHU WDF8-PCB DLI-IO-WDF811 SSA1 SSA2              
134700     MOVE WDF8-STATUS-CODE   TO STATUS-WS                                 
134800     PERFORM IMS-STATUSKONTROLL                                           
134900     .                                                                    
135000                                                                          
135010 IMS-GHU-WDF811M SECTION.                                                 
135020     MOVE 'IMS-GHU-WDF811M ' TO CURRENT-IMS-SECTION                       
135030                                                                          
135040     MOVE SPACE              TO ALL-SSA                                   
135050     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
135060         DELIMITED BY SIZE INTO SSA1                                      
135070     STRING 'WDF811  (WDF811KY =' W-WDF811KY-F8-X ')'                     
135080         DELIMITED BY SIZE INTO SSA2                                      
135090     MOVE '  GE'             TO GODK-STATUSKODER                          
135091     CALL CBLTDLI USING GHU WDF8-PCB DLI-IO-WDF811 SSA1 SSA2              
135092     MOVE WDF8-STATUS-CODE   TO STATUS-WS                                 
135093     PERFORM IMS-STATUSKONTROLL                                           
135094     .                                                                    
135095                                                                          
135100 IMS-GNP-WDF811 SECTION.                                                  
135110     MOVE 'IMS-GNP-WDF811  ' TO CURRENT-IMS-SECTION                       
135120                                                                          
135130     MOVE SPACE              TO ALL-SSA                                   
135200                                                                          
135300     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
135400         DELIMITED BY SIZE INTO SSA1                                      
135500     STRING 'WDF811  (WDF811KY>=' W-WDF811KY-X ')'                        
135600         DELIMITED BY SIZE INTO SSA2                                      
135700     MOVE '  GE'             TO GODK-STATUSKODER                          
135800     CALL CBLTDLI USING GNP WDF8-PCB DLI-IO-WDF811 SSA1 SSA2              
135900     MOVE WDF8-STATUS-CODE   TO STATUS-WS                                 
136000     PERFORM IMS-STATUSKONTROLL                                           
136100     .                                                                    
136200                                                                          
136300 IMS-GNP-WDF811-OKVAL SECTION.                                            
136310     MOVE 'IMS-GNP-WDF811-O' TO CURRENT-IMS-SECTION                       
136320                                                                          
136330     MOVE SPACE              TO ALL-SSA                                   
136400                                                                          
136500     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
136600         DELIMITED BY SIZE INTO SSA1                                      
136700     MOVE   'WDF811  '       TO SSA2                                      
136800     MOVE '  GE'             TO GODK-STATUSKODER                          
136900     CALL CBLTDLI USING GNP WDF8-PCB DLI-IO-WDF811 SSA1 SSA2              
137000     MOVE WDF8-STATUS-CODE   TO STATUS-WS                                 
137100     PERFORM IMS-STATUSKONTROLL                                           
137200     .                                                                    
137300                                                                          
137400 IMS-ISRT-WDF811 SECTION.                                                 
137410     MOVE 'IMS-ISRT-WDF811 ' TO CURRENT-IMS-SECTION                       
137420                                                                          
137430     MOVE SPACE              TO ALL-SSA                                   
137500                                                                          
137600     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
137700         DELIMITED BY SIZE INTO SSA1                                      
137800     MOVE 'WDF811   '      TO SSA2                                        
137900     MOVE '  II'           TO GODK-STATUSKODER                            
138000     CALL CBLTDLI USING ISRT WDF8-PCB DLI-IO-WDF811 SSA1 SSA2             
138100     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
138200     PERFORM IMS-STATUSKONTROLL                                           
138300     .                                                                    
138400                                                                          
138500 IMS-REPL-WDF811 SECTION.                                                 
138510     MOVE 'IMS-REPL-WDF811 ' TO CURRENT-IMS-SECTION                       
138520                                                                          
138530     MOVE SPACE              TO ALL-SSA                                   
138600                                                                          
138700     MOVE '  '             TO GODK-STATUSKODER                            
138800     CALL CBLTDLI USING REPL WDF8-PCB DLI-IO-WDF811                       
138900     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
139000     PERFORM IMS-STATUSKONTROLL                                           
139100     .                                                                    
139200                                                                          
139300 IMS-DLET-WDF811 SECTION.                                                 
139310     MOVE 'IMS-DLET-WDF811 ' TO CURRENT-IMS-SECTION                       
139320                                                                          
139330     MOVE SPACE              TO ALL-SSA                                   
139400                                                                          
139500     MOVE '  '               TO GODK-STATUSKODER                          
139600     CALL CBLTDLI USING DLET WDF8-PCB DLI-IO-WDF811                       
139700     MOVE WDF8-STATUS-CODE   TO STATUS-WS                                 
139800     PERFORM IMS-STATUSKONTROLL                                           
139900     .                                                                    
140000                                                                          
140100 IMS-GU-WDB201 SECTION.                                                   
140110     MOVE 'IMS-GU-WDB201   ' TO CURRENT-IMS-SECTION                       
140120                                                                          
140130     MOVE SPACE              TO ALL-SSA                                   
140200                                                                          
140300     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
140400         DELIMITED BY SIZE INTO SSA1                                      
140500     MOVE '  GE'             TO GODK-STATUSKODER                          
140600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
140700     MOVE WDB2-STATUS-CODE   TO STATUS-WS                                 
140800     PERFORM IMS-STATUSKONTROLL                                           
140900     .                                                                    
141000                                                                          
141010                                                                          
141100 IMS-STATUSKONTROLL SECTION.                                              
141200                                                                          
141300     SET STATUS-IX TO 1                                                   
141400     SEARCH GODK-STATUS                                                   
141500       AT END                                                             
141600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
141700         DELIMITED BY SIZE INTO FELTEXT                                   
141800         CALL FELLOG                                                      
141900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
142000         CONTINUE                                                         
142100     END-SEARCH                                                           
142200     .                                                                    
142300                                                                          
142400*    -COPY WY2000P1                                                       
