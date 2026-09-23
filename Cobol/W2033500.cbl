000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2033500.                                                
000300 AUTHOR.         JONNY SANDSTEN.                                          
000400 DATE-WRITTEN.   98/10/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        DETTA PROGRAM HANTERAR UPPDATERING OCH FRÅGOR PÅ                 
000900*        DIREKLEVERANSSTYRNINGEN, VILKA DISTRIKT OCH DEALERS              
001000*        SKALL HA DIREKTLEVERANS AV GIVEN LEVERANTÖRS-GRUPPART.           
001100*        BILDEN VISAR                                                     
001200*        - DISTRIKT- KUNDINTERVALL MED                                    
001300*          . MINKVANTGRÄNSER PER KLASS                                    
001400*          . LEVERANS FRÅN SDC PER KLASS                                  
001500*                                                                         
001600*        DET FINNS 3 OLIKA INMATNINGSKANTKODER:                           
001700*        -NYUPPLÄGG AV RAD (N)                                            
001800*        -EDITERING AV RAD (E)                                            
001900*        -BORTTAG AV RAD (D)                                              
002000*                                                                         
002100*        PROGRAMMET UPPDATERAR WLLEVF (WDF2)                              
002200*        PROGRAMMET LÄSER      WLLEVA (WDF1)                              
002300*        PROGRAMMET LÄSER      WLGMTA (WDB2)                              
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W2T335 W2T335U W2T335X                              
002700*        MID:         W2I33501                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W2O33501                                            
003100*                                                                         
003200* 2012-01-03  E'TRACKER: 10143271 CHINA WAREHOUSE PROJECT-1               
003300*                                                                         
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(08)   VALUE 'W2033500'.            
004300                                                                          
004400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004600                                                                          
004700 77  YES                         PIC X       VALUE 'Y'.                   
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000 77  SW-ENTRY                    PIC X       VALUE 'N'.                   
005100     88 NO-ENTRY                             VALUE 'N'.                   
005200 77  SW-SDC                      PIC X       VALUE 'N'.                   
005300     88 SDC                                  VALUE 'J'.                   
005400                                                                          
005500*    --- GENERELLA ARBETSFÄLT                                             
005600 77  KUNDNR-FOM-NOLL             PIC X      VALUE 'N'.                    
005700                                                                          
005800                                                                          
005900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006100 77  W-INDX                      PIC S9(4)  VALUE +0    COMP SYNC.        
006200 77  MAX-INDX                    PIC S9(4)  VALUE +6    COMP SYNC.        
006300 77  KV-IX                       PIC S9(4)  VALUE +0    COMP SYNC.        
006400 77  MAX-KV-IX                   PIC S9(4)  VALUE +5    COMP SYNC.        
006500 77  SDC-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
006600 77  MAX-SDC-IX                  PIC S9(4)  VALUE +5    COMP SYNC.        
006700                                                                          
006800 77  WS-KVBEART                  PIC S9(7)  COMP-3.                       
006900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007000                                                                          
007100 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007200     88  ALLT-OK                             VALUE 'J'.                   
007300                                                                          
007400 77  DISTR-SW                    PIC X       VALUE 'J'.                   
007500     88  DISTR-TOM                           VALUE 'J'.                   
007600                                                                          
007700 77  CMD-SW                      PIC X       VALUE 'J'.                   
007800     88  CMD-OK                              VALUE 'J'.                   
007900                                                                          
008000 77  NYPOST-SW                   PIC X       VALUE 'J'.                   
008100     88  NYPOST-OK                           VALUE 'J'.                   
008200     88  NYPOST-FEL                          VALUE 'N'.                   
008300                                                                          
008400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008500     88  INDATA-OK                           VALUE 'J'.                   
008600     88  INDATA-FEL                          VALUE 'N'.                   
008700                                                                          
008800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008900     88  NYCKLAR-OK                          VALUE 'J'.                   
009000     88  NYCKLAR-FEL                         VALUE 'N'.                   
009100                                                                          
009200 77    DISTRIKT-SW               PIC X       VALUE 'J'.                   
009300   88    DISTRIKT-OK                         VALUE 'J'.                   
009400   88    DISTRIKT-FEL                        VALUE 'N'.                   
009500                                                                          
009600 77    KUNDNR-SW                 PIC X       VALUE 'J'.                   
009700   88    KUNDNR-OK                           VALUE 'J'.                   
009800   88    KUNDNR-FEL                          VALUE 'N'.                   
009900                                                                          
010000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010100     88  EGEN-MID                            VALUE '2335'.                
010200     88  GODK-MID                            VALUE '2331' '2332'          
010300                                                   '2333' '2334'          
010400                                                   '2335' '2336'          
010500                                                   '2337' '2338'          
010600                                                   '2339'.                
010700     88  HELP-MID                            VALUE '0551'.                
010800     EJECT                                                                
010900                                                                          
011000 01  WS-IDDISTR-NUM              PIC 9(4)    VALUE ZERO.                  
011100 01  WS-IDKUNDNR-NUM             PIC 9(6)    VALUE ZERO.                  
011200     EJECT                                                                
011300                                                                          
011400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011500 01  GENERELLA-SUBPROGRAM.                                                
011600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012100     EJECT                                                                
012200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012300*01 -COPY WMEDAREA                                                        
012400     EJECT                                                                
012500*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
012600*01 -COPY WDATAREA                                                        
012700     SKIP3                                                                
012800 01  MESSAGE-CODES.                                                       
012900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
013000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
013100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
013200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
013300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
013400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
013500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013600     03  GROUP-MISSING           PIC X(3)    VALUE '275'.                 
013700     03  PART-EXISTS             PIC X(3)    VALUE '278'.                 
013800     03  PART-MISSING            PIC X(3)    VALUE '279'.                 
013900     03  DISTR-MISSING           PIC X(3)    VALUE '412'.                 
014000     03  DISTR-EXISTS            PIC X(3)    VALUE '070'.                 
014100     03  INFO-MISSING            PIC X(3)    VALUE '760'.                 
014200     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
014300     EJECT                                                                
014400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014500*                                                                         
014600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
014700     SKIP3                                                                
014800*01 -COPY WMSGINIT                                                        
014900     EJECT                                                                
015000*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
015100*                                                                         
015200 01  SPAR-AREA.                                                           
015300     03  SPAR-IDTRANS            PIC X(4)    VALUE '2335'.                
015400     03  SPAR-IDDISTR-FOM-ENTER  PIC 9(4)    VALUE ZERO.                  
015500     03  SPAR-IDDISTR-TOM-ENTER  PIC 9(4)    VALUE ZERO.                  
015600     03  SPAR-IDKUNDNR-FOM-ENTER PIC 9(6)    VALUE ZERO.                  
015700     03  SPAR-IDKUNDNR-TOM-ENTER PIC 9(6)    VALUE ZERO.                  
015800     03  SPAR-IDDISTR-FOM-NEXT   PIC 9(4)    VALUE ZERO.                  
015900     03  SPAR-IDDISTR-TOM-NEXT   PIC 9(4)    VALUE ZERO.                  
016000     03  SPAR-IDKUNDNR-FOM-NEXT  PIC 9(6)    VALUE ZERO.                  
016100     03  SPAR-IDKUNDNR-TOM-NEXT  PIC 9(6)    VALUE ZERO.                  
016200     03  SPAR-TABELL.                                                     
016300       05  SPAR-WDF211   OCCURS 6.                                        
016400           07  SPAR-WDF211-TAB.                                           
016500               09  SPAR-IDDISTR-FOM    PIC 9(4)    VALUE ZERO.            
016600               09  SPAR-IDDISTR-TOM    PIC 9(4)    VALUE ZERO.            
016700               09  SPAR-IDKUNDNR-FOM   PIC 9(6)    VALUE ZERO.            
016800               09  SPAR-IDKUNDNR-TOM   PIC 9(6)    VALUE ZERO.            
016900     EJECT                                                                
017000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
017100*                                                                         
017200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017300     SKIP3                                                                
017400*01  MID -COPY W2I33501                                                   
017500     EJECT                                                                
017600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017700     SKIP3                                                                
017800*01  -COPY WMSGAREA                                                       
017900     EJECT                                                                
018000     03  MOD REDEFINES MSG-AREA.                                          
018100*      05  -COPY W2O33501                                                 
018200     EJECT                                                                
018300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018400     SKIP3                                                                
018500*01  -COPY WMFSAREA                                                       
018600     EJECT                                                                
018700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018800*                                                                         
018900     EJECT                                                                
019000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019100     EJECT                                                                
019200 01  IMS-WS-4.                                                            
019300     03  FILLER                  PIC X(16)  VALUE 'MSG-KOM-AREA'.         
019400*      --- GENERELL IO-KOMMUNIKATIONSAREA FÖR DISPATCHER                  
019500*01  -COPY WMSGKOM                                                        
019600     SKIP3                                                                
019700 01  NYCKLAR-TILL-DLI.                                                    
019800*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
019900     03  W-WDF201KY-X.                                                    
020000         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
020100         05  W-IDDIRGRP          PIC X(10)   VALUE SPACE.                 
020200                                                                          
020300     03  W-WDF211KY-X.                                                    
020400         05  W-IDDISTR-FOM-X.                                             
020500             07  W-IDDISTR-FOM   PIC S9(5)   VALUE ZERO COMP-3.           
020600         05  W-IDDISTR-TOM-X.                                             
020700             07  W-IDDISTR-TOM   PIC S9(5)   VALUE ZERO COMP-3.           
020800         05  W-IDKUNDNR-FOM-X.                                            
020900             07  W-IDKUNDNR-FOM  PIC S9(7)   VALUE ZERO COMP-3.           
021000         05  W-IDKUNDNR-TOM-X.                                            
021100             07  W-IDKUNDNR-TOM  PIC S9(7)   VALUE ZERO COMP-3.           
021200                                                                          
021300     03  W-WDF101KY-X.                                                    
021400         05  W-IDLEVNR-WDF1      PIC  X(5)   VALUE SPACE.                 
021500                                                                          
021600     03  W-WDF118KY-X.                                                    
021700         05  W-IDDISTR-WDF1      PIC S9(5)   VALUE ZERO COMP-3.           
021800         05  W-IDKUNDNR-WDF1     PIC S9(7)   VALUE ZERO COMP-3.           
021900         05  W-KDORDKL-WDF1      PIC S9(1)   VALUE ZERO COMP-3.           
022000                                                                          
022100     03  W-IDGMT-X.                                                       
022200         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
022300         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
022400                                                                          
022500     03  W-IDDC-X.                                                        
022600         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
022700                                                                          
022800     SKIP2                                                                
022900*    --- STATUS-KOD FRÅN IMS                                              
023000 01  STATUS-WS                   PIC XX.                                  
023100     88  SEGMENT-FINNS                       VALUE '  '.                  
023200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023400     SKIP2                                                                
023500 01  GODK-STATUSKODER.                                                    
023600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023700     SKIP3                                                                
023800 01  SSA1                        PIC X(64).                               
023900 01  SSA2                        PIC X(512).                              
024000     EJECT                                                                
024100*    --- IMS FUNKTIONSKODER                                               
024200*01  -COPY W0003                                                          
024300     EJECT                                                                
024400*    ---  DLI INPUT-OUTPUT AREA                                           
024500                                                                          
024600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF201'.                      
024700 01  DLI-IO-WLLEVF01.                                                     
024800*    03  -COPY WDF201                                                     
024900     EJECT                                                                
025000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF211'.                      
025100 01  DLI-IO-WLLEVF11.                                                     
025200*    03  -COPY WDF211                                                     
025300     EJECT                                                                
025400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
025500 01  DLI-IO-WLLEVA01.                                                     
025600*    03  -COPY WDF101 -PRE WDF1-                                          
025700     EJECT                                                                
025800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF118'.                      
025900 01  DLI-IO-WLLEVA18.                                                     
026000*    03  -COPY WDF118                                                     
026100     EJECT                                                                
026200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
026300 01  DLI-IO-WLGMTA01.                                                     
026400*    03  -COPY WDB201                                                     
026500     EJECT                                                                
026600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
026700 01  DLI-IO-WDB601.                                                       
026800*    03  -COPY WDB601                                                     
026900     EJECT                                                                
027000 LINKAGE SECTION.                                                         
027100*01  -COPY W0009   -PRE MSG-                                              
027200     EJECT                                                                
027300*01  -COPY W0009   -PRE MSGKOM-                                           
027400     EJECT                                                                
027500*01  -COPY W0008   -PRE USEA-                                             
027600     05  FILLER                  PIC X.                                   
027700                                                                          
027800*01  -COPY W0008  -PRE WDF2-                                              
027900     05  FILLER                  PIC X.                                   
028000                                                                          
028100*01  -COPY W0008  -PRE WDF1-                                              
028200     05  FILLER                  PIC X.                                   
028300                                                                          
028400*01  -COPY W0008  -PRE WDB2-                                              
028500     05  FILLER                  PIC X.                                   
028600                                                                          
028700*01  -COPY W0008  -PRE WDB6-                                              
028800     05  FILLER                  PIC X.                                   
028900     EJECT                                                                
029000 PROCEDURE DIVISION  USING MSG-PCB  MSGKOM-PCB USEA-PCB                   
029100                           WDF2-PCB WDF1-PCB   WDB2-PCB                   
029200                           WDB6-PCB.                                      
029300 MAIN SECTION.                                                            
029400     ENTRY 'DLITCBL' USING MSG-PCB  MSGKOM-PCB USEA-PCB                   
029500                           WDF2-PCB WDF1-PCB   WDB2-PCB                   
029600                           WDB6-PCB.                                      
029700                                                                          
029800     PERFORM IMS-GET-MSG                                                  
029900     IF SEGMENT-FINNS                                                     
030000       PERFORM IMS-GET-WMSGKOM-MSG                                        
030100       PERFORM A-INIT                                                     
030200       PERFORM B-KOLLA-NYCKLAR                                            
030300       IF NYCKLAR-OK                                                      
030400         IF MFS-UPDATE OR MFS-UPD-X                                       
030500           PERFORM G-KOLLA-INPUT                                          
030600           IF INDATA-OK                                                   
030700             PERFORM H-UPPDATERA                                          
030800           END-IF                                                         
030900         ELSE                                                             
031000           IF MFS-FIRST                                                   
031100             PERFORM C-FOERSTA-SIDA                                       
031200           ELSE                                                           
031300             IF MFS-NEXT                                                  
031400               PERFORM D-NAESTA-SIDA                                      
031500             ELSE                                                         
031600               PERFORM E-SAMMA-SIDA                                       
031700             END-IF                                                       
031800           END-IF                                                         
031900         END-IF                                                           
032000         IF ALLT-OK                                                       
032100           PERFORM F-LAES-VISA-INFO                                       
032200         END-IF                                                           
032300       END-IF                                                             
032400       IF MFS-UPD-X                                                       
032500         COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM + 17            
032600         PERFORM IMS-INSERT-WMSGKOM-MSG                                   
032700       ELSE                                                               
032800         COMPUTE MSG-KVLL = LENGTH OF MOD-W2O33501 + 4                    
032900         PERFORM IMS-INSERT-MSG                                           
033000       END-IF                                                             
033100     END-IF                                                               
033200                                                                          
033300     MOVE ZERO TO RETURN-CODE                                             
033400     GOBACK                                                               
033500     .                                                                    
033600     EJECT                                                                
033700 A-INIT SECTION.                                                          
033800                                                                          
033900     IF MSG-DUBBLA-TRANSKODER                                             
034000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I33501                 
034100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
034200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
034300     ELSE                                                                 
034400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I33501                  
034500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
034600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
034700     END-IF                                                               
034800                                                                          
034900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
035000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
035100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
035200                                                                          
035300     MOVE LOW-VALUE TO MSG-AREA                                           
035400     MOVE 'W2O335N1' TO MFS-IDMOD                                         
035500     MOVE '2335' TO MOD-IDTRANS                                           
035600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
035700                                                                          
035800     IF EGEN-MID OR HELP-MID                                              
035900       CONTINUE                                                           
036000     ELSE                                                                 
036100       MOVE SPACE TO MFS-KDTRTYP                                          
036200       MOVE '7' TO MFS-IDPFK                                              
036300     END-IF                                                               
036400     .                                                                    
036500     EJECT                                                                
036600 B-KOLLA-NYCKLAR SECTION.                                                 
036700                                                                          
036800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
036900     MOVE '001'             TO MSGI-KDCALL                                
037000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
037100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
037200     MOVE '2335'            TO MSGI-IDTRANS                               
037300                                                                          
037400     IF EGEN-MID                                                          
037500       MOVE MID-IDLEVNR-IN   TO MSGI-IDLEVNR                              
037600       MOVE MID-IDDIRGRP-IN  TO MSGI-IDDIRGRP                             
037700       IF MID-IDDISTR-IN NOT = ALL '+'                                    
037800         MOVE MID-IDDISTR-IN    TO MSGI-IDDISTR                           
037900       ELSE                                                               
038000         MOVE MID-IDDISTR-UT    TO MID-IDDISTR-IN                         
038100                                   MSGI-IDDISTR                           
038200       END-IF                                                             
038300       IF MID-IDKUNDNR-IN NOT = ALL '+'                                   
038400         MOVE MID-IDKUNDNR-IN   TO MSGI-IDKUNDNR                          
038500       ELSE                                                               
038600         MOVE MID-IDKUNDNR-UT   TO MID-IDKUNDNR-IN                        
038700                                   MSGI-IDKUNDNR                          
038800       END-IF                                                             
038900     END-IF                                                               
039000                                                                          
039100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
039200     MOVE MSGI-SPAR-AREA     TO SPAR-AREA                                 
039300                                                                          
039400     IF MSGI-IDLAND-SPR = 'GB'                                            
039500       MOVE 'GB' TO MED-IDSKYLT                                           
039600     ELSE                                                                 
039700       MOVE 'S' TO MED-IDSKYLT                                            
039800     END-IF                                                               
039900                                                                          
040000     MOVE JA TO ALLT-SW                                                   
040100     MOVE JA TO NYCKLAR-SW                                                
040200     MOVE JA TO DISTR-SW                                                  
040300     MOVE SPACE TO MED-IDMFSFEL                                           
040400                   MED-IDMFSINF                                           
040500                   MSG-KOM-IDMFSMED                                       
040600                                                                          
040700*    -- KONTROLL AV IDLEVNR                                               
040800     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
040900                                                                          
041000     IF MSGI-IDLEVNR NOT = SPACE                                          
041100        MOVE MSGI-IDLEVNR  TO W-IDLEVNR                                   
041200                              W-IDLEVNR-WDF1                              
041300     ELSE                                                                 
041400        MOVE NEJ           TO NYCKLAR-SW                                  
041500     END-IF                                                               
041600                                                                          
041700*    -- KONTROLL AV IDDIRGRP                                              
041800     MOVE MFS-RENSA-FAELT TO MOD-IDDIRGRP-IN                              
041900                                                                          
042000     MOVE MSGI-IDDIRGRP   TO W-IDDIRGRP                                   
042100                                                                          
042200*    -- KONTROLL AV IDDISTR                                               
042300     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
042400                                                                          
042500     IF MID-IDDISTR-IN NOT = ALL '+'                                      
042600       INSPECT MID-IDDISTR-IN REPLACING LEADING SPACE BY ZERO             
042700       IF MID-IDDISTR-IN NUMERIC                                          
042800          MOVE MID-IDDISTR-IN   TO WS-IDDISTR-NUM                         
042900         IF WS-IDDISTR-NUM >= 1                                           
043000           MOVE WS-IDDISTR-NUM  TO W-IDDISTR-FOM                          
043100           MOVE WS-IDDISTR-NUM  TO W-IDDISTR-TOM                          
043200         END-IF                                                           
043300       ELSE                                                               
043400         MOVE NEJ               TO NYCKLAR-SW                             
043500       END-IF                                                             
043600       INSPECT MID-IDDISTR-IN REPLACING LEADING ZERO BY SPACE             
043700       MOVE MID-IDDISTR-IN      TO MOD-IDDISTR-UT                         
043800     ELSE                                                                 
043900       MOVE MFS-RENSA-FAELT     TO MOD-IDDISTR-UT                         
044000     END-IF                                                               
044100                                                                          
044200*    -- KONTROLL AV IDKUNDNR                                              
044300     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
044400                                                                          
044500     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
044600       INSPECT MID-IDKUNDNR-IN REPLACING LEADING SPACE BY ZERO            
044700       IF MID-IDKUNDNR-IN NUMERIC                                         
044800          MOVE MID-IDKUNDNR-IN  TO WS-IDKUNDNR-NUM                        
044900         IF WS-IDKUNDNR-NUM >= 1                                          
045000           MOVE WS-IDKUNDNR-NUM TO W-IDKUNDNR-FOM                         
045100           MOVE WS-IDKUNDNR-NUM TO W-IDKUNDNR-TOM                         
045200         END-IF                                                           
045300       ELSE                                                               
045400         MOVE NEJ               TO NYCKLAR-SW                             
045500       END-IF                                                             
045600       INSPECT MID-IDKUNDNR-IN REPLACING LEADING ZERO BY SPACE            
045700       MOVE MID-IDKUNDNR-IN     TO MOD-IDKUNDNR-UT                        
045800     END-IF                                                               
045900                                                                          
046000     IF GODK-MID OR NYCKLAR-OK                                            
046100       MOVE MSGI-IDLEVNR        TO MOD-IDLEVNR-UT                         
046200       MOVE MSGI-IDDIRGRP       TO MOD-IDDIRGRP-UT                        
046300     ELSE                                                                 
046400       MOVE MFS-RENSA-FAELT     TO MOD-IDLEVNR-UT                         
046500                                   MOD-IDDIRGRP-UT                        
046600     END-IF                                                               
046700                                                                          
046800     IF NYCKLAR-FEL                                                       
046900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
047000                             MSG-KOM-IDMFSMED                             
047100       CALL WMEDKONV USING MED-WMEDAREA                                   
047200       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
047300       PERFORM MFS-RENSA-FAELT-IN                                         
047400       PERFORM MFS-RENSA-FAELT-UT                                         
047500     END-IF                                                               
047600     .                                                                    
047700     EJECT                                                                
047800 C-FOERSTA-SIDA SECTION.                                                  
047900                                                                          
048000     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
048100                            MSG-KOM-IDMFSMED                              
048200     CALL WMEDKONV USING MED-WMEDAREA                                     
048300     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
048400                                                                          
048500     PERFORM MFS-RENSA-FAELT-IN                                           
048600     .                                                                    
048700     EJECT                                                                
048800 D-NAESTA-SIDA SECTION.                                                   
048900                                                                          
049000     IF SPAR-IDTRANS = '2335'                                             
049100       MOVE SPAR-IDDISTR-FOM-NEXT  TO W-IDDISTR-FOM                       
049200       MOVE SPAR-IDDISTR-TOM-NEXT  TO W-IDDISTR-TOM                       
049300       MOVE SPAR-IDKUNDNR-FOM-NEXT TO W-IDKUNDNR-FOM                      
049400       MOVE SPAR-IDKUNDNR-TOM-NEXT TO W-IDKUNDNR-TOM                      
049500     ELSE                                                                 
049600       PERFORM MFS-RENSA-FAELT-IN                                         
049700     END-IF                                                               
049800     .                                                                    
049900     EJECT                                                                
050000 E-SAMMA-SIDA SECTION.                                                    
050100                                                                          
050200     IF SPAR-IDTRANS = '2335' OR '0551'                                   
050300       MOVE JA                   TO SW-ENTRY                              
050400       IF MID-CMD-E = '+' AND                                             
050500          MID-IDDISTR-FOM-E = ALL '+'                                     
050600         MOVE NEJ                TO SW-ENTRY                              
050700         MOVE +1                 TO INDX                                  
050800         PERFORM UNTIL INDX > MAX-INDX                                    
050900           IF MID-CMD (INDX) = ALL '+'                                    
051000             CONTINUE                                                     
051100           ELSE                                                           
051200             MOVE JA             TO SW-ENTRY                              
051300             MOVE MAX-INDX       TO INDX                                  
051400           END-IF                                                         
051500           ADD +1                TO INDX                                  
051600         END-PERFORM                                                      
051700       END-IF                                                             
051800       IF NO-ENTRY                                                        
051900         PERFORM MFS-RENSA-FAELT-IN                                       
052000       ELSE                                                               
052100         MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                              
052200         CALL WMEDKONV USING MED-WMEDAREA                                 
052300         MOVE MED-MFSFEL     TO MOD-TEMFSFEL                              
052400         PERFORM MFS-ROER-EJ-FAELT-IN                                     
052500         PERFORM MFS-LAES-IN-IGEN                                         
052600       END-IF                                                             
052700     ELSE                                                                 
052800       PERFORM MFS-RENSA-FAELT-IN                                         
052900     END-IF                                                               
053000*      IF MID-CMD-E = ALL '+' AND MID-IDDISTR-FOM-E = ALL '+'             
053100*        PERFORM MFS-RENSA-FAELT-IN                                       
053200*      ELSE                                                               
053300*        MOVE +1 TO INDX                                                  
053400*        PERFORM UNTIL INDX > MAX-INDX                                    
053500*          IF MID-CMD (INDX) = ALL '+'                                    
053600*            PERFORM MFS-RENSA-FAELT-IN                                   
053700*          ELSE                                                           
053800*            MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                          
053900*                                   MSG-KOM-IDMFSMED                      
054000*            CALL WMEDKONV USING MED-WMEDAREA                             
054100*            MOVE MED-MFSFEL     TO MOD-TEMFSFEL                          
054200*            PERFORM MFS-ROER-EJ-FAELT-IN                                 
054300*            PERFORM MFS-LAES-IN-IGEN                                     
054400*            MOVE MAX-INDX       TO INDX                                  
054500*          END-IF                                                         
054600*          ADD +1 TO INDX                                                 
054700*        END-PERFORM                                                      
054800*      END-IF                                                             
054900*    ELSE                                                                 
055000*      PERFORM MFS-RENSA-FAELT-IN                                         
055100*    END-IF                                                               
055200     .                                                                    
055300     EJECT                                                                
055400 F-LAES-VISA-INFO SECTION.                                                
055500                                                                          
055600     PERFORM IMS-GHU-LEV                                                  
055700                                                                          
055800     IF SEGMENT-SAKNAS                                                    
055900       MOVE GROUP-MISSING TO MED-IDMFSFEL                                 
056000                             MSG-KOM-IDMFSMED                             
056100       CALL WMEDKONV USING MED-WMEDAREA                                   
056200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
056300       PERFORM MFS-RENSA-FAELT-UT                                         
056400     ELSE                                                                 
056500                                                                          
056600       PERFORM IMS-GNP-DIR                                                
056700                                                                          
056800       MOVE +1 TO INDX                                                    
056900       PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                    
057000         IF SEGMENT-FINNS                                                 
057100           IF INDX = 1                                                    
057200             MOVE DIR-IDDISTR-FOM    TO SPAR-IDDISTR-FOM-ENTER            
057300             MOVE DIR-IDDISTR-TOM    TO SPAR-IDDISTR-TOM-ENTER            
057400             MOVE DIR-IDKUNDNR-FOM   TO SPAR-IDKUNDNR-FOM-ENTER           
057500             MOVE DIR-IDKUNDNR-TOM   TO SPAR-IDKUNDNR-TOM-ENTER           
057600           END-IF                                                         
057700           MOVE DIR-IDDISTR-FOM      TO MOD-IDDISTR-FOM   (INDX)          
057800                                        SPAR-IDDISTR-FOM  (INDX)          
057900           MOVE DIR-IDDISTR-TOM      TO MOD-IDDISTR-TOM   (INDX)          
058000                                        SPAR-IDDISTR-TOM  (INDX)          
058100           MOVE DIR-IDKUNDNR-FOM     TO MOD-IDKUNDNR-FOM  (INDX)          
058200                                        SPAR-IDKUNDNR-FOM (INDX)          
058300           MOVE DIR-IDKUNDNR-TOM     TO MOD-IDKUNDNR-TOM  (INDX)          
058400                                        SPAR-IDKUNDNR-TOM (INDX)          
058500           MOVE +1 TO KV-IX                                               
058600           PERFORM UNTIL KV-IX > MAX-KV-IX                                
058700             MOVE DIR-KDDDGS     (KV-IX) TO                               
058800                                MOD-KDDDGS      (INDX, KV-IX)             
058900                                                                          
059000             IF DIR-FLDDGS (KV-IX) = JA                                   
059100                MOVE YES                TO                                
059200                                     MOD-FLDDGS (INDX, KV-IX)             
059300             ELSE                                                         
059400                MOVE DIR-FLDDGS (KV-IX) TO                                
059500                                     MOD-FLDDGS (INDX, KV-IX)             
059600             END-IF                                                       
059700                                                                          
059800             MOVE DIR-KVBEART-MIN(KV-IX) TO                               
059900                                MOD-KVBEART-MIN (INDX, KV-IX)             
060000                                                                          
060100             IF DIR-IDDC (KV-IX) > '  '                                   
060200               MOVE DIR-IDDC (KV-IX)     TO                               
060300                                        MOD-IDDC (INDX, KV-IX)            
060400             ELSE                                                         
060500               MOVE '  '                 TO                               
060600                                        MOD-IDDC (INDX, KV-IX)            
060700             END-IF                                                       
060800             ADD +1 TO KV-IX                                              
060900           END-PERFORM                                                    
061000         ELSE                                                             
061100           MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-FOM  (INDX)                
061200                                   MOD-IDDISTR-TOM  (INDX)                
061300                                   MOD-IDKUNDNR-FOM (INDX)                
061400                                   MOD-IDKUNDNR-TOM (INDX)                
061500                                                                          
061600           MOVE +1 TO KV-IX                                               
061700           PERFORM UNTIL KV-IX > MAX-KV-IX                                
061800            MOVE MFS-RENSA-FAELT TO MOD-KVBEART-MIN (INDX, KV-IX)         
061900            MOVE MFS-RENSA-FAELT TO MOD-KDDDGS      (INDX, KV-IX)         
062000            MOVE MFS-RENSA-FAELT TO MOD-FLDDGS      (INDX, KV-IX)         
062100            MOVE MFS-RENSA-FAELT TO MOD-IDDC        (INDX, KV-IX)         
062200            ADD +1 TO KV-IX                                               
062300           END-PERFORM                                                    
062400         END-IF                                                           
062500         ADD +1 TO INDX                                                   
062600         PERFORM IMS-GNP-DIR                                              
062700       END-PERFORM                                                        
062800                                                                          
062900       IF SEGMENT-FINNS                                                   
063000         MOVE DIR-IDDISTR-FOM      TO SPAR-IDDISTR-FOM-NEXT               
063100         MOVE DIR-IDDISTR-TOM      TO SPAR-IDDISTR-TOM-NEXT               
063200         MOVE DIR-IDKUNDNR-FOM     TO SPAR-IDKUNDNR-FOM-NEXT              
063300         MOVE DIR-IDKUNDNR-TOM     TO SPAR-IDKUNDNR-TOM-NEXT              
063400         IF MED-IDMFSINF NOT = INF-UPDATE-DONE                            
063500           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
063600                                        MSG-KOM-IDMFSMED                  
063700           CALL WMEDKONV USING MED-WMEDAREA                               
063800           MOVE MED-TEMFSINF         TO MOD-TEMFSINF                      
063900         END-IF                                                           
064000       ELSE                                                               
064100         INITIALIZE SPAR-IDDISTR-FOM-NEXT                                 
064200                    SPAR-IDDISTR-TOM-NEXT                                 
064300                    SPAR-IDKUNDNR-FOM-NEXT                                
064400                    SPAR-IDKUNDNR-TOM-NEXT                                
064500         MOVE INF-LAST-PAGE         TO MED-IDMFSINF                       
064600                                       MSG-KOM-IDMFSMED                   
064700         CALL WMEDKONV           USING MED-WMEDAREA                       
064800         MOVE MED-TEMFSINF          TO MOD-TEMFSINF                       
064900       END-IF                                                             
065000     END-IF                                                               
065100                                                                          
065200     MOVE '002'                  TO MSGI-KDCALL                           
065300     MOVE '2335'                 TO SPAR-IDTRANS                          
065400     MOVE SPAR-AREA              TO MSGI-SPAR-AREA                        
065500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
065600     .                                                                    
065700     EJECT                                                                
065800 G-KOLLA-INPUT SECTION.                                                   
065900                                                                          
066000     MOVE JA  TO INDATA-SW                                                
066100     MOVE NEJ TO CMD-SW                                                   
066200     MOVE +1 TO INDX                                                      
066300     PERFORM UNTIL INDX > MAX-INDX                                        
066400       IF MID-CMD (INDX) NOT = ALL '+'                                    
066500         MOVE JA  TO CMD-SW                                               
066600       END-IF                                                             
066700       ADD +1 TO INDX                                                     
066800     END-PERFORM                                                          
066900*    CALL FELLOG                                                          
067000     IF MID-CMD-E = ALL '+'                                               
067100     AND MID-IDDISTR-FOM-E  = ALL '+'                                     
067200     AND MID-IDDISTR-TOM-E  = ALL '+'                                     
067300     AND MID-IDKUNDNR-FOM-E = ALL '+'                                     
067400     AND MID-IDKUNDNR-TOM-E = ALL '+'                                     
067500     AND NOT CMD-OK                                                       
067600       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
067700                                    MSG-KOM-IDMFSMED                      
067800       CALL WMEDKONV USING MED-WMEDAREA                                   
067900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
068000       PERFORM MFS-ROER-EJ-FAELT-IN                                       
068100       PERFORM MFS-ROER-EJ-FAELT-UT                                       
068200       MOVE NEJ TO INDATA-SW                                              
068300     ELSE                                                                 
068400                                                                          
068500*    -- KONTROLL AV MID-CMD                                               
068600       MOVE +1 TO INDX                                                    
068700       PERFORM UNTIL INDX > MAX-INDX                                      
068800         IF MID-CMD (INDX) NOT = ' '                                      
068900           IF MID-CMD (INDX) = 'D'                                        
069000             MOVE NEJ                  TO NYPOST-SW                       
069100             MOVE MAX-INDX             TO INDX                            
069200             MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR (INDX)             
069300           END-IF                                                         
069400         END-IF                                                           
069500         ADD +1 TO INDX                                                   
069600       END-PERFORM                                                        
069700                                                                          
069800       IF NYPOST-OK                                                       
069900         IF MID-CMD-E = 'N' OR MID-CMD-E = 'E'                            
070000            MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-E-ATTR                   
070100         ELSE                                                             
070200            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
070300                                         MSG-KOM-IDMFSMED                 
070400            MOVE MFS-ALFA-FAELT-FEL   TO MOD-CMD-E-ATTR                   
070500            MOVE NEJ                  TO INDATA-SW                        
070600         END-IF                                                           
070700                                                                          
070800         PERFORM IMS-GHU-LEV                                              
070900                                                                          
071000         IF SEGMENT-SAKNAS                                                
071100           MOVE GROUP-MISSING TO MED-IDMFSFEL                             
071200                                 MSG-KOM-IDMFSMED                         
071300           CALL WMEDKONV USING MED-WMEDAREA                               
071400           MOVE MED-MFSFEL    TO MOD-TEMFSFEL                             
071500           PERFORM MFS-RENSA-FAELT-UT                                     
071600           PERFORM MFS-FORM-ATTR                                          
071700           MOVE NEJ           TO INDATA-SW                                
071800         END-IF                                                           
071900                                                                          
072000         IF INDATA-OK                                                     
072100           EVALUATE MID-CMD-E                                             
072200             WHEN 'N'                                                     
072300               PERFORM GA-KOLLA-NYPOST                                    
072400             WHEN 'E'                                                     
072500               PERFORM GB-KOLLA-AENDRING                                  
072600             WHEN 'D'                                                     
072700               PERFORM GC-KOLLA-BORTTAG                                   
072800           END-EVALUATE                                                   
072900         END-IF                                                           
073000       END-IF                                                             
073100                                                                          
073200       IF INDATA-FEL                                                      
073300         IF MED-IDMFSFEL = SPACE                                          
073400           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
073500                                        MSG-KOM-IDMFSMED                  
073600         END-IF                                                           
073700         CALL WMEDKONV USING MED-WMEDAREA                                 
073800         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
073900         PERFORM MFS-ROER-EJ-FAELT-UT                                     
074000         PERFORM MFS-ROER-EJ-FAELT-IN                                     
074100         MOVE NEJ TO ALLT-SW                                              
074200       END-IF                                                             
074300     END-IF                                                               
074400     .                                                                    
074500     EJECT                                                                
074600 GA-KOLLA-NYPOST SECTION.                                                 
074700*---KOLLA OM DISTRIKT/KUND FINNS PÅ BAS                                   
074800     MOVE NEJ TO KUNDNR-FOM-NOLL                                          
074900     MOVE JA  TO DISTRIKT-SW                                              
075000                 KUNDNR-SW                                                
075100                                                                          
075200     INSPECT MID-IDDISTR-FOM-E REPLACING LEADING SPACE BY ZERO            
075300     INSPECT MID-IDDISTR-TOM-E REPLACING LEADING SPACE BY ZERO            
075400     IF MID-IDDISTR-FOM-E NOT = ALL '+'                                   
075500       IF MID-IDDISTR-FOM-E NUMERIC                                       
075600         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-FOM-E-ATTR               
075700       ELSE                                                               
075800         MOVE NEJ TO INDATA-SW                                            
075900                     DISTRIKT-SW                                          
076000         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-FOM-E-ATTR               
076100       END-IF                                                             
076200     ELSE                                                                 
076300       MOVE NEJ TO INDATA-SW                                              
076400                   DISTRIKT-SW                                            
076500       MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-FOM-E-ATTR               
076600     END-IF                                                               
076700                                                                          
076800     IF MID-IDDISTR-TOM-E NOT = ALL '+'                                   
076900       IF MID-IDDISTR-FOM-E NOT = ALL '+'                                 
077000         IF MID-IDDISTR-TOM-E NUMERIC AND MID-IDDISTR-TOM-E               
077100             NOT <  MID-IDDISTR-FOM-E                                     
077200           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-TOM-E-ATTR             
077300         ELSE                                                             
077400           MOVE NEJ TO INDATA-SW                                          
077500           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-TOM-E-ATTR             
077600         END-IF                                                           
077700       ELSE                                                               
077800         MOVE NEJ TO INDATA-SW                                            
077900         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-TOM-E-ATTR             
078000       END-IF                                                             
078100     ELSE                                                                 
078200       IF DISTRIKT-OK                                                     
078300         MOVE MID-IDDISTR-FOM-E   TO MID-IDDISTR-TOM-E                    
078400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-TOM-E-ATTR               
078500       END-IF                                                             
078600     END-IF                                                               
078700                                                                          
078800     IF MID-IDKUNDNR-FOM-E NOT = ALL '+'                                  
078900       IF MID-IDDISTR-FOM-E = MID-IDDISTR-TOM-E AND                       
079000                                             DISTRIKT-OK                  
079100         IF MID-IDKUNDNR-FOM-E NUMERIC                                    
079200           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-FOM-E-ATTR            
079300         ELSE                                                             
079400           MOVE NEJ TO INDATA-SW                                          
079500                       KUNDNR-SW                                          
079600           MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-FOM-E-ATTR              
079700         END-IF                                                           
079800*      ELSE                                                               
079900*        MOVE NEJ TO INDATA-SW                                            
080000*                    KUNDNR-SW                                            
080100*        MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-FOM-E-ATTR                
080200       END-IF                                                             
080300     ELSE                                                                 
080400       MOVE JA TO KUNDNR-FOM-NOLL                                         
080500       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-FOM-E-ATTR                
080600     END-IF                                                               
080700                                                                          
080800     IF MID-IDKUNDNR-TOM-E NOT = ALL '+'                                  
080900       IF MID-IDKUNDNR-FOM-E NOT = ALL '+'                                
081000         IF MID-IDKUNDNR-TOM-E NUMERIC AND MID-IDKUNDNR-TOM-E             
081100             NOT <  MID-IDKUNDNR-FOM-E                                    
081200           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-TOM-E-ATTR            
081300         ELSE                                                             
081400           MOVE NEJ TO INDATA-SW                                          
081500                       KUNDNR-SW                                          
081600           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-TOM-E-ATTR            
081700         END-IF                                                           
081800       ELSE                                                               
081900         MOVE NEJ TO INDATA-SW                                            
082000                     KUNDNR-SW                                            
082100         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-TOM-E-ATTR                
082200       END-IF                                                             
082300     ELSE                                                                 
082400       IF MID-IDKUNDNR-FOM-E NOT = ALL '+'                                
082500         IF KUNDNR-OK                                                     
082600           MOVE MID-IDKUNDNR-FOM-E  TO MID-IDKUNDNR-TOM-E                 
082700           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-TOM-E-ATTR            
082800         END-IF                                                           
082900       ELSE                                                               
083000         MOVE 999999 TO MID-IDKUNDNR-TOM-E                                
083100         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-TOM-E-ATTR              
083200       END-IF                                                             
083300     END-IF                                                               
083400                                                                          
083500     IF KUNDNR-FOM-NOLL = JA                                              
083600       MOVE ZERO TO MID-IDKUNDNR-FOM-E                                    
083700     END-IF                                                               
083800                                                                          
083900     IF MID-IDKUNDNR-FOM-E = MID-IDKUNDNR-TOM-E                           
084000       MOVE MID-IDDISTR-FOM-E  TO W-IDDISTR                               
084100       MOVE MID-IDKUNDNR-FOM-E TO W-IDKUNDNR                              
084200       PERFORM IMS-GU-GMTA-WDB201                                         
084300       IF SEGMENT-SAKNAS                                                  
084400         MOVE NEJ TO INDATA-SW                                            
084500         MOVE NEJ TO DISTRIKT-SW                                          
084600         MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-FOM-E-ATTR                
084700                                   MOD-IDKUNDNR-TOM-E-ATTR                
084800         MOVE DISTR-MISSING     TO MED-IDMFSFEL                           
084900                                   MSG-KOM-IDMFSMED                       
085000         CALL WMEDKONV USING MED-WMEDAREA                                 
085100         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
085200       END-IF                                                             
085300     END-IF                                                               
085400                                                                          
085500     MOVE +1 TO KV-IX                                                     
085600     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
085700       IF MID-KDDDGS-E (KV-IX) NOT = ALL '+'                              
085800         IF MID-KDDDGS-E (KV-IX) = 'O' OR 'N'                             
085900           MOVE MFS-ALFA-FAELT-RAETT TO                                   
086000                               MOD-KDDDGS-E-ATTR (KV-IX)                  
086100         ELSE                                                             
086200           MOVE NEJ TO INDATA-SW                                          
086300           MOVE NEJ TO DISTRIKT-SW                                        
086400           MOVE MFS-ALFA-FAELT-FEL                                        
086500                                 TO MOD-KDDDGS-E-ATTR (KV-IX)             
086600         END-IF                                                           
086700       ELSE                                                               
086800         MOVE 'O'                TO MID-KDDDGS-E (KV-IX)                  
086900         MOVE MFS-ALFA-FAELT-RAETT                                        
087000                                 TO MOD-KDDDGS-E-ATTR (KV-IX)             
087100       END-IF                                                             
087200       IF MID-FLDDGS-E (KV-IX) NOT = ALL '+'                              
087300         IF MID-FLDDGS-E (KV-IX) = 'Y' OR 'J' OR 'N'                      
087400           MOVE MFS-ALFA-FAELT-RAETT TO                                   
087500                               MOD-FLDDGS-E-ATTR (KV-IX)                  
087600         ELSE                                                             
087700           MOVE NEJ TO INDATA-SW                                          
087800           MOVE NEJ TO DISTRIKT-SW                                        
087900           MOVE MFS-ALFA-FAELT-FEL                                        
088000                                 TO MOD-FLDDGS-E-ATTR (KV-IX)             
088100         END-IF                                                           
088200       ELSE                                                               
088300         MOVE 'N'                TO MID-FLDDGS-E (KV-IX)                  
088400         MOVE MFS-ALFA-FAELT-RAETT                                        
088500                                 TO MOD-FLDDGS-E-ATTR (KV-IX)             
088600       END-IF                                                             
088700       IF MID-KVBEART-MIN-E (KV-IX) NOT = ALL '+'                         
088800         IF MID-KVBEART-MIN-E (KV-IX) NUMERIC                             
088900           MOVE MFS-NUM-FAELT-RAETT TO                                    
089000                               MOD-KVBEART-MIN-E-ATTR (KV-IX)             
089100         ELSE                                                             
089200           MOVE NEJ TO INDATA-SW                                          
089300           MOVE NEJ TO DISTRIKT-SW                                        
089400           MOVE MFS-NUM-FAELT-FEL TO                                      
089500                               MOD-KVBEART-MIN-E-ATTR (KV-IX)             
089600         END-IF                                                           
089700       ELSE                                                               
089800         MOVE ZERO TO MID-KVBEART-MIN-E (KV-IX)                           
089900         MOVE MFS-NUM-FAELT-RAETT TO                                      
090000                               MOD-KVBEART-MIN-E-ATTR (KV-IX)             
090100       END-IF                                                             
090200       IF MID-IDDC-E (KV-IX)  NOT = ALL '+'                               
090300         MOVE 'J'                   TO SW-SDC                             
090400         IF MID-IDDC-E (KV-IX) NOT = '  '                                 
090500*       TESTA MOT WDB6                                                    
090600         MOVE MID-IDDC-E (KV-IX)    TO W-IDDC-B6                          
090700         PERFORM IMS-GU-WDB601                                            
090800          IF DCS-SDC AND (DCS-IDLANDX2 NOT = 'CN')                        
090900           CONTINUE                                                       
091000          ELSE                                                            
091100           MOVE 'N'                 TO SW-SDC                             
091200          END-IF                                                          
091300         END-IF                                                           
091400         IF (MID-IDDISTR-FOM-E = MID-IDDISTR-TOM-E AND                    
091500             SDC                                   AND                    
091600             MID-IDDC-E        (KV-IX) > '  '      AND                    
091700             MID-KVBEART-MIN-E (KV-IX) > 0         AND                    
091800            (MID-KDDDGS-E      (KV-IX) = 'O' OR 'N') AND                  
091900                                KV-IX  < 6)            OR                 
092000            (MID-IDDISTR-FOM-E = MID-IDDISTR-TOM-E AND                    
092100             SDC                                   AND                    
092200            (MID-IDDC-E        (KV-IX) = '  '      OR                     
092300             MID-IDDC-E        (KV-IX) = '++')     AND                    
092400                                KV-IX  < 6)                               
092500           MOVE MFS-ALFA-FAELT-RAETT TO                                   
092600                               MOD-IDDC-E-ATTR (KV-IX)                    
092700         ELSE                                                             
092800           MOVE NEJ     TO INDATA-SW                                      
092900           MOVE NEJ     TO DISTRIKT-SW                                    
093000           MOVE MFS-ALFA-FAELT-FEL TO                                     
093100                               MOD-IDDC-E-ATTR (KV-IX)                    
093200         END-IF                                                           
093300       ELSE                                                               
093400         MOVE '  '      TO MID-IDDC-E (KV-IX)                             
093500         MOVE MFS-ALFA-FAELT-RAETT TO                                     
093600                               MOD-IDDC-E-ATTR (KV-IX)                    
093700       END-IF                                                             
093800       ADD +1 TO KV-IX                                                    
093900     END-PERFORM                                                          
094000                                                                          
094100     IF DISTRIKT-OK                                                       
094200       MOVE MID-IDDISTR-FOM-E  TO W-IDDISTR-FOM                           
094300       MOVE MID-IDDISTR-TOM-E  TO W-IDDISTR-TOM                           
094400       MOVE +0                 TO W-IDKUNDNR-FOM                          
094500       MOVE +9999999           TO W-IDKUNDNR-TOM                          
094600                                                                          
094700       PERFORM  IMS-GU-DIR-OKVAL-KUND                                     
094800       IF SEGMENT-FINNS                                                   
094900         IF MID-IDDISTR-FOM-E = MID-IDDISTR-TOM-E AND                     
095000            MID-IDDISTR-FOM-E = DIR-IDDISTR-FOM AND                       
095100            MID-IDDISTR-FOM-E = DIR-IDDISTR-TOM                           
095200                                                                          
095300           MOVE MID-IDKUNDNR-FOM-E TO W-IDKUNDNR-FOM                      
095400           MOVE MID-IDKUNDNR-TOM-E TO W-IDKUNDNR-TOM                      
095500           IF MID-IDKUNDNR-TOM-E = 999999                                 
095600              MOVE +9999999 TO W-IDKUNDNR-TOM                             
095700           END-IF                                                         
095800                                                                          
095900           PERFORM IMS-GU-DIR-KVAL-KUND                                   
096000           IF SEGMENT-FINNS                                               
096100             MOVE NEJ TO INDATA-SW                                        
096200             MOVE MFS-NUM-FAELT-FEL TO MOD-IDKUNDNR-FOM-E-ATTR            
096300                                       MOD-IDKUNDNR-TOM-E-ATTR            
096400             MOVE DISTR-EXISTS      TO MED-IDMFSFEL                       
096500                                       MSG-KOM-IDMFSMED                   
096600           END-IF                                                         
096700         ELSE                                                             
096800           MOVE NEJ TO INDATA-SW                                          
096900           MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-FOM-E-ATTR               
097000                                     MOD-IDDISTR-TOM-E-ATTR               
097100           MOVE DISTR-EXISTS      TO MED-IDMFSFEL                         
097200                                     MSG-KOM-IDMFSMED                     
097300         END-IF                                                           
097400       ELSE                                                               
097500         MOVE MID-IDKUNDNR-FOM-E  TO W-IDKUNDNR-FOM                       
097600         MOVE MID-IDKUNDNR-TOM-E  TO W-IDKUNDNR-TOM                       
097700       END-IF                                                             
097800     END-IF                                                               
097900                                                                          
098000* KONTROLL ATT ÅTMINSTONE DEFAULT-PRM FINNS UPPLAGDA                      
098100* FÖR DIREKTLEVERANSSTYRNING I LEVERANTÖRSREGISTRET                       
098200     MOVE +9999                   TO W-IDDISTR-WDF1                       
098300     MOVE +999999                 TO W-IDKUNDNR-WDF1                      
098400                                                                          
098500     IF INDATA-OK                                                         
098600       MOVE +1 TO KV-IX                                                   
098700       PERFORM UNTIL KV-IX > MAX-KV-IX                                    
098800         IF MID-KVBEART-MIN-E (KV-IX) > ZERO                              
098900           COMPUTE W-KDORDKL-WDF1 = KV-IX - 1                             
099000           PERFORM IMS-GU-WDF118                                          
099100           IF SEGMENT-SAKNAS                                              
099200             MOVE NEJ               TO INDATA-SW                          
099300             MOVE MFS-NUM-FAELT-FEL TO                                    
099400                                    MOD-KVBEART-MIN-E-ATTR (KV-IX)        
099500                                    MOD-IDDISTR-FOM-E-ATTR                
099600                                    MOD-IDDISTR-TOM-E-ATTR                
099700             MOVE INFO-MISSING      TO MED-IDMFSFEL                       
099800                                       MSG-KOM-IDMFSMED                   
099900             MOVE +5                TO KV-IX                              
100000           END-IF                                                         
100100         END-IF                                                           
100200         ADD +1 TO KV-IX                                                  
100300       END-PERFORM                                                        
100400     END-IF                                                               
100500     .                                                                    
100600     EJECT                                                                
100700 GB-KOLLA-AENDRING SECTION.                                               
100800                                                                          
100900     INSPECT MID-IDDISTR-FOM-E REPLACING LEADING SPACE BY ZERO            
101000     INSPECT MID-IDDISTR-TOM-E REPLACING LEADING SPACE BY ZERO            
101100     IF MID-IDDISTR-FOM-E NOT = ALL '+'                                   
101200       IF MID-IDDISTR-FOM-E NUMERIC                                       
101300         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-FOM-E-ATTR               
101400         MOVE MID-IDDISTR-FOM-E  TO W-IDDISTR-FOM                         
101500       ELSE                                                               
101600         MOVE NEJ TO INDATA-SW                                            
101700         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-FOM-E-ATTR               
101800       END-IF                                                             
101900     ELSE                                                                 
102000       MOVE NEJ TO INDATA-SW                                              
102100       MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-FOM-E-ATTR               
102200     END-IF                                                               
102300                                                                          
102400     IF MID-IDDISTR-TOM-E NOT = ALL '+'                                   
102500       IF MID-IDDISTR-TOM-E NUMERIC                                       
102600         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-TOM-E-ATTR               
102700         MOVE MID-IDDISTR-TOM-E  TO W-IDDISTR-TOM                         
102800       ELSE                                                               
102900         MOVE NEJ TO INDATA-SW                                            
103000         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-TOM-E-ATTR               
103100       END-IF                                                             
103200     ELSE                                                                 
103300       MOVE NEJ TO INDATA-SW                                              
103400       MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-TOM-E-ATTR               
103500     END-IF                                                               
103600                                                                          
103700     IF MID-IDKUNDNR-FOM-E NOT = ALL '+'                                  
103800       IF MID-IDKUNDNR-FOM-E NUMERIC                                      
103900         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-FOM-E-ATTR              
104000         MOVE MID-IDKUNDNR-FOM-E  TO W-IDKUNDNR-FOM                       
104100       ELSE                                                               
104200         MOVE NEJ TO INDATA-SW                                            
104300         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-FOM-E-ATTR              
104400       END-IF                                                             
104500     ELSE                                                                 
104600       MOVE NEJ TO INDATA-SW                                              
104700       MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKUNDNR-FOM-E-ATTR              
104800     END-IF                                                               
104900                                                                          
105000     IF MID-IDKUNDNR-TOM-E NOT = ALL '+'                                  
105100       IF MID-IDKUNDNR-TOM-E NUMERIC                                      
105200         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-TOM-E-ATTR              
105300         MOVE MID-IDKUNDNR-TOM-E  TO W-IDKUNDNR-TOM                       
105400       ELSE                                                               
105500         MOVE NEJ TO INDATA-SW                                            
105600         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-TOM-E-ATTR              
105700       END-IF                                                             
105800     ELSE                                                                 
105900       MOVE NEJ TO INDATA-SW                                              
106000       MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKUNDNR-TOM-E-ATTR              
106100     END-IF                                                               
106200                                                                          
106300     MOVE +1 TO KV-IX                                                     
106400     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
106500       IF MID-KDDDGS-E (KV-IX) NOT = ALL '+'                              
106600         IF MID-KDDDGS-E (KV-IX) = 'O' OR 'N'                             
106700          MOVE MFS-ALFA-FAELT-RAETT TO                                    
106800                               MOD-KDDDGS-E-ATTR (KV-IX)                  
106900         ELSE                                                             
107000           MOVE NEJ TO INDATA-SW                                          
107100           MOVE MFS-ALFA-FAELT-FEL TO                                     
107200                               MOD-KDDDGS-E-ATTR (KV-IX)                  
107300         END-IF                                                           
107400       END-IF                                                             
107500       IF MID-FLDDGS-E (KV-IX) NOT = ALL '+'                              
107600         IF MID-FLDDGS-E (KV-IX) = 'Y' OR 'J' OR 'N'                      
107700          MOVE MFS-ALFA-FAELT-RAETT TO                                    
107800                               MOD-FLDDGS-E-ATTR (KV-IX)                  
107900         ELSE                                                             
108000           MOVE NEJ TO INDATA-SW                                          
108100           MOVE MFS-ALFA-FAELT-FEL TO                                     
108200                               MOD-FLDDGS-E-ATTR (KV-IX)                  
108300         END-IF                                                           
108400       END-IF                                                             
108500       IF MID-KVBEART-MIN-E (KV-IX) NOT = ALL '+'                         
108600         IF MID-KVBEART-MIN-E (KV-IX) NUMERIC                             
108700          MOVE MFS-NUM-FAELT-RAETT TO                                     
108800                               MOD-KVBEART-MIN-E-ATTR (KV-IX)             
108900         ELSE                                                             
109000           MOVE NEJ TO INDATA-SW                                          
109100           MOVE MFS-NUM-FAELT-FEL TO                                      
109200                               MOD-KVBEART-MIN-E-ATTR (KV-IX)             
109300         END-IF                                                           
109400       END-IF                                                             
109500       IF MID-IDDC-E (KV-IX) NOT = ALL '+'                                
109600         PERFORM IMS-GHU-DIR                                              
109700                                                                          
109800         IF SEGMENT-FINNS                                                 
109900           MOVE 'J'                   TO SW-SDC                           
110000*         KOLLA MOT WDB601                                                
110100           IF MID-IDDC-E (KV-IX) NOT = '  '                               
110200             MOVE MID-IDDC-E (KV-IX)    TO W-IDDC-B6                      
110300             PERFORM IMS-GU-WDB601                                        
110400             IF DCS-SDC AND ( DCS-IDLANDX2 NOT = 'CN')                    
110500               CONTINUE                                                   
110600             ELSE                                                         
110700               MOVE 'N'                 TO SW-SDC                         
110800             END-IF                                                       
110900           END-IF                                                         
111000           IF MID-KVBEART-MIN-E (KV-IX) = ALL '+'                         
111100             MOVE DIR-KVBEART-MIN (KV-IX)   TO WS-KVBEART                 
111200           ELSE                                                           
111300             MOVE MID-KVBEART-MIN-E (KV-IX) TO WS-KVBEART                 
111400           END-IF                                                         
111500           IF (MID-IDDISTR-FOM-E = MID-IDDISTR-TOM-E AND                  
111600               SDC                                   AND                  
111700               MID-IDDC-E        (KV-IX) > '  '      AND                  
111800               WS-KVBEART                > 0         AND                  
111900                                  KV-IX  < 6)            OR               
112000              (MID-IDDISTR-FOM-E = MID-IDDISTR-TOM-E AND                  
112100               SDC                                   AND                  
112200               MID-IDDC-E        (KV-IX) = '  '      AND                  
112300                                  KV-IX  < 6)                             
112400             MOVE MFS-ALFA-FAELT-RAETT TO                                 
112500                               MOD-IDDC-E-ATTR (KV-IX)                    
112600           ELSE                                                           
112700             MOVE NEJ TO INDATA-SW                                        
112800             MOVE MFS-ALFA-FAELT-FEL TO                                   
112900                               MOD-IDDC-E-ATTR (KV-IX)                    
113000           END-IF                                                         
113100         END-IF                                                           
113200       END-IF                                                             
113300       ADD +1 TO KV-IX                                                    
113400     END-PERFORM                                                          
113500                                                                          
113600     IF INDATA-OK                                                         
113700       PERFORM IMS-GHU-DIR                                                
113800       IF SEGMENT-SAKNAS                                                  
113900         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
114000                               MSG-KOM-IDMFSMED                           
114100         CALL WMEDKONV USING MED-WMEDAREA                                 
114200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
114300         MOVE NEJ TO INDATA-SW                                            
114400         PERFORM MFS-RENSA-FAELT-UT                                       
114500       END-IF                                                             
114600     END-IF                                                               
114700     .                                                                    
114800     EJECT                                                                
114900 GC-KOLLA-BORTTAG SECTION.                                                
115000                                                                          
115100     INSPECT MID-IDDISTR-FOM-E REPLACING LEADING SPACE BY ZERO            
115200     INSPECT MID-IDDISTR-TOM-E REPLACING LEADING SPACE BY ZERO            
115300     IF MID-IDDISTR-FOM-E NOT = ALL '+'                                   
115400       IF MID-IDDISTR-FOM-E NUMERIC                                       
115500         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-FOM-E-ATTR               
115600         MOVE MID-IDDISTR-FOM-E  TO W-IDDISTR-FOM                         
115700       ELSE                                                               
115800         MOVE NEJ TO INDATA-SW                                            
115900         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-FOM-E-ATTR               
116000       END-IF                                                             
116100     ELSE                                                                 
116200       MOVE NEJ TO INDATA-SW                                              
116300       MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-FOM-E-ATTR               
116400     END-IF                                                               
116500                                                                          
116600     IF MID-IDDISTR-TOM-E NOT = ALL '+'                                   
116700       IF MID-IDDISTR-TOM-E NUMERIC                                       
116800         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-TOM-E-ATTR               
116900         MOVE MID-IDDISTR-TOM-E  TO W-IDDISTR-TOM                         
117000       ELSE                                                               
117100         MOVE NEJ TO INDATA-SW                                            
117200         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-TOM-E-ATTR               
117300       END-IF                                                             
117400     ELSE                                                                 
117500       MOVE NEJ TO INDATA-SW                                              
117600       MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-TOM-E-ATTR               
117700     END-IF                                                               
117800                                                                          
117900     IF MID-IDKUNDNR-FOM-E NOT = ALL '+'                                  
118000       IF MID-IDKUNDNR-FOM-E NUMERIC                                      
118100         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-FOM-E-ATTR              
118200         MOVE MID-IDKUNDNR-FOM-E  TO W-IDKUNDNR-FOM                       
118300       ELSE                                                               
118400         MOVE NEJ TO INDATA-SW                                            
118500         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-FOM-E-ATTR              
118600       END-IF                                                             
118700     ELSE                                                                 
118800       MOVE NEJ TO INDATA-SW                                              
118900       MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKUNDNR-FOM-E-ATTR              
119000     END-IF                                                               
119100                                                                          
119200     IF MID-IDKUNDNR-TOM-E NOT = ALL '+'                                  
119300       IF MID-IDKUNDNR-TOM-E NUMERIC                                      
119400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-TOM-E-ATTR              
119500         MOVE MID-IDKUNDNR-TOM-E  TO W-IDKUNDNR-TOM                       
119600       ELSE                                                               
119700         MOVE NEJ TO INDATA-SW                                            
119800         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-TOM-E-ATTR              
119900       END-IF                                                             
120000     ELSE                                                                 
120100       MOVE NEJ TO INDATA-SW                                              
120200       MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKUNDNR-TOM-E-ATTR              
120300     END-IF                                                               
120400                                                                          
120500     IF INDATA-OK                                                         
120600       PERFORM IMS-GHU-DIR                                                
120700       IF SEGMENT-SAKNAS                                                  
120800         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
120900                               MSG-KOM-IDMFSMED                           
121000         CALL WMEDKONV USING MED-WMEDAREA                                 
121100         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
121200         MOVE NEJ TO INDATA-SW                                            
121300         PERFORM MFS-RENSA-FAELT-UT                                       
121400       END-IF                                                             
121500     END-IF                                                               
121600     .                                                                    
121700     EJECT                                                                
121800 H-UPPDATERA SECTION.                                                     
121900                                                                          
122000     INSPECT MID-IDDISTR-FOM-E REPLACING LEADING ZERO BY SPACE            
122100     INSPECT MID-IDDISTR-TOM-E REPLACING LEADING ZERO BY SPACE            
122200                                                                          
122300     EVALUATE MID-CMD-E                                                   
122400       WHEN 'N'                                                           
122500         PERFORM HA-SKAPA-NYPOST                                          
122600       WHEN 'E'                                                           
122700         PERFORM HB-UPPDATERA-POST                                        
122800       WHEN OTHER                                                         
122900         PERFORM HC-TA-BORT-POSTER                                        
123000     END-EVALUATE                                                         
123100                                                                          
123200*---FÖR ATT POSITIONERA SIG VID LÄSNING AV 1:A POST                       
123300*    MOVE SPAR-IDDISTR-FOM-ENTER  TO W-IDDISTR-FOM                        
123400*    MOVE SPAR-IDDISTR-TOM-ENTER  TO W-IDDISTR-TOM                        
123500*    MOVE SPAR-IDKUNDNR-FOM-ENTER TO W-IDKUNDNR-FOM                       
123600*    MOVE SPAR-IDKUNDNR-TOM-ENTER TO W-IDKUNDNR-TOM                       
123700                                                                          
123800     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
123900                             MSG-KOM-IDMFSMED                             
124000     CALL WMEDKONV USING MED-WMEDAREA                                     
124100     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
124200     PERFORM MFS-FORM-ATTR                                                
124300     PERFORM MFS-RENSA-FAELT-IN                                           
124400     .                                                                    
124500     EJECT                                                                
124600 HA-SKAPA-NYPOST SECTION.                                                 
124700                                                                          
124800     PERFORM IMS-GHU-LEV                                                  
124900     MOVE MID-IDDISTR-FOM-E  TO DIR-IDDISTR-FOM                           
125000     MOVE MID-IDDISTR-TOM-E  TO DIR-IDDISTR-TOM                           
125100     MOVE MID-IDKUNDNR-FOM-E TO DIR-IDKUNDNR-FOM                          
125200     MOVE MID-IDKUNDNR-TOM-E TO DIR-IDKUNDNR-TOM                          
125300                                                                          
125400     MOVE +1 TO KV-IX                                                     
125500     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
125600       MOVE MID-KVBEART-MIN-E (KV-IX) TO DIR-KVBEART-MIN (KV-IX)          
125700       MOVE MID-KDDDGS-E      (KV-IX) TO DIR-KDDDGS      (KV-IX)          
125800       IF MID-FLDDGS-E (KV-IX) = YES                                      
125900          MOVE JA                     TO DIR-FLDDGS      (KV-IX)          
126000       ELSE                                                               
126100          MOVE MID-FLDDGS-E   (KV-IX) TO DIR-FLDDGS      (KV-IX)          
126200       END-IF                                                             
126300       MOVE MID-IDDC-E        (KV-IX) TO DIR-IDDC        (KV-IX)          
126400       ADD +1 TO KV-IX                                                    
126500     END-PERFORM                                                          
126600                                                                          
126700     PERFORM IMS-ISRT-DIR                                                 
126800     .                                                                    
126900     EJECT                                                                
127000 HB-UPPDATERA-POST SECTION.                                               
127100                                                                          
127200     PERFORM IMS-GHU-DIR                                                  
127300     MOVE MID-IDDISTR-FOM-E  TO DIR-IDDISTR-FOM                           
127400     MOVE MID-IDDISTR-TOM-E  TO DIR-IDDISTR-TOM                           
127500     MOVE MID-IDKUNDNR-FOM-E TO DIR-IDKUNDNR-FOM                          
127600     MOVE MID-IDKUNDNR-TOM-E TO DIR-IDKUNDNR-TOM                          
127700                                                                          
127800     MOVE +1 TO KV-IX                                                     
127900     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
128000       IF MID-KVBEART-MIN-E (KV-IX) NOT = ALL '+'                         
128100         MOVE MID-KVBEART-MIN-E (KV-IX) TO DIR-KVBEART-MIN (KV-IX)        
128200       END-IF                                                             
128300                                                                          
128400       IF MID-KDDDGS-E      (KV-IX) NOT = ALL '+'                         
128500         MOVE MID-KDDDGS-E      (KV-IX) TO DIR-KDDDGS      (KV-IX)        
128600       END-IF                                                             
128700                                                                          
128800       IF MID-FLDDGS-E  (KV-IX) NOT = ALL '+'                             
128900         IF  MID-FLDDGS-E (KV-IX) = YES                                   
129000            MOVE JA                   TO DIR-FLDDGS (KV-IX)               
129100         ELSE                                                             
129200            MOVE MID-FLDDGS-E (KV-IX) TO DIR-FLDDGS (KV-IX)               
129300         END-IF                                                           
129400       END-IF                                                             
129500                                                                          
129600       IF MID-IDDC-E  (KV-IX) NOT = ALL '+'                               
129700         MOVE MID-IDDC-E (KV-IX)     TO DIR-IDDC (KV-IX)                  
129800*      ELSE                                                               
129900*        MOVE 'N'                    TO DIR-IDDC (KV-IX)                  
130000       END-IF                                                             
130100                                                                          
130200       ADD +1 TO KV-IX                                                    
130300     END-PERFORM                                                          
130400                                                                          
130500     PERFORM IMS-REPL-DIR                                                 
130600     .                                                                    
130700     EJECT                                                                
130800 HC-TA-BORT-POSTER SECTION.                                               
130900                                                                          
131000     MOVE +1 TO INDX                                                      
131100     PERFORM UNTIL INDX > MAX-INDX                                        
131200       IF MID-CMD (INDX)  = 'D'                                           
131300         MOVE SPAR-IDDISTR-FOM  (INDX) TO W-IDDISTR-FOM                   
131400         MOVE SPAR-IDDISTR-TOM  (INDX) TO W-IDDISTR-TOM                   
131500         MOVE SPAR-IDKUNDNR-FOM (INDX) TO W-IDKUNDNR-FOM                  
131600         MOVE SPAR-IDKUNDNR-TOM (INDX) TO W-IDKUNDNR-TOM                  
131700         PERFORM IMS-GHU-DIR                                              
131800         IF SEGMENT-FINNS                                                 
131900           PERFORM IMS-DLET-DIR                                           
132000         END-IF                                                           
132100       END-IF                                                             
132200       ADD +1 TO INDX                                                     
132300     END-PERFORM                                                          
132400     .                                                                    
132500     EJECT                                                                
132600 MFS-RENSA-FAELT-UT SECTION.                                              
132700                                                                          
132800*    --- ALLA UTDATA-FÄLT                                                 
132900*    --- INKL. BLÄDDRINGSNYCKLAR                                          
133000     MOVE MFS-RENSA-FAELT TO MOD-CMD-E                                    
133100                             MOD-IDDISTR-FOM-E                            
133200                             MOD-IDDISTR-TOM-E                            
133300                             MOD-IDKUNDNR-FOM-E                           
133400                             MOD-IDKUNDNR-TOM-E                           
133500                                                                          
133600     MOVE +1 TO KV-IX                                                     
133700     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
133800     MOVE MFS-RENSA-FAELT TO MOD-KVBEART-MIN-E  (KV-IX)                   
133900                             MOD-KDDDGS-E       (KV-IX)                   
134000                             MOD-FLDDGS-E       (KV-IX)                   
134100                             MOD-IDDC-E         (KV-IX)                   
134200       ADD +1 TO KV-IX                                                    
134300     END-PERFORM                                                          
134400                                                                          
134500     MOVE +1 TO KV-IX                                                     
134600                INDX                                                      
134700     PERFORM UNTIL INDX > MAX-INDX                                        
134800       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
134900       ADD +1 TO INDX                                                     
135000     END-PERFORM                                                          
135100     .                                                                    
135200     SKIP3                                                                
135300 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
135400                                                                          
135500*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
135600     MOVE MFS-RENSA-FAELT TO MOD-CMD            (INDX)                    
135700                             MOD-IDDISTR-FOM    (INDX)                    
135800                             MOD-IDDISTR-TOM    (INDX)                    
135900                             MOD-IDKUNDNR-FOM   (INDX)                    
136000                             MOD-IDKUNDNR-TOM   (INDX)                    
136100     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
136200       MOVE MFS-RENSA-FAELT TO MOD-KVBEART-MIN  (INDX, KV-IX)             
136300                               MOD-KDDDGS       (INDX, KV-IX)             
136400                               MOD-FLDDGS       (INDX, KV-IX)             
136500                               MOD-IDDC         (INDX, KV-IX)             
136600       ADD +1 TO KV-IX                                                    
136700     END-PERFORM                                                          
136800     .                                                                    
136900     SKIP3                                                                
137000 MFS-RENSA-FAELT-IN SECTION.                                              
137100                                                                          
137200*    --- ALLA INDATA-FÄLT                                                 
137300     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
137400                             MOD-IDDIRGRP-IN                              
137500                             MOD-IDDISTR-IN                               
137600                             MOD-IDKUNDNR-IN                              
137700                             MOD-IDDISTR-FOM-E                            
137800                             MOD-IDDISTR-TOM-E                            
137900                             MOD-IDKUNDNR-FOM-E                           
138000                             MOD-IDKUNDNR-TOM-E                           
138100     MOVE +1 TO INDX                                                      
138200     PERFORM UNTIL INDX > MAX-INDX                                        
138300       MOVE MFS-RENSA-FAELT TO MOD-CMD (INDX)                             
138400       ADD +1 TO INDX                                                     
138500     END-PERFORM                                                          
138600                                                                          
138700     MOVE +1 TO KV-IX                                                     
138800     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
138900     MOVE MFS-RENSA-FAELT TO MOD-KVBEART-MIN-E  (KV-IX)                   
139000                             MOD-KDDDGS-E       (KV-IX)                   
139100                             MOD-FLDDGS-E       (KV-IX)                   
139200                             MOD-IDDC-E         (KV-IX)                   
139300                                                                          
139400       ADD +1 TO KV-IX                                                    
139500     END-PERFORM                                                          
139600     .                                                                    
139700     EJECT                                                                
139800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
139900                                                                          
140000*    --- ALLA UTDATA-FÄLT                                                 
140100*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
140200     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-E                                  
140300                               MOD-IDDISTR-FOM-E                          
140400                               MOD-IDDISTR-TOM-E                          
140500                               MOD-IDKUNDNR-FOM-E                         
140600                               MOD-IDKUNDNR-TOM-E                         
140700                                                                          
140800     MOVE +1 TO KV-IX                                                     
140900     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
141000       MOVE MFS-ROER-EJ-FAELT TO MOD-KVBEART-MIN-E  (KV-IX)               
141100                                 MOD-KDDDGS-E       (KV-IX)               
141200                                 MOD-FLDDGS-E       (KV-IX)               
141300                                 MOD-IDDC-E         (KV-IX)               
141400       ADD +1 TO KV-IX                                                    
141500     END-PERFORM                                                          
141600                                                                          
141700     MOVE +1 TO INDX                                                      
141800     PERFORM UNTIL INDX > MAX-INDX                                        
141900       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
142000       ADD +1 TO INDX                                                     
142100     END-PERFORM                                                          
142200     SKIP2                                                                
142300     .                                                                    
142400 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
142500                                                                          
142600*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
142700     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD            (INDX)                  
142800                               MOD-IDDISTR-FOM    (INDX)                  
142900                               MOD-IDDISTR-TOM    (INDX)                  
143000                               MOD-IDKUNDNR-FOM   (INDX)                  
143100                               MOD-IDKUNDNR-TOM   (INDX)                  
143200     MOVE +1 TO KV-IX                                                     
143300     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
143400       MOVE MFS-ROER-EJ-FAELT TO MOD-KVBEART-MIN  (INDX, KV-IX)           
143500                                 MOD-KDDDGS       (INDX, KV-IX)           
143600                                 MOD-FLDDGS       (INDX, KV-IX)           
143700                                 MOD-IDDC         (INDX, KV-IX)           
143800     ADD +1 TO KV-IX                                                      
143900     END-PERFORM                                                          
144000                                                                          
144100     .                                                                    
144200     SKIP3                                                                
144300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
144400                                                                          
144500*    --- ALLA INDATA-FÄLT                                                 
144600     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-E                                  
144700                               MOD-IDDISTR-FOM-E                          
144800                               MOD-IDDISTR-TOM-E                          
144900                               MOD-IDKUNDNR-FOM-E                         
145000                               MOD-IDKUNDNR-TOM-E                         
145100     MOVE +1 TO KV-IX                                                     
145200     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
145300       MOVE MFS-ROER-EJ-FAELT TO MOD-KVBEART-MIN-E  (KV-IX)               
145400                                 MOD-KDDDGS-E       (KV-IX)               
145500                                 MOD-FLDDGS-E       (KV-IX)               
145600                                 MOD-IDDC-E         (KV-IX)               
145700                                                                          
145800       ADD +1 TO KV-IX                                                    
145900     END-PERFORM                                                          
146000                                                                          
146100     MOVE +1 TO INDX                                                      
146200     PERFORM UNTIL INDX > MAX-INDX                                        
146300       MOVE MFS-ROER-EJ-FAELT TO MOD-CMD (INDX)                           
146400       ADD +1 TO INDX                                                     
146500     END-PERFORM                                                          
146600     .                                                                    
146700     EJECT                                                                
146800 MFS-FORM-ATTR SECTION.                                                   
146900                                                                          
147000*    --- ALLA INDATA-FÄLT                                                 
147100     MOVE MFS-FORMATETS-ATTR TO MOD-CMD-E-ATTR                            
147200                                MOD-IDDISTR-FOM-E-ATTR                    
147300                                MOD-IDDISTR-TOM-E-ATTR                    
147400                                MOD-IDKUNDNR-FOM-E-ATTR                   
147500                                MOD-IDKUNDNR-TOM-E-ATTR                   
147600                                                                          
147700     MOVE +1 TO KV-IX                                                     
147800     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
147900     MOVE MFS-FORMATETS-ATTR TO MOD-KVBEART-MIN-E-ATTR (KV-IX)            
148000                                MOD-KDDDGS-E-ATTR      (KV-IX)            
148100                                MOD-FLDDGS-E-ATTR      (KV-IX)            
148200                                MOD-IDDC-E-ATTR        (KV-IX)            
148300                                                                          
148400       ADD +1 TO KV-IX                                                    
148500     END-PERFORM                                                          
148600     .                                                                    
148700     SKIP2                                                                
148800 MFS-LAES-IN-IGEN SECTION.                                                
148900                                                                          
149000*    --- ALLA INDATA-FÄLT                                                 
149100     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-E-ATTR                         
149200                                   MOD-IDDISTR-FOM-E-ATTR                 
149300                                   MOD-IDDISTR-TOM-E-ATTR                 
149400                                   MOD-IDKUNDNR-FOM-E-ATTR                
149500                                   MOD-IDKUNDNR-TOM-E-ATTR                
149600     MOVE +1 TO KV-IX                                                     
149700     PERFORM UNTIL KV-IX > MAX-KV-IX                                      
149800     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVBEART-MIN-E-ATTR (KV-IX)         
149900                                   MOD-KDDDGS-E-ATTR      (KV-IX)         
150000                                   MOD-FLDDGS-E-ATTR      (KV-IX)         
150100                                   MOD-IDDC-E-ATTR        (KV-IX)         
150200                                                                          
150300       ADD +1 TO KV-IX                                                    
150400     END-PERFORM                                                          
150500     MOVE +1 TO INDX                                                      
150600     PERFORM UNTIL INDX > MAX-INDX                                        
150700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR (INDX)                  
150800       ADD +1 TO INDX                                                     
150900     END-PERFORM                                                          
151000     .                                                                    
151100     EJECT                                                                
151200* --- IMS SEKTIONER ---                                                   
151300     SKIP3                                                                
151400 IMS-GET-MSG SECTION.                                                     
151500                                                                          
151600     MOVE '  QC' TO GODK-STATUSKODER                                      
151700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
151800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
151900     PERFORM IMS-STATUSKONTROLL                                           
152000     .                                                                    
152100     SKIP3                                                                
152200 IMS-INSERT-MSG SECTION.                                                  
152300                                                                          
152400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
152500     MOVE SPACE TO GODK-STATUSKODER                                       
152600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
152700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
152800     PERFORM IMS-STATUSKONTROLL                                           
152900     .                                                                    
153000     EJECT                                                                
153100 IMS-GET-WMSGKOM-MSG SECTION.                                             
153200                                                                          
153300     MOVE '  QD'   TO GODK-STATUSKODER                                    
153400     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
153500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
153600     PERFORM IMS-STATUSKONTROLL                                           
153700     .                                                                    
153800     EJECT                                                                
153900                                                                          
154000 IMS-INSERT-WMSGKOM-MSG SECTION.                                          
154100                                                                          
154200     MOVE '  '  TO GODK-STATUSKODER                                       
154300     CALL CBLTDLI USING ISRT MSGKOM-PCB MSG-KOM-WMSGKOM                   
154400     MOVE MSGKOM-STATUS-CODE TO STATUS-WS                                 
154500     PERFORM IMS-STATUSKONTROLL                                           
154600     .                                                                    
154700     EJECT                                                                
154800 IMS-GHU-LEV SECTION.                                                     
154900                                                                          
155000     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
155100          DELIMITED BY SIZE INTO SSA1                                     
155200     MOVE '  GE'                 TO GODK-STATUSKODER                      
155300     CALL CBLTDLI USING GHU WDF2-PCB DLI-IO-WLLEVF01 SSA1                 
155400     MOVE WDF2-STATUS-CODE       TO STATUS-WS                             
155500     PERFORM IMS-STATUSKONTROLL                                           
155600     .                                                                    
155700     EJECT                                                                
155800 IMS-GU-DIR-OKVAL-KUND SECTION.                                           
155900                                                                          
156000     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
156100          DELIMITED BY SIZE INTO SSA1                                     
156200     STRING 'WDF211  (IDDISTRF<=' W-IDDISTR-FOM-X                         
156300                    '&IDDISTRT>=' W-IDDISTR-FOM-X                         
156400                    '&IDKUNDNF>=' W-IDKUNDNR-FOM-X                        
156500                    '&IDKUNDNT<=' W-IDKUNDNR-TOM-X                        
156600                    '!IDDISTRF<=' W-IDDISTR-TOM-X                         
156700                    '&IDDISTRT>=' W-IDDISTR-TOM-X                         
156800                    '&IDKUNDNF>=' W-IDKUNDNR-FOM-X                        
156900                    '&IDKUNDNT<=' W-IDKUNDNR-TOM-X ')'                    
157000            DELIMITED BY SIZE INTO SSA2                                   
157100     MOVE '  GE'                  TO GODK-STATUSKODER                     
157200     CALL CBLTDLI USING GHU WDF2-PCB DLI-IO-WLLEVF11 SSA1 SSA2            
157300     MOVE WDF2-STATUS-CODE        TO STATUS-WS                            
157400     PERFORM IMS-STATUSKONTROLL                                           
157500     .                                                                    
157600     EJECT                                                                
157700 IMS-GU-DIR-KVAL-KUND SECTION.                                            
157800                                                                          
157900     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
158000          DELIMITED BY SIZE INTO SSA1                                     
158100     STRING 'WDF211  (IDDISTRF =' W-IDDISTR-FOM-X                         
158200                    '&IDDISTRT =' W-IDDISTR-TOM-X                         
158300                    '&IDKUNDNF<=' W-IDKUNDNR-FOM-X                        
158400                    '&IDKUNDNT>=' W-IDKUNDNR-FOM-X                        
158500                    '!IDDISTRF =' W-IDDISTR-FOM-X                         
158600                    '&IDDISTRT =' W-IDDISTR-TOM-X                         
158700                    '&IDKUNDNF<=' W-IDKUNDNR-TOM-X                        
158800                    '&IDKUNDNT>=' W-IDKUNDNR-TOM-X                        
158900                    '!IDDISTRF =' W-IDDISTR-FOM-X                         
159000                    '&IDDISTRT =' W-IDDISTR-TOM-X                         
159100                    '&IDKUNDNF>=' W-IDKUNDNR-FOM-X                        
159200                    '&IDKUNDNF<=' W-IDKUNDNR-TOM-X                        
159300                    '!IDDISTRF =' W-IDDISTR-FOM-X                         
159400                    '&IDDISTRT =' W-IDDISTR-TOM-X                         
159500                    '&IDKUNDNT>=' W-IDKUNDNR-FOM-X                        
159600                    '&IDKUNDNT<=' W-IDKUNDNR-TOM-X ')'                    
159700            DELIMITED BY SIZE INTO SSA2                                   
159800     MOVE '  GE'                  TO GODK-STATUSKODER                     
159900     CALL CBLTDLI USING GHU WDF2-PCB DLI-IO-WLLEVF11 SSA1 SSA2            
160000     MOVE WDF2-STATUS-CODE        TO STATUS-WS                            
160100     PERFORM IMS-STATUSKONTROLL                                           
160200     .                                                                    
160300     SKIP2                                                                
160400 IMS-GHU-DIR SECTION.                                                     
160500                                                                          
160600     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
160700          DELIMITED BY SIZE INTO SSA1                                     
160800     STRING 'WDF211  (WDF211KY =' W-WDF211KY-X ')'                        
160900          DELIMITED BY SIZE INTO SSA2                                     
161000     MOVE '  GE'                  TO GODK-STATUSKODER                     
161100     CALL CBLTDLI USING GHU WDF2-PCB DLI-IO-WLLEVF11 SSA1 SSA2            
161200     MOVE WDF2-STATUS-CODE        TO STATUS-WS                            
161300     PERFORM IMS-STATUSKONTROLL                                           
161400     .                                                                    
161500     SKIP3                                                                
161600 IMS-GNP-DIR SECTION.                                                     
161700                                                                          
161800     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
161900          DELIMITED BY SIZE INTO SSA1                                     
162000     STRING 'WDF211  (WDF211KY>=' W-WDF211KY-X ')'                        
162100          DELIMITED BY SIZE INTO SSA2                                     
162200     MOVE '  GE'                  TO GODK-STATUSKODER                     
162300     CALL CBLTDLI USING GNP WDF2-PCB DLI-IO-WLLEVF11 SSA1 SSA2            
162400     MOVE WDF2-STATUS-CODE        TO STATUS-WS                            
162500     PERFORM IMS-STATUSKONTROLL                                           
162600     .                                                                    
162700     SKIP3                                                                
162800 IMS-ISRT-DIR SECTION.                                                    
162900                                                                          
163000     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
163100          DELIMITED BY SIZE INTO SSA1                                     
163200     MOVE 'WDF211   ' TO SSA2                                             
163300     MOVE '  II'                 TO GODK-STATUSKODER                      
163400     CALL CBLTDLI USING ISRT WDF2-PCB DLI-IO-WLLEVF11 SSA1 SSA2           
163500     MOVE WDF2-STATUS-CODE       TO STATUS-WS                             
163600     PERFORM IMS-STATUSKONTROLL                                           
163700     .                                                                    
163800     SKIP3                                                                
163900 IMS-REPL-DIR SECTION.                                                    
164000                                                                          
164100     MOVE '  ' TO GODK-STATUSKODER                                        
164200     CALL CBLTDLI USING REPL WDF2-PCB DLI-IO-WLLEVF11                     
164300     MOVE WDF2-STATUS-CODE       TO STATUS-WS                             
164400     PERFORM IMS-STATUSKONTROLL                                           
164500     .                                                                    
164600     SKIP3                                                                
164700 IMS-DLET-DIR SECTION.                                                    
164800                                                                          
164900     MOVE '  ' TO GODK-STATUSKODER                                        
165000     CALL CBLTDLI USING DLET WDF2-PCB DLI-IO-WLLEVF11                     
165100     MOVE WDF2-STATUS-CODE       TO STATUS-WS                             
165200     PERFORM IMS-STATUSKONTROLL                                           
165300     .                                                                    
165400     EJECT                                                                
165500 IMS-GU-WDF118 SECTION.                                                   
165600                                                                          
165700     STRING 'WDF101  (IDLEVNR  =' W-WDF101KY-X ')'                        
165800          DELIMITED BY SIZE INTO SSA1                                     
165900     STRING 'WDF118  (WDF118KY =' W-WDF118KY-X ')'                        
166000          DELIMITED BY SIZE INTO SSA2                                     
166100     MOVE '  GE'                 TO GODK-STATUSKODER                      
166200     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WLLEVA18 SSA1 SSA2             
166300     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
166400     PERFORM IMS-STATUSKONTROLL                                           
166500     .                                                                    
166600     EJECT                                                                
166700 IMS-GU-GMTA-WDB201 SECTION.                                              
166800                                                                          
166900     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
167000          DELIMITED BY SIZE INTO SSA1                                     
167100     MOVE '  GE'             TO GODK-STATUSKODER                          
167200     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WLGMTA01 SSA1                  
167300     MOVE WDB2-STATUS-CODE   TO STATUS-WS                                 
167400     PERFORM IMS-STATUSKONTROLL                                           
167500     .                                                                    
167600     EJECT                                                                
167700 IMS-GU-WDB601 SECTION.                                                   
167800                                                                          
167900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
168000          DELIMITED BY SIZE INTO SSA1                                     
168100     MOVE '  GE'             TO GODK-STATUSKODER                          
168200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
168300     MOVE WDB6-STATUS-CODE   TO STATUS-WS                                 
168400     PERFORM IMS-STATUSKONTROLL                                           
168500     IF SEGMENT-SAKNAS                                                    
168600        MOVE SPACE TO DCS-KDDC                                            
168700     END-IF                                                               
168800     .                                                                    
168900     EJECT                                                                
169000 IMS-STATUSKONTROLL SECTION.                                              
169100                                                                          
169200     SET STATUS-IX TO 1                                                   
169300     SEARCH GODK-STATUS                                                   
169400       AT END                                                             
169500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
169600         DELIMITED BY SIZE INTO FELTEXT                                   
169700         CALL FELLOG                                                      
169800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
169900         CONTINUE                                                         
170000     END-SEARCH                                                           
170100     .                                                                    
