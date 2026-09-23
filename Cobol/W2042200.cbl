000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2042200.                                                
000400 AUTHOR.         INGER STENING.                                           
000500 DATE-WRITTEN.   12/10/29.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SUBMITT-PGM                                                      
001000*                                                                         
001100*        Detta pgm beställer en lista. Vissa urval kan anges.             
001200*                                                                         
001300*        Variablerna som skall ut kan väljas.                             
001400*        Listning i form av Excel                                         
001500*        som attchment till Outlook-mail kan väljas.                      
001600*        List-pgmet startas via sop med ovanstående som parametrar        
001700*        (Listans värden hämtas sen från lagerband, extrakt från          
001800*         orderingång och servicegrad, samt diverse DLI-call)             
001900*                                                                         
002000*        OBS VID FÖRÄNDRING AV BILDENS FUNKTION, V.G. UPPDATERA           
002100*        HJÄLP-BILDEN PÅ 0553  TRANS,W2T422                               
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W2T422                                              
002500*        MID:         W2I42201                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W2O42201                                            
002900*                                                                         
003000*    ÄNDRING:                                                             
003100*        Mars-2003: (C.E.) Vissa val-variabler tagits bort och            
003200*        andra har tillkommit.                                            
003300*        Beställning av släplista flyttas till annan bild (2323 ?)        
003400*                                                                         
003410*        2016-04-28  eTracker 10248818 add possiblity to have             
003420*                    Planner number on screen 2322 and 2422.              
003500*                                                                         
003600*   ÄNDRINGAR:                                                            
003700*                                                                         
003800                                                                          
003900     SKIP3                                                                
004000 ENVIRONMENT DIVISION.                                                    
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(08)   VALUE 'W2042200'.            
004700                                                                          
004800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005000                                                                          
005100 77  JA                          PIC X       VALUE 'J'.                   
005110 77  YES                         PIC X       VALUE 'Y'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300                                                                          
005400 77  IX                          PIC S9(9)  VALUE +0    COMP SYNC.        
005500                                                                          
005600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005800     88  INDATA-OK                           VALUE 'J'.                   
005900     88  INDATA-FEL                          VALUE 'N'.                   
006000                                                                          
006100 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006200     88  ALLT-OK                             VALUE 'J'.                   
006300                                                                          
006400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006500     88  EGEN-MID                            VALUE '2422'.                
006600     88  GODK-MID                            VALUE '2421' '2422'          
006700                                                   '2423' '2424'          
006800                                                   '2425' '2426'          
006900                                                   '2427' '2428'          
007000                                                   '2429'.                
007100     EJECT                                                                
007200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007300 01  GENERELLA-SUBPROGRAM.                                                
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007700     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
007800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008100*01 -COPY WMEDAREA                                                        
008200     SKIP3                                                                
008300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008400     SKIP3                                                                
008500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT (USERBASEN)                  
008600*01 -COPY WMSGINIT                                                        
008700     EJECT                                                                
008800 01  NYCKLAR-TILL-DLI.                                                    
008900   03    W-KDARBTYP-X.                                                    
009000     05    W-KDARBTYP             PIC X(8)    VALUE SPACE.                
009100                                                                          
009200   03    W-IDPERSON-X.                                                    
009300     05    W-IDPERSON             PIC S9(3)   COMP-3 VALUE +0.            
009400                                                                          
009500   03    W-IDDC-X.                                                        
009600     05    W-IDDC                 PIC X(2)    VALUE SPACE.                
009700                                                                          
009800 01  MESSAGE-CODES.                                                       
009900     03  ERR-CORR-HIGH-LIT-FLDS  PIC X(3)    VALUE '001'.                 
010000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010200     03  ERR-DATA-NOT-NUMERIC    PIC X(3)    VALUE '020'.                 
010300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010400     03  ERR-START-GREATER       PIC X(3)    VALUE '240'.                 
010500     03  ERR-CONFLICT-CHOICE     PIC X(3)    VALUE '287'.                 
010600     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
010700     03  ERR-WRONG-INTERVAL      PIC X(3)    VALUE '738'.                 
010800     03  ERR-DC-MISSING          PIC X(3)    VALUE '026'.                 
010900     SKIP2                                                                
011000 01  FILLER.                                                              
011100     03  MAILSEND.                                                        
011200         05  FILLER              PIC X(13)   VALUE                        
011310                                 'MAIL ORDERED '.                         
011400 01  FILLER.                                                              
011500     03  LISTUPPSTART.                                                    
011600         05  FILLER              PIC X(30)   VALUE                        
011700                                 'LISTA UPPSTARTAD, PRINTER: '.           
011800         05  TEXT-PRINTER        PIC X(25)   VALUE SPACE.                 
011900     EJECT                                                                
012000 01  PROG-TO-PROG-SW.                                                     
012100*    03  -COPY WMSGSOP                                                    
012200     EJECT                                                                
012300 01  WS-PARAMETRAR.                                                       
012400     03  WS-IDUSER.                                                       
012500         05  IDUSER              PIC X(8) VALUE SPACES.                   
012600     03  WS-URVAL1.                                                       
012700         05  IDDC                PIC X(02) VALUE SPACE.                   
012800         05  IDBERED-FOM         PIC 9(03) VALUE ZERO.                    
012900         05  IDBERED-TOM         PIC 9(03) VALUE ZERO.                    
013000         05  IDBERED-GRP OCCURS 5 TIMES.                                  
013100             07  IDBERED         PIC 9(03) VALUE ZERO.                    
013210         05  FL-IDBERED          PIC X(01) VALUE SPACES.                  
013220         05  FL-IDLEVNR-SHIP     PIC X(01) VALUE SPACES.                  
013300         05  IDLEVNR-GRP OCCURS 5 TIMES.                                  
013400             07  IDLEVNR         PIC X(05) VALUE SPACES.                  
013500         05  KDERS-FOM           PIC 9(02) VALUE 99.                      
013600         05  KDERS-TOM           PIC 9(02) VALUE 99.                      
013700         05  KDERS-GRP    OCCURS 5 TIMES.                                 
013800             07  KDERS           PIC 9(02) VALUE 99.                      
013900         05  FLERSDAT-VIPS       PIC X(01) VALUE SPACE.                   
014000         05  FILLER              PIC X(15) VALUE SPACE.                   
014100                                                                          
014200     03  WS-URVAL2.                                                       
014300         05  IDFKNGRP-FOM        PIC 9(04) VALUE ZERO.                    
014400         05  IDFKNGRP-TOM        PIC 9(04) VALUE ZERO.                    
014500         05  IDFKNGRP-GRP OCCURS 5 TIMES.                                 
014600             07  IDFKNGRP        PIC 9(04) VALUE ZERO.                    
014700         05  BEFT-GRP     OCCURS 4 TIMES.                                 
014800             07  BEFT            PIC 9(02) VALUE ZERO.                    
014900         05  BEART-SOEK          PIC X(25) VALUE SPACES.                  
015000         05  KDPRODSL-FOM        PIC 9(02) VALUE ZERO.                    
015100         05  KDPRODSL-TOM        PIC 9(02) VALUE ZERO.                    
015200         05  IDPROJ-URV          PIC X(04) VALUE SPACES.                  
015300         05  ADLAGOMR            PIC 9(02) VALUE ZERO.                    
015400         05  ADGANG-FOM          PIC 9(02) VALUE ZERO.                    
015500         05  ADGANG-TOM          PIC 9(02) VALUE ZERO.                    
015600         05  KVVECKOR-AVROP      PIC 9(02) VALUE ZERO.                    
015700         05  KVVECKOR-KVPB       PIC 9(02) VALUE ZERO.                    
015800         05  FILLER              PIC X(01) VALUE SPACE.                   
015900                                                                          
016000*    här börjar den nya layouten                                          
016100     03  WS-LISTA.                                                        
016200         05 VAL-IDLEVNR          PIC X(1) VALUE SPACE.                    
016300         05 VAL-BELEV            PIC X(1) VALUE SPACE.                    
016400         05 VAL-BEART-GB         PIC X(1) VALUE SPACE.                    
016500         05 VAL-KVLS             PIC X(1) VALUE SPACE.                    
016600         05 VAL-KVAKS            PIC X(1) VALUE SPACE.                    
016700         05 VAL-VOR-ROS          PIC X(1) VALUE SPACE.                    
016800         05 VAL-AVROP            PIC X(1) VALUE SPACE.                    
016900         05 VAL-LEVBESK          PIC X(1) VALUE SPACE.                    
017000         05 VAL-KVAVIS           PIC X(1) VALUE SPACE.                    
017100         05 VAL-KVVECKOR-LT      PIC X(1) VALUE SPACE.                    
017200         05 VAL-IDBERED          PIC X(1) VALUE SPACE.                    
017300         05 VAL-TILEVDAG         PIC X(1) VALUE SPACE.                    
017400         05 VAL-INLEV            PIC X(1) VALUE SPACE.                    
017500         05 VAL-IDINK            PIC X(1) VALUE SPACE.                    
017600         05 VAL-KDAVT            PIC X(1) VALUE SPACE.                    
017700         05 VAL-KVPB             PIC X(1) VALUE SPACE.                    
017800         05 VAL-TREND            PIC X(1) VALUE SPACE.                    
017900         05 VAL-SEASON           PIC X(1) VALUE SPACE.                    
018000         05 VAL-DEMHIST          PIC X(1) VALUE SPACE.                    
018100         05 VAL-DEMHIST-YEAR     PIC X(1) VALUE SPACE.                    
018200         05 VAL-KDERS            PIC X(1) VALUE SPACE.                    
018300         05 VAL-TIERSDAT-VIPS    PIC X(1) VALUE SPACE.                    
018400         05 VAL-PRMATRL-PRAVCOST PIC X(1) VALUE SPACE.                    
018500         05 VAL-KDFPKPRI         PIC X(1) VALUE SPACE.                    
018600         05 VAL-KDSORT           PIC X(1) VALUE SPACE.                    
018700         05 VAL-TIREFSTO-LOC     PIC X(1) VALUE SPACE.                    
018800         05 VAL-VKART-VLARTNTO   PIC X(1) VALUE SPACE.                    
018900         05 VAL-TIFINLV          PIC X(1) VALUE SPACE.                    
019000         05 VAL-DAPUBL           PIC X(1) VALUE SPACE.                    
019100         05 VAL-TIURPROD         PIC X(1) VALUE SPACE.                    
019200         05 VAL-KDPRODSL         PIC X(1) VALUE SPACE.                    
019300         05 VAL-IDFKNGRP         PIC X(1) VALUE SPACE.                    
019600         05 VAL-SERVICE          PIC X(1) VALUE SPACE.                    
019700         05 VAL-STYRPARAM        PIC X(1) VALUE SPACE.                    
019800         05 VAL-QUANT            PIC X(1) VALUE SPACE.                    
019900         05 VAL-IDARTB-EMB       PIC X(1) VALUE SPACE.                    
020000         05 VAL-KVREFOVL         PIC X(1) VALUE SPACE.                    
020100         05 VAL-KVSPANT          PIC X(1) VALUE SPACE.                    
020200         05 VAL-KVSLAGER         PIC X(1) VALUE SPACE.                    
020300         05 VAL-KDARTUTS         PIC X(1) VALUE SPACE.                    
020400         05 VAL-ADLAGOMR         PIC X(1) VALUE SPACE.                    
020500         05 VAL-FLJIT            PIC X(1) VALUE SPACE.                    
020600         05 VAL-IDPROJ           PIC X(1) VALUE SPACE.                    
020700         05 VAL-FLLSRDEL         PIC X(1) VALUE SPACE.                    
020800         05 VAL-IDKR             PIC X(1) VALUE SPACE.                    
020900                                                                          
021000     03  WS-FLAGGA  REDEFINES WS-LISTA.                                   
021100         05 VAL-FLAGGA OCCURS 45 PIC X(1).                                
021200                                                                          
021300     03  WS-IDMAIL.                                                       
021400         05  IDMAIL              PIC X(58) VALUE SPACE.                   
021500     EJECT                                                                
021600*01  -COPY W006PRT                                                        
021700     EJECT                                                                
021800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
021900*                                                                         
022000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
022100     SKIP3                                                                
022200*01  MID -COPY W2I42201                                                   
022300     EJECT                                                                
022400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
022500     SKIP3                                                                
022600*01  -COPY WMSGAREA                                                       
022700     EJECT                                                                
022800     03  MOD REDEFINES MSG-AREA.                                          
022900*      05  -COPY W2O42201                                                 
023000     EJECT                                                                
023100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
023200     SKIP3                                                                
023300*01  -COPY WMFSAREA                                                       
023400     EJECT                                                                
023500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023600     SKIP3                                                                
023700*    --- STATUS-KOD FRÅN IMS                                              
023800 01  STATUS-WS                   PIC XX.                                  
023900     88  SEGMENT-FINNS                       VALUE '  '.                  
024000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024200     SKIP2                                                                
024300 01  GODK-STATUSKODER.                                                    
024400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024500     EJECT                                                                
024600*- - - - - - - - - - - - - - -DLI INPUT-OUTPUT AREA                       
024700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-P3'.           
024800     SKIP3                                                                
024900 01  DLI-IO-P3.                                                           
025000*    03  -COPY WDP311                                                     
025100     EJECT                                                                
025200 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDB601'.        
025300     SKIP3                                                                
025400 01  DLI-IO-WDB601.                                                       
025500*    03  -COPY WDB601                                                     
025600     EJECT                                                                
025700 01  SSA1                        PIC X(64).                               
025800 01  SSA2                        PIC X(64).                               
025900*    --- IMS FUNKTIONSKODER                                               
026000*01  -COPY W0003                                                          
026100     EJECT                                                                
026200 LINKAGE SECTION.                                                         
026300*01  -COPY W0009   -PRE MSG-                                              
026400                                                                          
026500*01  -COPY W0009   -PRE ALT-                                              
026600                                                                          
026700*01  -COPY W0008   -PRE WDP3-                                             
026800     05  FILLER                  PIC X.                                   
026900                                                                          
027000*01  -COPY W0008   -PRE WDB6-                                             
027100     05  FILLER                  PIC X.                                   
027200                                                                          
027300*01  -COPY W0008   -PRE USEA-                                             
027400     05  FILLER                  PIC X.                                   
027500     EJECT                                                                
027600 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP3-PCB WDB6-PCB              
027700                           USEA-PCB.                                      
027800 MAIN SECTION.                                                            
027900     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP3-PCB WDB6-PCB              
028000                           USEA-PCB.                                      
028100                                                                          
028200     PERFORM IMS-GET-MSG                                                  
028300     IF SEGMENT-FINNS                                                     
028400       PERFORM A-INIT                                                     
028500       IF MFS-UPDATE                                                      
028600          PERFORM B-KOLLA-INPUT                                           
028700          IF INDATA-OK                                                    
028800             PERFORM C-UPPDATERA                                          
028900             PERFORM MFS-RENSA-FAELT-IN                                   
029000             MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                         
029100          END-IF                                                          
029200       ELSE                                                               
029300          IF NOT EGEN-MID                                                 
029500             PERFORM MFS-RENSA-FAELT-IN                                   
029600          ELSE                                                            
029610             IF MID-URVAL = ALL '+' AND                                   
029620                MID-LISTA = ALL '+'                                       
029621                PERFORM MFS-RENSA-FAELT-IN                                
029630             ELSE                                                         
029700                PERFORM D-SAMMA-SIDA                                      
029710             END-IF                                                       
029800          END-IF                                                          
029900       END-IF                                                             
030000                                                                          
030100       ADD LENGTH OF MOD-W2O42201 +4 GIVING MSG-KVLL                      
030200       PERFORM IMS-INSERT-MSG                                             
030300     END-IF                                                               
030400                                                                          
030500     MOVE ZERO TO RETURN-CODE                                             
030600     GOBACK                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 A-INIT SECTION.                                                          
031000                                                                          
031100     IF MSG-DUBBLA-TRANSKODER                                             
031200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I42201                 
031300       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
031400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
031500     ELSE                                                                 
031600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I42201                  
031700       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
031800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
031900     END-IF                                                               
032000                                                                          
032100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
032200     MOVE MSG-IDPFK TO MFS-IDPFK                                          
032300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
032400                                                                          
032500*    --- Läs userbasen                                                    
032600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
032700     MOVE '001'             TO MSGI-KDCALL                                
032800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
032900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
033000     MOVE '2422'            TO MSGI-IDTRANS                               
033100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033200                                                                          
033300     MOVE LOW-VALUE TO MSG-AREA                                           
033400     MOVE 'W2O422N1' TO MFS-IDMOD                                         
033500     MOVE '2422' TO MOD-IDTRANS                                           
033600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
033700                                                                          
033800     IF NOT EGEN-MID                                                      
033900       MOVE SPACE TO MFS-KDTRTYP                                          
034000       MOVE '7' TO MFS-IDPFK                                              
034100     END-IF                                                               
034200     .                                                                    
034300     EJECT                                                                
034400 B-KOLLA-INPUT   SECTION.                                                 
034500                                                                          
034600     INITIALIZE WS-URVAL1 WS-URVAL2                                       
034700     MOVE JA TO INDATA-SW                                                 
034800                                                                          
034900     IF MID-URVAL = ALL '+' OR MID-LISTA = ALL '+'                        
035000        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
035100        CALL WMEDKONV USING MED-WMEDAREA                                  
035200        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
035300        IF MID-URVAL = ALL '+' AND MID-LISTA = ALL '+'                    
035400           PERFORM MFS-RENSA-FAELT-IN                                     
035500        ELSE                                                              
035600           PERFORM MFS-LAES-IN-IGEN                                       
035700           PERFORM MFS-ROER-EJ-FAELT-IN                                   
035800        END-IF                                                            
035900        MOVE NEJ TO INDATA-SW                                             
036000        MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                       
036100     ELSE                                                                 
036200        PERFORM BA-KONTROLL-URVAL                                         
036300        PERFORM BB-FLYTTA-FLAGGA-VAL-TILL-MOD                             
036400        PERFORM BC-KONTROLL-OVR                                           
036500                                                                          
036600        IF INDATA-OK                                                      
036700           PERFORM BD-KONTROLL-IDDC                                       
036800        END-IF                                                            
036900        IF INDATA-OK                                                      
037000           PERFORM BE-KONTROLL-FOM-TOM                                    
037100        END-IF                                                            
037200        IF INDATA-OK                                                      
037300           PERFORM BF-KONTROLL-KOMBINATION                                
037400        END-IF                                                            
037500                                                                          
037600        IF INDATA-FEL                                                     
037700          IF MOD-TEMFSFEL NOT > SPACE                                     
037800             CALL WMEDKONV USING MED-WMEDAREA                             
037900             MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                            
038000          END-IF                                                          
038100          PERFORM MFS-ROER-EJ-FAELT-IN                                    
038200        END-IF                                                            
038300     END-IF                                                               
038400     .                                                                    
038500     EJECT                                                                
038600 BA-KONTROLL-URVAL SECTION.                                               
038700                                                                          
038800     IF MID-IDDC NOT = ALL '+'                                            
038900        IF MID-IDDC NOT NUMERIC                                           
039000           MOVE MFS-NUM-FAELT-FEL        TO MOD-IDDC-ATTR                 
039100           MOVE NEJ TO INDATA-SW                                          
039200           MOVE ERR-DATA-NOT-NUMERIC     TO MED-IDMFSFEL                  
039300        ELSE                                                              
039400           MOVE MFS-NUM-FAELT-RAETT      TO MOD-IDDC-ATTR                 
039500           MOVE MID-IDDC                 TO IDDC                          
039600        END-IF                                                            
039700     END-IF                                                               
039800                                                                          
039900     IF MID-IDPERSON-FOM NOT = ALL '+'                                    
040000        IF MID-IDPERSON-FOM NOT NUMERIC                                   
040100           MOVE MFS-NUM-FAELT-FEL        TO MOD-IDPERSON-FOM-ATTR         
040200           MOVE NEJ TO INDATA-SW                                          
040300           MOVE ERR-DATA-NOT-NUMERIC     TO MED-IDMFSFEL                  
040400        ELSE                                                              
040500           MOVE MFS-NUM-FAELT-RAETT      TO MOD-IDPERSON-FOM-ATTR         
040600           MOVE MID-IDPERSON-FOM         TO IDBERED-FOM                   
040700        END-IF                                                            
040800     END-IF                                                               
040900                                                                          
041000     IF MID-IDPERSON-TOM NOT = ALL '+'                                    
041100        IF MID-IDPERSON-TOM NOT NUMERIC                                   
041200           MOVE MFS-NUM-FAELT-FEL        TO MOD-IDPERSON-TOM-ATTR         
041300           MOVE NEJ TO INDATA-SW                                          
041400           MOVE ERR-DATA-NOT-NUMERIC     TO MED-IDMFSFEL                  
041500        ELSE                                                              
041600           MOVE MFS-NUM-FAELT-RAETT      TO MOD-IDPERSON-TOM-ATTR         
041700           MOVE MID-IDPERSON-TOM         TO IDBERED-TOM                   
041800        END-IF                                                            
041900     END-IF                                                               
042000                                                                          
042100     MOVE +1 TO IX                                                        
042200     PERFORM UNTIL IX > 5                                                 
042300       IF MID-IDPERSON(IX) NOT = ALL '+'                                  
042400          IF MID-IDPERSON(IX) NOT NUMERIC                                 
042500             MOVE MFS-NUM-FAELT-FEL      TO MOD-IDPERSON-ATTR (IX)        
042600             MOVE NEJ TO INDATA-SW                                        
042700             MOVE ERR-DATA-NOT-NUMERIC   TO MED-IDMFSFEL                  
042800          ELSE                                                            
042900             IF (MID-IDPERSON-FOM NOT = ALL '+' OR                        
043000                 MID-IDPERSON-TOM NOT = ALL '+')                          
043100                MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPERSON-FOM-ATTR         
043200                MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPERSON-TOM-ATTR         
043300                MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPERSON-ATTR(IX)         
043400                MOVE NEJ TO INDATA-SW                                     
043500                MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL               
043600             ELSE                                                         
043700                MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPERSON-ATTR(IX)         
043800                MOVE MID-IDPERSON (IX)   TO IDBERED (IX)                  
043900             END-IF                                                       
044000          END-IF                                                          
044100       ELSE                                                               
044200          MOVE ZERO                      TO IDBERED (IX)                  
044300       END-IF                                                             
044400       ADD +1 TO IX                                                       
044500     END-PERFORM                                                          
044600                                                                          
044700*    --- Här kollas mot SEC-IDLEV på WDP7 om restriktioner finns          
044800     If MSGI-KDARBTYP-SEC-IDLEV = SPACE Or LOW-VALUE                      
044900       MOVE +1 TO IX                                                      
045000       PERFORM UNTIL IX > 5                                               
045100         IF MID-IDLEVNR(IX) NOT = ALL '+'                                 
045200            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-ATTR(IX)             
045300            MOVE MID-IDLEVNR (IX)        TO IDLEVNR (IX)                  
045400         ELSE                                                             
045500            MOVE SPACES                  TO IDLEVNR (IX)                  
045600         END-IF                                                           
045700         ADD +1 TO IX                                                     
045800       END-PERFORM                                                        
045900     Else                                                                 
046000*      --- Endast det leverantör-ID som tillhör USER får listas,          
046100*      --- och SKALL listas, även om inget är inmatat.                    
046200       Move +1 To IX                                                      
046300       Perform Until IX > 5                                               
046400         If IX = +1                                                       
046500           Move MSGI-KDARBTYP-SEC-IDLEV  To IDLEVNR (IX)                  
046600         Else                                                             
046700           Move SPACES To IDLEVNR (IX)                                    
046800         End-If                                                           
046900         Add +1 TO IX                                                     
047000       End-Perform                                                        
047100     End-If                                                               
047300     IF MID-FLAGGA-IDBERED NOT = '+'                                      
047400       IF MID-FLAGGA-IDBERED = 'X' OR 'J' OR 'Y'                          
047500         MOVE MFS-ALFA-FAELT-RAETT To                                     
047600                                      MOD-FLAGGA-IDBERED-ATTR             
047700         MOVE 'X'                        TO FL-IDBERED                    
047800       END-IF                                                             
047900     ELSE                                                                 
048000       MOVE SPACES                       TO FL-IDBERED                    
048100     END-IF                                                               
048200                                                                          
048210     IF MID-FLAGGA-IDLEVNR-SHIP NOT = '+'                                 
048220       IF MID-FLAGGA-IDLEVNR-SHIP = 'X' OR 'J' OR 'Y'                     
048230         MOVE MFS-ALFA-FAELT-RAETT To                                     
048240                                      MOD-FLAGGA-IDLEVNR-SHIP-ATTR        
048250         MOVE 'X'                        TO FL-IDLEVNR-SHIP               
048260       END-IF                                                             
048270     ELSE                                                                 
048280       MOVE SPACES                       TO FL-IDLEVNR-SHIP               
048290     END-IF                                                               
048291                                                                          
048300     IF MID-KDERS-FOM NOT = ALL '+'                                       
048400        IF MID-KDERS-FOM NOT NUMERIC                                      
048500           MOVE MFS-NUM-FAELT-FEL        TO MOD-KDERS-FOM-ATTR            
048600           MOVE NEJ TO INDATA-SW                                          
048700           MOVE ERR-DATA-NOT-NUMERIC     TO MED-IDMFSFEL                  
048800        ELSE                                                              
048900           IF MID-KDERS-FOM > 52                                          
049000             MOVE MFS-NUM-FAELT-FEL TO MOD-KDERS-FOM-ATTR                 
049100             MOVE NEJ TO INDATA-SW                                        
049200             MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                  
049300           ELSE                                                           
049400             MOVE MFS-NUM-FAELT-RAETT    TO MOD-KDERS-FOM-ATTR            
049500             MOVE MID-KDERS-FOM          TO KDERS-FOM                     
049600           END-IF                                                         
049700        END-IF                                                            
049800     ELSE                                                                 
049900*       -- VÄRDET 99 STÅR FÖR OIFYLLT VÄRDE                               
050000        MOVE 99 TO KDERS-FOM                                              
050100     END-IF                                                               
050200                                                                          
050300     IF MID-KDERS-TOM NOT = ALL '+'                                       
050400        IF MID-KDERS-TOM NOT NUMERIC                                      
050500           MOVE MFS-NUM-FAELT-FEL        TO MOD-KDERS-TOM-ATTR            
050600           MOVE NEJ TO INDATA-SW                                          
050700           MOVE ERR-DATA-NOT-NUMERIC     TO MED-IDMFSFEL                  
050800        ELSE                                                              
050900           IF MID-KDERS-TOM > 52                                          
051000             MOVE MFS-NUM-FAELT-FEL TO MOD-KDERS-TOM-ATTR                 
051100             MOVE NEJ TO INDATA-SW                                        
051200             MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                  
051300           ELSE                                                           
051400             MOVE MFS-NUM-FAELT-RAETT    TO MOD-KDERS-TOM-ATTR            
051500             MOVE MID-KDERS-TOM          TO KDERS-TOM                     
051600           END-IF                                                         
051700        END-IF                                                            
051800     ELSE                                                                 
051900*       -- VÄRDET 99 STÅR FÖR OIFYLLT VÄRDE                               
052000        MOVE 99 TO KDERS-TOM                                              
052100     END-IF                                                               
052200                                                                          
052300     MOVE +1 TO IX                                                        
052400     PERFORM UNTIL IX > 5                                                 
052500       IF MID-KDERS(IX) NOT = ALL '+'                                     
052600          IF MID-KDERS(IX) NOT NUMERIC                                    
052700             MOVE MFS-NUM-FAELT-FEL      TO MOD-KDERS-ATTR (IX)           
052800             MOVE NEJ TO INDATA-SW                                        
052900             MOVE ERR-DATA-NOT-NUMERIC   TO MED-IDMFSFEL                  
053000          ELSE                                                            
053100             IF (MID-KDERS-FOM NOT = ALL '+' OR                           
053200                 MID-KDERS-TOM NOT = ALL '+')                             
053300                MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-FOM-ATTR            
053400                MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-TOM-ATTR            
053500                MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-ATTR(IX)            
053600                MOVE NEJ TO INDATA-SW                                     
053700                MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL               
053800             ELSE                                                         
053900                IF MID-KDERS (IX) > 52                                    
054000                  MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-ATTR(IX)          
054100                  MOVE NEJ TO INDATA-SW                                   
054200                  MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL             
054300                ELSE                                                      
054400                  MOVE MFS-NUM-FAELT-RAETT TO MOD-KDERS-ATTR(IX)          
054500                  MOVE MID-KDERS (IX)      TO KDERS (IX)                  
054600                END-IF                                                    
054700             END-IF                                                       
054800          END-IF                                                          
054900       ELSE                                                               
055000*         -- VÄRDET 99 STÅR FÖR OIFYLLT VÄRDE                             
055100          MOVE 99                          TO KDERS (IX)                  
055200       END-IF                                                             
055300       ADD +1 TO IX                                                       
055400     END-PERFORM                                                          
055500                                                                          
055600*    --- FLERSDAT-VIPS                                                    
055700     IF MID-FLERSDAT-VIPS not = all '+'                                   
055710        IF MID-FLERSDAT-VIPS = YES OR JA OR NEJ                           
055800           MOVE MFS-NUM-FAELT-RAETT TO MOD-FLERSDAT-VIPS-ATTR             
055900           MOVE MID-FLERSDAT-VIPS   TO FLERSDAT-VIPS                      
056000        ELSE                                                              
056100           MOVE MFS-NUM-FAELT-FEL   TO MOD-FLERSDAT-VIPS-ATTR             
056200           MOVE NEJ TO INDATA-SW                                          
056300           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
056400        END-IF                                                            
056410     END-IF                                                               
056500                                                                          
056600*    --- FUNKTIONSGRUPP                                                   
056700*    --- INTERVALL FRÅN OCH MED                                           
056800     IF MID-IDFKNGRP-FOM NOT = ALL '+'                                    
056900        IF MID-IDFKNGRP-FOM NOT NUMERIC                                   
057000           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-FOM-ATTR              
057100           MOVE NEJ TO INDATA-SW                                          
057200           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
057300        ELSE                                                              
057400           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-FOM-ATTR              
057500           MOVE MID-IDFKNGRP-FOM    TO IDFKNGRP-FOM                       
057600        END-IF                                                            
057700     END-IF                                                               
057800                                                                          
057900*    --- INTERVALL TILL OCH MED                                           
058000     IF MID-IDFKNGRP-TOM NOT = ALL '+'                                    
058100        IF MID-IDFKNGRP-TOM NOT NUMERIC                                   
058200           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-TOM-ATTR              
058300           MOVE NEJ TO INDATA-SW                                          
058400           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
058500        ELSE                                                              
058600           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-TOM-ATTR              
058700           MOVE MID-IDFKNGRP-TOM    TO IDFKNGRP-TOM                       
058800        END-IF                                                            
058900     END-IF                                                               
059000                                                                          
059100*    --- FUNKTIONSGRUPP 5 EXPLICITA VAL                                   
059200     MOVE +1 TO IX                                                        
059300     PERFORM UNTIL IX > +5                                                
059400       IF MID-IDFKNGRP(IX) NOT = ALL '+'                                  
059500          IF MID-IDFKNGRP(IX) NOT NUMERIC                                 
059600             MOVE MFS-NUM-FAELT-FEL      TO MOD-IDFKNGRP-ATTR(IX)         
059700             MOVE NEJ TO INDATA-SW                                        
059800             MOVE ERR-DATA-NOT-NUMERIC   TO MED-IDMFSFEL                  
059900          ELSE                                                            
060000             IF (MID-IDFKNGRP-FOM NOT = ALL '+'                           
060100             OR  MID-IDFKNGRP-TOM NOT = ALL '+')                          
060200                MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-FOM-ATTR         
060300                MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-TOM-ATTR         
060400                MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-ATTR(IX)         
060500                MOVE NEJ TO INDATA-SW                                     
060600                MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL               
060700             ELSE                                                         
060800                MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-ATTR(IX)         
060900                MOVE MID-IDFKNGRP(IX)    TO IDFKNGRP(IX)                  
061000             END-IF                                                       
061100          END-IF                                                          
061200       ELSE                                                               
061300          MOVE ZERO                      TO IDFKNGRP(IX)                  
061400       END-IF                                                             
061500       ADD +1 TO IX                                                       
061600     END-PERFORM                                                          
061700*                                                                         
061800*    --- FÖRPACKNINGSTYPER                                                
061900     MOVE +1 TO IX                                                        
062000     PERFORM UNTIL IX > +4                                                
062100       IF MID-BEFT(IX) NOT = ALL '+'                                      
062200                                                                          
062300          IF MID-BEFT(IX) NOT NUMERIC                                     
062400             MOVE MFS-NUM-FAELT-FEL   TO MOD-BEFT-ATTR (IX)               
062500             MOVE NEJ TO INDATA-SW                                        
062600             MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                    
062700          ELSE                                                            
062800             MOVE MFS-NUM-FAELT-RAETT TO MOD-BEFT-ATTR(IX)                
062900             MOVE MID-BEFT (IX)       TO BEFT (IX)                        
063000          END-IF                                                          
063100       ELSE                                                               
063200             MOVE ZERO                TO BEFT (IX)                        
063300       END-IF                                                             
063400       ADD +1 TO IX                                                       
063500     END-PERFORM                                                          
063600*                                                                         
063700*    ---   BENÄMNINGS-SÖKNING                                             
063800     IF MID-BEART-SOEK NOT = ALL '+'                                      
063900        IF MID-BEART-SOEK = SPACE                                         
064000           MOVE ALL '+'          TO MID-BEART-SOEK                        
064100           MOVE MFS-RENSA-FAELT  TO MOD-BEART-SOEK                        
064200        ELSE                                                              
064300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-SOEK-ATTR               
064400           MOVE MID-BEART-SOEK       TO BEART-SOEK                        
064500        END-IF                                                            
064600     END-IF                                                               
064700*                                                                         
064800*    --- PRODSL                                                           
064900     IF MID-KDPRODSL-FOM NOT = ALL '+'                                    
065000        IF MID-KDPRODSL-FOM NUMERIC                                       
065100           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-FOM-ATTR              
065200           MOVE MID-KDPRODSL-FOM    TO KDPRODSL-FOM                       
065300        ELSE                                                              
065400           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPRODSL-FOM-ATTR              
065500           MOVE NEJ TO INDATA-SW                                          
065600           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
065700        END-IF                                                            
065800     END-IF                                                               
065900*     nytt vid 06:6                                                       
066000     IF MID-KDPRODSL-TOM NOT = ALL '+'                                    
066100        IF MID-KDPRODSL-TOM NUMERIC                                       
066200           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-TOM-ATTR              
066300           MOVE MID-KDPRODSL-TOM    TO KDPRODSL-TOM                       
066400        ELSE                                                              
066500           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPRODSL-TOM-ATTR              
066600           MOVE NEJ TO INDATA-SW                                          
066700           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
066800        END-IF                                                            
066900     END-IF                                                               
067000*                                                                         
067100     IF MID-IDPROJ-URV NOT = ALL '+' AND SPACE                            
067200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-URV-ATTR                 
067300         MOVE MID-IDPROJ-URV       TO IDPROJ-URV                          
067400     END-IF                                                               
067500*                                                                         
067600*    ---   LAG.OMR                                                        
067700     IF MID-ADLAGOMR NOT = ALL '+'                                        
067800        IF MID-ADLAGOMR NOT NUMERIC                                       
067900           MOVE MFS-NUM-FAELT-FEL    TO MOD-ADLAGOMR-ATTR                 
068000           MOVE NEJ TO INDATA-SW                                          
068100           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
068200        ELSE                                                              
068300           MOVE MFS-NUM-FAELT-RAETT  TO MOD-ADLAGOMR-ATTR                 
068400           MOVE MID-ADLAGOMR         TO ADLAGOMR                          
068500        END-IF                                                            
068600     END-IF                                                               
068700*                                                                         
068800*    ---   GÅNG-FOM                                                       
068900     IF MID-ADGANG-FOM NOT = ALL '+'                                      
069000        IF MID-ADGANG-FOM NOT NUMERIC                                     
069100           MOVE MFS-NUM-FAELT-FEL    TO MOD-ADGANG-FOM-ATTR               
069200           MOVE NEJ TO INDATA-SW                                          
069300           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
069400        ELSE                                                              
069500           MOVE MFS-NUM-FAELT-RAETT  TO MOD-ADGANG-FOM-ATTR               
069600           MOVE MID-ADGANG-FOM       TO ADGANG-FOM                        
069700        END-IF                                                            
069800     END-IF                                                               
069900                                                                          
070000*    ---   GÅNG-TOM                                                       
070100     IF MID-ADGANG-TOM NOT = ALL '+'                                      
070200        IF MID-ADGANG-TOM NOT NUMERIC                                     
070300           MOVE MFS-NUM-FAELT-FEL    TO MOD-ADGANG-TOM-ATTR               
070400           MOVE NEJ TO INDATA-SW                                          
070500           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
070600        ELSE                                                              
070700           MOVE MFS-NUM-FAELT-RAETT  TO MOD-ADGANG-TOM-ATTR               
070800           MOVE MID-ADGANG-TOM       TO ADGANG-TOM                        
070900        END-IF                                                            
071000     END-IF                                                               
071100*                                                                         
071200*    --- ANTAL VECKOR TOT.BEHOV                                           
071300*    --- Detta urvalsfält finns bland List-dataval-fälten                 
071400     IF MID-KVVECKOR-KVPB NOT = ALL '+'                                   
071500        IF MID-KVVECKOR-KVPB NOT NUMERIC                                  
071600           MOVE MFS-NUM-FAELT-FEL    TO MOD-KVVECKOR-KVPB-ATTR            
071700           MOVE NEJ TO INDATA-SW                                          
071800           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
071900        ELSE                                                              
072000           MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVVECKOR-KVPB-ATTR            
072100           MOVE MID-KVVECKOR-KVPB    TO KVVECKOR-KVPB                     
072200        END-IF                                                            
072300     END-IF                                                               
072400*                                                                         
072500*    --- ANTAL VECKOR KVAVROP                                             
072600*    --- Detta urvalsfält finns bland List-dataval-fälten                 
072700     IF MID-KVVECKOR-AVROP NOT = ALL '+'                                  
072800        IF MID-KVVECKOR-AVROP NOT NUMERIC                                 
072900           MOVE MFS-NUM-FAELT-FEL    TO MOD-KVVECKOR-AVROP-ATTR           
073000           MOVE NEJ TO INDATA-SW                                          
073100           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
073200        ELSE                                                              
073300           MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVVECKOR-AVROP-ATTR           
073400           MOVE MID-KVVECKOR-AVROP   TO KVVECKOR-AVROP                    
073500        END-IF                                                            
073600     END-IF                                                               
073700     .                                                                    
073800     EJECT                                                                
073900 BB-FLYTTA-FLAGGA-VAL-TILL-MOD SECTION.                                   
074000                                                                          
074100     MOVE +1 TO IX                                                        
074200     PERFORM UNTIL IX > 45                                                
074300       IF MID-FLAGGA (IX) NOT = '+' AND MID-FLAGGA (IX) > SPACE           
074400         IF IX = 6 OR 7 OR 17 OR 18                                       
074500            MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLAGGA-ATTR (IX)            
074600         ELSE                                                             
074700            IF MID-FLAGGA (IX) = 'S'                                      
074800               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLAGGA-ATTR (IX)            
074900               MOVE NEJ TO INDATA-SW                                      
075000            END-IF                                                        
075100         END-IF                                                           
075200         MOVE MID-FLAGGA (IX)           TO VAL-FLAGGA (IX)                
075300       END-IF                                                             
075400       ADD +1 TO IX                                                       
075500     END-PERFORM                                                          
075600     .                                                                    
075700     EJECT                                                                
075800 BC-KONTROLL-OVR   SECTION.                                               
075900                                                                          
076000     IF  MID-KDARBTYP-IN NOT = ALL '+'                                    
076100     AND MID-IDPERSON-IN NOT = ALL '+'                                    
076200         IF MID-IDPERSON-IN NUMERIC                                       
076300           MOVE MID-KDARBTYP-IN        TO W-KDARBTYP                      
076400           MOVE MID-IDPERSON-IN        TO W-IDPERSON                      
076500           PERFORM IMS-GU-WDP311                                          
076600           IF SEGMENT-FINNS                                               
076700             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDARBTYP-UT-ATTR            
076800             MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDPERSON-UT-ATTR            
076900             MOVE PERS-IDMAIL TO IDMAIL                                   
077000           ELSE                                                           
077100             MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDARBTYP-UT-ATTR            
077200             MOVE MFS-NUM-FAELT-FEL    TO MOD-IDPERSON-UT-ATTR            
077300             MOVE NEJ TO INDATA-SW                                        
077400             MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                  
077500           END-IF                                                         
077600         ELSE                                                             
077700           MOVE MFS-NUM-FAELT-FEL      TO MOD-IDPERSON-UT-ATTR            
077800           MOVE NEJ                    TO INDATA-SW                       
077900           MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                    
078000         END-IF                                                           
078100     ELSE                                                                 
078200         MOVE MFS-ALFA-FAELT-FEL       TO MOD-KDARBTYP-UT-ATTR            
078300         MOVE MFS-NUM-FAELT-FEL        TO MOD-IDPERSON-UT-ATTR            
078400         MOVE NEJ                      TO INDATA-SW                       
078500         MOVE ERR-CORR-HIGH-LIT-FLDS   TO MED-IDMFSFEL                    
078600     END-IF                                                               
078700     .                                                                    
078800     EJECT                                                                
078900 BD-KONTROLL-IDDC    SECTION.                                             
079000     SKIP2                                                                
079100     MOVE MID-IDDC                     TO W-IDDC                          
079200     PERFORM IMS-GU-WDB601                                                
079300     IF SEGMENT-FINNS                                                     
079301        IF DCS-NDC-CN                                                     
079302        OR (DCS-NDC-NA AND DCS-USA)                                       
079303           CONTINUE                                                       
079304        ELSE                                                              
079305           MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDDC-ATTR                   
079306           MOVE NEJ                    TO INDATA-SW                       
079307           MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                    
079308        END-IF                                                            
079310     ELSE                                                                 
079500       MOVE MFS-ALFA-FAELT-FEL         TO MOD-IDDC-ATTR                   
079600       MOVE NEJ                        TO INDATA-SW                       
079700       MOVE ERR-DC-MISSING             TO MED-IDMFSFEL                    
079800     END-IF                                                               
079900     .                                                                    
080000     EJECT                                                                
080100 BE-KONTROLL-FOM-TOM SECTION.                                             
080200     SKIP2                                                                
080300     IF  IDBERED-FOM > ZERO                                               
080400     AND IDBERED-TOM = ZERO                                               
080500        MOVE IDBERED-FOM TO IDBERED-TOM                                   
080600     END-IF                                                               
080700                                                                          
080800     IF IDBERED-FOM > IDBERED-TOM                                         
080900        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPERSON-FOM-ATTR                 
081000        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPERSON-TOM-ATTR                 
081100        MOVE NEJ                 TO INDATA-SW                             
081200        MOVE ERR-START-GREATER   TO MED-IDMFSFEL                          
081300     END-IF                                                               
081400                                                                          
081500     IF  KDERS-FOM < 99                                                   
081600     AND KDERS-TOM = 99                                                   
081700        MOVE KDERS-FOM TO KDERS-TOM                                       
081800     END-IF                                                               
081900                                                                          
082000     IF KDERS-FOM > KDERS-TOM                                             
082100        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-FOM-ATTR                    
082200        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-TOM-ATTR                    
082300        MOVE NEJ TO INDATA-SW                                             
082400        MOVE ERR-START-GREATER   TO MED-IDMFSFEL                          
082500     END-IF                                                               
082600                                                                          
082700     IF  IDFKNGRP-FOM > ZERO                                              
082800     AND IDFKNGRP-TOM = ZERO                                              
082900        MOVE IDFKNGRP-FOM TO IDFKNGRP-TOM                                 
083000     END-IF                                                               
083100                                                                          
083200     IF IDFKNGRP-FOM > IDFKNGRP-TOM                                       
083300        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-FOM-ATTR                 
083400        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-TOM-ATTR                 
083500        MOVE NEJ TO INDATA-SW                                             
083600        MOVE ERR-START-GREATER   TO MED-IDMFSFEL                          
083700     END-IF                                                               
083800                                                                          
083900     IF  KDPRODSL-FOM > ZERO                                              
084000     AND KDPRODSL-TOM = ZERO                                              
084100        MOVE KDPRODSL-FOM TO KDPRODSL-TOM                                 
084200     ELSE                                                                 
084300       IF  KDPRODSL-FOM = ZERO                                            
084400       AND KDPRODSL-TOM > ZERO                                            
084500          MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-FOM-ATTR                 
084600          MOVE NEJ TO INDATA-SW                                           
084700          MOVE ERR-WRONG-INTERVAL TO MED-IDMFSFEL                         
084800       END-IF                                                             
084900     END-IF                                                               
085000                                                                          
085100     IF KDPRODSL-FOM > KDPRODSL-TOM                                       
085200        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPRODSL-FOM-ATTR                 
085300        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPRODSL-TOM-ATTR                 
085400        MOVE NEJ TO INDATA-SW                                             
085500        MOVE ERR-START-GREATER  TO MED-IDMFSFEL                           
085600     END-IF                                                               
085700                                                                          
085800*                                                                         
085900*    --- kontroll LAG.OMR + GÅNG-FOM + GÅNG-TOM                           
086000*                                                                         
086100     IF  ADLAGOMR   = Zero                                                
086200     AND ADGANG-FOM = Zero                                                
086300     AND ADGANG-TOM = Zero                                                
086400        CONTINUE                                                          
086500     ELSE                                                                 
086600       If (ADGANG-FOM = Zero and ADGANG-TOM = Zero)                       
086700       or (ADGANG-FOM > Zero and ADGANG-TOM = Zero)                       
086800       or (ADGANG-FOM > ADGANG-TOM )                                      
086900          MOVE MFS-NUM-FAELT-FEL         TO MOD-ADGANG-FOM-ATTR           
087000          MOVE MFS-NUM-FAELT-FEL         TO MOD-ADGANG-TOM-ATTR           
087100          MOVE NEJ TO INDATA-SW                                           
087200          MOVE ERR-CORR-HIGH-LIT-FLDS    TO MED-IDMFSFEL                  
087300       Else                                                               
087400          If ADLAGOMR = Zero                                              
087500             MOVE MFS-NUM-FAELT-FEL      TO MOD-ADLAGOMR-ATTR             
087600             MOVE NEJ TO INDATA-SW                                        
087700             MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                  
087800          End-if                                                          
087900       End-If                                                             
088000     END-IF                                                               
088100     .                                                                    
088200     EJECT                                                                
088300 BF-KONTROLL-KOMBINATION SECTION.                                         
088400     SKIP2                                                                
088500*    -- Endast ETT av dessa fält får vara beställda                       
088600     IF (VAL-IDKR    NOT = SPACE AND '+' ) AND                            
088700        (VAL-LEVBESK NOT = SPACE AND '+' )                                
088800*       -------------------------                                         
088900        If VAL-LEVBESK Not = SPACE                                        
089000          Move MFS-ALFA-FAELT-FEL To MOD-FLAGGA-ATTR (08)                 
089100        End-If                                                            
089200        If VAL-IDKR    Not = SPACE                                        
089300          Move MFS-ALFA-FAELT-FEL To MOD-FLAGGA-ATTR (45)                 
089400        End-If                                                            
089500                                                                          
089600        Move NEJ TO INDATA-SW                                             
089700        Move ERR-CONFLICT-CHOICE TO MED-IDMFSFEL                          
089800     END-IF                                                               
089900                                                                          
090100     .                                                                    
090200     EJECT                                                                
090300 C-UPPDATERA      SECTION.                                                
090400                                                                          
090500     MOVE '2422'   TO MSGSOP-IDTRANS                                      
090600     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
090700     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
090800* MAIL EXCELFIL                                                           
090900     MOVE 'W217S5' TO MSGSOP-IDPROCESS                                    
091000                                                                          
091100     STRING 'IDUSER(' MSG-SIGNON-USERID ') URVAL1('                       
091200            WS-URVAL1 ') URVAL2(' WS-URVAL2 ') LISTA('                    
091300            WS-LISTA ') MAIL(' WS-IDMAIL ')'                              
091400            DELIMITED BY SIZE INTO MSGSOP-TESYMBV                         
091500                                                                          
091600            MOVE MAILSEND       TO MOD-TEMFSINF                           
091700     PERFORM IMS-INSERT-ALTMSG                                            
091800     .                                                                    
091900     EJECT                                                                
092000 D-SAMMA-SIDA      SECTION.                                               
092100                                                                          
092200     MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                  
092300     CALL WMEDKONV USING MED-WMEDAREA                                     
092400     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
092500                                                                          
092600     PERFORM MFS-LAES-IN-IGEN                                             
092700     PERFORM MFS-ROER-EJ-FAELT-IN                                         
092800     .                                                                    
092900     EJECT                                                                
093000 MFS-RENSA-FAELT-IN SECTION.                                              
093100                                                                          
093200*    --- ALLA INDATA-FÄLT                                                 
093300     MOVE MFS-RENSA-FAELT TO MOD-KDARBTYP-UT                              
093400                             MOD-IDPERSON-UT                              
093500                             MOD-IDDC                                     
093600                             MOD-IDPERSON-FOM                             
093700                             MOD-IDPERSON-TOM                             
093800                             MOD-FLAGGA-IDLEVNR-SHIP                      
093810                             MOD-FLAGGA-IDBERED                           
093900                             MOD-KDERS-FOM                                
094000                             MOD-KDERS-TOM                                
094100                             MOD-FLERSDAT-VIPS                            
094200                             MOD-IDFKNGRP-FOM                             
094300                             MOD-IDFKNGRP-TOM                             
094400                             MOD-BEART-SOEK                               
094500                             MOD-KDPRODSL-FOM                             
094600                             MOD-KDPRODSL-TOM                             
094700                             MOD-IDPROJ-URV                               
094800                             MOD-ADLAGOMR                                 
094900                             MOD-ADGANG-FOM                               
095000                             MOD-ADGANG-TOM                               
095100                             MOD-KVVECKOR-KVPB                            
095200                             MOD-KVVECKOR-AVROP                           
095300     MOVE +1 TO IX                                                        
095400     PERFORM UNTIL IX > 5                                                 
095500       MOVE MFS-RENSA-FAELT TO MOD-IDPERSON (IX)                          
095600       MOVE MFS-RENSA-FAELT TO MOD-KDERS  (IX)                            
095700       MOVE MFS-RENSA-FAELT TO MOD-IDFKNGRP(IX)                           
095800       ADD +1 TO IX                                                       
095900     END-PERFORM                                                          
096000                                                                          
096100     MOVE +1 TO IX                                                        
096200     PERFORM UNTIL IX > 5                                                 
096300       If IX = +1                                                         
096400         If MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                  
096500           MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR(IX)                        
096600         Else                                                             
096700*          --- Visa för USER vilket LEVNR som kommer att användas         
096800           Move MSGI-KDARBTYP-SEC-IDLEV To MOD-IDLEVNR(IX)                
096900         End-If                                                           
097000       Else                                                               
097100         MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR(IX)                          
097200       End-If                                                             
097300                                                                          
097400       ADD +1 TO IX                                                       
097500     END-PERFORM                                                          
097600                                                                          
097700     MOVE +1 TO IX                                                        
097800     PERFORM UNTIL IX > 4                                                 
097900       MOVE MFS-RENSA-FAELT TO MOD-BEFT    (IX)                           
098000       ADD +1 TO IX                                                       
098100     END-PERFORM                                                          
098200                                                                          
098300     MOVE +1 TO IX                                                        
098400     PERFORM UNTIL IX > 45                                                
098500       MOVE MFS-RENSA-FAELT TO MOD-FLAGGA (IX)                            
098600       ADD +1 TO IX                                                       
098700     END-PERFORM                                                          
098800     .                                                                    
098900     EJECT                                                                
099000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
099100                                                                          
099200*    --- ALLA INDATA-FÄLT                                                 
099300     MOVE MFS-ROER-EJ-FAELT TO  MOD-KDARBTYP-UT                           
099400                                MOD-IDPERSON-UT                           
099500                                MOD-IDDC                                  
099600                                MOD-IDPERSON-FOM                          
099700                                MOD-IDPERSON-TOM                          
099800                                MOD-FLAGGA-IDLEVNR-SHIP                   
099810                                MOD-FLAGGA-IDBERED                        
099900                                MOD-KDERS-FOM                             
100000                                MOD-KDERS-TOM                             
100100                                MOD-FLERSDAT-VIPS                         
100200                                MOD-IDFKNGRP-FOM                          
100300                                MOD-IDFKNGRP-TOM                          
100400                                MOD-BEART-SOEK                            
100500                                MOD-KDPRODSL-FOM                          
100600                                MOD-KDPRODSL-TOM                          
100700                                MOD-IDPROJ-URV                            
100800                                MOD-ADLAGOMR                              
100900                                MOD-ADGANG-FOM                            
101000                                MOD-ADGANG-TOM                            
101100                                MOD-KVVECKOR-KVPB                         
101200                                MOD-KVVECKOR-AVROP                        
101300                                                                          
101400     MOVE +1 TO IX                                                        
101500     PERFORM UNTIL IX > 5                                                 
101600       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPERSON (IX)                        
101700       MOVE MFS-ROER-EJ-FAELT TO MOD-KDERS  (IX)                          
101800       MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR(IX)                          
101900       MOVE MFS-ROER-EJ-FAELT TO MOD-IDFKNGRP(IX)                         
102000       ADD +1 TO IX                                                       
102100     END-PERFORM                                                          
102200                                                                          
102300     MOVE +1 TO IX                                                        
102400     PERFORM UNTIL IX > 4                                                 
102500       MOVE MFS-ROER-EJ-FAELT TO MOD-BEFT   (IX)                          
102600       ADD +1 TO IX                                                       
102700     END-PERFORM                                                          
102800                                                                          
102900     MOVE +1 TO IX                                                        
103000     PERFORM UNTIL IX > 45                                                
103100       MOVE MFS-ROER-EJ-FAELT TO MOD-FLAGGA (IX)                          
103200       ADD +1 TO IX                                                       
103300     END-PERFORM                                                          
103400     .                                                                    
103500     EJECT                                                                
103600 MFS-LAES-IN-IGEN SECTION.                                                
103700                                                                          
103800*    --- ALLA INDATA-FÄLT                                                 
103900     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPERSON-FOM-ATTR                  
104000                                   MOD-IDPERSON-TOM-ATTR                  
104100                                   MOD-KDERS-FOM-ATTR                     
104200                                   MOD-KDERS-TOM-ATTR                     
104300                                   MOD-IDFKNGRP-FOM-ATTR                  
104400                                   MOD-IDFKNGRP-TOM-ATTR                  
104500                                   MOD-BEART-SOEK-ATTR                    
104600                                   MOD-KVVECKOR-KVPB-ATTR                 
104700                                   MOD-KVVECKOR-AVROP-ATTR                
104800                                   MOD-KDARBTYP-UT-ATTR                   
104900                                   MOD-IDPERSON-UT-ATTR                   
105000                                   MOD-KDPRODSL-FOM-ATTR                  
105100                                   MOD-KDPRODSL-TOM-ATTR                  
105200                                   MOD-IDPROJ-URV-ATTR                    
105300                                   MOD-ADLAGOMR                           
105400                                   MOD-ADGANG-FOM                         
105500                                   MOD-ADGANG-TOM                         
105600                                                                          
105700     MOVE +1 TO IX                                                        
105800     PERFORM UNTIL IX > 5                                                 
105900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPERSON-ATTR (IX)               
106000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDERS-ATTR  (IX)                 
106010       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-ATTR(IX)                 
106020       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDFKNGRP-ATTR(IX)                
106100       ADD +1 TO IX                                                       
106200     END-PERFORM                                                          
106300                                                                          
107000     MOVE +1 TO IX                                                        
107100     PERFORM UNTIL IX > 4                                                 
107300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEFT-ATTR (IX)                   
107400       ADD +1 TO IX                                                       
107500     END-PERFORM                                                          
107600                                                                          
107700     MOVE +1 TO IX                                                        
107800     PERFORM UNTIL IX > 45                                                
107900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLAGGA-ATTR (IX)                 
108000       ADD +1 TO IX                                                       
108100     END-PERFORM                                                          
108200     .                                                                    
108300     EJECT                                                                
108400* --- IMS SEKTIONER ---                                                   
108500     SKIP3                                                                
108600 IMS-GET-MSG SECTION.                                                     
108700                                                                          
108800     MOVE '  QC' TO GODK-STATUSKODER                                      
108900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
109000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
109100     PERFORM IMS-STATUSKONTROLL                                           
109200     .                                                                    
109300     SKIP3                                                                
109400 IMS-INSERT-MSG SECTION.                                                  
109500                                                                          
109600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
109700     MOVE SPACE TO GODK-STATUSKODER                                       
109800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
109900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
110000     PERFORM IMS-STATUSKONTROLL                                           
110100     .                                                                    
110200     SKIP3                                                                
110300 IMS-INSERT-ALTMSG SECTION.                                               
110400                                                                          
110500     MOVE SPACE TO GODK-STATUSKODER                                       
110600     CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                      
110700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
110800     PERFORM IMS-STATUSKONTROLL                                           
110900     .                                                                    
111000     EJECT                                                                
111100 IMS-GU-WDP311 SECTION.                                                   
111200     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
111300            DELIMITED BY SIZE INTO SSA1                                   
111400     STRING 'WDP311  (IDPERSON ='  W-IDPERSON-X ')'                       
111500            DELIMITED BY SIZE INTO SSA2                                   
111600     MOVE '  GE' TO GODK-STATUSKODER                                      
111700     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-P3 SSA1 SSA2                   
111800     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
111900     PERFORM IMS-STATUSKONTROLL                                           
112000     .                                                                    
112100     SKIP3                                                                
112200 IMS-GU-WDB601 SECTION.                                                   
112300                                                                          
112400     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
112500          DELIMITED BY SIZE INTO SSA1                                     
112600     MOVE '  GE' TO GODK-STATUSKODER                                      
112700     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB601 SSA1                   
112800     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
112900     PERFORM IMS-STATUSKONTROLL                                           
113000     .                                                                    
113100     SKIP3                                                                
113200 IMS-STATUSKONTROLL SECTION.                                              
113300                                                                          
113400     SET STATUS-IX TO 1                                                   
113500     SEARCH GODK-STATUS                                                   
113600       AT END                                                             
113700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
113800         DELIMITED BY SIZE INTO FELTEXT                                   
113900         CALL FELLOG                                                      
114000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
114100     END-SEARCH                                                           
114200     .                                                                    
