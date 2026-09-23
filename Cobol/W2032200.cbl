000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2032200.                                                
000400 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000500 DATE-WRITTEN.   91/10/10.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SUBMITT-PGM                                                      
001000*                                                                         
001100*        Detta pgm beställer en lista. Vissa urval kan anges.             
001200*                                                                         
001300*        Sortering kan väljas.                                            
001400*        Variablerna som skall ut kan väljas.                             
001500*        Skrivare kan väljas.                                             
001600*        Listning i form av Excel eller Word-dokument                     
001700*        som attchment till Outlook-mail kan väljas.                      
001800*        List-pgmet startas via sop med ovanstående som parametrar        
001900*        (Listans värden hämtas sen från lagerband, extrakt från          
002000*         orderingång och servicegrad, samt diverse DLI-call)             
002100*                                                                         
002200*        OBS VID FÖRÄNDRING AV BILDENS FUNKTION, V.G. UPPDATERA           
002300*        HJÄLP-BILDEN PÅ 0553  TRANS,W2T322                               
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W2T322                                              
002700*        MID:         W2I32201                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W2O32201                                            
003100*                                                                         
003200*    ÄNDRING:                                                             
003300*        Mars-2003: (C.E.) Vissa val-variabler tagits bort och            
003400*        andra har tillkommit.                                            
003500*        Beställning av släplista flyttas till annan bild (2323 ?)        
003600*                                                                         
003700*                                                                         
003800*   ÄNDRINGAR:                                                            
003900*      2003-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
004000*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
004100*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
004200*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
004300*                                                                         
004400*      2006-05-24. Ändring enligt eTracker 855881.                        
004500*                  Tillägg av urvalsfält: Prodsl, Projkod.                
004600*                  Borttag, Ändring och Tillägg av                        
004700*                  listdatafält.  Planerad rel: 06:6       /C.E.          
004710*                                                                         
004720*      2011-11-15. Ändring enligt eTracker 5578283.                       
004730*                  Tillägg/Ändring av urvals/sök fält:                    
004731*                  Nytt sökfält:                                          
004732*                    Lag.omr + Gång                                       
004733*                  Nya fält:                                              
004734*                    Dir.lev, Avs.dag, Trend, Vikt/Volym, Ursprung        
004740*                  Ändrade fält:                                          
004741*                    VOR/RO, Släp, Säsong, Aut/JIT, Pris, År i lgr        
004760*                    Proj/Mod., Spärrade, Maxpunkt, Uart/LSR              
004770*                                                                         
004780*                  Borttag av utskrift av skrivare, val av att få         
004790*                  ut resultatet i word samt sortering.                   
004792*                  /Inger Stening                                         
004793*                                                                         
004794*      2016-04-28  eTracker 10248818 add possiblity to have               
004795*                  Planner number on screen 2322 and 2422.                
004796*                  /Inger Stening                                         
004797*                                                                         
004800                                                                          
004900     SKIP3                                                                
005000 ENVIRONMENT DIVISION.                                                    
005100     EJECT                                                                
005200 DATA DIVISION.                                                           
005300 WORKING-STORAGE SECTION.                                                 
005400                                                                          
005500*    -- CHECKED BY WY2000                                                 
005600 77  IDPGM                       PIC X(08)   VALUE 'W2032200'.            
005700                                                                          
005800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
006000                                                                          
006100 77  JA                          PIC X       VALUE 'J'.                   
006200 77  NEJ                         PIC X       VALUE 'N'.                   
006300                                                                          
006400 77  IX                          PIC S9(9)  VALUE +0    COMP SYNC.        
006410                                                                          
006411 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006420                                                                          
006500                                                                          
006600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007000     88  INDATA-OK                           VALUE 'J'.                   
007100     88  INDATA-FEL                          VALUE 'N'.                   
007200                                                                          
007300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007400     88  ALLT-OK                             VALUE 'J'.                   
007500                                                                          
007600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007700     88  EGEN-MID                            VALUE '2322'.                
007800     88  GODK-MID                            VALUE '2321' '2322'          
007900                                                   '2323' '2324'          
008000                                                   '2325' '2326'          
008100                                                   '2327' '2328'          
008200                                                   '2329'.                
008800     EJECT                                                                
008900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009000 01  GENERELLA-SUBPROGRAM.                                                
009100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009400     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
009500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009600     EJECT                                                                
009700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009800*01 -COPY WMEDAREA                                                        
009900     SKIP3                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010100     SKIP3                                                                
010200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT (USERBASEN)                  
010300*01 -COPY WMSGINIT                                                        
010400     EJECT                                                                
010500 01  NYCKLAR-TILL-DLI.                                                    
010600   03    W-KDARBTYP-X.                                                    
010700     05    W-KDARBTYP             PIC X(8)    VALUE SPACE.                
010800                                                                          
010900   03    W-IDPERSON-X.                                                    
011000     05    W-IDPERSON            PIC S9(3)   COMP-3 VALUE +0.             
011100                                                                          
011200 01  MESSAGE-CODES.                                                       
011300     03  ERR-CORR-HIGH-LIT-FLDS  PIC X(3)    VALUE '001'.                 
011400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
011500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011600     03  ERR-DATA-NOT-NUMERIC    PIC X(3)    VALUE '020'.                 
011700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011800     03  ERR-START-GREATER       PIC X(3)    VALUE '240'.                 
011900     03  ERR-CONFLICT-CHOICE     PIC X(3)    VALUE '287'.                 
012000     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
012100     03  ERR-WRONG-INTERVAL      PIC X(3)    VALUE '738'.                 
012200     SKIP2                                                                
012300 01  FILLER.                                                              
012400     03  MAILSEND.                                                        
012500         05  FILLER              PIC X(13)   VALUE                        
012600                                 'MAIL ORDERED '.                         
012700 01  FILLER.                                                              
012800     03  LISTUPPSTART.                                                    
012900         05  FILLER              PIC X(30)   VALUE                        
013000                                 'LIST STARTED,     PRINTER: '.           
013100         05  TEXT-PRINTER        PIC X(25)   VALUE SPACE.                 
013200     EJECT                                                                
013300 01  PROG-TO-PROG-SW.                                                     
013400*    03  -COPY WMSGSOP                                                    
013500     EJECT                                                                
013600 01  WS-PARAMETRAR.                                                       
013700     03  WS-IDUSER.                                                       
013800         05  IDUSER              PIC X(8) VALUE SPACES.                   
013900     03  WS-URVAL1.                                                       
014000         05  IDPERSON-FOM        PIC 9(3) VALUE ZEROES.                   
014100         05  IDPERSON-TOM        PIC 9(3) VALUE ZEROES.                   
014200         05  IDPERSON-GRP OCCURS 5 TIMES.                                 
014300             07  IDPERSON        PIC 9(3) VALUE ZEROES.                   
014400         05  IDLEVNR-GRP OCCURS 9 TIMES.                                  
014500             07  IDLEVNR         PIC X(5) VALUE SPACES.                   
014600         05  KVVECKOR-AVROP      PIC 9(2) VALUE ZEROES.                   
014700         05  KVVECKOR-KVPB       PIC 9(2) VALUE ZEROES.                   
014800         05  KDPRODSL-FOM        PIC 9(2) VALUE ZEROES.                   
014900         05  KDPRODSL-TOM        PIC 9(2) VALUE ZEROES.                   
015000         05  IDPROJ-URV          PIC X(4) VALUE SPACES.                   
015100         05  FL-IDLEVNR-SHIP     PIC X(1) VALUE SPACES.                   
015110         05  FL-IDBERED          PIC X(1) VALUE SPACES.                   
015300                                                                          
015400     03  WS-URVAL2.                                                       
015500         05  KDERS-FOM           PIC 9(2) VALUE 99.                       
015600         05  KDERS-TOM           PIC 9(2) VALUE 99.                       
015700         05  KDERS-GRP    OCCURS 5 TIMES.                                 
015800             07  KDERS           PIC 9(2) VALUE 99.                       
015900         05  IDFKNGRP-FOM        PIC 9(4) VALUE ZEROES.                   
016000         05  IDFKNGRP-TOM        PIC 9(4) VALUE ZEROES.                   
016100         05  IDFKNGRP-GRP OCCURS 4 TIMES.                                 
016200             07  IDFKNGRP        PIC 9(4) VALUE ZEROES.                   
016300         05  BEFT-GRP     OCCURS 4 TIMES.                                 
016400             07  BEFT            PIC 9(2) VALUE ZEROES.                   
016500         05  BEART-SOEK          PIC X(25) VALUE SPACES.                  
016600         05  ADLAGOMR            PIC 9(02) VALUE ZEROES.                  
016700         05  ADGANG-FOM          PIC 9(02) VALUE ZEROES.                  
016800         05  ADGANG-TOM          PIC 9(02) VALUE ZEROES.                  
016900         05  KDOTFREK            PIC X(01) VALUE SPACES.                  
016910         05  FILLER              PIC X(02) VALUE SPACES.                  
017000                                                                          
017100                                                                          
017200*    här börjar den nya layouten                                          
017300     03  WS-LISTA.                                                        
017400         05 VAL-IDLEVNR          PIC X(1) VALUE SPACE.                    
017500         05 VAL-LEVBET           PIC X(1) VALUE SPACE.                    
017600         05 VAL-BEART-S          PIC X(1) VALUE SPACE.                    
017700         05 VAL-BEART-GB         PIC X(1) VALUE SPACE.                    
017800         05 VAL-STONHAND         PIC X(1) VALUE SPACE.                    
017900         05 VAL-KVAKS            PIC X(1) VALUE SPACE.                    
018000         05 VAL-VOR-ROS          PIC X(1) VALUE SPACE.                    
018100         05 VAL-KVSLAP           PIC X(1) VALUE SPACE.                    
018200         05 VAL-LEVBESK          PIC X(1) VALUE SPACE.                    
018300         05 VAL-KVART-FORAVIS    PIC X(1) VALUE SPACE.                    
018400         05 VAL-TPO              PIC X(1) VALUE SPACE.                    
018500         05 VAL-KVVECKOR-LT      PIC X(1) VALUE SPACE.                    
018600         05 VAL-KDERS            PIC X(1) VALUE SPACE.                    
018700         05 VAL-IDPERSON         PIC X(1) VALUE SPACE.                    
018800         05 VAL-DIRLEV           PIC X(1) VALUE SPACE.                    
018900         05 VAL-AVSDAG           PIC X(1) VALUE SPACE.                    
019000         05 VAL-TREND            PIC X(1) VALUE SPACE.                    
019100         05 VAL-SAESONG          PIC X(1) VALUE SPACE.                    
019200         05 VAL-TISINLV          PIC X(1) VALUE SPACE.                    
019300         05 VAL-KVPB             PIC X(1) VALUE SPACE.                    
019400         05 VAL-KVOI-RULL        PIC X(1) VALUE SPACE.                    
019500         05 VAL-KVOI-IAR-PL-5    PIC X(1) VALUE SPACE.                    
019600         05 VAL-KDLEVPLF         PIC X(1) VALUE SPACE.                    
019700         05 VAL-IDINK            PIC X(1) VALUE SPACE.                    
019800         05 VAL-PRARTBES         PIC X(1) VALUE SPACE.                    
019900         05 VAL-BESTREST         PIC X(1) VALUE SPACE.                    
020000         05 VAL-AVTAL            PIC X(1) VALUE SPACE.                    
020100         05 VAL-FLFORP-SI        PIC X(1) VALUE SPACE.                    
020200         05 VAL-VKART            PIC X(1) VALUE SPACE.                    
020300         05 VAL-TIREFSTO         PIC X(1) VALUE SPACE.                    
020400         05 VAL-IDPROJ           PIC X(1) VALUE SPACE.                    
020500         05 VAL-TIFINLV          PIC X(1) VALUE SPACE.                    
020600         05 VAL-TIURPROD         PIC X(1) VALUE SPACE.                    
020700         05 VAL-RDIR             PIC X(1) VALUE SPACE.                    
020800         05 VAL-KDPRODSL         PIC X(1) VALUE SPACE.                    
020900         05 VAL-IDFKNGRP         PIC X(1) VALUE SPACE.                    
021000         05 VAL-FLIART           PIC X(1) VALUE SPACE.                    
021100         05 VAL-STYRPARAM        PIC X(1) VALUE SPACE.                    
021200         05 VAL-SERVICE          PIC X(1) VALUE SPACE.                    
021300         05 VAL-KDSORT           PIC X(1) VALUE SPACE.                    
021400         05 VAL-KVANTER          PIC X(1) VALUE SPACE.                    
021500         05 VAL-FORPINFO         PIC X(1) VALUE SPACE.                    
021600         05 VAL-KVSPANT          PIC X(1) VALUE SPACE.                    
021700         05 VAL-S-LAGER          PIC X(1) VALUE SPACE.                    
021800         05 VAL-KVMP             PIC X(1) VALUE SPACE.                    
021900         05 VAL-IDKR             PIC X(1) VALUE SPACE.                    
022000         05 VAL-KAMPANJ          PIC X(1) VALUE SPACE.                    
022100         05 VAL-ADART            PIC X(1) VALUE SPACE.                    
022200         05 VAL-ADINPORT         PIC X(1) VALUE SPACE.                    
022300         05 VAL-KDUART           PIC X(1) VALUE SPACE.                    
022400         05 VAL-URSPRUNG         PIC X(1) VALUE SPACE.                    
022410         05 VAL-MOTFREK         PIC X(1) VALUE SPACE.                     
022500                                                                          
022600     03  WS-FLAGGA  REDEFINES WS-LISTA.                                   
022700         05 VAL-FLAGGA OCCURS 52 PIC X(1).                                
022710                                                                          
022800     03  MAX-ANTAL-FLAGGOR       PIC 9(2) VALUE 52.                       
033600                                                                          
033700     03  WS-PRINTER.                                                      
033800         05  IDPRINTER           PIC X(8) VALUE SPACE.                    
033900                                                                          
034000     03  WS-IDMAIL.                                                       
034100         05  IDMAIL              PIC X(58) VALUE SPACE.                   
034200     EJECT                                                                
034300*01  -COPY W006PRT                                                        
034400     EJECT                                                                
034500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
034600*                                                                         
034700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
034800     SKIP3                                                                
034900*01  MID -COPY W2I32201                                                   
035000     EJECT                                                                
035100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
035200     SKIP3                                                                
035300*01  -COPY WMSGAREA                                                       
035400     EJECT                                                                
035500     03  MOD REDEFINES MSG-AREA.                                          
035600*      05  -COPY W2O32201                                                 
035700     EJECT                                                                
035800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
035900     SKIP3                                                                
036000*01  -COPY WMFSAREA                                                       
036100     EJECT                                                                
036200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
036300     SKIP3                                                                
036400*    --- STATUS-KOD FRÅN IMS                                              
036500 01  STATUS-WS                   PIC XX.                                  
036600     88  SEGMENT-FINNS                       VALUE '  '.                  
036700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
036800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
036900     SKIP2                                                                
037000 01  GODK-STATUSKODER.                                                    
037100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
037200     EJECT                                                                
037300*- - - - - - - - - - - - - - -DLI INPUT-OUTPUT AREA                       
037400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-P3'.           
037500     SKIP3                                                                
037600 01  DLI-IO-P3.                                                           
037700*    03  -COPY WDP311                                                     
037800     EJECT                                                                
037900 01  SSA1                        PIC X(64).                               
038000 01  SSA2                        PIC X(64).                               
038100*    --- IMS FUNKTIONSKODER                                               
038200*01  -COPY W0003                                                          
038300     EJECT                                                                
038400 LINKAGE SECTION.                                                         
038500*01  -COPY W0009   -PRE MSG-                                              
038600                                                                          
038700*01  -COPY W0009   -PRE ALT-                                              
038800                                                                          
038900*01  -COPY W0008   -PRE WDP3-                                             
039000     05  FILLER                  PIC X.                                   
039100                                                                          
039200*01  -COPY W0008   -PRE USEA-                                             
039300     05  FILLER                  PIC X.                                   
039400     EJECT                                                                
039500 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP3-PCB USEA-PCB.             
039600 MAIN SECTION.                                                            
039700     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP3-PCB USEA-PCB.             
039800                                                                          
039900     PERFORM IMS-GET-MSG                                                  
040000     IF SEGMENT-FINNS                                                     
040100       PERFORM A-INIT                                                     
040200       IF MFS-UPDATE                                                      
040300          PERFORM B-KOLLA-INPUT                                           
040400          IF INDATA-OK                                                    
040500             PERFORM C-UPPDATERA                                          
040600             PERFORM MFS-RENSA-FAELT-IN                                   
040700             MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                         
040800          END-IF                                                          
040900       ELSE                                                               
041010          IF NOT EGEN-MID                                                 
041011             PERFORM MFS-RENSA-FAELT-IN                                   
041012          ELSE                                                            
041020             IF MID-URVAL = ALL '+' AND                                   
041100                MID-LISTA = ALL '+'                                       
041200                PERFORM MFS-RENSA-FAELT-IN                                
041300             ELSE                                                         
041400                PERFORM D-SAMMA-SIDA                                      
041500             END-IF                                                       
041510          END-IF                                                          
041600       END-IF                                                             
041720                                                                          
041800       ADD LENGTH OF MOD-W2O32201 +4 GIVING MSG-KVLL                      
041900       PERFORM IMS-INSERT-MSG                                             
042000     END-IF                                                               
042100                                                                          
042200     MOVE ZERO TO RETURN-CODE                                             
042300     GOBACK                                                               
042400     .                                                                    
042500     EJECT                                                                
042600 A-INIT SECTION.                                                          
042700                                                                          
042800     IF MSG-DUBBLA-TRANSKODER                                             
042900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I32201                 
043000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
043100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
043200     ELSE                                                                 
043300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I32201                  
043400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
043500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
043600     END-IF                                                               
043700                                                                          
043800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
043900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
044000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
044100                                                                          
044200*    --- Läs userbasen                                                    
044300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
044400     MOVE '001'             TO MSGI-KDCALL                                
044500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
044600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
044700     MOVE '2322'            TO MSGI-IDTRANS                               
044800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
044900                                                                          
044910     MOVE +2 TO SPRAK-IX                                                  
044920     MOVE 'GB ' TO MED-IDSKYLT                                            
044930                                                                          
045000     MOVE LOW-VALUE TO MSG-AREA                                           
045100     MOVE 'W2O322N1' TO MFS-IDMOD                                         
045200     MOVE '2322' TO MOD-IDTRANS                                           
045300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
045400                                                                          
045500     IF NOT EGEN-MID                                                      
045600       MOVE SPACE TO MFS-KDTRTYP                                          
045700       MOVE '7' TO MFS-IDPFK                                              
045800     END-IF                                                               
045900     .                                                                    
046000     EJECT                                                                
046100 B-KOLLA-INPUT   SECTION.                                                 
046200                                                                          
046300     INITIALIZE WS-URVAL1 WS-URVAL2                                       
046400     MOVE JA TO INDATA-SW                                                 
046500                                                                          
046600     IF MID-URVAL = ALL '+' OR MID-LISTA = ALL '+'                        
046700        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
046800        CALL WMEDKONV USING MED-WMEDAREA                                  
046900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
047000        IF MID-URVAL = ALL '+' AND MID-LISTA = ALL '+'                    
047100           PERFORM MFS-RENSA-FAELT-IN                                     
047200        ELSE                                                              
047300           PERFORM MFS-LAES-IN-IGEN                                       
047400           PERFORM MFS-ROER-EJ-FAELT-IN                                   
047500        END-IF                                                            
047600        MOVE NEJ TO INDATA-SW                                             
047700        MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                       
047800     ELSE                                                                 
047900        PERFORM BA-KONTROLL-URVAL                                         
048000        PERFORM BB-FLYTTA-FLAGGA-VAL-TILL-MOD                             
048100        PERFORM BC-KONTROLL-OVR                                           
048200                                                                          
048300        IF INDATA-OK                                                      
048400           PERFORM BD-KONTROLL-FOM-TOM                                    
048500        END-IF                                                            
048600        IF INDATA-OK                                                      
048700           PERFORM BE-KONTROLL-KOMBINATION                                
048800        END-IF                                                            
048900                                                                          
049000        IF INDATA-FEL                                                     
049100          IF MOD-TEMFSFEL NOT > SPACE                                     
049200             CALL WMEDKONV USING MED-WMEDAREA                             
049300             MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                            
049400          END-IF                                                          
049500          PERFORM MFS-ROER-EJ-FAELT-IN                                    
049600        END-IF                                                            
049700     END-IF                                                               
049800     .                                                                    
049900     EJECT                                                                
050000 BA-KONTROLL-URVAL SECTION.                                               
050100                                                                          
050200     IF MID-IDPERSON-FOM NOT = ALL '+'                                    
050300        IF MID-IDPERSON-FOM NOT NUMERIC                                   
050400           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPERSON-FOM-ATTR              
050500           MOVE NEJ TO INDATA-SW                                          
050600           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
050700        ELSE                                                              
050800           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPERSON-FOM-ATTR              
050900           MOVE MID-IDPERSON-FOM    TO IDPERSON-FOM                       
051000        END-IF                                                            
051100     END-IF                                                               
051200                                                                          
051300     IF MID-IDPERSON-TOM NOT = ALL '+'                                    
051400        IF MID-IDPERSON-TOM NOT NUMERIC                                   
051500           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPERSON-TOM-ATTR              
051600           MOVE NEJ TO INDATA-SW                                          
051700           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
051800        ELSE                                                              
051900           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPERSON-TOM-ATTR              
052000           MOVE MID-IDPERSON-TOM    TO IDPERSON-TOM                       
052100        END-IF                                                            
052200     END-IF                                                               
052300                                                                          
052400     MOVE +1 TO IX                                                        
052500     PERFORM UNTIL IX > 5                                                 
052600       IF MID-IDPERSON(IX) NOT = ALL '+'                                  
052700          IF MID-IDPERSON(IX) NOT NUMERIC                                 
052800             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPERSON-ATTR (IX)           
052900             MOVE NEJ TO INDATA-SW                                        
053000             MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                    
053100          ELSE                                                            
053200             IF (MID-IDPERSON-FOM NOT = ALL '+' OR                        
053300                 MID-IDPERSON-TOM NOT = ALL '+')                          
053400                MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPERSON-FOM-ATTR         
053500                MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPERSON-TOM-ATTR         
053600                MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPERSON-ATTR(IX)         
053700                MOVE NEJ TO INDATA-SW                                     
053800                MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL               
053900             ELSE                                                         
054000                MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPERSON-ATTR(IX)         
054100                MOVE MID-IDPERSON (IX)   TO IDPERSON (IX)                 
054200             END-IF                                                       
054300          END-IF                                                          
054400       ELSE                                                               
054500                MOVE ZEROES              TO IDPERSON (IX)                 
054600       END-IF                                                             
054700       ADD +1 TO IX                                                       
054800     END-PERFORM                                                          
054900                                                                          
055000*    --- Här kollas mot SEC-IDLEV på WDP7 om restriktioner finns          
055100     If MSGI-KDARBTYP-SEC-IDLEV = SPACE Or LOW-VALUE                      
055200       MOVE +1 TO IX                                                      
055300       PERFORM UNTIL IX > 9                                               
055400         IF MID-IDLEVNR(IX) NOT = ALL '+'                                 
055500            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-ATTR(IX)             
055600            MOVE MID-IDLEVNR (IX)  TO IDLEVNR (IX)                        
055700         ELSE                                                             
055800            MOVE SPACES            TO IDLEVNR (IX)                        
055900         END-IF                                                           
056000         ADD +1 TO IX                                                     
056100       END-PERFORM                                                        
056200     Else                                                                 
056300*      --- Endast det leverantör-ID som tillhör USER får listas,          
056400*      --- och SKALL listas, även om inget är inmatat.                    
056500       Move +1 To IX                                                      
056600       Perform Until IX > 9                                               
056700         If IX = +1                                                       
056800           Move MSGI-KDARBTYP-SEC-IDLEV To IDLEVNR (IX)                   
056900         Else                                                             
057000           Move SPACES To IDLEVNR (IX)                                    
057100         End-If                                                           
057200         Add +1 TO IX                                                     
057300       End-Perform                                                        
057400     End-If                                                               
057500                                                                          
057520     IF MID-FLAGGA-IDBERED NOT = '+'                                      
057530       IF MID-FLAGGA-IDBERED = 'X' OR 'J' OR 'Y'                          
057540         MOVE MFS-ALFA-FAELT-RAETT To                                     
057550                                      MOD-FLAGGA-IDBERED-ATTR             
057560         MOVE 'X'                        TO FL-IDBERED                    
057570       END-IF                                                             
057580     ELSE                                                                 
057590       MOVE SPACES                       TO FL-IDBERED                    
057591     END-IF                                                               
057593                                                                          
057600     IF MID-FLAGGA-IDLEVNR-SHIP NOT = '+'                                 
057700       IF MID-FLAGGA-IDLEVNR-SHIP = 'X' OR 'J' OR 'Y'                     
057800         MOVE MFS-ALFA-FAELT-RAETT                                        
057900                              TO MOD-FLAGGA-IDLEVNR-SHIP-ATTR             
058000         MOVE 'X'             TO FL-IDLEVNR-SHIP                          
058100       END-IF                                                             
058200     ELSE                                                                 
058300       MOVE SPACES            TO FL-IDLEVNR-SHIP                          
058400     END-IF                                                               
058500                                                                          
058600     IF MID-KDERS-FOM NOT = ALL '+'                                       
058700        IF MID-KDERS-FOM NOT NUMERIC                                      
058800           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-FOM-ATTR                 
058900           MOVE NEJ TO INDATA-SW                                          
059000           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
059100        ELSE                                                              
059200           IF MID-KDERS-FOM > 52                                          
059300             MOVE MFS-NUM-FAELT-FEL TO MOD-KDERS-FOM-ATTR                 
059400             MOVE NEJ TO INDATA-SW                                        
059500             MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                  
059600           ELSE                                                           
059700             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDERS-FOM-ATTR               
059800             MOVE MID-KDERS-FOM       TO KDERS-FOM                        
059900           END-IF                                                         
060000        END-IF                                                            
060100     ELSE                                                                 
060200*       -- VÄRDET 99 STÅR FÖR OIFYLLT VÄRDE                               
060300        MOVE 99 TO KDERS-FOM                                              
060400     END-IF                                                               
060500                                                                          
060600     IF MID-KDERS-TOM NOT = ALL '+'                                       
060700        IF MID-KDERS-TOM NOT NUMERIC                                      
060800           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-TOM-ATTR                 
060900           MOVE NEJ TO INDATA-SW                                          
061000           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
061100        ELSE                                                              
061200           IF MID-KDERS-TOM > 52                                          
061300             MOVE MFS-NUM-FAELT-FEL TO MOD-KDERS-TOM-ATTR                 
061400             MOVE NEJ TO INDATA-SW                                        
061500             MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                  
061600           ELSE                                                           
061700             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDERS-TOM-ATTR               
061800             MOVE MID-KDERS-TOM       TO KDERS-TOM                        
061900           END-IF                                                         
062000        END-IF                                                            
062100     ELSE                                                                 
062200*       -- VÄRDET 99 STÅR FÖR OIFYLLT VÄRDE                               
062300        MOVE 99 TO KDERS-TOM                                              
062400     END-IF                                                               
062500                                                                          
062600     MOVE +1 TO IX                                                        
062700     PERFORM UNTIL IX > 5                                                 
062800       IF MID-KDERS(IX) NOT = ALL '+'                                     
062900          IF MID-KDERS(IX) NOT NUMERIC                                    
063000             MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-ATTR (IX)              
063100             MOVE NEJ TO INDATA-SW                                        
063200             MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                    
063300          ELSE                                                            
063400             IF (MID-KDERS-FOM NOT = ALL '+' OR                           
063500                 MID-KDERS-TOM NOT = ALL '+')                             
063600                MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-FOM-ATTR            
063700                MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-TOM-ATTR            
063800                MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-ATTR(IX)            
063900                MOVE NEJ TO INDATA-SW                                     
064000                MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL               
064100             ELSE                                                         
064200                IF MID-KDERS (IX) > 52                                    
064300                  MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-ATTR(IX)          
064400                  MOVE NEJ TO INDATA-SW                                   
064500                  MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL             
064600                ELSE                                                      
064700                  MOVE MFS-NUM-FAELT-RAETT TO MOD-KDERS-ATTR(IX)          
064800                  MOVE MID-KDERS (IX)      TO KDERS (IX)                  
064900                END-IF                                                    
065000             END-IF                                                       
065100          END-IF                                                          
065200       ELSE                                                               
065300*         -- VÄRDET 99 STÅR FÖR OIFYLLT VÄRDE                             
065400          MOVE 99                  TO KDERS (IX)                          
065500       END-IF                                                             
065600       ADD +1 TO IX                                                       
065700     END-PERFORM                                                          
065800                                                                          
065900*    --- FUNKTIONSGRUPP                                                   
066000*    --- INTERVALL FRÅN OCH MED                                           
066100     IF MID-IDFKNGRP-FOM NOT = ALL '+'                                    
066200        IF MID-IDFKNGRP-FOM NOT NUMERIC                                   
066300           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-FOM-ATTR              
066400           MOVE NEJ TO INDATA-SW                                          
066500           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
066600        ELSE                                                              
066700           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-FOM-ATTR              
066800           MOVE MID-IDFKNGRP-FOM    TO IDFKNGRP-FOM                       
066900        END-IF                                                            
067000     END-IF                                                               
067100                                                                          
067200*    --- INTERVALL TILL OCH MED                                           
067300     IF MID-IDFKNGRP-TOM NOT = ALL '+'                                    
067400        IF MID-IDFKNGRP-TOM NOT NUMERIC                                   
067500           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-TOM-ATTR              
067600           MOVE NEJ TO INDATA-SW                                          
067700           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
067800        ELSE                                                              
067900           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-TOM-ATTR              
068000           MOVE MID-IDFKNGRP-TOM    TO IDFKNGRP-TOM                       
068100        END-IF                                                            
068200     END-IF                                                               
068300                                                                          
068400*    --- FUNKTIONSGRUPP 4 EXPLICITA VAL                                   
068500     MOVE +1 TO IX                                                        
068600     PERFORM UNTIL IX > +4                                                
068700       IF MID-IDFKNGRP(IX) NOT = ALL '+'                                  
068800          IF MID-IDFKNGRP(IX) NOT NUMERIC                                 
068900             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-ATTR(IX)            
069000             MOVE NEJ TO INDATA-SW                                        
069100             MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                    
069200          ELSE                                                            
069300             IF (MID-IDFKNGRP-FOM NOT = ALL '+'                           
069400             OR  MID-IDFKNGRP-TOM NOT = ALL '+')                          
069500                MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-FOM-ATTR         
069600                MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-TOM-ATTR         
069700                MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-ATTR(IX)         
069800                MOVE NEJ TO INDATA-SW                                     
069900                MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL               
070000             ELSE                                                         
070100                MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-ATTR(IX)         
070200                MOVE MID-IDFKNGRP(IX)    TO IDFKNGRP(IX)                  
070300             END-IF                                                       
070400          END-IF                                                          
070500       ELSE                                                               
070600                MOVE ZERO                TO IDFKNGRP(IX)                  
070700       END-IF                                                             
070800       ADD +1 TO IX                                                       
070900     END-PERFORM                                                          
071000*                                                                         
071100*    --- FÖRPACKNINGSTYPER                                                
071200     MOVE +1 TO IX                                                        
071300     PERFORM UNTIL IX > +4                                                
071400       IF MID-BEFT(IX) NOT = ALL '+'                                      
071500                                                                          
071600          IF MID-BEFT(IX) NOT NUMERIC                                     
071700             MOVE MFS-NUM-FAELT-FEL   TO MOD-BEFT-ATTR (IX)               
071800             MOVE NEJ TO INDATA-SW                                        
071900             MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                    
072000          ELSE                                                            
072100             MOVE MFS-NUM-FAELT-RAETT TO MOD-BEFT-ATTR(IX)                
072200             MOVE MID-BEFT (IX)       TO BEFT (IX)                        
072300          END-IF                                                          
072400       ELSE                                                               
072500             MOVE ZERO                TO BEFT (IX)                        
072600       END-IF                                                             
072700       ADD +1 TO IX                                                       
072800     END-PERFORM                                                          
072900*                                                                         
073000*    ---   BENÄMNINGS-SÖKNING                                             
073100     IF MID-BEART-SOEK NOT = ALL '+'                                      
073200        IF MID-BEART-SOEK = SPACE                                         
073300           MOVE ALL '+'          TO MID-BEART-SOEK                        
073400           MOVE MFS-RENSA-FAELT  TO MOD-BEART-SOEK                        
073500        ELSE                                                              
073600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-SOEK-ATTR               
073700           MOVE MID-BEART-SOEK       TO BEART-SOEK                        
073800        END-IF                                                            
073900     END-IF                                                               
077300*                                                                         
077400*     nytt vid 06:6                                                       
077500     IF MID-KDPRODSL-FOM NOT = ALL '+'                                    
077600        IF MID-KDPRODSL-FOM NUMERIC                                       
077700           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-FOM-ATTR              
077800           MOVE MID-KDPRODSL-FOM    TO KDPRODSL-FOM                       
077900        ELSE                                                              
078000           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPRODSL-FOM-ATTR              
078100           MOVE NEJ TO INDATA-SW                                          
078200           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
078300        END-IF                                                            
078400     END-IF                                                               
078500*     nytt vid 06:6                                                       
078600     IF MID-KDPRODSL-TOM NOT = ALL '+'                                    
078700        IF MID-KDPRODSL-TOM NUMERIC                                       
078800           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-TOM-ATTR              
078900           MOVE MID-KDPRODSL-TOM    TO KDPRODSL-TOM                       
079000        ELSE                                                              
079100           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPRODSL-TOM-ATTR              
079200           MOVE NEJ TO INDATA-SW                                          
079300           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
079400        END-IF                                                            
079500     END-IF                                                               
079600*                                                                         
079700     IF MID-IDPROJ-URV NOT = ALL '+' AND SPACE                            
079800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-URV-ATTR                 
079900         MOVE MID-IDPROJ-URV       TO IDPROJ-URV                          
080000     END-IF                                                               
080101*                                                                         
080102*    ---   LAG.OMR                                                        
080103     IF MID-ADLAGOMR NOT = ALL '+'                                        
080104        IF MID-ADLAGOMR NOT NUMERIC                                       
080105           MOVE MFS-NUM-FAELT-FEL    TO MOD-ADLAGOMR-ATTR                 
080106           MOVE NEJ TO INDATA-SW                                          
080107           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
080108        ELSE                                                              
080109           MOVE MFS-NUM-FAELT-RAETT  TO MOD-ADLAGOMR-ATTR                 
080110           MOVE MID-ADLAGOMR         TO ADLAGOMR                          
080111        END-IF                                                            
080112     END-IF                                                               
080113*                                                                         
080114*    ---   GÅNG-FOM                                                       
080115     IF MID-ADGANG-FOM NOT = ALL '+'                                      
080116        IF MID-ADGANG-FOM NOT NUMERIC                                     
080117           MOVE MFS-NUM-FAELT-FEL    TO MOD-ADGANG-FOM-ATTR               
080118           MOVE NEJ TO INDATA-SW                                          
080119           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
080120        ELSE                                                              
080121           MOVE MFS-NUM-FAELT-RAETT  TO MOD-ADGANG-FOM-ATTR               
080122           MOVE MID-ADGANG-FOM       TO ADGANG-FOM                        
080123        END-IF                                                            
080124     END-IF                                                               
081400                                                                          
081510*    ---   GÅNG-TOM                                                       
081600     IF MID-ADGANG-TOM NOT = ALL '+'                                      
081700        IF MID-ADGANG-TOM NOT NUMERIC                                     
081800           MOVE MFS-NUM-FAELT-FEL    TO MOD-ADGANG-TOM-ATTR               
081900           MOVE NEJ TO INDATA-SW                                          
082000           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
082100        ELSE                                                              
082200           MOVE MFS-NUM-FAELT-RAETT  TO MOD-ADGANG-TOM-ATTR               
082300           MOVE MID-ADGANG-TOM       TO ADGANG-TOM                        
082400        END-IF                                                            
082500     END-IF                                                               
082510                                                                          
082600*    ---   KDOTFREK                                                       
082610     IF MID-KDOTFREK NOT = ALL '+'                                        
082620        IF MID-KDOTFREK = SPACE                                           
082621           MOVE MFS-NUM-FAELT-FEL    TO MOD-KDOTFREK-ATTR                 
082622           MOVE NEJ TO INDATA-SW                                          
082623           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
082624        ELSE                                                              
082625           MOVE MFS-NUM-FAELT-RAETT  TO MOD-KDOTFREK-ATTR                 
082626           MOVE MID-KDOTFREK         TO KDOTFREK                          
082627        END-IF                                                            
082628     END-IF                                                               
082630*                                                                         
082700*    --- ANTAL VECKOR TOT.BEHOV                                           
082800*    --- Detta urvalsfält finns bland List-dataval-fälten                 
082900     IF MID-KVVECKOR-KVPB NOT = ALL '+'                                   
083000        IF MID-KVVECKOR-KVPB NOT NUMERIC                                  
083100           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVVECKOR-KVPB-ATTR             
083200           MOVE NEJ TO INDATA-SW                                          
083300           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
083400        ELSE                                                              
083500           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVVECKOR-KVPB-ATTR             
083600           MOVE MID-KVVECKOR-KVPB   TO KVVECKOR-KVPB                      
083700        END-IF                                                            
083800     END-IF                                                               
083900*                                                                         
084000*    --- ANTAL VECKOR KVAVROP                                             
084100*    --- Detta urvalsfält finns bland List-dataval-fälten                 
084200     IF MID-KVVECKOR-AVROP NOT = ALL '+'                                  
084300        IF MID-KVVECKOR-AVROP NOT NUMERIC                                 
084400           MOVE MFS-NUM-FAELT-FEL   TO MOD-KVVECKOR-AVROP-ATTR            
084500           MOVE NEJ TO INDATA-SW                                          
084600           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
084700        ELSE                                                              
084800           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVVECKOR-AVROP-ATTR            
084900           MOVE MID-KVVECKOR-AVROP  TO KVVECKOR-AVROP                     
085000        END-IF                                                            
085100     END-IF                                                               
085200     .                                                                    
085300     EJECT                                                                
085400 BB-FLYTTA-FLAGGA-VAL-TILL-MOD SECTION.                                   
085500                                                                          
085600     MOVE +1 TO IX                                                        
085700     PERFORM UNTIL IX > MAX-ANTAL-FLAGGOR                                 
085800       IF MID-FLAGGA (IX) NOT = '+' AND MID-FLAGGA (IX) > SPACE           
085810         IF IX = 7 OR 8 OR 11 OR 17 OR 18 OR 43 OR 47                     
085811            MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAGGA-ATTR (IX)             
085812         ELSE                                                             
085813            IF MID-FLAGGA (IX) = 'S'                                      
085814               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLAGGA-ATTR (IX)            
085815               MOVE NEJ TO INDATA-SW                                      
085816            END-IF                                                        
085820         END-IF                                                           
086000         MOVE MID-FLAGGA (IX)      TO VAL-FLAGGA (IX)                     
086300       END-IF                                                             
086400       ADD +1 TO IX                                                       
086500     END-PERFORM                                                          
088400     .                                                                    
088500     EJECT                                                                
088600 BC-KONTROLL-OVR   SECTION.                                               
088700                                                                          
088800     IF  MID-KDARBTYP-IN NOT = ALL '+'                                    
088900     AND MID-IDPERSON-IN NOT = ALL '+'                                    
089000         IF MID-IDPERSON-IN NUMERIC                                       
089100           MOVE MID-KDARBTYP-IN TO W-KDARBTYP                             
089200           MOVE MID-IDPERSON-IN TO W-IDPERSON                             
089300           PERFORM IMS-GU-WDP311                                          
089400           IF SEGMENT-FINNS                                               
089500             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDARBTYP-IN-ATTR            
089600             MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDPERSON-IN-ATTR            
089800             MOVE PERS-IDMAIL TO IDMAIL                                   
090700           ELSE                                                           
090800             MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDARBTYP-IN-ATTR            
090900             MOVE MFS-NUM-FAELT-FEL    TO MOD-IDPERSON-IN-ATTR            
091000             MOVE NEJ TO INDATA-SW                                        
091100             MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                  
091200           END-IF                                                         
091300         ELSE                                                             
091400           MOVE MFS-NUM-FAELT-FEL TO MOD-IDPERSON-IN-ATTR                 
091500           MOVE NEJ TO INDATA-SW                                          
091600           MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                    
091700         END-IF                                                           
091800     ELSE                                                                 
091900         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDARBTYP-IN-ATTR                
092000         MOVE MFS-NUM-FAELT-FEL    TO MOD-IDPERSON-IN-ATTR                
092100         MOVE NEJ                  TO INDATA-SW                           
092200         MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                      
092300     END-IF                                                               
092400     .                                                                    
092500     EJECT                                                                
093900 BD-KONTROLL-FOM-TOM SECTION.                                             
094000     SKIP2                                                                
094100     IF  IDPERSON-FOM > ZERO                                              
094200     AND IDPERSON-TOM = ZERO                                              
094300        MOVE IDPERSON-FOM TO IDPERSON-TOM                                 
094400     END-IF                                                               
094500                                                                          
094600     IF IDPERSON-FOM > IDPERSON-TOM                                       
094700        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPERSON-FOM-ATTR                 
094800        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDPERSON-TOM-ATTR                 
094900        MOVE NEJ TO INDATA-SW                                             
095000        MOVE ERR-START-GREATER   TO MED-IDMFSFEL                          
095100     END-IF                                                               
095200                                                                          
095300     IF  KDERS-FOM < 99                                                   
095400     AND KDERS-TOM = 99                                                   
095500        MOVE KDERS-FOM TO KDERS-TOM                                       
095600     END-IF                                                               
095700                                                                          
095800     IF KDERS-FOM > KDERS-TOM                                             
095900        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-FOM-ATTR                    
096000        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-TOM-ATTR                    
096100        MOVE NEJ TO INDATA-SW                                             
096200        MOVE ERR-START-GREATER   TO MED-IDMFSFEL                          
096300     END-IF                                                               
096400                                                                          
096500     IF  IDFKNGRP-FOM > ZERO                                              
096600     AND IDFKNGRP-TOM = ZEROES                                            
096700        MOVE IDFKNGRP-FOM TO IDFKNGRP-TOM                                 
096800     END-IF                                                               
096900                                                                          
097000     IF IDFKNGRP-FOM > IDFKNGRP-TOM                                       
097100        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-FOM-ATTR                 
097200        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-TOM-ATTR                 
097300        MOVE NEJ TO INDATA-SW                                             
097400        MOVE ERR-START-GREATER   TO MED-IDMFSFEL                          
097500     END-IF                                                               
097600                                                                          
097700     IF  KDPRODSL-FOM > ZERO                                              
097800     AND KDPRODSL-TOM = ZEROES                                            
097900        MOVE KDPRODSL-FOM TO KDPRODSL-TOM                                 
098000     ELSE                                                                 
098100       IF  KDPRODSL-FOM = ZEROES                                          
098200       AND KDPRODSL-TOM > ZERO                                            
098300          MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-FOM-ATTR                 
098400          MOVE NEJ TO INDATA-SW                                           
098500          MOVE ERR-WRONG-INTERVAL TO MED-IDMFSFEL                         
098600       END-IF                                                             
098700     END-IF                                                               
098800                                                                          
098900     IF KDPRODSL-FOM > KDPRODSL-TOM                                       
099000        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPRODSL-FOM-ATTR                 
099100        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPRODSL-TOM-ATTR                 
099200        MOVE NEJ TO INDATA-SW                                             
099300        MOVE ERR-START-GREATER  TO MED-IDMFSFEL                           
099400     END-IF                                                               
099500                                                                          
099510*                                                                         
099520*    --- kontroll LAG.OMR + GÅNG-FOM + GÅNG-TOM                           
099530*                                                                         
099540     IF  ADLAGOMR   = Zero                                                
099550     AND ADGANG-FOM = Zero                                                
099560     AND ADGANG-TOM = Zero                                                
099570        CONTINUE                                                          
099580     ELSE                                                                 
099590       If (ADGANG-FOM = Zero and ADGANG-TOM = Zero)                       
099592       or (ADGANG-FOM > Zero and ADGANG-TOM = Zero)                       
099593       or (ADGANG-FOM > ADGANG-TOM )                                      
099594          MOVE MFS-NUM-FAELT-FEL         TO MOD-ADGANG-FOM-ATTR           
099595          MOVE MFS-NUM-FAELT-FEL         TO MOD-ADGANG-TOM-ATTR           
099596          MOVE NEJ TO INDATA-SW                                           
099597          MOVE ERR-CORR-HIGH-LIT-FLDS    TO MED-IDMFSFEL                  
099599       Else                                                               
099600          If ADLAGOMR = Zero                                              
099601             MOVE MFS-NUM-FAELT-FEL      TO MOD-ADLAGOMR-ATTR             
099602             MOVE NEJ TO INDATA-SW                                        
099603             MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                  
099605          End-if                                                          
099606       End-If                                                             
099607     END-IF                                                               
101400     .                                                                    
101500     EJECT                                                                
101600 BE-KONTROLL-KOMBINATION SECTION.                                         
101700     SKIP2                                                                
101800*    -- Endast ETT av dessa fält får vara beställda                       
101900     IF ( (VAL-IDKR       NOT = SPACE AND '+' ) AND                       
102000          ( ( VAL-KAMPANJ NOT = SPACE AND '+' )                           
102100                       OR                                                 
102200            ( VAL-TPO     NOT = SPACE AND '+' )                           
102210                       OR                                                 
102220            ( VAL-LEVBESK NOT = SPACE AND '+' ) ) )                       
102300*       -------------------------                                         
102400     OR ( (VAL-KAMPANJ    NOT = SPACE AND '+' ) AND                       
102500          ( ( VAL-IDKR    NOT = SPACE AND '+' )                           
102600                       OR                                                 
102610            ( VAL-TPO     NOT = SPACE AND '+' )                           
102620                       OR                                                 
102700            ( VAL-LEVBESK NOT = SPACE AND '+' ) ) )                       
102800*       -------------------------                                         
102900     OR ( (VAL-LEVBESK    NOT = SPACE AND '+' ) AND                       
103000          ( ( VAL-IDKR    NOT = SPACE AND '+' )                           
103100                       OR                                                 
103110            ( VAL-TPO     NOT = SPACE AND '+' )                           
103120                       OR                                                 
103200            ( VAL-KAMPANJ NOT = SPACE AND '+' ) ) )                       
103300*       -------------------------                                         
103310     OR ( (VAL-TPO        NOT = SPACE AND '+' ) AND                       
103320          ( ( VAL-IDKR    NOT = SPACE AND '+' )                           
103330                       OR                                                 
103340            ( VAL-LEVBESK NOT = SPACE AND '+' )                           
103350                       OR                                                 
103360            ( VAL-KAMPANJ NOT = SPACE AND '+' ) ) )                       
103370*       -------------------------                                         
103400        If VAL-LEVBESK Not = SPACE                                        
103500          Move MFS-ALFA-FAELT-FEL To MOD-FLAGGA-ATTR (09)                 
103600        End-If                                                            
103610        If VAL-TPO     Not = SPACE                                        
103620          Move MFS-ALFA-FAELT-FEL To MOD-FLAGGA-ATTR (11)                 
103630        End-If                                                            
103700        If VAL-IDKR    Not = SPACE                                        
103800          Move MFS-ALFA-FAELT-FEL To MOD-FLAGGA-ATTR (46)                 
103900        End-If                                                            
104000        If VAL-KAMPANJ Not = SPACE                                        
104100          Move MFS-ALFA-FAELT-FEL To MOD-FLAGGA-ATTR (47)                 
104200        End-If                                                            
104300                                                                          
104400        Move NEJ TO INDATA-SW                                             
104500        Move ERR-CONFLICT-CHOICE TO MED-IDMFSFEL                          
104600     END-IF                                                               
104700                                                                          
104800*    KONTROLL LAGEROMR + GÅNG KONTROLL MOT ... (6163)                     
104900     .                                                                    
105000     EJECT                                                                
105100 C-UPPDATERA      SECTION.                                                
105200                                                                          
105300     MOVE '2322'   TO MSGSOP-IDTRANS                                      
105400     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
105500     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
105800* MAIL EXCELFIL                                                           
105900     MOVE 'W217S3' TO MSGSOP-IDPROCESS                                    
106000                                                                          
106100     STRING 'IDUSER(' MSG-SIGNON-USERID ') URVAL1('                       
106200            WS-URVAL1 ') URVAL2(' WS-URVAL2 ') LISTA('                    
106300            WS-LISTA ') MAIL(' WS-IDMAIL ')'                              
106500            DELIMITED BY SIZE INTO MSGSOP-TESYMBV                         
106600                                                                          
106700            MOVE MAILSEND       TO MOD-TEMFSINF                           
107000     PERFORM IMS-INSERT-ALTMSG                                            
107100     .                                                                    
107200     EJECT                                                                
107300 D-SAMMA-SIDA      SECTION.                                               
107400                                                                          
107500     MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                  
107600     CALL WMEDKONV USING MED-WMEDAREA                                     
107700     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
107800                                                                          
107900     PERFORM MFS-LAES-IN-IGEN                                             
108000     PERFORM MFS-ROER-EJ-FAELT-IN                                         
108100     .                                                                    
108200     EJECT                                                                
108300 MFS-RENSA-FAELT-IN SECTION.                                              
108400                                                                          
108500*    --- ALLA INDATA-FÄLT                  -IN                            
108600     MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-FOM                             
108700                             MOD-IDPERSON-TOM                             
108710                             MOD-FLAGGA-IDBERED                           
108720                             MOD-FLAGGA-IDLEVNR-SHIP                      
108800                             MOD-KDERS-FOM                                
108900                             MOD-KDERS-TOM                                
108910                             MOD-KDOTFREK                                 
109000                             MOD-IDFKNGRP-FOM                             
109100                             MOD-IDFKNGRP-TOM                             
109200                             MOD-BEART-SOEK                               
109300                             MOD-KVVECKOR-KVPB                            
109400                             MOD-KVVECKOR-AVROP                           
109500                             MOD-KDARBTYP-IN                              
109600                             MOD-IDPERSON-IN                              
109700                             MOD-KDPRODSL-FOM                             
109800                             MOD-KDPRODSL-TOM                             
109900                             MOD-IDPROJ-URV                               
110000                             MOD-ADLAGOMR                                 
110010                             MOD-ADGANG-FOM                               
110020                             MOD-ADGANG-TOM                               
110100                                                                          
110200     MOVE +1 TO IX                                                        
110300     PERFORM UNTIL IX > 5                                                 
110400       MOVE MFS-RENSA-FAELT TO MOD-IDPERSON (IX)                          
110500       MOVE MFS-RENSA-FAELT TO MOD-KDERS  (IX)                            
110600       ADD +1 TO IX                                                       
110700     END-PERFORM                                                          
110800                                                                          
110900     MOVE +1 TO IX                                                        
111000     PERFORM UNTIL IX > 9                                                 
111100       If IX = +1                                                         
111200         If MSGI-KDARBTYP-SEC-IDLEV = SPACE Or LOW-VALUE                  
111300           MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR(IX)                        
111400         Else                                                             
111500*          --- Visa för USER vilket LEVNR som kommer att användas         
111600           Move MSGI-KDARBTYP-SEC-IDLEV To MOD-IDLEVNR(IX)                
111700         End-If                                                           
111800       Else                                                               
111900         MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR(IX)                          
112000       End-If                                                             
112100                                                                          
112200       ADD +1 TO IX                                                       
112300     END-PERFORM                                                          
112400                                                                          
112500     MOVE +1 TO IX                                                        
112600     PERFORM UNTIL IX > 4                                                 
112700       MOVE MFS-RENSA-FAELT TO MOD-IDFKNGRP(IX)                           
112800       MOVE MFS-RENSA-FAELT TO MOD-BEFT    (IX)                           
112900       ADD +1 TO IX                                                       
113000     END-PERFORM                                                          
113100                                                                          
113200     MOVE +1 TO IX                                                        
113300     PERFORM UNTIL IX > MAX-ANTAL-FLAGGOR                                 
113400       MOVE MFS-RENSA-FAELT TO MOD-FLAGGA (IX)                            
113500       ADD +1 TO IX                                                       
113600     END-PERFORM                                                          
113700     .                                                                    
113800     EJECT                                                                
113900 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
114000                                                                          
114100*    --- ALLA INDATA-FÄLT                    -IN                          
114200     MOVE MFS-ROER-EJ-FAELT TO  MOD-IDPERSON-FOM                          
114300                                MOD-IDPERSON-TOM                          
114400                                MOD-FLAGGA-IDBERED                        
114410                                MOD-FLAGGA-IDLEVNR-SHIP                   
114500                                MOD-KDERS-FOM                             
114600                                MOD-KDERS-TOM                             
114610                                MOD-KDOTFREK                              
114700                                MOD-IDFKNGRP-FOM                          
114800                                MOD-IDFKNGRP-TOM                          
114900                                MOD-BEART-SOEK                            
115000                                MOD-KVVECKOR-KVPB                         
115100                                MOD-KVVECKOR-AVROP                        
115200                                MOD-KDARBTYP-IN                           
115300                                MOD-IDPERSON-IN                           
115400                                MOD-KDPRODSL-FOM                          
115500                                MOD-KDPRODSL-TOM                          
115600                                MOD-IDPROJ-URV                            
115610                                MOD-ADLAGOMR                              
115620                                MOD-ADGANG-FOM                            
115630                                MOD-ADGANG-TOM                            
115700                                                                          
115800     MOVE +1 TO IX                                                        
115900     PERFORM UNTIL IX > 5                                                 
116000       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPERSON (IX)                        
116100       MOVE MFS-ROER-EJ-FAELT TO MOD-KDERS  (IX)                          
116200       ADD +1 TO IX                                                       
116300     END-PERFORM                                                          
116400                                                                          
116500     MOVE +1 TO IX                                                        
116600     PERFORM UNTIL IX > 9                                                 
116700       MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR(IX)                          
116800       ADD +1 TO IX                                                       
116900     END-PERFORM                                                          
117000                                                                          
117100     MOVE +1 TO IX                                                        
117200     PERFORM UNTIL IX > 4                                                 
117300       MOVE MFS-ROER-EJ-FAELT TO MOD-IDFKNGRP(IX)                         
117400       MOVE MFS-ROER-EJ-FAELT TO MOD-BEFT    (IX)                         
117500       ADD +1 TO IX                                                       
117600     END-PERFORM                                                          
117700                                                                          
117800     MOVE +1 TO IX                                                        
117900     PERFORM UNTIL IX > MAX-ANTAL-FLAGGOR                                 
118000       MOVE MFS-ROER-EJ-FAELT TO MOD-FLAGGA (IX)                          
118100       ADD +1 TO IX                                                       
118200     END-PERFORM                                                          
118300     .                                                                    
118400     EJECT                                                                
118500 MFS-LAES-IN-IGEN SECTION.                                                
118600                                                                          
118700*    --- ALLA INDATA-FÄLT                                                 
118800     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPERSON-FOM-ATTR                  
118900                                   MOD-IDPERSON-TOM-ATTR                  
119000                                   MOD-KDERS-FOM-ATTR                     
119100                                   MOD-KDERS-TOM-ATTR                     
119110                                   MOD-KDOTFREK-ATTR                      
119200                                   MOD-IDFKNGRP-FOM-ATTR                  
119300                                   MOD-IDFKNGRP-TOM-ATTR                  
119400                                   MOD-BEART-SOEK-ATTR                    
119500                                   MOD-KVVECKOR-KVPB-ATTR                 
119600                                   MOD-KVVECKOR-AVROP-ATTR                
119700                                   MOD-KDARBTYP-IN-ATTR                   
119800                                   MOD-IDPERSON-IN-ATTR                   
119900                                   MOD-KDPRODSL-FOM-ATTR                  
120000                                   MOD-KDPRODSL-TOM-ATTR                  
120100                                   MOD-IDPROJ-URV-ATTR                    
120110                                   MOD-ADLAGOMR                           
120120                                   MOD-ADGANG-FOM                         
120130                                   MOD-ADGANG-TOM                         
120200                                                                          
120300     MOVE +1 TO IX                                                        
120400     PERFORM UNTIL IX > 5                                                 
120500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPERSON-ATTR (IX)               
120600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDERS-ATTR  (IX)                 
120700       ADD +1 TO IX                                                       
120800     END-PERFORM                                                          
120900                                                                          
121000     MOVE +1 TO IX                                                        
121100     PERFORM UNTIL IX > 9                                                 
121200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-ATTR(IX)                 
121300       ADD +1 TO IX                                                       
121400     END-PERFORM                                                          
121500                                                                          
121600     MOVE +1 TO IX                                                        
121700     PERFORM UNTIL IX > 4                                                 
121800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDFKNGRP-ATTR(IX)                
121900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEFT-ATTR (IX)                   
122000       ADD +1 TO IX                                                       
122100     END-PERFORM                                                          
122200                                                                          
122300     MOVE +1 TO IX                                                        
122400     PERFORM UNTIL IX > MAX-ANTAL-FLAGGOR                                 
122500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLAGGA-ATTR (IX)                 
122600       ADD +1 TO IX                                                       
122700     END-PERFORM                                                          
122800     .                                                                    
122900     EJECT                                                                
123000* --- IMS SEKTIONER ---                                                   
123100     SKIP3                                                                
123200 IMS-GET-MSG SECTION.                                                     
123300                                                                          
123400     MOVE '  QC' TO GODK-STATUSKODER                                      
123500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
123600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
123700     PERFORM IMS-STATUSKONTROLL                                           
123800     .                                                                    
123900     SKIP3                                                                
124000 IMS-INSERT-MSG SECTION.                                                  
124100                                                                          
124200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
124300     MOVE SPACE TO GODK-STATUSKODER                                       
124400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
124500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
124600     PERFORM IMS-STATUSKONTROLL                                           
124700     .                                                                    
124800     SKIP3                                                                
124900 IMS-INSERT-ALTMSG SECTION.                                               
125000                                                                          
125100     MOVE SPACE TO GODK-STATUSKODER                                       
125200     CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                      
125300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
125400     PERFORM IMS-STATUSKONTROLL                                           
125500     .                                                                    
125600     EJECT                                                                
125700 IMS-GU-WDP311 SECTION.                                                   
125800     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
125900            DELIMITED BY SIZE INTO SSA1                                   
126000     STRING 'WDP311  (IDPERSON ='  W-IDPERSON-X ')'                       
126100            DELIMITED BY SIZE INTO SSA2                                   
126200     MOVE '  GE' TO GODK-STATUSKODER                                      
126300     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-P3 SSA1 SSA2                   
126400     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
126500     PERFORM IMS-STATUSKONTROLL                                           
126600     .                                                                    
126700     SKIP3                                                                
126800 IMS-STATUSKONTROLL SECTION.                                              
126900                                                                          
127000     SET STATUS-IX TO 1                                                   
127100     SEARCH GODK-STATUS                                                   
127200       AT END                                                             
127300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
127400         DELIMITED BY SIZE INTO FELTEXT                                   
127500         CALL FELLOG                                                      
127600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
127700     END-SEARCH                                                           
127800     .                                                                    
