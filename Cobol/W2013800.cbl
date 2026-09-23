000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2013800.                                                
000410 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000500 DATE-WRITTEN.   SEP 2001.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERING AV SLAG-FLCDCBEH (WDK7)                              
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
001200*                              WLUSEA (WDP7)                              
001300*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001400*                                                                         
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W2T138                                              
001800*        MID:         W2I13801                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W2O13801                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002703                                                                          
002710*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W2013800'.            
002900                                                                          
003000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003410 77  IX                          PIC 9(3)    VALUE ZERO.                  
003420 77  SPRAK-IX                    PIC 9(3)    VALUE ZERO.                  
003440 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003450 01  WS.                                                                  
003494  03 WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
003550                                                                          
003600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003700                                                                          
004100                                                                          
004200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004300     88  INDATA-OK                           VALUE 'J'.                   
004400     88  INDATA-FEL                          VALUE 'N'.                   
004500                                                                          
004600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004700     88  NYCKLAR-OK                          VALUE 'J'.                   
004800     88  NYCKLAR-FEL                         VALUE 'N'.                   
004810                                                                          
005000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005100     88  EGEN-MID                            VALUE '2138'.                
005200     88  GODK-MID                            VALUE '2138'.                
005700     88  HELP-MID                            VALUE '0551'.                
005800     EJECT                                                                
005900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006000 01  GENERELLA-SUBPROGRAM.                                                
006100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006410     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006500     EJECT                                                                
006600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006700*01 -COPY WMEDAREA                                                        
006810     EJECT                                                                
006900 01  MESSAGE-CODES.                                                       
007410     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007411     03  CONFLICT                PIC X(3)    VALUE '002'.                 
007420     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007430     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007440     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
007450     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
007460     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007470     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
007480     03  PRIS-SAKNAS             PIC X(3)    VALUE '301'.                 
007490     03  ARTIKEL-SAKNAS-SDC      PIC X(3)    VALUE '305'.                 
007491     03  DIREKTLEV               PIC X(3)    VALUE '306'.                 
007492     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
007493     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007494                                                                          
007507     EJECT                                                                
007508*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
007509*01  -COPY WDATAREA                                                       
007510     EJECT                                                                
007600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007700*                                                                         
007800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007900     SKIP3                                                                
008000*01 -COPY WMSGINIT                                                        
008010     EJECT                                                                
008200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008500     SKIP3                                                                
008600*01  MID -COPY W2I13801                                                   
008700     EJECT                                                                
008800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
008900     SKIP3                                                                
009000*01  -COPY WMSGAREA                                                       
009100     EJECT                                                                
009200     03  MOD REDEFINES MSG-AREA.                                          
009300*      05  -COPY W2O13801                                                 
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009600     SKIP3                                                                
009700*01  -COPY WMFSAREA                                                       
009800     EJECT                                                                
009900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010000*                                                                         
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010300     SKIP3                                                                
010600 01  NYCKLAR-TILL-DLI.                                                    
010700     03  W-IDARTNR-X.                                                     
010800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010900     03  W-IDDC-X.                                                        
011000         05  W-IDDC              PIC X(02)    VALUE SPACE.                
011100     03  W-IDUSER-X.                                                      
011200         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
011210     03  W-IDSKYLT-X.                                                     
011220         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
011240     03 W-IDLEVNR-X.                                                      
011250         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
011260     03 W-KDSEGKEY-X.                                                     
011270         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
011280     03  W-IDDC-B6-X.                                                     
011290         05 W-IDDC-B6                  PIC X(2).                          
011293                                                                          
011300     SKIP2                                                                
011400*    --- STATUS-KOD FRÅN IMS                                              
011500 01  STATUS-WS                   PIC XX.                                  
011600     88  SEGMENT-FINNS                       VALUE '  '.                  
011700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011800     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
011810                                                   'GB'.                  
011900     SKIP2                                                                
012000 01  GODK-STATUSKODER.                                                    
012100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012200     SKIP3                                                                
012300 01  SSA1                        PIC X(64).                               
012400 01  SSA2                        PIC X(64).                               
012500     EJECT                                                                
012600*    --- IMS FUNKTIONSKODER                                               
012700*01  -COPY W0003                                                          
012800     EJECT                                                                
012900*    ---  DLI INPUT-OUTPUT AREA                                           
012910 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012920     SKIP3                                                                
012930                                                                          
012940 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
012950 01  DLI-IO-WDK711.                                                       
012960*    03  -COPY WDK711                                                     
012970     EJECT                                                                
012971                                                                          
012972 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDD311'.         
012973 01  DLI-IO-WDD311.                                                       
012974*    03  -COPY WDD311                                                     
012975                                                                          
012976 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDD301'.         
012977 01  DLI-IO-WDD301.                                                       
012978*    03  -COPY WDD301                                                     
012980                                                                          
012990 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
013000 01   DLI-IO-AREA-B601.                                                   
013100*     03  -COPY WDB601                                                    
013200                                                                          
015910     EJECT                                                                
016010 LINKAGE SECTION.                                                         
016100                                                                          
016200*01  -COPY W0009   -PRE MSG-                                              
016300     EJECT                                                                
016400*01  -COPY W0008  -PRE  WDP7-                                             
016500     05  FILLER                  PIC X.                                   
016870     EJECT                                                                
016880*01  -COPY W0008  -PRE  WDD3-                                             
016890     05  FILLER                  PIC X.                                   
016900     EJECT                                                                
017000*01  -COPY W0008  -PRE  WDK7-                                             
017100     05  FILLER                  PIC X.                                   
017500     EJECT                                                                
017600*01  -COPY W0008  -PRE  WDB6-                                             
017700     05  FILLER                  PIC X.                                   
017701     EJECT                                                                
017710 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDD3-PCB WDK7-PCB             
017720                           WDB6-PCB.                                      
017800 MAIN SECTION.                                                            
017900     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDD3-PCB WDK7-PCB             
018000                           WDB6-PCB.                                      
018100                                                                          
018200     PERFORM IMS-GET-MSG                                                  
018300     IF SEGMENT-FINNS                                                     
018400       PERFORM A-INIT                                                     
018500       PERFORM B-KOLLA-NYCKLAR                                            
018600       IF NYCKLAR-OK                                                      
018700         IF MFS-UPDATE                                                    
018800           PERFORM G-KOLLA-INPUT                                          
018900           IF INDATA-OK                                                   
019000             PERFORM H-UPPDATERA                                          
019100           END-IF                                                         
019200         ELSE                                                             
019300           IF MFS-FIRST                                                   
019400             PERFORM C-FOERSTA-SIDA                                       
019500           ELSE                                                           
019600             PERFORM E-SAMMA-SIDA                                         
019700           END-IF                                                         
019800         END-IF                                                           
019900         IF INDATA-OK                                                     
020000           PERFORM F-LAES-VISA-INFO                                       
020100         END-IF                                                           
020200       END-IF                                                             
020210       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O13801 + 4                      
020400       PERFORM IMS-INSERT-MSG                                             
020500     END-IF                                                               
020600                                                                          
020700     MOVE ZERO TO RETURN-CODE                                             
020800     GOBACK                                                               
020900     .                                                                    
021000     EJECT                                                                
021100 A-INIT SECTION.                                                          
021200                                                                          
021300     IF MSG-DUBBLA-TRANSKODER                                             
021400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I13801                 
021500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
021600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
021700     ELSE                                                                 
021800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I13801                  
021900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
022000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
022100     END-IF                                                               
022200                                                                          
022300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
022400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
022500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
022600                                                                          
022700     MOVE LOW-VALUE TO MSG-AREA                                           
022800     MOVE 'W2O138N1' TO MFS-IDMOD                                         
022900     MOVE '2138' TO MOD-IDTRANS                                           
023000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
023100                                                                          
023200     IF EGEN-MID OR HELP-MID                                              
023300       CONTINUE                                                           
023400     ELSE                                                                 
023500       MOVE SPACE TO MFS-KDTRTYP                                          
023600       MOVE '7' TO MFS-IDPFK                                              
023700     END-IF                                                               
023790                                                                          
023791     ACCEPT DAGENS-DATUM FROM DATE                                        
023800     .                                                                    
023900     EJECT                                                                
024000 B-KOLLA-NYCKLAR SECTION.                                                 
024100                                                                          
024200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
024300     MOVE '001'             TO MSGI-KDCALL                                
024400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
024500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024600     MOVE '2138'            TO MSGI-IDTRANS                               
024700     IF EGEN-MID                                                          
024800       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
025010       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
025011     ELSE                                                                 
025020       IF  MID-IDARTNR-IN NUMERIC                                         
025030       AND MID-IDARTNR-IN > ZERO                                          
025040         MOVE MID-IDARTNR-IN                                              
025050                            TO MSGI-IDARTNR                               
025110       END-IF                                                             
025120     END-IF                                                               
025200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
025201                                                                          
025202*    -- KONTROLL AV SPRÅKKOD                                              
025203     IF MSGI-IDLAND-SPR = 'SE'                                            
025204       MOVE +1    TO SPRAK-IX                                             
025205       MOVE 'S  ' TO MED-IDSKYLT                                          
025206     ELSE                                                                 
025207       MOVE +2    TO SPRAK-IX                                             
025208       MOVE 'GB ' TO MED-IDSKYLT                                          
025209     END-IF                                                               
025210                                                                          
025400     MOVE JA TO NYCKLAR-SW                                                
025600                                                                          
025700*    -- KONTROLL AV IDARTNR                                               
025800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
025900                                                                          
026400     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
026500     IF MSGI-IDARTNR NUMERIC                                              
026600       MOVE MSGI-IDARTNR     TO WS-IDARTNR                                
026700     ELSE                                                                 
026800       MOVE NEJ              TO NYCKLAR-SW                                
026900     END-IF                                                               
027000                                                                          
027100*    -- KONTROLL AV IDDC                                                  
027200     MOVE MFS-RENSA-FAELT    TO MOD-IDDC-IN                               
027300                                                                          
027400     MOVE MSGI-IDDC-KEY    TO W-IDDC-B6                                   
027500     PERFORM IMS-GU-WDB601                                                
028208                                                                          
028209*    IF GOOD-DC                                                           
028210     IF DCS-KDDC = SPACE OR DCS-DDC                                       
028220        MOVE NEJ TO NYCKLAR-SW                                            
028260     END-IF                                                               
029300                                                                          
029400     MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                              
029500                              W-IDARTNR                                   
029600     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
029700     MOVE MSGI-IDDC-KEY    TO MOD-IDDC-UT                                 
029800                              W-IDDC                                      
029900                                                                          
030000     IF NYCKLAR-FEL                                                       
030100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
030200       CALL WMEDKONV USING MED-WMEDAREA                                   
030300       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
030400       PERFORM MFS-RENSA-FAELT-IN                                         
030500       PERFORM MFS-RENSA-FAELT-UT                                         
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 C-FOERSTA-SIDA SECTION.                                                  
031000                                                                          
031100     PERFORM MFS-RENSA-FAELT-IN                                           
031200     .                                                                    
031300     EJECT                                                                
031400 E-SAMMA-SIDA SECTION.                                                    
031500                                                                          
031600     IF MID-INPUT = ALL '+'                                               
031700       PERFORM MFS-RENSA-FAELT-IN                                         
031800     ELSE                                                                 
031900       IF EGEN-MID OR HELP-MID                                            
032000         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
032100         CALL WMEDKONV USING MED-WMEDAREA                                 
032200         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
032300         PERFORM MFS-LAES-IN-IGEN                                         
032400                                                                          
032500         PERFORM EA-MID-INDATA-TILL-MOD                                   
032600       ELSE                                                               
032700         PERFORM MFS-RENSA-FAELT-IN                                       
032800       END-IF                                                             
032900     END-IF                                                               
033000     .                                                                    
033100 EA-MID-INDATA-TILL-MOD SECTION.                                          
033200* * * * * FÖR VARJE MID-FÄLT                                              
033300* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
033400* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
033500                                                                          
034200     IF MID-FLCDCBEH = ALL '+'                                            
034300       MOVE MFS-RENSA-FAELT TO MOD-FLCDCBEH-IN                            
034400     ELSE                                                                 
034500       MOVE MID-FLCDCBEH    TO MOD-FLCDCBEH-IN                            
034600     END-IF                                                               
039700     .                                                                    
039800     EJECT                                                                
039900 F-LAES-VISA-INFO SECTION.                                                
040000                                                                          
040400     PERFORM FA-HAEMTA-BENAEMNING                                         
041400     PERFORM FB-VISA-SDC-LAGERINFO                                        
043000     .                                                                    
048000     EJECT                                                                
048100 FA-HAEMTA-BENAEMNING SECTION.                                            
048200                                                                          
048300     PERFORM IMS-GU-D301                                                  
048400     IF SEGMENT-FINNS                                                     
048500       MOVE MED-IDSKYLT TO W-IDSKYLT                                      
048600       PERFORM IMS-GNP-D311                                               
048700       IF SEGMENT-FINNS                                                   
048800         MOVE TEXT-BEART        TO MOD-BEART                              
048900       ELSE                                                               
049000         MOVE MFS-RENSA-FAELT   TO MOD-BEART                              
049100       END-IF                                                             
049200     END-IF                                                               
049300     .                                                                    
049400     EJECT                                                                
049500 FB-VISA-SDC-LAGERINFO SECTION.                                           
049600                                                                          
049700     PERFORM IMS-GU-K711                                                  
049800     IF SEGMENT-FINNS                                                     
050000        MOVE SLAG-FLCDCBEH   TO MOD-FLCDCBEH                              
065000                                                                          
065100     ELSE                                                                 
065200        MOVE ARTIKEL-SAKNAS-SDC                                           
065210                             TO MED-IDMFSFEL                              
065300        CALL WMEDKONV USING MED-WMEDAREA                                  
065400        MOVE MED-MFSFEL      TO MOD-TEMFSFEL                              
065410        MOVE MFS-RENSA-FAELT TO MOD-FLCDCBEH                              
065600     END-IF                                                               
065700     .                                                                    
068140     EJECT                                                                
068150 G-KOLLA-INPUT SECTION.                                                   
068160                                                                          
068170     MOVE JA  TO INDATA-SW                                                
068190                                                                          
068198     PERFORM IMS-GHU-K711                                                 
068199     IF SEGMENT-SAKNAS                                                    
068204         MOVE NEJ TO INDATA-SW                                            
068206     END-IF                                                               
068207                                                                          
068208     IF INDATA-OK                                                         
068209       IF MID-INPUT = ALL '+' AND SEGMENT-FINNS                           
068210         MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                        
068211         CALL WMEDKONV USING MED-WMEDAREA                                 
068212         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
068213         PERFORM MFS-ROER-EJ-FAELT-IN                                     
068214         PERFORM MFS-ROER-EJ-FAELT-UT                                     
068215         MOVE NEJ TO INDATA-SW                                            
068216       ELSE                                                               
068231         PERFORM GA-KOLLA-DATAELEMENT                                     
068233         IF INDATA-FEL                                                    
068234           PERFORM MFS-ROER-EJ-FAELT-IN                                   
068235           PERFORM MFS-ROER-EJ-FAELT-UT                                   
068236         END-IF                                                           
068237                                                                          
068238       END-IF                                                             
068239     END-IF                                                               
068243     .                                                                    
068244     EJECT                                                                
068405                                                                          
068406 GA-KOLLA-DATAELEMENT SECTION.                                            
068407                                                                          
068425*------KOLLA FLCDCBEH                                                     
068426                                                                          
068427     IF MID-FLCDCBEH = ALL '+'                                            
068428       MOVE MFS-RENSA-FAELT TO MOD-FLCDCBEH-IN                            
068429     ELSE                                                                 
068430       IF MID-FLCDCBEH = JA OR NEJ                                        
068432         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLCDCBEH-IN-ATTR                
068433       ELSE                                                               
068434         MOVE NEJ TO INDATA-SW                                            
068435         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
068436         MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLCDCBEH-IN-ATTR              
068437       END-IF                                                             
068438     END-IF                                                               
068639                                                                          
068640     IF INDATA-FEL                                                        
068641       CALL WMEDKONV USING MED-WMEDAREA                                   
068642       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
068643     END-IF                                                               
068644     .                                                                    
068645     EJECT                                                                
068646 H-UPPDATERA SECTION.                                                     
068647                                                                          
068648     MOVE MID-FLCDCBEH       TO SLAG-FLCDCBEH                             
068649     PERFORM IMS-REPL-K711                                                
068678                                                                          
068679     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
068680     CALL WMEDKONV USING MED-WMEDAREA                                     
068681     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
068682     PERFORM MFS-FORM-ATTR                                                
068683     PERFORM MFS-RENSA-FAELT-IN                                           
068684     .                                                                    
068685     EJECT                                                                
069007 MFS-RENSA-FAELT-UT SECTION.                                              
069008                                                                          
069009*    --- ALLA UTDATA-FÄLT                                                 
069010     MOVE MFS-RENSA-FAELT TO MOD-BEART                                    
069020                             MOD-FLCDCBEH                                 
069029     .                                                                    
069030     SKIP3                                                                
069031 MFS-RENSA-FAELT-IN SECTION.                                              
069032                                                                          
069040*    --- ALLA INDATA-FÄLT                                                 
069100     MOVE MFS-RENSA-FAELT    TO MOD-FLCDCBEH-IN                           
069300     .                                                                    
069400     EJECT                                                                
069500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
069600                                                                          
069700*    --- ALLA UTDATA-FÄLT                                                 
069900                                                                          
069910     MOVE MFS-ROER-EJ-FAELT  TO MOD-BEART                                 
069911                                MOD-FLCDCBEH                              
070000     .                                                                    
070100     SKIP3                                                                
070210 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
070300                                                                          
070400*    --- ALLA INDATA-FÄLT                                                 
070610     MOVE MFS-ROER-EJ-FAELT  TO MOD-FLCDCBEH-IN                           
070690     .                                                                    
070691     EJECT                                                                
070900 MFS-FORM-ATTR SECTION.                                                   
071000                                                                          
071100*    --- ALLA INDATA-FÄLT                                                 
071200     MOVE MFS-FORMATETS-ATTR TO MOD-FLCDCBEH-IN-ATTR                      
071400     .                                                                    
071500     SKIP2                                                                
071600 MFS-LAES-IN-IGEN SECTION.                                                
071700                                                                          
071800*    --- ALLA INDATA-FÄLT                                                 
071900     MOVE MFS-ADD-LAES-IN-FAELT                                           
072000                             TO MOD-FLCDCBEH-IN-ATTR                      
072100     .                                                                    
072200     EJECT                                                                
072300* --- IMS SEKTIONER ---                                                   
072400     SKIP3                                                                
072500 IMS-GET-MSG SECTION.                                                     
072600                                                                          
072700     MOVE '  QC' TO GODK-STATUSKODER                                      
072800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
072900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
073000     PERFORM IMS-STATUSKONTROLL                                           
073100     .                                                                    
073200     SKIP3                                                                
073300 IMS-INSERT-MSG SECTION.                                                  
073400                                                                          
073420     IF MSGI-IDLAND-SPR = 'SE'                                            
073430        MOVE '0' TO MFS-KDHUVOMR                                          
073440     END-IF                                                               
073800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
073900     MOVE SPACE TO GODK-STATUSKODER                                       
074000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
074100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
074200     PERFORM IMS-STATUSKONTROLL                                           
074300     .                                                                    
074437     EJECT                                                                
075500 IMS-GU-K711 SECTION.                                                     
075600                                                                          
075700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
075800          DELIMITED BY SIZE INTO SSA1                                     
075900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
076000          DELIMITED BY SIZE INTO SSA2                                     
076100     MOVE '  GE' TO GODK-STATUSKODER                                      
076200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
076300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
076400     PERFORM IMS-STATUSKONTROLL                                           
076500     .                                                                    
076600     SKIP3                                                                
076610 IMS-GHU-K711 SECTION.                                                    
076620                                                                          
076630     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
076640          DELIMITED BY SIZE INTO SSA1                                     
076650     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
076660          DELIMITED BY SIZE INTO SSA2                                     
076670     MOVE '  GE' TO GODK-STATUSKODER                                      
076680     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
076690     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
076691     PERFORM IMS-STATUSKONTROLL                                           
076692     .                                                                    
079742     EJECT                                                                
079744 IMS-REPL-K711 SECTION.                                                   
079745                                                                          
079746     MOVE '  ' TO GODK-STATUSKODER                                        
079747     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
079748     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
079749     PERFORM IMS-STATUSKONTROLL                                           
079750     .                                                                    
079751     EJECT                                                                
079752 IMS-GU-D301 SECTION.                                                     
079753                                                                          
079754     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
079755          DELIMITED BY SIZE INTO SSA1                                     
079756     MOVE '  GE' TO GODK-STATUSKODER                                      
079758     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD301 SSA1                    
079759     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
079760     PERFORM IMS-STATUSKONTROLL                                           
079761     .                                                                    
079762     EJECT                                                                
079763 IMS-GNP-D311 SECTION.                                                    
079764                                                                          
079765     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
079766          DELIMITED BY SIZE INTO SSA1                                     
079767     MOVE '  GE' TO GODK-STATUSKODER                                      
079769     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD311 SSA1                   
079770     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
079771     PERFORM IMS-STATUSKONTROLL                                           
079780     .                                                                    
081600     EJECT                                                                
081700 IMS-GU-WDB601    SECTION.                                                
081701     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
081702          DELIMITED BY SIZE INTO SSA1                                     
081703     MOVE '  GE' TO GODK-STATUSKODER                                      
081704     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
081705     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
081706     PERFORM IMS-STATUSKONTROLL                                           
081707     IF SEGMENT-SAKNAS                                                    
081708         MOVE SPACE TO DCS-KDDC                                           
081709     END-IF                                                               
081710     .                                                                    
081720 IMS-STATUSKONTROLL SECTION.                                              
081800                                                                          
081900     SET STATUS-IX TO 1                                                   
082000     SEARCH GODK-STATUS                                                   
082100       AT END                                                             
082200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
082300         DELIMITED BY SIZE INTO FELTEXT                                   
082400         CALL FELLOG                                                      
082500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
082600         CONTINUE                                                         
082700     END-SEARCH                                                           
082800     .                                                                    
082900     EJECT                                                                
