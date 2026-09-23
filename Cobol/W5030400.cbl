000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W5030400.                                                
000400 AUTHOR.         GUN ANDERSSON.                                           
000500 DATE-WRITTEN.   MAJ 1985.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION.                                                            
000900*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
001000*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0176               
001100*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
001200*        INVENTERINGEN - INRAPPORTERING AV RE1-UPPGIFTER.                 
001300*                                                                         
001400*                                                                         
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W50304                                              
001800*        MID:         W5I30401                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W5O30401                                            
002200     EJECT                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP3                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77   PROGRAM-NAMN           VALUE 'W5030400'                             
003100                                 PIC X(8).                                
003200 77    JA                        PIC X       VALUE 'J'.                   
003300 77    NEJ                       PIC X       VALUE 'N'.                   
003400 77    INV-DC-EJ-KLAR            PIC X       VALUE 'N'.                   
003500 77    IDARTNR-WS                PIC X(9)    VALUE SPACE.                 
003600 77    SEG-ANT                   PIC S9(5)   VALUE +0   COMP-3.           
003700 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
003800 77    KOLLIND                   PIC S9(9)   VALUE +0   COMP SYNC.        
003900 77    DETTA-CL                  PIC S9      VALUE +0   COMP-3.           
004000 77    MAX-MOD-LAENGD            PIC S9(4)  VALUE +397  COMP SYNC.        
004100 77    MAX-GRAENS                PIC S9(9)  VALUE +24   COMP SYNC.        
004200 77    CD-IX                     PIC S9(3)  VALUE ZERO  COMP-3.           
004300                                                                          
004400 77    NYCKLAR-SW                PIC X      VALUE 'J'.                    
004500       88  NYCKLAR-OK                       VALUE 'J'.                    
004600       88  NYCKLAR-FEL                      VALUE 'N'.                    
004700                                                                          
004800     EJECT                                                                
004900                                                                          
005000 01  DYNAMISKA-SUBPROGRAM.                                                
005100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005300                                                                          
005400 01  GENERELLA-SUBPROGRAM.                                                
005500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
005700*01 -COPY WMSGINIT                                                        
005800     EJECT                                                                
005900*    --- VALID IDDC CODES                                                 
006000*01 -COPY WWDC99                                                          
006100     EJECT                                                                
006200 01    FILLER                    PIC X(10) VALUE 'DLINYCKLAR'.            
006300 01    NYCKLAR-TILL-DLI.                                                  
006400   03    W-IDARTNR-X.                                                     
006500     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
006600   03    W-KDSEGKEY-X.                                                    
006700     05    W-KDSEGKEY            PIC X(1)    VALUE '1'.                   
006800   03    W-IDDC-X.                                                        
006900     05    W-IDDC                PIC X(2)    VALUE SPACE.                 
007000   03    W-WDH111KY-X.                                                    
007100     05    W-IDDC-WDH1           PIC X(2)  VALUE SPACE.                   
007200     05    W-KDINVKAT-WDH1       PIC S9(3) COMP-3.                        
007300     05    W-TISEGKEY-WDH1       PIC S9(9) COMP-3.                        
007400     05    W-DAREGDAT-SORT       PIC  9(8).                               
007500                                                                          
007600   03    W-WDH111KY-MIN-X.                                                
007700     05    W-IDDC-WDH1-MIN     PIC X(2)  VALUE SPACE.                     
007800     05    W-KDINVKAT-WDH1-MIN PIC S9(3) VALUE ZERO       COMP-3.         
007900     05    W-TISEGKEY-WDH1-MIN PIC S9(9) VALUE ZERO       COMP-3.         
008000     05    W-DAREGDAT-SORT-MIN    PIC  9(8) VALUE ZERO.                   
008100   03    W-WDH111KY-MAX-X.                                                
008200     05    W-IDDC-WDH1-MAX     PIC X(2)  VALUE SPACE.                     
008300     05    W-KDINVKAT-WDH1-MAX PIC S9(3) VALUE +049       COMP-3.         
008400     05    W-TISEGKEY-WDH1-MAX PIC S9(9) VALUE +999999999 COMP-3.         
008500     05    W-DAREGDAT-SORT-MAX PIC 9(8) VALUE 99999999.                   
008600     EJECT                                                                
008700 01    FILLER                    PIC X(11) VALUE 'MEDDELANDEN'.           
008800 01    MEDDELANDE.                                                        
008900   03    FILLER                  PIC X(40)   VALUE                        
009000             'FYLL I INMATNINGSFÄLT'.                                     
009100   03    FILLER                  PIC X(40)   VALUE                        
009200             'GIVE INPUT'.                                                
009300   03    FILLER                  PIC X(40)   VALUE                        
009400             'TRYCK PF11 FÖR UPPDATERING'.                                
009500   03    FILLER                  PIC X(40)   VALUE                        
009600             'PRESS PF11 FOR UPDATING'.                                   
009700   03    FILLER                  PIC X(40)   VALUE                        
009800             'UPPLYSTA FÄLT FEL'.                                         
009900   03    FILLER                  PIC X(40)   VALUE                        
010000             'HIGHLIGHTED FIELDS WRONG'.                                  
010100   03    FILLER                  PIC X(40)   VALUE                        
010200             'ARTIKELNUMMER SAKNAS'.                                      
010300   03    FILLER                  PIC X(40)   VALUE                        
010400             'PARTNO DOES NOT EXIST'.                                     
010500   03    FILLER                  PIC X(40)   VALUE                        
010600             'ARTIKELN SAKNAR ADRESS'.                                    
010700   03    FILLER                  PIC X(40)   VALUE                        
010800             'PARTNO MISSES ADDRESS'.                                     
010900   03    FILLER                  PIC X(40)   VALUE                        
011000             'ARTIKELN SAKNAR STANDARDPRIS'.                              
011100   03    FILLER                  PIC X(40)   VALUE                        
011200             'PARTNO MISSES STDPRICE'.                                    
011300   03    FILLER                  PIC X(40)   VALUE                        
011400             'INVENTERING FINNS REDAN, UTSKRIVEN'.                        
011500   03    FILLER                  PIC X(40)   VALUE                        
011600             'PARTNO ALREADY IN PROCESS, PRINTED'.                        
011700   03    FILLER                  PIC X(40)   VALUE                        
011800             'DUBBLETT'.                                                  
011900   03    FILLER                  PIC X(40)   VALUE                        
012000             'DUPLICATE'.                                                 
012100   03    FILLER                  PIC X(40)   VALUE                        
012200             'INVENTERING FINNS REDAN, EJ UTSKRIVEN'.                     
012300   03    FILLER                  PIC X(40)   VALUE                        
012400             'PARTNO ALREADY IN PROCESS, NOT PRINTED'.                    
012500   03    FILLER                  PIC X(40)   VALUE                        
012600             'DÅLIGT OBJEKT FÅR EJ UPPDATERAS'.                           
012700   03    FILLER                  PIC X(40)   VALUE                        
012800             'BAD CORE WILL NOT BE UPDATED'.                              
012900                                                                          
013000 01  FEL-MEDDEL  REDEFINES MEDDELANDE.                                    
013100   03    FEL-1   OCCURS 2        PIC X(40).                               
013200   03    FEL-2   OCCURS 2        PIC X(40).                               
013300   03    FEL-3   OCCURS 2        PIC X(40).                               
013400   03    FEL-4   OCCURS 2        PIC X(40).                               
013500   03    FEL-5   OCCURS 2        PIC X(40).                               
013600   03    FEL-6   OCCURS 2        PIC X(40).                               
013700   03    FEL-7   OCCURS 2        PIC X(40).                               
013800   03    FEL-8   OCCURS 2        PIC X(40).                               
013900   03    FEL-9   OCCURS 2        PIC X(40).                               
014000   03    FEL-10  OCCURS 2        PIC X(40).                               
014100                                                                          
014200 01  UPPLYSNING.                                                          
014300   03  FILLER                    PIC X(60)   VALUE                        
014400           'UPPDATERING GJORD'.                                           
014500   03  FILLER                    PIC X(60)   VALUE                        
014600           'UPDATING DONE'.                                               
014700                                                                          
014800 01  UPPLYS  REDEFINES  UPPLYSNING.                                       
014900   03  UPPL-1   OCCURS 2         PIC X(60).                               
015000   EJECT                                                                  
015100 01  FILLER                      PIC X(7)    VALUE 'DIVERSE'.             
015200 01  DIVERSE.                                                             
015300   03  DAGENS-DATUM              PIC S9(6).                               
015400   03  LAGRA-OMR  OCCURS 24      PIC S9(3)   COMP-3.                      
015500   03  LAGRA-GANG OCCURS 24      PIC S9(3)   COMP-3.                      
015600   03  LAGRA-PLATS OCCURS 24     PIC S9(5)   COMP-3.                      
015700   03  LAGRA-VVKL OCCURS 24      PIC S9      COMP-3.                      
015800   03  LAGRA-IDFKNGRP OCCURS 24  PIC S9(5)   COMP-3.                      
015900   03  LAGRA-KDPRODSL OCCURS 24  PIC S9(3)   COMP-3.                      
016000   03  LAGRA-KDPSLLOC OCCURS 24  PIC S9(3)   COMP-3.                      
016100   03  FELTEXT                   PIC X(60)   VALUE SPACE.                 
016200   03  FELFLAGGA                 PIC X       VALUE 'N'.                   
016300   03  ANT-INV                   PIC S9(5)   VALUE +0 COMP-3.             
016400   03  W-ADARTADR                PIC 9(11).                               
016500   03  W-ADRESS REDEFINES W-ADARTADR .                                    
016600     05  W-ADLAGOMR              PIC 9(3).                                
016700     05  W-ADGANG                PIC 9(3).                                
016800     05  W-ADPLATS               PIC 9(5).                                
016900   03  RAPP-KOLL                 PIC X.                                   
017000       88  RAPPORT-FINNS                     VALUE 'J'.                   
017100       88  RAPPORT-SAKNAS                    VALUE 'N'.                   
017200   03  ARTIKEL-FINNS             PIC X       VALUE 'N'.                   
017300   03  STANDARDPRIS-FINNS        PIC X       VALUE 'N'.                   
017400   03  ADRESS-FINNS              PIC X       VALUE 'N'.                   
017500   03  INVENT-FINNS OCCURS 24    PIC X.                                   
017600   03  UPPDATERING               PIC X       VALUE 'N'.                   
017700                                                                          
017800 01  WS-INV-DAREGDAT-AREA.                                                
017900     03  WS-INV-DAREGDAT     PIC 9(9) VALUE ZERO.                         
018000     03  FILLER REDEFINES WS-INV-DAREGDAT.                                
018100       05  WS-INV-NOLL         PIC 9(1).                                  
018200       05  WS-INV-SEKEL        PIC 9(2).                                  
018300       05  WS-INV-AAMMDD       PIC 9(6).                                  
018400                                                                          
018500 01  WS-TISEGKEYAREA.                                                     
018600     03  WS-TIAAAAMMDDL      PIC 9(9) VALUE ZERO.                         
018700     03  FILLER REDEFINES WS-TIAAAAMMDDL.                                 
018800         05  WS-AAR          PIC 9(2).                                    
018900         05  WS-TIAAMMDD     PIC 9(6).                                    
019000         05  WS-LOPNR        PIC 9(1).                                    
019100     03  WS-TISEGKEY         PIC S9(9)  VALUE ZERO COMP-3.                
019200                                                                          
019300     EJECT                                                                
019400 01  FILLER                      PIC X(16)   VALUE 'OBJEKT-TEST'.         
019500     SKIP3                                                                
019600 01  TEST-IDARTNR                PIC 9(9)    COMP-3.                      
019700*01  FILLER -COPY WWBYT09    -RED TEST-IDARTNR                            
019800     EJECT                                                                
019900******************************************************************        
020000*                                                                         
020100*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
020200*                                                                         
020300 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
020400     SKIP3                                                                
020500*01    MID -COPY W5I30401.                                                
020600     EJECT                                                                
020700*01    -COPY WMSGAREA                                                     
020800     EJECT                                                                
020900*  03    MOD -COPY W5O30401  -RED MSG-AREA.                               
021000     EJECT                                                                
021100*01    -COPY WMFSAREA                                                     
021200     EJECT                                                                
021300******************************************************************        
021400*                                                                         
021500*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021600*                                                                         
021700 01    IMS-WS.                                                            
021800   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
021900     SKIP3                                                                
022000*                        **** STATUS-KOD FRÅN IMS                         
022100   03    STATUS-WS               PIC XX.                                  
022200     88    SEGMENT-FINNS                     VALUE '  '.                  
022300     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
022400     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
022500     88    INDEX-FINNS-REDAN                 VALUE 'NI'.                  
022600     SKIP3                                                                
022700   03    GODK-STATUSKODER.                                                
022800     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
022900     SKIP3                                                                
023000 01    SSA1                      PIC X(128).                              
023100 01    SSA2                      PIC X(128).                              
023200     EJECT                                                                
023300*                            IMS FUNKTIONSKODER                           
023400*01    -COPY W0003                                                        
023500     EJECT                                                                
023600*01  -COPY WDH101       -PRE INV-.                                        
023700     EJECT                                                                
023800*01  -COPY WDH111                                                         
023900     EJECT                                                                
024000*01  -COPY WDH121                                                         
024100     EJECT                                                                
024200*                            DLI INPUT-OUTPUT AREA                        
024300 01    DLI-IO-AREA.                                                       
024400   03    IO-AREA                 PIC X(900)  VALUE SPACE.                 
024500     SKIP3                                                                
024600*  03    WLARTC01 -COPY WDK601        -RED IO-AREA.                       
024700     EJECT                                                                
024800*  03    WLARTC11 -COPY WDK611        -RED IO-AREA.                       
024900     EJECT                                                                
025000 01    DLI-IO-AREA2.                                                      
025100   03    IO-AREA2                PIC X(600)  VALUE SPACE.                 
025200     SKIP3                                                                
025300*  03    WLARTS01 -COPY WDK701        -RED IO-AREA2.                      
025400     EJECT                                                                
025500*  03    WLARTS11 -COPY WDK711        -RED IO-AREA2.                      
025600     EJECT                                                                
025700 LINKAGE SECTION.                                                         
025800*01    -COPY W0009     -PRE MSG-                                          
025900     EJECT                                                                
026000*01    -COPY W0008     -PRE USEA-                                         
026100     05  FILLER                  PIC X.                                   
026200     EJECT                                                                
026300*01    -COPY W0008     -PRE INV-                                          
026400     05  FILLER                  PIC X.                                   
026500     EJECT                                                                
026600*01    -COPY W0008     -PRE ARTC-                                         
026700     05  FILLER                  PIC X.                                   
026800     EJECT                                                                
026900*01    -COPY W0008     -PRE ARTS-                                         
027000     05  FILLER                  PIC X.                                   
027100     EJECT                                                                
027200 PROCEDURE DIVISION USING MSG-PCB USEA-PCB INV-PCB                        
027300                          ARTC-PCB ARTS-PCB.                              
027400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB INV-PCB                       
027500                          ARTC-PCB  ARTS-PCB.                             
027600                                                                          
027700     PERFORM IMS-GET-MSG                                                  
027800     IF SEGMENT-FINNS                                                     
027900        PERFORM A-INIT-SPARA-INPUT                                        
028000        IF NYCKLAR-OK                                                     
028100          IF MFS-IDTRANS = '5304'                                         
028200             PERFORM B-KOLLA-BEFRAPP                                      
028300             IF MFS-UPDATE                                                
028400                IF RAPPORT-FINNS                                          
028500                   PERFORM C-KONTROLLERA-INDATA                           
028600                   IF FELTEXT NOT = SPACE                                 
028700                      PERFORM S04-SIGNAL-BILD                             
028800                   END-IF                                                 
028900                ELSE                                                      
029000                   MOVE FEL-1 (DETTA-CL) TO FELTEXT                       
029100                   PERFORM S04-SIGNAL-BILD                                
029200                END-IF                                                    
029300             ELSE                                                         
029400                IF RAPPORT-FINNS                                          
029500                   MOVE FEL-2 (DETTA-CL) TO FELTEXT                       
029600                   PERFORM S05-ADD-FAELT                                  
029700                   PERFORM S04-SIGNAL-BILD                                
029800                END-IF                                                    
029900             END-IF                                                       
030000          END-IF                                                          
030100        END-IF                                                            
030200                                                                          
030300        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
030400        PERFORM IMS-INSERT-MSG                                            
030500     END-IF                                                               
030600     MOVE ZERO TO RETURN-CODE                                             
030700     GOBACK                                                               
030800     .                                                                    
030900     EJECT                                                                
031000 A-INIT-SPARA-INPUT SECTION.                                              
031100                                                                          
031200     IF MSG-DUBBLA-TRANSKODER                                             
031300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I30401                 
031400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
031500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
031600       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
031700     ELSE                                                                 
031800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I30401                  
031900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
032000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
032100       MOVE ' ' TO MFS-KDTRTYP                                            
032200     END-IF                                                               
032300                                                                          
032400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
032500     MOVE '001'             TO MSGI-KDCALL                                
032600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
032700     MOVE '5304'               TO MSGI-IDTRANS                            
032800     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
032900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033000                                                                          
033100                                                                          
033200     MOVE LOW-VALUE TO MSG-AREA                                           
033300     MOVE 'W5O30401' TO MFS-IDMOD                                         
033400     MOVE '5304' TO MOD-IDTRANS                                           
033500                                                                          
033600     MOVE MSGI-IDDC    TO WS-IDDC                                         
033700                                                                          
033800     IF SWEDISH-TEXT                                                      
033900       MOVE +1 TO DETTA-CL                                                
034000     ELSE                                                                 
034100       MOVE +2 TO DETTA-CL                                                
034200     END-IF                                                               
034300                                                                          
034400     MOVE WS-IDDC TO MOD-IDDC-UT                                          
034500     INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                  
034600                                                                          
034700     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
034800                                                                          
034900     PERFORM S01-RENSA-FAELT                                              
035000     ACCEPT  DAGENS-DATUM  FROM DATE                                      
035100                                                                          
035200     .                                                                    
035300     EJECT                                                                
035400 B-KOLLA-BEFRAPP   SECTION.                                               
035500*                                                                         
035600*  KONTROLL OM NÅGON RAPPORTERING HAR GJORTS PÅ BILDEN                    
035700*                                                                         
035800     MOVE +1 TO INDX                                                      
035900     MOVE NEJ TO RAPP-KOLL                                                
036000                                                                          
036100     PERFORM UNTIL INDX > MAX-GRAENS OR RAPPORT-FINNS                     
036200       IF MID-IDARTNR (INDX) NOT = ALL '+'  OR                            
036300        MID-KDINVPRIO (INDX) NOT = ALL '+'  OR                            
036400        MID-TEINVANM (INDX) NOT = ALL '+'                                 
036500         MOVE JA TO RAPP-KOLL                                             
036600       ELSE                                                               
036700         ADD +1 TO INDX                                                   
036800       END-IF                                                             
036900     END-PERFORM                                                          
037000     .                                                                    
037100     EJECT                                                                
037200 C-KONTROLLERA-INDATA  SECTION.                                           
037300*                                                                         
037400*  INDATAKONTROLLER                                                       
037500*  FORMELLA KONTROLLER                                                    
037600*                                                                         
037700     MOVE NEJ TO FELFLAGGA                                                
037800     MOVE +1 TO INDX                                                      
037900                                                                          
038000     PERFORM UNTIL INDX > MAX-GRAENS                                      
038100       IF  (MID-INV-RE1-GRP (INDX) NOT = ALL '+')                         
038200         IF MID-IDARTNR (INDX) = ALL '+'                                  
038300           MOVE JA TO FELFLAGGA                                           
038400           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR (INDX)              
038500           IF FELTEXT = SPACE                                             
038600             MOVE FEL-3 (DETTA-CL) TO FELTEXT                             
038700           END-IF                                                         
038800         ELSE                                                             
038900           IF MID-IDARTNR (INDX) NOT NUMERIC                              
039000             MOVE JA TO FELFLAGGA                                         
039100             MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR (INDX)            
039200             IF FELTEXT = SPACE                                           
039300               MOVE FEL-3 (DETTA-CL) TO FELTEXT                           
039400             END-IF                                                       
039500           ELSE                                                           
039600             MOVE MID-IDARTNR (INDX) TO W-IDARTNR TEST-IDARTNR            
039700                                                                          
039800             IF BYT09-OBJEKT                                              
039900               MOVE JA TO FELFLAGGA                                       
040000               MOVE MFS-NUM-FAELT-FEL TO                                  
040100               MOD-IDARTNR-ATTR (INDX)                                    
040200               IF FELTEXT = SPACE                                         
040300                 MOVE FEL-10 (DETTA-CL) TO FELTEXT                        
040400               END-IF                                                     
040500             ELSE                                                         
040600                IF CDC                                                    
040700                  PERFORM S02-LAES-ARTBAS-WDK6                            
040800                  IF ARTIKEL-FINNS = NEJ                                  
040900                    MOVE JA TO FELFLAGGA                                  
041000                    MOVE MFS-NUM-FAELT-FEL TO                             
041100                    MOD-IDARTNR-ATTR (INDX)                               
041200                    IF FELTEXT = SPACE                                    
041300                      MOVE FEL-3 (DETTA-CL) TO FELTEXT                    
041400                    END-IF                                                
041500                  ELSE                                                    
041600                    IF STANDARDPRIS-FINNS = JA                            
041700                      IF ADRESS-FINNS = JA                                
041800                        MOVE MFS-NUM-FAELT-RAETT TO                       
041900                        MOD-IDARTNR-ATTR (INDX)                           
042000                      ELSE                                                
042100                        MOVE JA TO FELFLAGGA                              
042200                        MOVE MFS-NUM-FAELT-FEL TO                         
042300                        MOD-IDARTNR-ATTR (INDX)                           
042400                        IF FELTEXT = SPACE                                
042500                          MOVE FEL-3 (DETTA-CL) TO FELTEXT                
042600                        END-IF                                            
042700                      END-IF                                              
042800                    ELSE                                                  
042900                      MOVE JA TO FELFLAGGA                                
043000                      MOVE MFS-NUM-FAELT-FEL TO                           
043100                      MOD-IDARTNR-ATTR (INDX)                             
043200                      IF FELTEXT = SPACE                                  
043300                        MOVE FEL-3 (DETTA-CL) TO FELTEXT                  
043400                      END-IF                                              
043500                    END-IF                                                
043600                  END-IF                                                  
043700                ELSE                                                      
043800                  PERFORM S03-LAES-ARTBAS-K6-K7                           
043900                  IF ARTIKEL-FINNS = NEJ                                  
044000                    MOVE JA TO FELFLAGGA                                  
044100                    MOVE MFS-NUM-FAELT-FEL TO                             
044200                    MOD-IDARTNR-ATTR (INDX)                               
044300                    IF FELTEXT = SPACE                                    
044400                      MOVE FEL-3 (DETTA-CL) TO FELTEXT                    
044500                    END-IF                                                
044600                  ELSE                                                    
044700                    IF STANDARDPRIS-FINNS = JA                            
044800                      IF ADRESS-FINNS = JA                                
044900                        MOVE MFS-NUM-FAELT-RAETT TO                       
045000                        MOD-IDARTNR-ATTR (INDX)                           
045100                      ELSE                                                
045200                        MOVE JA TO FELFLAGGA                              
045300                        MOVE MFS-NUM-FAELT-FEL TO                         
045400                        MOD-IDARTNR-ATTR (INDX)                           
045500                        IF FELTEXT = SPACE                                
045600                          MOVE FEL-3 (DETTA-CL) TO FELTEXT                
045700                        END-IF                                            
045800                      END-IF                                              
045900                    ELSE                                                  
046000                      MOVE JA TO FELFLAGGA                                
046100                      MOVE MFS-NUM-FAELT-FEL TO                           
046200                      MOD-IDARTNR-ATTR (INDX)                             
046300                      IF FELTEXT = SPACE                                  
046400                        MOVE FEL-3 (DETTA-CL) TO FELTEXT                  
046500                      END-IF                                              
046600                    END-IF                                                
046700                  END-IF                                                  
046800                END-IF                                                    
046900             END-IF                                                       
047000           END-IF                                                         
047100         END-IF                                                           
047200         IF MID-KDINVPRIO (INDX) NOT = ALL '+'                            
047300           IF MID-KDINVPRIO (INDX) NOT NUMERIC                            
047400             MOVE JA TO FELFLAGGA                                         
047500             MOVE MFS-NUM-FAELT-FEL TO                                    
047600             MOD-KDINVPRIO-ATTR (INDX)                                    
047700             IF FELTEXT = SPACE                                           
047800               MOVE FEL-3 (DETTA-CL) TO FELTEXT                           
047900             END-IF                                                       
048000           ELSE                                                           
048100             IF MID-KDINVPRIO (INDX) = 1 OR 2 OR 5 OR 6                   
048200               MOVE MFS-NUM-FAELT-RAETT TO                                
048300               MOD-KDINVPRIO-ATTR (INDX)                                  
048400             ELSE                                                         
048500               MOVE JA TO FELFLAGGA                                       
048600               MOVE MFS-NUM-FAELT-FEL TO                                  
048700               MOD-KDINVPRIO-ATTR (INDX)                                  
048800               IF FELTEXT = SPACE                                         
048900                 MOVE FEL-3 (DETTA-CL) TO FELTEXT                         
049000               END-IF                                                     
049100             END-IF                                                       
049200           END-IF                                                         
049300         END-IF                                                           
049400         IF MID-TEINVANM  (INDX) NOT = ALL '+'                            
049500           MOVE MFS-ALFA-FAELT-RAETT TO                                   
049600           MOD-TEINVANM-ATTR (INDX)                                       
049700         END-IF                                                           
049800       END-IF                                                             
049900       ADD +1 TO INDX                                                     
050000     END-PERFORM                                                          
050100                                                                          
050200     IF FELFLAGGA = NEJ                                                   
050300       PERFORM CA-KOLLA-DUBLETTER                                         
050400     END-IF                                                               
050500                                                                          
050600     IF FELFLAGGA = NEJ                                                   
050700       PERFORM D-UPPDATERA-BAS                                            
050800     ELSE                                                                 
050900       IF FELTEXT = SPACE                                                 
051000         MOVE FEL-3 (DETTA-CL) TO FELTEXT                                 
051100       END-IF                                                             
051200     END-IF                                                               
051300     .                                                                    
051400     EJECT                                                                
051500 CA-KOLLA-DUBLETTER  SECTION.                                             
051600*                                                                         
051700*  KONTROLL OM SAMMA ARTIKELNUMMER HAR RAPPORTERATS                       
051800*  TVÅ GÅNGER PÅ SAMMA BILD                                               
051900                                                                          
052000     MOVE +1 TO INDX                                                      
052100     MOVE +1 TO KOLLIND                                                   
052200                                                                          
052300     PERFORM UNTIL KOLLIND > MAX-GRAENS                                   
052400       IF MID-IDARTNR (KOLLIND) NOT = ALL '+'                             
052500         MOVE +1 TO INDX                                                  
052600         PERFORM UNTIL INDX > KOLLIND                                     
052700                    OR INDX = KOLLIND                                     
052800                    OR FELFLAGGA = JA                                     
052900           IF MID-IDARTNR (INDX) NOT = ALL '+'                            
053000             IF MID-IDARTNR (INDX) = MID-IDARTNR (KOLLIND)                
053100               MOVE MFS-NUM-FAELT-FEL TO                                  
053200                    MOD-IDARTNR-ATTR (KOLLIND)                            
053300               MOVE JA TO FELFLAGGA                                       
053400               MOVE FEL-8 (DETTA-CL) TO FELTEXT                           
053500             ELSE                                                         
053600               ADD +1 TO INDX                                             
053700             END-IF                                                       
053800           ELSE                                                           
053900             ADD +1 TO INDX                                               
054000           END-IF                                                         
054100         END-PERFORM                                                      
054200       END-IF                                                             
054300       ADD +1 TO KOLLIND                                                  
054400     END-PERFORM                                                          
054500     .                                                                    
054600     EJECT                                                                
054700 D-UPPDATERA-BAS  SECTION.                                                
054800*                                                                         
054900*  UPPDATERING AV WDH1                                                    
055000*  UPPDATERING GÖRS OM INVENTERING INTE REDAN FINNS ELLER                 
055100*  OM DEN NYA RAPPORTEN HAR PRIORITET                                     
055200*                                                                         
055300     MOVE +1 TO INDX                                                      
055400     MOVE WS-IDDC  TO W-IDDC-WDH1                                         
055500                      W-IDDC-WDH1-MIN                                     
055600                      W-IDDC-WDH1-MAX                                     
055700     MOVE JA TO UPPDATERING                                               
055800                                                                          
055900     PERFORM UNTIL INDX > MAX-GRAENS                                      
056000       MOVE NEJ TO INVENT-FINNS (INDX)                                    
056100       IF MID-IDARTNR (INDX) NOT = ALL '+'                                
056200         MOVE MID-IDARTNR (INDX) TO W-IDARTNR                             
056300                                                                          
056400*** BESTÄM OM AKTUELLT DC HAR NÅGON EJ AVSLUTAD INVENTERING               
056500         MOVE NEJ    TO INV-DC-EJ-KLAR                                    
056600         PERFORM IMS-GHU-WDH101                                           
056700         IF SEGMENT-FINNS                                                 
056800           PERFORM IMS-GHNP-WDH111                                        
056900           PERFORM UNTIL SEGMENT-SAKNAS                                   
057000             IF INV-FLINVBEH = 'N'                                        
057100               MOVE JA TO INV-DC-EJ-KLAR                                  
057200             END-IF                                                       
057300             PERFORM IMS-GHNP-WDH111                                      
057400           END-PERFORM                                                    
057500*** KVALIFICERA ROTEN IGEN                                                
057600           PERFORM IMS-GHU-WDH101                                         
057700         END-IF                                                           
057800                                                                          
057900         IF INV-DC-EJ-KLAR = JA                                           
058000           PERFORM IMS-GHNP-WDH111                                        
058100           PERFORM UNTIL SEGMENT-SAKNAS                                   
058200             MOVE JA TO INVENT-FINNS (INDX)                               
058300             IF MID-KDINVPRIO (INDX) = ALL '+'                            
058400               MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR (INDX)          
058500               MOVE NEJ TO UPPDATERING                                    
058600               IF INV-FLINVSKR = 'J'                                      
058700                 MOVE FEL-7 (DETTA-CL) TO FELTEXT                         
058800               ELSE                                                       
058900                 MOVE FEL-9 (DETTA-CL) TO FELTEXT                         
059000               END-IF                                                     
059100             ELSE                                                         
059200               IF MID-KDINVPRIO (INDX) NOT = 5 AND 6                      
059300                 MOVE MFS-NUM-FAELT-FEL TO                                
059400                      MOD-IDARTNR-ATTR (INDX)                             
059500                 MOVE NEJ TO UPPDATERING                                  
059600                 IF INV-FLINVSKR = 'J'                                    
059700                   MOVE FEL-7 (DETTA-CL) TO FELTEXT                       
059800                 ELSE                                                     
059900                   MOVE FEL-9 (DETTA-CL) TO FELTEXT                       
060000                 END-IF                                                   
060100               END-IF                                                     
060200             END-IF                                                       
060300             PERFORM IMS-GHNP-WDH111                                      
060400           END-PERFORM                                                    
060500         ELSE                                                             
060600           IF MID-KDINVPRIO (INDX) NOT = ALL '+'                          
060700             IF MID-KDINVPRIO (INDX) NOT = 1 AND 2                        
060800               MOVE MFS-NUM-FAELT-FEL TO                                  
060900                    MOD-KDINVPRIO-ATTR (INDX)                             
061000               MOVE NEJ TO UPPDATERING                                    
061100               MOVE FEL-3 (DETTA-CL) TO FELTEXT                           
061200             END-IF                                                       
061300           END-IF                                                         
061400         END-IF                                                           
061500       END-IF                                                             
061600       ADD +1 TO INDX                                                     
061700     END-PERFORM                                                          
061800                                                                          
061900     IF UPPDATERING = JA                                                  
062000       MOVE +1 TO INDX                                                    
062100       PERFORM UNTIL INDX > MAX-GRAENS                                    
062200         IF MID-IDARTNR (INDX) NOT = ALL '+'                              
062300           MOVE MID-IDARTNR (INDX) TO W-IDARTNR                           
062400           PERFORM DA-UPPDATERA-WDH1                                      
062500         END-IF                                                           
062600         ADD +1 TO INDX                                                   
062700       END-PERFORM                                                        
062800       PERFORM S06-FORMATETS-ATTR                                         
062900       MOVE UPPL-1 (DETTA-CL) TO MOD-TEMFSINF                             
063000     END-IF                                                               
063100     .                                                                    
063200     EJECT                                                                
063300 DA-UPPDATERA-WDH1  SECTION.                                              
063400*                                                                         
063500*  UPPDATERING AV WDH1                                                    
063600*                                                                         
063700     IF INVENT-FINNS (INDX) = JA                                          
063800       MOVE MID-IDARTNR(INDX)  TO W-IDARTNR                               
063900       PERFORM IMS-GHU-WDH101                                             
064000       IF SEGMENT-FINNS                                                   
064100         PERFORM IMS-GHNP-WDH111                                          
064200         PERFORM UNTIL SEGMENT-SAKNAS                                     
064300          IF INV-FLINVBEH = 'N'                                           
064400           IF MID-KDINVPRIO(INDX) NOT = ALL '+'                           
064500             IF INV-KDINVPRIO = 5                                         
064600               MOVE 1 TO INV-KDINVPRIO                                    
064700             ELSE                                                         
064800               IF INV-KDINVPRIO = 6                                       
064900                 MOVE 2 TO INV-KDINVPRIO                                  
065000               END-IF                                                     
065100             END-IF                                                       
065200           END-IF                                                         
065300           MOVE 'N'     TO INV-FLINVSKR                                   
065400           IF MID-TEINVANM(INDX) NOT = ALL '+'                            
065500             MOVE MID-TEINVANM(INDX) TO INV-TEINVANM                      
065600           END-IF                                                         
065700           PERFORM IMS-REPL-WDH111                                        
065800          END-IF                                                          
065900          PERFORM IMS-GHNP-WDH111                                         
066000         END-PERFORM                                                      
066100       END-IF                                                             
066200     ELSE                                                                 
066300       PERFORM IMS-GHU-WDH101                                             
066400       IF SEGMENT-SAKNAS                                                  
066500         MOVE MID-IDARTNR(INDX)  TO INV-ART-IDARTNR                       
066600         PERFORM IMS-ISRT-WDH1-ROT                                        
066700       END-IF                                                             
066800                                                                          
066900       MOVE WS-IDDC              TO INV-IDDC                              
067000       MOVE +1                   TO INV-KDINVKAT                          
067100       MOVE ZERO                 TO INV-KDINVKAT-OLD                      
067200       MOVE LAGRA-OMR (INDX)     TO INV-ADLAGOMR                          
067300       MOVE LAGRA-GANG (INDX)    TO INV-ADGANG                            
067400       MOVE LAGRA-PLATS (INDX)   TO INV-ADPLATS                           
067500       MOVE NEJ                  TO INV-FLINVBEH                          
067600       MOVE 'N'                  TO INV-FLINVSKR                          
067700       MOVE NEJ                  TO INV-FLINV2B                           
067800       MOVE NEJ                  TO INV-FLINV2C                           
067900       MOVE NEJ                  TO INV-FLINV2D                           
068000       MOVE NEJ                  TO INV-FLINV3E                           
068100       MOVE NEJ                  TO INV-FLINV4N                           
068200       MOVE NEJ                  TO INV-FLINV4P                           
068300       MOVE NEJ                  TO INV-FLINV4R                           
068400       MOVE NEJ                  TO INV-FLINV85                           
068500       MOVE SPACE                TO INV-FILLER1                           
068600                                    INV-FILLER2                           
068700       MOVE LAGRA-IDFKNGRP(INDX) TO INV-IDFKNGRP                          
068800       IF MID-KDINVPRIO(INDX) NOT = ALL '+'                               
068900         MOVE MID-KDINVPRIO(INDX) TO INV-KDINVPRIO                        
069000       ELSE                                                               
069100         MOVE +2                 TO INV-KDINVPRIO                         
069200       END-IF                                                             
069300       MOVE LAGRA-KDPRODSL(INDX) TO INV-KDPRODSL                          
069400       MOVE LAGRA-KDPSLLOC(INDX) TO INV-KDPSLLOC                          
069500       MOVE LAGRA-VVKL (INDX)    TO INV-KDVVKL                            
069600                                                                          
069700       MOVE ZERO                 TO INV-KVJUSTKV                          
069800       IF MID-TEINVANM (INDX) NOT = ALL '+'                               
069900         MOVE MID-TEINVANM (INDX) TO INV-TEINVANM                         
070000       ELSE                                                               
070100         MOVE SPACE              TO INV-TEINVANM                          
070200       END-IF                                                             
070300       MOVE ZERO                 TO INV-IDPRTOMG                          
070400                                    INV-IDLOPNR                           
070500                                    INV-KVAKS-OLD                         
070600                                    INV-KVEFRS-OLD                        
070700                                    INV-KVLS-OLD                          
070800       MOVE 20                   TO WS-AAR                                
070900       MOVE WS-AAR               TO WS-INV-SEKEL                          
071000       MOVE DAGENS-DATUM         TO WS-INV-AAMMDD                         
071100       MOVE WS-INV-DAREGDAT      TO INV-DAREGDAT-CRE                      
071200       MOVE WS-INV-DAREGDAT      TO INV-DAREGDAT                          
071300       MOVE DAGENS-DATUM         TO WS-TIAAMMDD                           
071400*      COMPUTE INV-DAREGDAT-SORT =                                        
071500*        99999999 - WS-INV-DAREGDAT                                       
071600       MOVE 99999999 TO INV-DAREGDAT-SORT                                 
071700       MOVE 0                    TO WS-LOPNR                              
071800       MOVE WS-TIAAAAMMDDL       TO INV-TISEGKEY                          
071900       MOVE ZERO                 TO INV-DAREGDAT-PR1                      
072000                                    INV-DAREGDAT-PR2                      
072100                                    INV-DAREGDAT-PR3                      
072200*      MOVE SPACE                TO INV-IDUSER-PR1                        
072300*                                   INV-IDUSER-PR2                        
072400*                                   INV-IDUSER-PR3                        
072500                                                                          
072600       PERFORM IMS-ISRT-WDH111                                            
072700                                                                          
072800       PERFORM UNTIL SEGMENT-FINNS                                        
072900         IF SEGMENT-FINNS-REDAN OR INDEX-FINNS-REDAN                      
073000           ADD 1 TO INV-TISEGKEY                                          
073100           PERFORM IMS-ISRT-WDH111                                        
073200         END-IF                                                           
073300       END-PERFORM                                                        
073400                                                                          
073500*** INSERT PÅ WDH21 SEGMENTET ***                                         
073600       MOVE MSGI-IDUSER TO INVL-IDUSER                                    
073700       MOVE '0'         TO INVL-KDSEGKEY                                  
073800       PERFORM IMS-INSERT-WDH121                                          
073900       MOVE SPACE       TO INVL-IDUSER                                    
074000       MOVE '1'         TO INVL-KDSEGKEY                                  
074100       PERFORM IMS-INSERT-WDH121                                          
074200       MOVE SPACE       TO INVL-IDUSER                                    
074300       MOVE '2'         TO INVL-KDSEGKEY                                  
074400       PERFORM IMS-INSERT-WDH121                                          
074500       MOVE SPACE       TO INVL-IDUSER                                    
074600       MOVE '3'         TO INVL-KDSEGKEY                                  
074700       PERFORM IMS-INSERT-WDH121                                          
074800                                                                          
074900**** SLUT PÅ INSERT PÅ WDH21 SEGMENT                                      
075000     END-IF                                                               
075100     .                                                                    
075200     EJECT                                                                
075300 S01-RENSA-FAELT  SECTION.                                                
075400*                                                                         
075500*  RENSA RAPPORTERINGSFÄLT                                                
075600*                                                                         
075700     MOVE +1 TO INDX                                                      
075800                                                                          
075900     PERFORM UNTIL INDX > MAX-GRAENS                                      
076000       MOVE MFS-RENSA-FAELT  TO MOD-IDARTNR (INDX)                        
076100                                MOD-KDINVPRIO (INDX)                      
076200                                MOD-TEINVANM  (INDX)                      
076300       ADD +1 TO INDX                                                     
076400     END-PERFORM                                                          
076500                                                                          
076600     MOVE +0 TO INDX                                                      
076700     .                                                                    
076800     EJECT                                                                
076900 S02-LAES-ARTBAS-WDK6  SECTION.                                           
077000*                                                                         
077100*  LÄS WDK6                                                               
077200*                                                                         
077300     MOVE NEJ TO ARTIKEL-FINNS                                            
077400     MOVE NEJ TO ADRESS-FINNS                                             
077500     PERFORM IMS-GET-ARTIKEL-ROT                                          
077600                                                                          
077700     IF SEGMENT-FINNS AND ART-KDERS-UTG = +0                              
077800       MOVE ART-IDFKNGRP TO LAGRA-IDFKNGRP(INDX)                          
077900       MOVE ART-KDPRODSL TO LAGRA-KDPRODSL(INDX)                          
078000       PERFORM IMS-GNP-WDK6ARTIKEL                                        
078100       IF SEGMENT-FINNS                                                   
078200         MOVE CLAG-KDVVKL TO LAGRA-VVKL (INDX)                            
078300         MOVE JA TO ARTIKEL-FINNS                                         
078400         IF CLAG-PRARTSTD = 0                                             
078500           IF FELTEXT = SPACE                                             
078600             MOVE FEL-6 (DETTA-CL) TO FELTEXT                             
078700           END-IF                                                         
078800         ELSE                                                             
078900           MOVE JA TO STANDARDPRIS-FINNS                                  
079000         END-IF                                                           
079100                                                                          
079200         MOVE CLAG-ADLAGOMR TO W-ADLAGOMR                                 
079300         MOVE CLAG-ADGANG   TO W-ADGANG                                   
079400         MOVE CLAG-ADPLATS  TO W-ADPLATS                                  
079500         IF W-ADARTADR > 0                                                
079600           MOVE JA TO ADRESS-FINNS                                        
079700           MOVE CLAG-ADLAGOMR     TO LAGRA-OMR   (INDX)                   
079800           MOVE CLAG-ADGANG       TO LAGRA-GANG  (INDX)                   
079900           MOVE CLAG-ADPLATS      TO LAGRA-PLATS (INDX)                   
080000         ELSE                                                             
080100           MOVE CLAG-ADLAGOMR-SVS TO W-ADLAGOMR                           
080200           MOVE CLAG-ADGANG-SVS   TO W-ADGANG                             
080300           MOVE CLAG-ADPLATS-SVS  TO W-ADPLATS                            
080400           IF W-ADARTADR > 0                                              
080500              MOVE JA TO ADRESS-FINNS                                     
080600              MOVE CLAG-ADLAGOMR-SVS TO LAGRA-OMR   (INDX)                
080700              MOVE CLAG-ADGANG-SVS   TO LAGRA-GANG  (INDX)                
080800              MOVE CLAG-ADPLATS-SVS  TO LAGRA-PLATS (INDX)                
080900           ELSE                                                           
081000              MOVE +1 TO CD-IX                                            
081100              PERFORM UNTIL CD-IX > 4                                     
081200                 IF CLAG-ADLAGOMR-CD(CD-IX) = ZERO                        
081300                   CONTINUE                                               
081400                 ELSE                                                     
081500                   MOVE CLAG-ADLAGOMR-CD(CD-IX)                           
081600                                            TO LAGRA-OMR(INDX)            
081700                                               W-ADLAGOMR                 
081800                   MOVE CLAG-ADGANG-CD(CD-IX)                             
081900                                            TO LAGRA-GANG(INDX)           
082000                                               W-ADGANG                   
082100                   MOVE CLAG-ADPLATS-CD(CD-IX)                            
082200                                            TO LAGRA-PLATS(INDX)          
082300                                               W-ADPLATS                  
082400                   MOVE +4 TO CD-IX                                       
082500                 END-IF                                                   
082600                 ADD +1 TO CD-IX                                          
082700              END-PERFORM                                                 
082800           END-IF                                                         
082900           IF W-ADARTADR > 0                                              
083000              MOVE JA TO ADRESS-FINNS                                     
083100           ELSE                                                           
083200              IF FELTEXT = SPACE                                          
083300                 MOVE FEL-5 (DETTA-CL) TO FELTEXT                         
083400              END-IF                                                      
083500           END-IF                                                         
083600         END-IF                                                           
083700       ELSE                                                               
083800         IF FELTEXT = SPACE                                               
083900           MOVE FEL-4 (DETTA-CL) TO FELTEXT                               
084000         END-IF                                                           
084100       END-IF                                                             
084200     ELSE                                                                 
084300       IF FELTEXT = SPACE                                                 
084400         MOVE FEL-4 (DETTA-CL) TO FELTEXT                                 
084500       END-IF                                                             
084600     END-IF                                                               
084700     .                                                                    
084800     EJECT                                                                
084900 S03-LAES-ARTBAS-K6-K7 SECTION.                                           
085000                                                                          
085100*  LÄS WDK7                                                               
085200*  ÄVEN WDK6 LÄSES HÄR FÖR ATT HÄMTA VISSA VÄRDEN SOM                     
085300*  BARA FINNS DÄR.                                                        
085400                                                                          
085500     MOVE NEJ TO ARTIKEL-FINNS                                            
085600     MOVE NEJ TO ADRESS-FINNS                                             
085700                                                                          
085800     PERFORM IMS-GET-ARTIKEL-ROT                                          
085900     IF SEGMENT-FINNS AND ART-KDERS-UTG = +0                              
086000       MOVE ART-IDFKNGRP TO LAGRA-IDFKNGRP(INDX)                          
086100       MOVE ART-KDPRODSL TO LAGRA-KDPRODSL(INDX)                          
086200       PERFORM IMS-GNP-WDK6ARTIKEL                                        
086300       IF SEGMENT-FINNS                                                   
089300         PERFORM S07-LAES-ARTBAS-K6-K7                                    
089500       ELSE                                                               
089600         IF FELTEXT = SPACE                                               
089700           MOVE FEL-4 (DETTA-CL) TO FELTEXT                               
089800         END-IF                                                           
089900       END-IF                                                             
090000     ELSE                                                                 
090100       IF FELTEXT = SPACE                                                 
090200         MOVE FEL-4 (DETTA-CL) TO FELTEXT                                 
090300       END-IF                                                             
090400     END-IF                                                               
090500     .                                                                    
090600     EJECT                                                                
090700 S04-SIGNAL-BILD  SECTION.                                                
090800*                                                                         
090900*  HÅLL KVAR RAPPORTERINGSFÄLTEN PÅ BILDEN VID FELSIGNAL                  
091000*                                                                         
091100     MOVE +1 TO INDX                                                      
091200                                                                          
091300     PERFORM UNTIL INDX > MAX-GRAENS                                      
091400       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR (INDX)                       
091500       MOVE MFS-ROER-EJ-FAELT TO MOD-KDINVPRIO (INDX)                     
091600       MOVE MFS-ROER-EJ-FAELT TO MOD-TEINVANM (INDX)                      
091700       ADD +1 TO INDX                                                     
091800     END-PERFORM                                                          
091900                                                                          
092000     MOVE FELTEXT TO MOD-TEMFSFEL                                         
092100     .                                                                    
092200     EJECT                                                                
092300 S05-ADD-FAELT    SECTION.                                                
092400*                                                                         
092500*  LÄS IN RAPPORTERINGSFÄLTEN IGEN                                        
092600*                                                                         
092700     MOVE +1 TO INDX                                                      
092800                                                                          
092900     PERFORM UNTIL INDX > MAX-GRAENS                                      
093000       IF MID-IDARTNR (INDX) NOT = ALL '+'                                
093100         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-ATTR (INDX)            
093200       END-IF                                                             
093300       IF MID-KDINVPRIO (INDX) NOT = ALL '+'                              
093400         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDINVPRIO-ATTR (INDX)          
093500       END-IF                                                             
093600       IF MID-TEINVANM (INDX) NOT = ALL '+'                               
093700         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEINVANM-ATTR (INDX)           
093800       END-IF                                                             
093900       ADD +1 TO INDX                                                     
094000     END-PERFORM                                                          
094100                                                                          
094200     MOVE +0 TO INDX                                                      
094300     .                                                                    
094400     EJECT                                                                
094500 S06-FORMATETS-ATTR  SECTION.                                             
094600*                                                                         
094700*  RENSA BILDEN                                                           
094800*                                                                         
094900     MOVE +1 TO INDX                                                      
095000                                                                          
095100     PERFORM UNTIL INDX > MAX-GRAENS                                      
095200       MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-ATTR (INDX)                 
095300       MOVE MFS-FORMATETS-ATTR TO MOD-KDINVPRIO-ATTR (INDX)               
095400       MOVE MFS-FORMATETS-ATTR TO MOD-TEINVANM-ATTR (INDX)                
095500       ADD +1 TO INDX                                                     
095600     END-PERFORM                                                          
095700                                                                          
095800     MOVE +0 TO INDX                                                      
095900     .                                                                    
096000     EJECT                                                                
096100 S07-LAES-ARTBAS-K6-K7 SECTION.                                           
096200         MOVE CLAG-KDVVKL      TO LAGRA-VVKL (INDX)                       
096300         MOVE CLAG-KDPSLLOC    TO LAGRA-KDPSLLOC(INDX)                    
096400         IF CLAG-PRARTSTD = 0                                             
096500           IF FELTEXT = SPACE                                             
096600             MOVE FEL-6 (DETTA-CL) TO FELTEXT                             
096700           END-IF                                                         
096800         ELSE                                                             
096900           MOVE JA TO STANDARDPRIS-FINNS                                  
097000         END-IF                                                           
097100                                                                          
097200         MOVE WS-IDDC TO W-IDDC                                           
097300         PERFORM IMS-GET-WDK7ARTIKEL                                      
097400         IF SEGMENT-FINNS                                                 
097500           MOVE JA TO ARTIKEL-FINNS                                       
097600           MOVE SLAG-ADLAGOMR TO W-ADLAGOMR                               
097700           MOVE SLAG-ADGANG     TO W-ADGANG                               
097800           MOVE SLAG-ADPLATS    TO W-ADPLATS                              
097900           IF W-ADARTADR > 0                                              
098000             MOVE JA TO ADRESS-FINNS                                      
098100             MOVE SLAG-ADLAGOMR   TO LAGRA-OMR   (INDX)                   
098200             MOVE SLAG-ADGANG     TO LAGRA-GANG  (INDX)                   
098300             MOVE SLAG-ADPLATS    TO LAGRA-PLATS (INDX)                   
098400           ELSE                                                           
098500             IF FELTEXT = SPACE                                           
098600               MOVE FEL-5 (DETTA-CL) TO FELTEXT                           
098700             END-IF                                                       
098800           END-IF                                                         
098900         ELSE                                                             
099000           IF FELTEXT = SPACE                                             
099100             MOVE FEL-4 (DETTA-CL) TO FELTEXT                             
099200           END-IF                                                         
099300         END-IF                                                           
099400     .                                                                    
099500     EJECT                                                                
099600* IMS SEKTIONER                                                           
099700     SKIP3                                                                
099800 IMS-GET-MSG SECTION.                                                     
099900     MOVE '  QC' TO GODK-STATUSKODER                                      
100000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
100100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
100200     PERFORM IMS-STATUSKONTROLL                                           
100300     SKIP3                                                                
100400     .                                                                    
100500 IMS-INSERT-MSG SECTION.                                                  
100600     IF ENGLISH-TEXT                                                      
100700       MOVE 'N' TO MFS-KDHUVOMR                                           
100800     END-IF                                                               
100900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
101000     MOVE SPACE TO GODK-STATUSKODER                                       
101100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
101200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
101300     PERFORM IMS-STATUSKONTROLL                                           
101400     .                                                                    
101500     EJECT                                                                
101600***   WDK6   ***                                                          
101700     SKIP3                                                                
101800 IMS-GET-ARTIKEL-ROT     SECTION.                                         
101900     SKIP2                                                                
102000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
102100            DELIMITED BY SIZE INTO SSA1                                   
102200     MOVE '  GE' TO GODK-STATUSKODER                                      
102300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
102400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
102500     PERFORM IMS-STATUSKONTROLL                                           
102600     .                                                                    
102700     SKIP3                                                                
102800 IMS-GNP-WDK6ARTIKEL SECTION.                                             
102900     SKIP2                                                                
103000     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
103100            DELIMITED BY SIZE INTO SSA1                                   
103200     MOVE '  GE' TO GODK-STATUSKODER                                      
103300     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
103400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
103500     PERFORM IMS-STATUSKONTROLL                                           
103600     .                                                                    
103700     EJECT                                                                
103800***   WDK7   ***                                                          
103900     SKIP3                                                                
104000 IMS-GET-WDK7ARTIKEL     SECTION.                                         
104100     SKIP2                                                                
104200     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
104300            DELIMITED BY SIZE INTO SSA1                                   
104400     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
104500            DELIMITED BY SIZE INTO SSA2                                   
104600     MOVE '  GE' TO GODK-STATUSKODER                                      
104700     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA2 SSA1 SSA2                
104800     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
104900     PERFORM IMS-STATUSKONTROLL                                           
105000     .                                                                    
105100     EJECT                                                                
105200***  WDH1-BASEN  ***                                                      
105300     SKIP2                                                                
105400 IMS-GHU-WDH101 SECTION.                                                  
105500     SKIP2                                                                
105600     STRING                                                               
105700     'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                                
105800     DELIMITED BY SIZE INTO SSA1                                          
105900     MOVE '  GE' TO GODK-STATUSKODER                                      
106000     CALL CBLTDLI USING GHU INV-PCB INV-ART-WDH101 SSA1                   
106100     MOVE INV-STATUS-CODE TO STATUS-WS                                    
106200     PERFORM IMS-STATUSKONTROLL                                           
106300     .                                                                    
106400     SKIP3                                                                
106500 IMS-GHNP-WDH111   SECTION.                                               
106600     STRING                                                               
106700     'WDH111  (WDH111KY>=' W-WDH111KY-MIN-X                               
106800             '&WDH111KY<=' W-WDH111KY-MAX-X ')'                           
106900     DELIMITED BY SIZE INTO SSA1                                          
107000     MOVE '  GE' TO GODK-STATUSKODER                                      
107100     CALL CBLTDLI USING GHNP INV-PCB INV-WDH111 SSA1                      
107200     MOVE INV-STATUS-CODE TO STATUS-WS                                    
107300     PERFORM IMS-STATUSKONTROLL                                           
107400     .                                                                    
107500     EJECT                                                                
107600 IMS-ISRT-WDH1-ROT  SECTION.                                              
107700     SKIP2                                                                
107800     MOVE 'WDH101 ' TO SSA1                                               
107900     MOVE '    ' TO GODK-STATUSKODER                                      
108000     CALL CBLTDLI USING ISRT INV-PCB INV-ART-WDH101 SSA1                  
108100     MOVE INV-STATUS-CODE TO STATUS-WS                                    
108200     PERFORM IMS-STATUSKONTROLL                                           
108300     .                                                                    
108400     SKIP3                                                                
108500 IMS-ISRT-WDH111 SECTION.                                                 
108600     SKIP2                                                                
108700     MOVE 'WDH111 ' TO SSA1                                               
108800     MOVE '  IINI' TO GODK-STATUSKODER                                    
108900     CALL CBLTDLI USING ISRT INV-PCB INV-WDH111 SSA1                      
109000     MOVE INV-STATUS-CODE TO STATUS-WS                                    
109100     PERFORM IMS-STATUSKONTROLL                                           
109200     .                                                                    
109300     EJECT                                                                
109400 IMS-INSERT-WDH121      SECTION.                                          
109500                                                                          
109600     MOVE 'WDH121 ' TO SSA1                                               
109700     MOVE '  II' TO GODK-STATUSKODER                                      
109800     CALL CBLTDLI USING ISRT INV-PCB INVL-WDH121   SSA1                   
109900     MOVE INV-STATUS-CODE TO STATUS-WS                                    
110000     PERFORM IMS-STATUSKONTROLL                                           
110100     .                                                                    
110200     EJECT                                                                
110300 IMS-REPL-WDH111       SECTION.                                           
110400     SKIP2                                                                
110500     MOVE '    ' TO GODK-STATUSKODER                                      
110600     CALL CBLTDLI USING REPL INV-PCB INV-WDH111                           
110700     MOVE INV-STATUS-CODE TO STATUS-WS                                    
110800     PERFORM IMS-STATUSKONTROLL                                           
110900     .                                                                    
111000     EJECT                                                                
111100 IMS-STATUSKONTROLL SECTION.                                              
111200     SKIP2                                                                
111300     SET STATUS-IX TO 1                                                   
111400     SEARCH GODK-STATUS                                                   
111500       AT END                                                             
111600         CALL FELLOG                                                      
111700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
111800     END-SEARCH                                                           
111900     .                                                                    
