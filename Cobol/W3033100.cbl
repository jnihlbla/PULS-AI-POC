000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W3033100.                                                
000400 AUTHOR.         ELEONOR ÖSTRÖM.                                          
000500 DATE-WRITTEN.   07/05/21.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SUBMITT-PGM                                                      
001000*                                                                         
001100*        DETTA PGM BESTÄLLER EN LISTA. VISSA URVAL KAN ANGES.             
001200*                                                                         
001300*        SORTERING KAN VÄLJAS.                                            
001400*        VARIABLERNA SOM SKALL UT KAN VÄLJAS.                             
001500*        SKRIVARE KAN VÄLJAS.                                             
001600*        LISTNING I FORM AV EXCEL                                         
001700*        SOM ATTACHMENT  TILL OUTLOOK MAIL KAN VÄLJAS.                    
001800*        LIST-PGMET STARTAS VIA SOP MED OVANSTÅENDE SOM PARAMETRAR        
001900*        (LISTANS VÄRDEN HÄMTAS SEN FRÅN LAGERBAND, SAMT DIVERSE          
002000*         DLI-call)                                                       
002100*                                                                         
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W3T331                                              
002700*        MID:         W3I33101                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W3O33101                                            
003100*                                                                         
003200*    ETRACKER 4054855                                                     
003300*                                                                         
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(08)   VALUE 'W3033100'.            
004300                                                                          
004400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004600                                                                          
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900                                                                          
005000 77  IX                          PIC S9(9)  VALUE +0    COMP SYNC.        
005100                                                                          
005200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005300                                                                          
005400                                                                          
005500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005600     88  INDATA-OK                           VALUE 'J'.                   
005700     88  INDATA-FEL                          VALUE 'N'.                   
005800                                                                          
005900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006000     88  ALLT-OK                             VALUE 'J'.                   
006100                                                                          
006200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006300     88  EGEN-MID                            VALUE '3331'.                
006400     88  GODK-MID                            VALUE '3331'.                
006500     88  HELP-MID                            VALUE '0551'.                
006900     SKIP2                                                                
007000 01  ARBETS-AREOR.                                                        
007100     03  WS-LIST-TYP             PIC S9(1)   COMP-3 VALUE ZERO.           
007200     03  WS-LIST-LANGD           PIC S9(3)   COMP-3 VALUE ZERO.           
007300                                                                          
007400     EJECT                                                                
007500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007600 01  GENERELLA-SUBPROGRAM.                                                
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008400*01 -COPY WMEDAREA                                                        
008500     SKIP3                                                                
008600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008700     SKIP3                                                                
008800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT (USERBASEN)                  
008900*01 -COPY WMSGINIT                                                        
009000     EJECT                                                                
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
010710     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010800     SKIP2                                                                
010900 01  FILLER.                                                              
011000     03  MAILSEND.                                                        
011100         05  FILLER              PIC X(13)   VALUE                        
011200                                 'MAIL BESTÄLLD'.                         
011300 01  FILLER.                                                              
011400     03  LISTUPPSTART.                                                    
011500         05  FILLER              PIC X(30)   VALUE                        
011600                                 'LISTA UPPSTARTAD, PRINTER. '.           
011700         05  TEXT-PRINTER        PIC X(25)   VALUE SPACE.                 
011800     EJECT                                                                
011801 01  FELMEDDELANDE.                                                       
011803     03  FEL-1                   PIC X(40)                                
011804         VALUE 'ENTER AND NO INPUT  '.                                    
011805                                                                          
011806     03  FEL-2                   PIC X(40)                                
011807         VALUE 'AT LEAST ONE PRICEAREA MUST BE GIVEN'.                    
011808                                                                          
011810 01  INFO-MEDDELANDE.                                                     
011830     03  INFO-1                  PIC X(24)                                
011840         VALUE 'PRESS ENTER TO UPDATE   '.                                
011850     EJECT                                                                
011870     03  INFO-2                  PIC X(24)                                
011880         VALUE 'ENTER AND NO DATA       '.                                
011890     EJECT                                                                
011900 01  PROG-TO-PROG-SW.                                                     
012000*    03  -COPY WMSGSOP                                                    
012100     EJECT                                                                
012200 01  WS-PARAMETRAR.                                                       
012300     03  WS-IDUSER.                                                       
012310       05 FILLER.                                                         
012400         07  IDUSER              PIC X(8) VALUE SPACES.                   
012500     03  WS-URVAL1.                                                       
012510       05 FILLER1.                                                        
012600         07  IDPROMR-GRP OCCURS 3 TIMES.                                  
012700             09  IDMARKBO        PIC X    VALUE SPACE.                    
012800             09  IDPROMRN        PIC X(2) VALUE SPACES.                   
012900         07  IDFKNGRP-GRP OCCURS 4 TIMES.                                 
013000             09  IDFKNGRP-FOM    PIC 9(4) VALUE ZEROES.                   
013100             09  IDFKNGRP-TOM    PIC 9(4) VALUE ZEROES.                   
013200         07  IDDISTR-GRP OCCURS 4 TIMES.                                  
013300             09  IDDISTR-FOM     PIC 9(4) VALUE ZEROES.                   
013400             09  IDDISTR-TOM     PIC 9(4) VALUE ZEROES.                   
013500                                                                          
013600     03  WS-URVAL2.                                                       
013700         05  KDPRODSL-GRP OCCURS 4 TIMES.                                 
013800             07  KDPRODSL-FOM    PIC 9(2) VALUE ZEROES.                   
013900             07  KDPRODSL-TOM    PIC 9(2) VALUE ZEROES.                   
014000         05  KDERS-GRP OCCURS 4 TIMES.                                    
014100             07  KDERS-FOM       PIC 9(2) VALUE ZEROES.                   
014200             07  KDERS-TOM       PIC 9(2) VALUE ZEROES.                   
014300         05  FLEXCEL             PIC X(1) VALUE SPACE.                    
014400         05  FLMAIL              PIC X(1) VALUE SPACE.                    
014600                                                                          
014700     03  WS-FLEXCEL.                                                      
014710         05 FLEXC                PIC X(1) VALUE SPACE.                    
014720                                                                          
014800     03  WS-LISTA.                                                        
014900         05 VAL-IDARTNR          PIC X(1) VALUE SPACE.                    
015000         05 VAL-IDPROMR          PIC X(1) VALUE SPACE.                    
015100         05 VAL-IDFKNGRP         PIC X(1) VALUE SPACE.                    
015200         05 VAL-IDDISTR          PIC X(1) VALUE SPACE.                    
015300         05 VAL-IDKUNDNR         PIC X(1) VALUE SPACE.                    
015400         05 VAL-KDPRODSL         PIC X(1) VALUE SPACE.                    
015500         05 VAL-KDERS            PIC X(1) VALUE SPACE.                    
015600         05 VAL-BEART-GB         PIC X(1) VALUE SPACE.                    
015700         05 VAL-MO-PRIS          PIC X(1) VALUE SPACE.                    
015800         05 VAL-DO-PRIS          PIC X(1) VALUE SPACE.                    
015900         05 VAL-PRARTBTO-MARK    PIC X(1) VALUE SPACE.                    
016000                                                                          
016100     03  WS-FLAGGA  REDEFINES WS-LISTA.                                   
016200         05 VAL-FLAGGA OCCURS 11 PIC X(1).                                
016300                                                                          
016600*    här börjar den nya längdberäkningen                                  
016700                                                                          
016800     03  WS-LANGD.                                                        
016900         05 LNG-IDARTNR          PIC 9(3) VALUE 9.                        
017000         05 LNG-IDPROMR          PIC 9(3) VALUE 4.                        
017100         05 LNG-IDFKNGRP         PIC 9(3) VALUE 4.                        
017200         05 LNG-IDDISTR          PIC 9(3) VALUE 4.                        
017300         05 LNG-IDKUNDNR         PIC 9(3) VALUE 6.                        
017400         05 LNG-KDPRODSL         PIC 9(3) VALUE 4.                        
017500         05 LNG-KDERS            PIC 9(3) VALUE 2.                        
017600         05 LNG-MO-NOR-PRIS      PIC 9(3) VALUE 11.                       
017700         05 LNG-DO-NOR-PRIS      PIC 9(3) VALUE 11.                       
017800         05 LNG-PRARTBTO-MARK    PIC 9(3) VALUE 11.                       
017900         05 LNG-BEART-GB         PIC 9(3) VALUE 20.                       
018000*                                                                         
018100     03  WS-LLANGD  REDEFINES WS-LANGD.                                   
018200         05  FALT-LANGD OCCURS 11 PIC 9(3).                               
018300                                                                          
018800     03  WS-SORTERING.                                                    
018900         05  KDSORT1             PIC 9(1) VALUE ZERO.                     
019000                                                                          
019600     EJECT                                                                
019900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
020000*                                                                         
020100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
020200     SKIP3                                                                
020300*01  MID -COPY W3I33101                                                   
020400     EJECT                                                                
020500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
020600     SKIP3                                                                
020700*01  -COPY WMSGAREA                                                       
020800     EJECT                                                                
020900     03  MOD REDEFINES MSG-AREA.                                          
021000*      05  -COPY W3O33101                                                 
021100     EJECT                                                                
021200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021300     SKIP3                                                                
021400*01  -COPY WMFSAREA                                                       
021500     EJECT                                                                
021600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021700     SKIP3                                                                
021800*    --- STATUS-KOD FRÅN IMS                                              
021900 01  STATUS-WS                   PIC XX.                                  
022000     88  SEGMENT-FINNS                       VALUE '  '.                  
022100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022300     SKIP2                                                                
022400 01  GODK-STATUSKODER.                                                    
022500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022600     EJECT                                                                
023300 01  SSA1                        PIC X(64).                               
023400 01  SSA2                        PIC X(64).                               
023500*    --- IMS FUNKTIONSKODER                                               
023600*01  -COPY W0003                                                          
023700     EJECT                                                                
023800 LINKAGE SECTION.                                                         
023900*01  -COPY W0009   -PRE MSG-                                              
024000                                                                          
024100*01  -COPY W0009   -PRE ALT-                                              
024200                                                                          
024600*01  -COPY W0008   -PRE USEA-                                             
024700     05  FILLER                  PIC X.                                   
024800     EJECT                                                                
024900 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB.                      
025000 MAIN SECTION.                                                            
025100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB.                      
025200                                                                          
025300     PERFORM IMS-GET-MSG                                                  
025400     IF SEGMENT-FINNS                                                     
025500       PERFORM A-INIT                                                     
025700       PERFORM B-KOLLA-INPUT                                              
025800       IF INDATA-OK                                                       
025900         PERFORM C-UPPDATERA                                              
026000         PERFORM MFS-RENSA-FAELT-IN                                       
026100         MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                             
026300       ELSE                                                               
026500         IF MID-URVAL = ALL '+'                                           
026700           PERFORM MFS-RENSA-FAELT-IN                                     
026800         ELSE                                                             
026900           PERFORM D-SAMMA-SIDA                                           
027000         END-IF                                                           
027100       END-IF                                                             
027200                                                                          
027300       ADD LENGTH OF MOD-W3O33101 +4 GIVING MSG-KVLL                      
027400       PERFORM IMS-INSERT-MSG                                             
027500     END-IF                                                               
027600                                                                          
027700     MOVE ZERO TO RETURN-CODE                                             
027800     GOBACK                                                               
027900     .                                                                    
028000     EJECT                                                                
028100 A-INIT SECTION.                                                          
028200                                                                          
028300     IF MSG-DUBBLA-TRANSKODER                                             
028400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I33101                 
028500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
028600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
028700     ELSE                                                                 
028800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I33101                  
028900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
029000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
029100     END-IF                                                               
029200                                                                          
029300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
029400     MOVE MSG-IDPFK TO MFS-IDPFK                                          
029500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
029600                                                                          
029700*    --- LÄS USERBASEN                                                    
029800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
029900     MOVE '001'             TO MSGI-KDCALL                                
030000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
030100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
030200     MOVE '3331'            TO MSGI-IDTRANS                               
030300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
030400                                                                          
030500     MOVE LOW-VALUE TO MSG-AREA                                           
030600     MOVE 'W3O33101' TO MFS-IDMOD                                         
030700     MOVE '3331' TO MOD-IDTRANS                                           
030800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
030900                                                                          
031000     IF NOT EGEN-MID                                                      
031100       MOVE SPACE TO MFS-KDTRTYP                                          
031200       MOVE '7' TO MFS-IDPFK                                              
031300     END-IF                                                               
031400     .                                                                    
031500     EJECT                                                                
031600 B-KOLLA-INPUT   SECTION.                                                 
031700                                                                          
031800     INITIALIZE WS-URVAL1 WS-URVAL2 WS-FLEXCEL                            
031900     MOVE JA TO INDATA-SW                                                 
032000                                                                          
032010*KOLLA HÄR FÖRSTA GÅNGEN MAN GÅR IN PÅ BILDEN DÅ SKA EJ                   
032020*MEDD OM INGET INDATA....                                                 
032030     IF EGEN-MID                                                          
032060       IF MID-URVAL = ALL '+'                                             
032610          MOVE FEL-1 TO MOD-TEMFSINF                                      
033000          PERFORM MFS-RENSA-FAELT-IN                                      
033500          MOVE NEJ TO INDATA-SW                                           
033600          MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                     
033700       ELSE                                                               
033710         IF MID-IDMARKBO(1) = ALL '+' AND                                 
033720           MID-IDMARKBO(2) = ALL '+' AND                                  
033730           MID-IDMARKBO(3) = ALL '+'                                      
033732           PERFORM MFS-LAES-IN-IGEN                                       
033733           PERFORM MFS-ROER-EJ-FAELT-IN                                   
033734           MOVE NEJ TO INDATA-SW                                          
033735           MOVE FEL-2   TO MOD-TEMFSFEL                                   
033740         ELSE                                                             
033800          PERFORM BA-KONTROLL-URVAL                                       
033900          PERFORM BB-KONTROLL-LISTBREDD                                   
034000          PERFORM BC-KONTROLL-OVR                                         
034100                                                                          
034200          IF INDATA-OK                                                    
034300             PERFORM BD-KONTROLL-FOM-TOM                                  
034400          END-IF                                                          
034800                                                                          
034900          IF INDATA-FEL                                                   
035000            IF MOD-TEMFSFEL NOT > SPACE                                   
035100               CALL WMEDKONV USING MED-WMEDAREA                           
035200               MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                          
035300            END-IF                                                        
035400            PERFORM MFS-ROER-EJ-FAELT-IN                                  
035500          END-IF                                                          
035510         END-IF                                                           
035600       END-IF                                                             
035610     ELSE                                                                 
035611       MOVE NEJ TO INDATA-SW                                              
035612       MOVE INFO-1 TO MOD-TEMFSINF                                        
035630     END-IF                                                               
035700     .                                                                    
035800     EJECT                                                                
035900 BA-KONTROLL-URVAL SECTION.                                               
036000                                                                          
036010     MOVE +1 TO IX                                                        
036020     PERFORM UNTIL IX > 3                                                 
036030       IF MID-IDMARKBO(IX) NOT = ALL '+'                                  
036031         IF MID-IDMARKBO(IX) > SPACE                                      
036050           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROMR-ATTR(IX)              
036060           MOVE MID-IDMARKBO(IX) TO IDMARKBO(IX)                          
036070         ELSE                                                             
036071           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROMR-ATTR(IX)              
036072           MOVE NEJ TO INDATA-SW                                          
036073           MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                             
036074         END-IF                                                           
036090       END-IF                                                             
036091       ADD +1 TO IX                                                       
036092     END-PERFORM                                                          
036093                                                                          
036094     MOVE +1 TO IX                                                        
036095     PERFORM UNTIL IX > 3                                                 
036096       IF MID-IDPROMRN(IX) NOT = ALL '+'                                  
036097         IF MID-IDPROMRN(IX) NOT NUMERIC                                  
036098           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROMR-ATTR(IX)              
036099           MOVE NEJ TO INDATA-SW                                          
036100           MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                      
036101         ELSE                                                             
036102           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROMR-ATTR(IX)              
036103           MOVE MID-IDPROMRN(IX) TO IDPROMRN(IX)                          
036105         END-IF                                                           
036106       END-IF                                                             
036107       ADD +1 TO IX                                                       
036108     END-PERFORM                                                          
036109                                                                          
036110     MOVE +1 TO IX                                                        
036200     PERFORM UNTIL IX > 4                                                 
036300       IF MID-IDFKNGRP-FOM(IX) NOT = ALL '+'                              
036400          IF MID-IDFKNGRP-FOM(IX) NOT NUMERIC                             
036500             MOVE MFS-NUM-FAELT-FEL  TO MOD-IDFKNGRP-FOM-ATTR(IX)         
036600             MOVE NEJ TO INDATA-SW                                        
036710             MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                    
036800          ELSE                                                            
036900             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-FOM-ATTR(IX)        
037000             MOVE MID-IDFKNGRP-FOM (IX)    TO IDFKNGRP-FOM (IX)           
037100          END-IF                                                          
037200       END-IF                                                             
037300                                                                          
037400       IF MID-IDFKNGRP-TOM(IX) NOT = ALL '+'                              
037500          IF MID-IDFKNGRP-TOM(IX) NOT NUMERIC                             
037600             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-TOM-ATTR(IX)        
037700             MOVE NEJ TO INDATA-SW                                        
037800             MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                    
037900          ELSE                                                            
038000             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-TOM-ATTR(IX)        
038100             MOVE MID-IDFKNGRP-TOM (IX)    TO IDFKNGRP-TOM (IX)           
038200          END-IF                                                          
038300       END-IF                                                             
038400                                                                          
038500       IF MID-IDDISTR-FOM(IX) NOT = ALL '+'                               
038600          IF MID-IDDISTR-FOM(IX) NOT NUMERIC                              
038700             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-FOM-ATTR(IX)         
038800             MOVE NEJ TO INDATA-SW                                        
038900             MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                    
039000          ELSE                                                            
039100             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-FOM-ATTR(IX)         
039200             MOVE MID-IDDISTR-FOM (IX)    TO IDDISTR-FOM (IX)             
039300          END-IF                                                          
039400       END-IF                                                             
039500                                                                          
039600       IF MID-IDDISTR-TOM(IX) NOT = ALL '+'                               
039700          IF MID-IDDISTR-TOM(IX) NOT NUMERIC                              
039800             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-TOM-ATTR(IX)         
039900             MOVE NEJ TO INDATA-SW                                        
040000             MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                    
040100          ELSE                                                            
040200             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-TOM-ATTR(IX)         
040300             MOVE MID-IDDISTR-TOM (IX)    TO IDDISTR-TOM (IX)             
040400          END-IF                                                          
040500       END-IF                                                             
040600                                                                          
040700       IF MID-KDPRODSL-FOM(IX) NOT = ALL '+'                              
040800          IF MID-KDPRODSL-FOM(IX) NOT NUMERIC                             
040900             MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPRODSL-FOM-ATTR(IX)        
041000             MOVE NEJ TO INDATA-SW                                        
041100             MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                    
041200          ELSE                                                            
041300             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-FOM-ATTR(IX)        
041400             MOVE MID-KDPRODSL-FOM (IX)    TO KDPRODSL-FOM (IX)           
041500          END-IF                                                          
041600       END-IF                                                             
041700                                                                          
041800       IF MID-KDPRODSL-TOM(IX) NOT = ALL '+'                              
041900          IF MID-KDPRODSL-TOM(IX) NOT NUMERIC                             
042000             MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPRODSL-TOM-ATTR(IX)        
042100             MOVE NEJ TO INDATA-SW                                        
042200             MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                    
042300          ELSE                                                            
042400             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-TOM-ATTR(IX)        
042500             MOVE MID-KDPRODSL-TOM (IX)    TO KDPRODSL-TOM (IX)           
042600          END-IF                                                          
042700       END-IF                                                             
042800                                                                          
042900       IF MID-KDERS-FOM(IX) NOT = ALL '+'                                 
043000          IF MID-KDERS-FOM(IX) NOT NUMERIC                                
043100             MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-FOM-ATTR(IX)           
043200             MOVE NEJ TO INDATA-SW                                        
043300             MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                    
043400          ELSE                                                            
043500            IF MID-KDERS-FOM(IX) > 52                                     
043600              MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-FOM-ATTR(IX)          
043700              MOVE NEJ TO INDATA-SW                                       
043800              MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                 
043900            ELSE                                                          
044000              MOVE MFS-NUM-FAELT-RAETT TO MOD-KDERS-FOM-ATTR(IX)          
044100              MOVE MID-KDERS-FOM (IX)    TO KDERS-FOM (IX)                
044200            END-IF                                                        
044300          END-IF                                                          
044400       ELSE                                                               
044500*            -- VÄRDET 99 STÅR FÖR OIFYLLT VÄRDE                          
044600         MOVE 99 TO KDERS-FOM (IX)                                        
044700       END-IF                                                             
044800                                                                          
044900       IF MID-KDERS-TOM(IX) NOT = ALL '+'                                 
045000          IF MID-KDERS-TOM(IX) NOT NUMERIC                                
045100             MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-TOM-ATTR(IX)           
045200             MOVE NEJ TO INDATA-SW                                        
045300             MOVE ERR-DATA-NOT-NUMERIC TO MED-IDMFSFEL                    
045400          ELSE                                                            
045500            IF MID-KDERS-TOM(IX) > 52                                     
045600              MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-TOM-ATTR(IX)          
045700              MOVE NEJ TO INDATA-SW                                       
045800              MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                 
045900            ELSE                                                          
046000              MOVE MFS-NUM-FAELT-RAETT TO MOD-KDERS-TOM-ATTR(IX)          
046100              MOVE MID-KDERS-TOM (IX)    TO KDERS-TOM (IX)                
046200            END-IF                                                        
046300          END-IF                                                          
046400       ELSE                                                               
046500*            -- VÄRDET 99 STÅR FÖR OIFYLLT VÄRDE                          
046600         MOVE 99 TO KDERS-TOM (IX)                                        
046700       END-IF                                                             
046800                                                                          
046900       ADD +1 TO IX                                                       
047000     END-PERFORM                                                          
047100                                                                          
047200                                                                          
048500     .                                                                    
048600     EJECT                                                                
048700 BB-KONTROLL-LISTBREDD SECTION.                                           
048800                                                                          
048900*    ALLTID SAMMA ANTAL FÄLT SKA MED I EXCELARKET                         
049000     MOVE +1 TO IX                                                        
049100     PERFORM UNTIL IX > 11                                                
049200       MOVE 'S'                  TO VAL-FLAGGA (IX)                       
049300       ADD FALT-LANGD(IX)        TO WS-LIST-LANGD                         
049400       ADD 1                     TO WS-LIST-LANGD                         
049500       ADD +1 TO IX                                                       
049600     END-PERFORM                                                          
049700                                                                          
049800     .                                                                    
049900     EJECT                                                                
050000 BC-KONTROLL-OVR   SECTION.                                               
050100                                                                          
050200***  VAL AV SORTERING 1 (DEFAULT) TILL 3         ***                      
050300                                                                          
050400     IF MID-KDSORT1 NOT = '+' AND SPACE                                   
050500       IF MID-KDSORT1 NOT = '1' AND                                       
050600          MID-KDSORT1 NOT = '2' AND                                       
050700          MID-KDSORT1 NOT = '3'                                           
050800             MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDSORT1-ATTR                
050900             MOVE NEJ TO INDATA-SW                                        
051000             MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                  
051100       ELSE                                                               
051200             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT1-ATTR                
051300             MOVE MID-KDSORT1          TO KDSORT1                         
051400       END-IF                                                             
051500     ELSE                                                                 
051700       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT1-ATTR                      
051800       MOVE 1                    TO KDSORT1                               
051900     END-IF                                                               
052000                                                                          
052010     MOVE JA TO FLEXCEL                                                   
052020                                                                          
052030     IF MID-FLAGGA-EXCE NOT = '+'                                         
052100       IF MID-FLAGGA-EXCE NOT = 'R' AND                                   
052200          MID-FLAGGA-EXCE NOT = 'E'                                       
053000          MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLAGGA-EXCE-ATTR               
053100          MOVE NEJ TO INDATA-SW                                           
053200          MOVE ERR-CORR-HIGH-LIT-FLDS TO MED-IDMFSFEL                     
053400          MOVE MID-FLAGGA-EXCE TO MOD-TEMFSFEL                            
054900       ELSE                                                               
057000         IF MID-FLAGGA-EXCE = 'R'                                         
057200           MOVE NEJ                  TO FLEXCEL                           
058200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAGGA-EXCE-ATTR              
058300           MOVE MID-FLAGGA-EXCE      TO FLEXC                             
059200         ELSE                                                             
059300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAGGA-EXCE-ATTR              
059400           MOVE MID-FLAGGA-EXCE      TO FLEXC                             
059800         END-IF                                                           
059900       END-IF                                                             
059901     ELSE                                                                 
059902       MOVE 'E'                  TO MID-FLAGGA-EXCE                       
059903                                    FLEXC                                 
059904       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAGGA-EXCE-ATTR                  
059910     END-IF                                                               
060000     .                                                                    
060100     EJECT                                                                
061500 BD-KONTROLL-FOM-TOM SECTION.                                             
061600     SKIP2                                                                
061700                                                                          
061800     MOVE 1 TO IX                                                         
061900     PERFORM UNTIL IX > 4                                                 
062000                                                                          
062100       IF  IDFKNGRP-FOM (IX) > ZERO                                       
062200       AND IDFKNGRP-TOM (IX) = ZEROES                                     
062300          MOVE IDFKNGRP-FOM (IX) TO IDFKNGRP-TOM (IX)                     
062400       END-IF                                                             
062500                                                                          
062600       IF IDFKNGRP-FOM (IX) > IDFKNGRP-TOM (IX)                           
062700          MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-FOM-ATTR (IX)          
062800          MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFKNGRP-TOM-ATTR (IX)          
062900          MOVE NEJ TO INDATA-SW                                           
063000          MOVE ERR-START-GREATER   TO MED-IDMFSFEL                        
063100       END-IF                                                             
063200                                                                          
063300       IF  IDDISTR-FOM (IX) > ZERO                                        
063400       AND IDDISTR-TOM (IX) = ZEROES                                      
063500          MOVE IDDISTR-FOM (IX) TO IDDISTR-TOM (IX)                       
063600       END-IF                                                             
063700                                                                          
063800       IF IDDISTR-FOM (IX) > IDDISTR-TOM (IX)                             
063900          MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-FOM-ATTR (IX)           
064000          MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-TOM-ATTR (IX)           
064100          MOVE NEJ TO INDATA-SW                                           
064200          MOVE ERR-START-GREATER   TO MED-IDMFSFEL                        
064300       END-IF                                                             
064400                                                                          
065700       IF  KDPRODSL-FOM (IX) > ZERO                                       
065800       AND KDPRODSL-TOM (IX) = ZEROES                                     
065900          MOVE KDPRODSL-FOM (IX) TO KDPRODSL-TOM (IX)                     
066000       ELSE                                                               
066100         IF  KDPRODSL-FOM (IX) = ZEROES                                   
066200         AND KDPRODSL-TOM (IX) > ZERO                                     
066300            MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-FOM-ATTR (IX)          
066400            MOVE NEJ TO INDATA-SW                                         
066500            MOVE ERR-WRONG-INTERVAL TO MED-IDMFSFEL                       
066600         END-IF                                                           
066700       END-IF                                                             
066800                                                                          
066900       IF KDPRODSL-FOM (IX) > KDPRODSL-TOM (IX)                           
067000          MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPRODSL-FOM-ATTR (IX)          
067100          MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPRODSL-TOM-ATTR (IX)          
067200          MOVE NEJ TO INDATA-SW                                           
067300          MOVE ERR-START-GREATER  TO MED-IDMFSFEL                         
067400       END-IF                                                             
067500                                                                          
067600       IF  KDERS-FOM (IX) < 99                                            
067700       AND KDERS-TOM (IX) = 99                                            
067800          MOVE KDERS-FOM (IX) TO KDERS-TOM (IX)                           
067900       END-IF                                                             
068000                                                                          
068100       IF KDERS-FOM (IX) > KDERS-TOM (IX)                                 
068200          MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-FOM-ATTR (IX)             
068300          MOVE MFS-NUM-FAELT-FEL   TO MOD-KDERS-TOM-ATTR (IX)             
068400          MOVE NEJ TO INDATA-SW                                           
068500          MOVE ERR-START-GREATER   TO MED-IDMFSFEL                        
068600       END-IF                                                             
068610                                                                          
068700       ADD +1 TO IX                                                       
068900     END-PERFORM                                                          
069000                                                                          
069100     .                                                                    
069200     EJECT                                                                
069700 C-UPPDATERA      SECTION.                                                
069800                                                                          
069900     MOVE '3331'   TO MSGSOP-IDTRANS                                      
070000     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
070100     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
070400*UTRSKRIFT                                                                
070410                                                                          
070500       MOVE 'W335S1' TO MSGSOP-IDPROCESS                                  
070600                                                                          
070700       STRING 'IDUSER(' MSG-SIGNON-USERID ') URVAL1('                     
070800              WS-URVAL1 ') URVAL2(' WS-URVAL2 ') LISTA('                  
070900              WS-LISTA ') SORT(' WS-SORTERING ') UTSKRIFT('               
071000              WS-FLEXCEL ')'                                              
071100              DELIMITED BY SIZE INTO MSGSOP-TESYMBV                       
071200                                                                          
071310            IF MID-FLAGGA-EXCE = 'R'                                      
071400              MOVE LISTUPPSTART TO MOD-TEMFSINF                           
071600* MAIL EXCELFIL                                                           
072500            ELSE                                                          
072600              MOVE MAILSEND     TO MOD-TEMFSINF                           
072700            END-IF                                                        
072800                                                                          
073000     PERFORM IMS-INSERT-ALTMSG                                            
073100     .                                                                    
073200     EJECT                                                                
073300 D-SAMMA-SIDA      SECTION.                                               
073400                                                                          
073500*    MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                  
073600*    CALL WMEDKONV USING MED-WMEDAREA                                     
073700     MOVE INFO-1 TO MOD-TEMFSINF                                          
073800                                                                          
073900     PERFORM MFS-LAES-IN-IGEN                                             
074000     PERFORM MFS-ROER-EJ-FAELT-IN                                         
074100     .                                                                    
074200     EJECT                                                                
074300 MFS-RENSA-FAELT-IN SECTION.                                              
074400                                                                          
074500*    --- ALLA INDATA-FÄLT                  -IN                            
074600     MOVE MFS-RENSA-FAELT TO MOD-KDSORT1                                  
075000                             MOD-FLAGGA-EXCE                              
075100                                                                          
075200     MOVE +1 TO IX                                                        
075300     PERFORM UNTIL IX > 3                                                 
075400       MOVE MFS-RENSA-FAELT TO MOD-IDMARKBO(IX)                           
075410       MOVE MFS-RENSA-FAELT TO MOD-IDPROMRN(IX)                           
075500       ADD +1 TO IX                                                       
075600     END-PERFORM                                                          
075700                                                                          
075800     MOVE +1 TO IX                                                        
075900     PERFORM UNTIL IX > 4                                                 
076000       MOVE MFS-RENSA-FAELT TO MOD-IDFKNGRP-FOM(IX)                       
076010       MOVE MFS-RENSA-FAELT TO MOD-IDFKNGRP-TOM(IX)                       
076100       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-FOM (IX)                       
076200       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-TOM (IX)                       
076300       MOVE MFS-RENSA-FAELT TO MOD-KDPRODSL-FOM(IX)                       
076310       MOVE MFS-RENSA-FAELT TO MOD-KDPRODSL-TOM(IX)                       
076400       MOVE MFS-RENSA-FAELT TO MOD-KDERS-FOM   (IX)                       
076410       MOVE MFS-RENSA-FAELT TO MOD-KDERS-TOM   (IX)                       
076500       ADD +1 TO IX                                                       
076600     END-PERFORM                                                          
076700                                                                          
076800     .                                                                    
076900     EJECT                                                                
077000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
077100                                                                          
077200*    --- ALLA INDATA-FÄLT                    -IN                          
077300     MOVE MFS-ROER-EJ-FAELT TO  MOD-KDSORT1                               
077700                                MOD-FLAGGA-EXCE                           
077800                                                                          
077900     MOVE +1 TO IX                                                        
078000     PERFORM UNTIL IX > 3                                                 
078100       MOVE MFS-ROER-EJ-FAELT TO MOD-IDMARKBO(IX)                         
078110       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPROMRN(IX)                         
078200       ADD +1 TO IX                                                       
078300     END-PERFORM                                                          
078400                                                                          
078600     MOVE +1 TO IX                                                        
078700     PERFORM UNTIL IX > 4                                                 
078800       MOVE MFS-ROER-EJ-FAELT TO MOD-IDFKNGRP-FOM(IX)                     
078810       MOVE MFS-ROER-EJ-FAELT TO MOD-IDFKNGRP-TOM(IX)                     
078900       MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-FOM (IX)                     
079000       MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-TOM (IX)                     
079100       MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRODSL-FOM(IX)                     
079110       MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRODSL-TOM(IX)                     
079200       MOVE MFS-ROER-EJ-FAELT TO MOD-KDERS-FOM   (IX)                     
079210       MOVE MFS-ROER-EJ-FAELT TO MOD-KDERS-TOM   (IX)                     
079300       ADD +1 TO IX                                                       
079400     END-PERFORM                                                          
079500                                                                          
079600     .                                                                    
079700     EJECT                                                                
079800 MFS-LAES-IN-IGEN SECTION.                                                
079900                                                                          
080000*    --- ALLA INDATA-FÄLT                                                 
080100     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDSORT1-ATTR                       
080500                                   MOD-FLAGGA-EXCE-ATTR                   
080600                                                                          
080700     MOVE +1 TO IX                                                        
080800     PERFORM UNTIL IX > 3                                                 
080900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPROMR-ATTR (IX)                
081000       ADD +1 TO IX                                                       
081100     END-PERFORM                                                          
081200                                                                          
081300     MOVE +1 TO IX                                                        
081400     PERFORM UNTIL IX > 4                                                 
081500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDFKNGRP-FOM-ATTR(IX)            
081510       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDFKNGRP-TOM-ATTR(IX)            
081600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-FOM-ATTR (IX)            
081700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-TOM-ATTR (IX)            
081800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRODSL-FOM-ATTR(IX)            
081810       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRODSL-TOM-ATTR(IX)            
081900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDERS-FOM-ATTR (IX)              
081910       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDERS-TOM-ATTR (IX)              
082000       ADD +1 TO IX                                                       
082100     END-PERFORM                                                          
082200     .                                                                    
082300     EJECT                                                                
082400* --- IMS SEKTIONER ---                                                   
082500     SKIP3                                                                
082600 IMS-GET-MSG SECTION.                                                     
082700                                                                          
082800     MOVE '  QC' TO GODK-STATUSKODER                                      
082900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
083000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
083100     PERFORM IMS-STATUSKONTROLL                                           
083200     .                                                                    
083300     SKIP3                                                                
083400 IMS-INSERT-MSG SECTION.                                                  
083500                                                                          
083600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
083700     MOVE SPACE TO GODK-STATUSKODER                                       
083800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
083900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
084000     PERFORM IMS-STATUSKONTROLL                                           
084100     .                                                                    
084200     SKIP3                                                                
084300 IMS-INSERT-ALTMSG SECTION.                                               
084400                                                                          
084500     MOVE SPACE TO GODK-STATUSKODER                                       
084600     CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                      
084700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
084800     PERFORM IMS-STATUSKONTROLL                                           
084900     .                                                                    
085000     EJECT                                                                
086200 IMS-STATUSKONTROLL SECTION.                                              
086300                                                                          
086400     SET STATUS-IX TO 1                                                   
086500     SEARCH GODK-STATUS                                                   
086600       AT END                                                             
086700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
086800         DELIMITED BY SIZE INTO FELTEXT                                   
086900         CALL FELLOG                                                      
087000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
087100     END-SEARCH                                                           
087200     .                                                                    
