000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5011400.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   94/08/25.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SPIS FRÅGA / ÄNDRA ARTIKEL FÖRSLAG                               
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WLPRIE (WDC6)                              
001200*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W5T114                                              
001600*        MID:         W5I11401                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W5O11401                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W5011400'.            
002900                                                                          
003000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500                                                                          
003600*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003800 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
003900 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004000*    --- DET RÄTTA VÄRDET PÅ NEDANSTÅENDE FÄLT SÄTTS I A-INIT             
004100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +534 COMP SYNC.         
004200                                                                          
004300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004400 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
004500 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
004600 77  WS-KDPRBEH                  PIC X(1)    VALUE SPACE.                 
004700 77  WS-REAENDR                  PIC X(6)    VALUE SPACE.                 
004800 77  WS-REAENDR-NUM              PIC 9(4)V9(1) VALUE ZERO.                
004900                                                                          
005000 77  WS-IDLEVNR-INM              PIC X(5)    VALUE SPACE.                 
005100 77  WS-MARKNING                 PIC X(6)    VALUE SPACE.                 
005200 77  WS-SUMMA-PRINK-KOM          PIC 9(7)V9(2) VALUE ZERO.                
005300 77  WS-SUMMA-PRINK-AKT          PIC 9(7)V9(2) VALUE ZERO.                
005400 77  WS-REAENDR-STDPRIS          PIC S9(5)V9(1) VALUE ZERO.               
005500 77  WS-RETULF                   PIC 9(3)V9(4) VALUE ZERO.                
005600 77  WS-PRKURS                   PIC 9(6)V9(5) VALUE ZERO.                
005700 77  WS-PRARTBEL-PR              PIC 9(8)V9(5) VALUE ZERO.                
005800 77  WS-TIPRLIST                 PIC 9(6)      VALUE ZERO.                
005900 77  WS-PRINK-KOM-FSLAG          PIC 9(7)V9(2) VALUE ZERO.                
006000 77  WS-STDPRIS-FSLAG            PIC 9(7)V9(2) VALUE ZERO.                
006100 77  WS-STDPRIS-AKT              PIC 9(7)V9(2) VALUE ZERO.                
006200 77  WS-REAENDR-INK-FSLAG        PIC S9(5)V9(1) VALUE ZERO.               
006300 77  WS-REAENDR-STD-FSLAG        PIC S9(5)V9(1) VALUE ZERO.               
006400 77  WS-PRINK-BYT                PIC 9(7)V9(2) VALUE ZERO.                
006500 77  WS-REAENDR-BYT              PIC S9(5)V9(1) VALUE ZERO.               
006600                                                                          
006700                                                                          
006800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006900     88  INDATA-OK                           VALUE 'J'.                   
007000     88  INDATA-FEL                          VALUE 'N'.                   
007100                                                                          
007200 77  ANGRA-SW                    PIC X       VALUE 'J'.                   
007300     88  ANGRA-UPPDAT                        VALUE 'J'.                   
007400     88  ANGRA-UPPDAT-NEJ                    VALUE 'N'.                   
007500                                                                          
007600 77  PF11-VARNING-SW             PIC X       VALUE 'J'.                   
007700     88  PF11-VARNING-JA                     VALUE 'J'.                   
007800     88  PF11-VARNING-NEJ                    VALUE 'N'.                   
007900                                                                          
008000 77  FORANDRING-SW               PIC X       VALUE 'J'.                   
008100     88  FORANDRING-PRIS                     VALUE 'J'.                   
008200     88  FORANDRING-PRIS-NEJ                 VALUE 'N'.                   
008300                                                                          
008400 77  NYA-PRISER-SW               PIC X       VALUE 'J'.                   
008500     88  NYA-PRISER-JA                       VALUE 'J'.                   
008600     88  NYA-PRISER-NEJ                      VALUE 'N'.                   
008700     88  NYA-PRISER-UPPDAT                   VALUE 'U'.                   
008800                                                                          
008900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009000     88  NYCKLAR-OK                          VALUE 'J'.                   
009100     88  NYCKLAR-FEL                         VALUE 'N'.                   
009200                                                                          
009300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
009400     88  ALLT-OK                             VALUE 'J'.                   
009500                                                                          
009600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009700     88  EGEN-MID                            VALUE '5114'.                
009800     88  SPIS-MID                            VALUE '5111' '5114'          
009900                                                   '5115'.                
010000     88  GODK-MID                            VALUE '5111' '5112'          
010100                                                   '5113' '5114'          
010200                                                   '5115' '5116'          
010300                                                   '5117' '5118'          
010400                                                   '5119'.                
010500     88  HELP-MID                            VALUE '0551'.                
010600     EJECT                                                                
010700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010800 01  GENERELLA-SUBPROGRAM.                                                
010900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011200     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
011300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011500     EJECT                                                                
011600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011700*01 -COPY WMSGINIT                                                        
011800     EJECT                                                                
011900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012000*01 -COPY WMEDAREA                                                        
012100     SKIP3                                                                
012200 01  MESSAGE-CODES.                                                       
012300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
012400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
012500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
012600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
012700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
012800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
012900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013000     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
013100     EJECT                                                                
013200*01  -COPY WDATAREA                                                       
013300     EJECT                                                                
013400*01  -COPY WDECAREA                                                       
013500     EJECT                                                                
013600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
013700*                                                                         
013800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013900     SKIP3                                                                
014000*01  MID -COPY W5I11401                                                   
014100     EJECT                                                                
014200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014300     SKIP3                                                                
014400*01  -COPY WMSGAREA                                                       
014500     EJECT                                                                
014600     03  MOD REDEFINES MSG-AREA.                                          
014700*      05  -COPY W5O11401                                                 
014800     EJECT                                                                
014900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015000     SKIP3                                                                
015100*01  -COPY WMFSAREA                                                       
015200     EJECT                                                                
015300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015400*                                                                         
015500     SKIP2                                                                
015600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015700     SKIP3                                                                
015800 01  NYCKLAR-TILL-DLI.                                                    
015900     03  W-WDC6A1KY-X.                                                    
016000         05  W-WDC6A1KY          PIC S9(14)   VALUE ZERO COMP-3.          
016100     03  W-IDARTNR-X.                                                     
016200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016300     03  W-IDLEVNR-X.                                                     
016400         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
016500     SKIP2                                                                
016600     03  W-IDARTNR-WDD3-X.                                                
016700         05  W-IDARTNR-WDD3      PIC S9(9)   VALUE ZERO COMP-3.           
016800     03  W-IDSKYLT-X.                                                     
016900         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
017000*    --- STATUS-KOD FRÅN IMS                                              
017100 01  STATUS-WS                   PIC XX.                                  
017200     88  SEGMENT-FINNS                       VALUE '  '.                  
017300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017500     SKIP2                                                                
017600 01  GODK-STATUSKODER.                                                    
017700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017800     SKIP3                                                                
017900 01  SSA1                        PIC X(64).                               
018000 01  SSA2                        PIC X(64).                               
018100     EJECT                                                                
018200*    --- IMS FUNKTIONSKODER                                               
018300*01  -COPY W0003                                                          
018400     EJECT                                                                
018500*    ---  DLI INPUT-OUTPUT AREA                                           
018600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
018700     SKIP3                                                                
018800 01  DLI-IO-AREA.                                                         
018900     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
019000     SKIP3                                                                
019100     03  WLPRIE01 REDEFINES IO-AREA.                                      
019200*        05  -COPY WDC601  -PRE PRIE-                                     
019300 01  DLI-IO-AREA2.                                                        
019400     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
019500     SKIP3                                                                
019600     03  WLBENA11 REDEFINES IO-AREA2.                                     
019700*        05  -COPY WDD311  -PRE BENA-                                     
019800     EJECT                                                                
019900 LINKAGE SECTION.                                                         
020000                                                                          
020100*01  -COPY W0009   -PRE MSG-                                              
020200     EJECT                                                                
020300*01  -COPY W0008  -PRE USEA-                                              
020400     05  FILLER                  PIC X.                                   
020500     EJECT                                                                
020600*01  -COPY W0008  -PRE PRIE-                                              
020700     05  FILLER                  PIC X.                                   
020800     EJECT                                                                
020900*01  -COPY W0008  -PRE BENA-                                              
021000     05  FILLER                  PIC X.                                   
021100     EJECT                                                                
021200 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
021300                                   PRIE-PCB BENA-PCB.                     
021400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
021500                                   PRIE-PCB BENA-PCB.                     
021600                                                                          
021700     PERFORM IMS-GET-MSG                                                  
021800     IF SEGMENT-FINNS                                                     
021900       PERFORM A-INIT                                                     
022000       PERFORM B-KOLLA-NYCKLAR                                            
022100       IF NYCKLAR-OK                                                      
022200         IF MFS-UPDATE                                                    
022300           PERFORM G-KOLLA-INPUT                                          
022400           IF INDATA-OK                                                   
022500             IF PF11-VARNING-NEJ                                          
022600               PERFORM H-UPPDATERA                                        
022700             END-IF                                                       
022800           END-IF                                                         
022900         ELSE                                                             
023000           MOVE MFS-RENSA-FAELT TO MOD-FLSVAR-UPPDAT                      
023100           IF MFS-FIRST                                                   
023200             PERFORM C-FOERSTA-SIDA                                       
023300           ELSE                                                           
023400             IF MFS-NEXT                                                  
023500               PERFORM D-NAESTA-SIDA                                      
023600             ELSE                                                         
023700               PERFORM E-SAMMA-SIDA                                       
023800             END-IF                                                       
023900           END-IF                                                         
024000         END-IF                                                           
024100         IF ALLT-OK                                                       
024200           PERFORM F-LAES-VISA-INFO                                       
024300         END-IF                                                           
024400       END-IF                                                             
024500       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
024600       PERFORM IMS-INSERT-MSG                                             
024700     END-IF                                                               
024800                                                                          
024900     MOVE ZERO TO RETURN-CODE                                             
025000     GOBACK                                                               
025100     .                                                                    
025200     EJECT                                                                
025300 A-INIT SECTION.                                                          
025400                                                                          
025500     IF MSG-DUBBLA-TRANSKODER                                             
025600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I11401                 
025700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
025800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025900     ELSE                                                                 
026000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I11401                  
026100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
026200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026300     END-IF                                                               
026400                                                                          
026500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
026600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
026700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
026800                                                                          
026900     MOVE LOW-VALUE TO MSG-AREA                                           
027000     MOVE 'W5O114N1' TO MFS-IDMOD                                         
027100     MOVE '5114' TO MOD-IDTRANS                                           
027200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
027300                                                                          
027400     IF EGEN-MID OR HELP-MID                                              
027500       CONTINUE                                                           
027600     ELSE                                                                 
027700       MOVE SPACE TO MFS-KDTRTYP                                          
027800       MOVE '7' TO MFS-IDPFK                                              
027900     END-IF                                                               
028000                                                                          
028100     MOVE NEJ TO NYA-PRISER-SW                                            
028200     .                                                                    
028300     EJECT                                                                
028400 B-KOLLA-NYCKLAR SECTION.                                                 
028500                                                                          
028600     MOVE JA TO NYCKLAR-SW                                                
028700                                                                          
028800*    -- KONTROLL AV IDARTNR                                               
028900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
029000                                                                          
029100     MOVE ALL '+' TO MSGI-WMSGINIT                                        
029200     MOVE '001'             TO MSGI-KDCALL                                
029300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
029400     MOVE '5114'               TO MSGI-IDTRANS                            
029500     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
029600                                                                          
029700     IF MFS-IDTRANS = '5114'                                              
029800         MOVE MID-IDLEVNR-IN   TO MSGI-IDLEVNR                            
029900         MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                            
030000     ELSE                                                                 
030100       IF MID-IDARTNR-IN NUMERIC                                          
030200       AND MID-IDARTNR-IN > ZERO                                          
030300         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
030400       END-IF                                                             
030500       IF SPIS-MID                                                        
030600         CONTINUE                                                         
030700       ELSE                                                               
030800         MOVE SPACE     TO MID-IDLEVNR-IN                                 
030900       END-IF                                                             
031000     END-IF                                                               
031100                                                                          
031200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
031300     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
031400                                                                          
031500     IF MSGI-IDLAND-SPR = 'GB'                                            
031600       MOVE +2 TO SPRAK-IX                                                
031700       MOVE 'GB ' TO MED-IDSKYLT                                          
031800       MOVE 'GB ' TO W-IDSKYLT                                            
031900     ELSE                                                                 
032000       MOVE +1 TO SPRAK-IX                                                
032100       MOVE 'S  ' TO MED-IDSKYLT                                          
032200       MOVE 'S  ' TO W-IDSKYLT                                            
032300     END-IF                                                               
032400                                                                          
032500     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
032600     MOVE WS-IDARTNR    TO MID-IDARTNR-ENTER                              
032700                                                                          
032800     IF MID-IDARTNR-IN = ALL '+'                                          
032900       CONTINUE                                                           
033000     ELSE                                                                 
033100       MOVE '7'         TO MFS-IDPFK                                      
033200       MOVE SPACE       TO MFS-KDTRTYP                                    
033300     END-IF                                                               
033400                                                                          
033500     IF WS-IDARTNR NUMERIC                                                
033600       MOVE WS-IDARTNR TO W-IDARTNR                                       
033700                          W-IDARTNR-WDD3                                  
033800     ELSE                                                                 
033900       MOVE NEJ TO NYCKLAR-SW                                             
034000     END-IF                                                               
034100                                                                          
034200*    -- KONTROLL AV IDLEVNR                                               
034300     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
034400                                                                          
034500     IF MID-IDLEVNR-IN = ALL '+'                                          
034600       MOVE MID-IDLEVNR-UT TO WS-IDLEVNR                                  
034700     ELSE                                                                 
034800       MOVE MID-IDLEVNR-IN TO WS-IDLEVNR                                  
034900       MOVE '7'         TO MFS-IDPFK                                      
035000       MOVE SPACE       TO MFS-KDTRTYP                                    
035100     END-IF                                                               
035200                                                                          
035300     MOVE WS-IDLEVNR TO W-IDLEVNR                                         
035400                                                                          
035500*    -- KONTROLL AV KDPRBEH                                               
035600     MOVE MFS-RENSA-FAELT TO MOD-KDPRBEH-IN                               
035700                                                                          
035800     IF MID-KDPRBEH-IN = ALL '+'                                          
035900       MOVE MID-KDPRBEH-UT TO WS-KDPRBEH                                  
036000     ELSE                                                                 
036100       MOVE MID-KDPRBEH-IN TO WS-KDPRBEH                                  
036200       MOVE '7'         TO MFS-IDPFK                                      
036300       MOVE SPACE       TO MFS-KDTRTYP                                    
036400     END-IF                                                               
036500                                                                          
036600*    MOVE WS-KDPRBEH TO W-KDPRBEH                                         
036700                                                                          
036800*    -- KONTROLL AV REAENDR                                               
036900     MOVE MFS-RENSA-FAELT TO MOD-REAENDR-IN                               
037000                                                                          
037100     IF SPIS-MID                                                          
037200       IF MID-REAENDR-IN = ALL '+'                                        
037300         MOVE MID-REAENDR-UT TO WS-REAENDR                                
037400       ELSE                                                               
037500         MOVE MID-REAENDR-IN TO WS-REAENDR                                
037600         MOVE '7'       TO MFS-IDPFK                                      
037700         MOVE SPACE     TO MFS-KDTRTYP                                    
037800       END-IF                                                             
037900                                                                          
038000       MOVE WS-REAENDR TO DEC-IDFRIDATA                                   
038100       MOVE 4        TO DEC-KVHELTAL                                      
038200       MOVE 1        TO DEC-KVDECIMAL                                     
038300                                                                          
038400       CALL WDECEDIT USING DEC-WDECAREA                                   
038500                                                                          
038600       IF DEC-KDSVAR-OK                                                   
038700         MOVE DEC-IDEDITDATA TO WS-REAENDR-NUM                            
038800*        MOVE WS-REAENDR-NUM TO W-REAENDR                                 
038900       ELSE                                                               
039000         MOVE NEJ TO NYCKLAR-SW                                           
039100       END-IF                                                             
039200     ELSE                                                                 
039300       MOVE ZERO TO WS-REAENDR-NUM                                        
039400     END-IF                                                               
039500                                                                          
039600     IF SPIS-MID                                                          
039700       MOVE WS-IDLEVNR TO MOD-IDLEVNR-UT                                  
039800     ELSE                                                                 
039900       MOVE SPACE TO MOD-IDLEVNR-UT                                       
040000     END-IF                                                               
040100     MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                    
040200     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
040300     MOVE WS-KDPRBEH      TO MOD-KDPRBEH-UT                               
040400     MOVE WS-REAENDR-NUM  TO MOD-REAENDR-UT                               
040500                                                                          
040600     IF NYCKLAR-FEL                                                       
040700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
040800       CALL WMEDKONV USING MED-WMEDAREA                                   
040900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
041000       PERFORM MFS-RENSA-FAELT-IN                                         
041100       PERFORM MFS-RENSA-FAELT-UT                                         
041200     END-IF                                                               
041300     .                                                                    
041400     EJECT                                                                
041500 C-FOERSTA-SIDA SECTION.                                                  
041600                                                                          
041700*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
041800                                                                          
041900     MOVE JA TO ALLT-SW                                                   
042000     PERFORM MFS-RENSA-FAELT-IN                                           
042100     MOVE MFS-STAENG-FAELT TO MOD-FLSVAR-ATTR                             
042200     .                                                                    
042300     EJECT                                                                
042400 D-NAESTA-SIDA SECTION.                                                   
042500                                                                          
042600     MOVE MID-IDARTNR-ENTER TO W-IDARTNR                                  
042700                               W-IDARTNR-WDD3                             
042800     MOVE JA TO ALLT-SW                                                   
042900     MOVE MFS-STAENG-FAELT TO MOD-FLSVAR-ATTR                             
043000     .                                                                    
043100     EJECT                                                                
043200 E-SAMMA-SIDA SECTION.                                                    
043300                                                                          
043400     IF EGEN-MID OR HELP-MID                                              
043500       IF MID-IDARTNR-ENTER NUMERIC                                       
043600         MOVE MID-IDARTNR-ENTER TO W-IDARTNR                              
043700       ELSE                                                               
043800         MOVE ZERO TO W-IDARTNR                                           
043900       END-IF                                                             
044000       MOVE MID-IDLEVNR-ENTER TO W-IDLEVNR                                
044100       IF MID-INPUT = ALL '+'                                             
044200         MOVE JA TO ALLT-SW                                               
044300         MOVE MID-IDARTNR-ENTER TO W-IDARTNR                              
044400                                   W-IDARTNR-WDD3                         
044500         MOVE MFS-STAENG-FAELT TO MOD-FLSVAR-ATTR                         
044600         PERFORM MFS-RENSA-FAELT-IN                                       
044700         MOVE MFS-RENSA-FAELT TO MOD-FLSVAR-UPPDAT                        
044800       ELSE                                                               
044900         MOVE NEJ TO ALLT-SW                                              
045000         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
045100         CALL WMEDKONV USING MED-WMEDAREA                                 
045200         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
045300         PERFORM MFS-ROER-EJ-FAELT-IN                                     
045400         PERFORM MFS-ROER-EJ-FAELT-UT                                     
045500         MOVE MFS-ROER-EJ-FAELT TO MOD-FLSVAR-UPPDAT                      
045600                                   MOD-SVAR-VISNING                       
045700         PERFORM EA-MID-INDATA-TILL-MOD                                   
045800       END-IF                                                             
045900     ELSE                                                                 
046000       PERFORM MFS-RENSA-FAELT-IN                                         
046100       MOVE MFS-RENSA-FAELT TO MOD-FLSVAR-UPPDAT                          
046200     END-IF                                                               
046300     .                                                                    
046400     EJECT                                                                
046500 EA-MID-INDATA-TILL-MOD SECTION.                                          
046600                                                                          
046700* * * * * FÖR VARJE MID-FÄLT                                              
046800* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
046900* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
047000* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
047100     SKIP2                                                                
047200     IF MID-RETULF-IN NOT = ALL '+'                                       
047300       MOVE MID-RETULF-IN TO DEC-IDFRIDATA                                
047400       MOVE 3             TO DEC-KVHELTAL                                 
047500       MOVE 4             TO DEC-KVDECIMAL                                
047600                                                                          
047700       CALL WDECEDIT USING DEC-WDECAREA                                   
047800                                                                          
047900       IF DEC-KDSVAR-OK                                                   
048000         MOVE DEC-IDEDITDATA   TO MOD-RETULF-IN                           
048100         MOVE MFS-NUM-FAELT-RAETT TO MOD-RETULF-IN-ATTR                   
048200       ELSE                                                               
048300         MOVE MFS-NUM-FAELT-FEL TO MOD-RETULF-IN-ATTR                     
048400         MOVE NEJ TO INDATA-SW                                            
048500       END-IF                                                             
048600     ELSE                                                                 
048700       MOVE MFS-RENSA-FAELT       TO MOD-RETULF-IN                        
048800     END-IF                                                               
048900                                                                          
049000     IF MID-PRKURS-IN NOT = ALL '+'                                       
049100       MOVE MID-PRKURS-IN TO DEC-IDFRIDATA                                
049200       MOVE 6             TO DEC-KVHELTAL                                 
049300       MOVE 4             TO DEC-KVDECIMAL                                
049400                                                                          
049500       CALL WDECEDIT USING DEC-WDECAREA                                   
049600                                                                          
049700       IF DEC-KDSVAR-OK                                                   
049800         MOVE DEC-IDEDITDATA   TO MOD-PRKURS-IN                           
049900         MOVE MFS-NUM-FAELT-RAETT TO MOD-PRKURS-IN-ATTR                   
050000       ELSE                                                               
050100         MOVE MFS-NUM-FAELT-FEL TO MOD-PRKURS-IN-ATTR                     
050200         MOVE NEJ TO INDATA-SW                                            
050300       END-IF                                                             
050400     ELSE                                                                 
050500       MOVE MFS-RENSA-FAELT       TO MOD-PRKURS-IN                        
050600     END-IF                                                               
050700                                                                          
050800     IF MID-TIPRLIST-INM NOT = ALL '+'                                    
050900       MOVE MID-TIPRLIST-INM TO MOD-TIPRLIST-INM                          
051000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TIPRLIST-INM-ATTR                
051100     ELSE                                                                 
051200       MOVE MFS-RENSA-FAELT       TO MOD-TIPRLIST-INM                     
051300     END-IF                                                               
051400                                                                          
051500     IF MID-IDLEVNR-INM NOT = ALL '+'                                     
051600       MOVE MID-IDLEVNR-INM TO MOD-IDLEVNR-INM                            
051700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-INM-ATTR                 
051800     ELSE                                                                 
051900       MOVE MFS-RENSA-FAELT       TO MOD-IDLEVNR-INM                      
052000     END-IF                                                               
052100                                                                          
052200     IF MID-PRARTBEL-PR-INM NOT = ALL '+'                                 
052300       MOVE MID-PRARTBEL-PR-INM TO DEC-IDFRIDATA                          
052400       MOVE 8                   TO DEC-KVHELTAL                           
052500       MOVE 3                   TO DEC-KVDECIMAL                          
052600                                                                          
052700       CALL WDECEDIT USING DEC-WDECAREA                                   
052800                                                                          
052900       IF DEC-KDSVAR-OK                                                   
053000         MOVE DEC-IDEDITDATA   TO MOD-PRARTBEL-PR-INM                     
053100         MOVE MFS-NUM-FAELT-RAETT TO MOD-PRARTBEL-PR-ATTR                 
053200       ELSE                                                               
053300         MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTBEL-PR-ATTR                   
053400         MOVE NEJ TO INDATA-SW                                            
053500       END-IF                                                             
053600     ELSE                                                                 
053700       MOVE MFS-RENSA-FAELT       TO MOD-PRARTBEL-PR-INM                  
053800     END-IF                                                               
053900                                                                          
054000     IF MID-KDVALISO-INM NOT = ALL '+'                                    
054100       MOVE MID-KDVALISO-INM TO MOD-KDVALISO-INM                          
054200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDVALISO-INM-ATTR                
054300     ELSE                                                                 
054400       MOVE MFS-RENSA-FAELT       TO MOD-KDVALISO-INM                     
054500     END-IF                                                               
054600                                                                          
054700     IF MID-KDPRBEH-INM NOT = ALL '+'                                     
054800       MOVE MID-KDPRBEH-INM TO MOD-KDPRBEH-INM                            
054900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRBEH-INM-ATTR                 
055000     ELSE                                                                 
055100       MOVE MFS-RENSA-FAELT       TO MOD-KDPRBEH-INM                      
055200     END-IF                                                               
055300                                                                          
055400     IF MID-TEARTNOT-IN-UT NOT = ALL '+'                                  
055500       MOVE MID-TEARTNOT-IN-UT TO MOD-TEARTNOT-IN-UT                      
055600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEARTNOT-IN-UT-ATTR              
055700     ELSE                                                                 
055800       MOVE MFS-ROER-EJ-FAELT     TO MOD-TEARTNOT-IN-UT                   
055900     END-IF                                                               
056000                                                                          
056100     IF MID-FLSVAR-UPPDAT  NOT = ALL '+'                                  
056200       MOVE MID-FLSVAR-UPPDAT  TO MOD-FLSVAR-UPPDAT                       
056300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLSVAR-ATTR                      
056400       IF MID-FLSVAR-UPPDAT = JA                                          
056500         MOVE NEJ                 TO MOD-FLAGGA                           
056600       END-IF                                                             
056700     ELSE                                                                 
056800       MOVE MFS-ROER-EJ-FAELT     TO MOD-FLSVAR-UPPDAT                    
056900       MOVE NEJ TO MOD-FLAGGA                                             
057000     END-IF                                                               
057100     .                                                                    
057200     EJECT                                                                
057300 F-LAES-VISA-INFO SECTION.                                                
057400                                                                          
057500     PERFORM IMS-GHU-WDC601                                               
057600                                                                          
057700     IF SEGMENT-SAKNAS                                                    
057800        MOVE ERR-PART-MISSING TO MED-IDMFSFEL                             
057900        CALL WMEDKONV USING MED-WMEDAREA                                  
058000        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
058100        PERFORM MFS-RENSA-FAELT-UT                                        
058200     ELSE                                                                 
058300                                                                          
058400       MOVE PRIE-ART-IDARTNR       TO MOD-IDARTNR-ENTER                   
058500       MOVE PRIE-ART-IDLEVNR       TO MOD-IDLEVNR-ENTER                   
058600                                                                          
058700       PERFORM FA-UTFOR-BERAKNINGAR                                       
058800                                                                          
058900*      MOVE PRIE-ART-REAENDR       TO MOD-REAENDR-UT                      
059000*      MOVE PRIE-ART-KDPRBEH       TO MOD-KDPRBEH-UT                      
059100       MOVE PRIE-ART-IDPRANSV      TO MOD-IDPRANSV                        
059200       MOVE WS-MARKNING            TO MOD-MARKNING                        
059300       PERFORM IMS-GU-WDD3                                                
059400       IF SEGMENT-FINNS                                                   
059500         MOVE BENA-TEXT-BEART      TO MOD-BEART-SVE                       
059600       ELSE                                                               
059700         MOVE SPACE                TO MOD-BEART-SVE                       
059800       END-IF                                                             
059900       MOVE PRIE-ART-KVBEHOVAR     TO MOD-KVBEHOVAR                       
060000       MOVE PRIE-ART-KVDISP-SPIS   TO MOD-KVDISP-SPIS                     
060100       MOVE PRIE-ART-PRINK-AKT     TO MOD-PRINK-AKT-INK                   
060200                                                                          
060300******* OM INMATNINGSFÄLTEN RETULF ,PRKURS ELLER PRARTBEL-PR *****        
060400******* ÄR ÄNDRADE GES PRELIMINÄRA PRISER SOM EJ UPPDATERAS  *****        
060500******* PÅ BASEN (WDC601) UTAN ENDAST VISAS FÖRSTA PF11-     *****        
060600******* TRYCKNINGEN.                                         *****        
060700******************************************************************        
060800                                                                          
060900       IF NYA-PRISER-JA                                                   
061000         MOVE WS-PRINK-KOM-FSLAG   TO MOD-PRINK-KOM-INK                   
061100                                      MOD-PRINK-KOM-BES                   
061200         MOVE WS-REAENDR-INK-FSLAG TO MOD-REAENDR-INK                     
061300         MOVE WS-STDPRIS-FSLAG     TO MOD-PRINK-KOM-TOT                   
061400                                      MOD-PRINK-KOM-SJK                   
061500         MOVE WS-STDPRIS-AKT       TO MOD-PRINK-AKT-TOT                   
061600         MOVE WS-REAENDR-STD-FSLAG TO MOD-REAENDR-TOT                     
061700       ELSE                                                               
061800         MOVE PRIE-ART-PRINK-KOM   TO MOD-PRINK-KOM-INK                   
061900                                      MOD-PRINK-KOM-BES                   
062000         MOVE PRIE-ART-REAENDR     TO MOD-REAENDR-INK                     
062100         MOVE WS-SUMMA-PRINK-KOM   TO MOD-PRINK-KOM-TOT                   
062200                                      MOD-PRINK-KOM-SJK                   
062300         MOVE WS-SUMMA-PRINK-AKT   TO MOD-PRINK-AKT-TOT                   
062400         MOVE WS-REAENDR-STDPRIS   TO MOD-REAENDR-TOT                     
062500       END-IF                                                             
062600                                                                          
062700       MOVE PRIE-ART-RETULF        TO MOD-RETULF-UT                       
062800       MOVE PRIE-ART-PRARTBES      TO MOD-PRARTBES                        
062900       MOVE PRIE-ART-PRKURS        TO MOD-PRKURS-UT                       
063000       MOVE PRIE-ART-PRARTSJK      TO MOD-PRARTSJK                        
063100       MOVE PRIE-ART-FLIART        TO MOD-FLIART                          
063200       MOVE PRIE-ART-PRDIRLON-KOM  TO MOD-PRDIRLON-KOM                    
063300       MOVE PRIE-ART-PRDIRLON-AKT  TO MOD-PRDIRLON-AKT                    
063400       MOVE PRIE-ART-IDLEVNR-HUV   TO MOD-IDLEVNR-HUV                     
063500       MOVE PRIE-ART-PROVRPAL-KOM  TO MOD-PROVRPAL-KOM                    
063600       MOVE PRIE-ART-PROVRPAL-AKT  TO MOD-PROVRPAL-AKT                    
063700       MOVE PRIE-ART-KDPRODSL      TO MOD-KDPRODSL                        
063800       MOVE PRIE-ART-PRDMTRL-KOM   TO MOD-PRDMTRL-KOM                     
063900       MOVE PRIE-ART-PRDMTRL-AKT   TO MOD-PRDMTRL-AKT                     
064000       MOVE PRIE-ART-TIPRLIST      TO MOD-TIPRLIST-UTM                    
064100       MOVE PRIE-ART-IDLEVNR       TO MOD-IDLEVNR-UTM                     
064200       MOVE PRIE-ART-PRARTBEL-PR   TO MOD-PRARTBEL-PR-UTM                 
064300       MOVE PRIE-ART-KDVALISO      TO MOD-KDVALISO-UTM                    
064400       MOVE PRIE-ART-KDSTASPIS     TO MOD-KDSTASPIS                       
064500       MOVE PRIE-ART-KDPRBEH       TO MOD-KDPRBEH-UTM                     
064600       MOVE PRIE-ART-REDIRLEV      TO MOD-REDIRLEV                        
064700       MOVE PRIE-ART-FLAPC         TO MOD-FLAPC                           
064800       MOVE PRIE-ART-FLPRFIL       TO MOD-FLPRFIL                         
064900       MOVE PRIE-ART-TEARTNOT      TO MOD-TEARTNOT-IN-UT                  
065000     END-IF                                                               
065100     .                                                                    
065200     EJECT                                                                
065300 FA-UTFOR-BERAKNINGAR SECTION.                                            
065400     SKIP2                                                                
065500     IF PRIE-ART-IDPRANSV = 'VCAS'                                        
065600       IF PRIE-ART-KDHF > ZERO                                            
065700         MOVE 'HF ART' TO WS-MARKNING                                     
065800       ELSE                                                               
065900         IF PRIE-ART-IDLEVNR-HUV = '1002 '                                
066000           MOVE 'SATS' TO WS-MARKNING                                     
066100         ELSE                                                             
066200           MOVE 'EXTERN' TO WS-MARKNING                                   
066300         END-IF                                                           
066400       END-IF                                                             
066500     ELSE                                                                 
066600       IF PRIE-ART-IDPRANSV = 'VCC'                                       
066700         MOVE 'EXTERN' TO WS-MARKNING                                     
066800       ELSE                                                               
066900         MOVE 'EXTERN' TO WS-MARKNING                                     
067000       END-IF                                                             
067100     END-IF                                                               
067200                                                                          
067300     COMPUTE WS-SUMMA-PRINK-KOM ROUNDED = PRIE-ART-PRINK-KOM +            
067400             PRIE-ART-PRDIRLON-KOM + PRIE-ART-PRDMTRL-KOM +               
067500             PRIE-ART-PROVRPAL-KOM                                        
067600                                                                          
067700     COMPUTE WS-SUMMA-PRINK-AKT ROUNDED = PRIE-ART-PRINK-AKT +            
067800             PRIE-ART-PRDIRLON-AKT + PRIE-ART-PRDMTRL-AKT +               
067900             PRIE-ART-PROVRPAL-AKT                                        
068000                                                                          
068100     COMPUTE WS-REAENDR-STDPRIS ROUNDED =                                 
068200        ( 100 * WS-SUMMA-PRINK-KOM / WS-SUMMA-PRINK-AKT ) - 100           
068300                                                                          
068400     IF WS-REAENDR-STDPRIS > 10000 OR                                     
068500        WS-SUMMA-PRINK-KOM = ZERO                                         
068600       MOVE 9999 TO WS-REAENDR-STDPRIS                                    
068700     END-IF                                                               
068800     .                                                                    
068900     EJECT                                                                
069000 G-KOLLA-INPUT SECTION.                                                   
069100                                                                          
069200     MOVE JA  TO INDATA-SW                                                
069300                 ALLT-SW                                                  
069400     MOVE NEJ TO PF11-VARNING-SW                                          
069500                 NYA-PRISER-SW                                            
069600                 FORANDRING-SW                                            
069700                 ANGRA-SW                                                 
069800                                                                          
069900     IF MID-INPUT = ALL '+'                                               
070000       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
070100       CALL WMEDKONV USING MED-WMEDAREA                                   
070200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
070300       PERFORM MFS-ROER-EJ-FAELT-IN                                       
070400       PERFORM MFS-ROER-EJ-FAELT-UT                                       
070500       MOVE NEJ TO INDATA-SW                                              
070600                   ALLT-SW                                                
070700     ELSE                                                                 
070800       IF MID-FLAGGA = 'J'                                                
070900         IF MID-FLSVAR-UPPDAT NOT = ALL '+'                               
071000           IF MID-FLSVAR-UPPDAT = JA OR NEJ                               
071100             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSVAR-ATTR                 
071200           ELSE                                                           
071300             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSVAR-ATTR                 
071400           END-IF                                                         
071500           IF INDATA-OK                                                   
071600             IF MID-FLSVAR-UPPDAT = JA                                    
071700               PERFORM GB-BEHANDLA-KOM-PRIS                               
071800               MOVE MFS-RENSA-FAELT TO MOD-FLSVAR-UPPDAT                  
071900               MOVE MFS-STAENG-FAELT TO MOD-FLSVAR-ATTR                   
072000               MOVE NEJ TO ANGRA-SW                                       
072100             ELSE                                                         
072200               PERFORM GC-ANGRA-UPPDATERING                               
072300               MOVE JA TO ANGRA-SW                                        
072400             END-IF                                                       
072500           END-IF                                                         
072600         ELSE                                                             
072700           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLSVAR-ATTR                     
072800           MOVE MFS-ROER-EJ-FAELT  TO MOD-FLSVAR-UPPDAT                   
072900                                      MOD-SVAR-VISNING                    
073000           MOVE NEJ TO INDATA-SW                                          
073100         END-IF                                                           
073200       ELSE                                                               
073300         IF MID-RETULF-IN NOT = ALL '+'                                   
073400           MOVE JA TO FORANDRING-SW                                       
073500           MOVE MID-RETULF-IN TO DEC-IDFRIDATA                            
073600           MOVE 3           TO DEC-KVHELTAL                               
073700           MOVE 4           TO DEC-KVDECIMAL                              
073800                                                                          
073900           CALL WDECEDIT USING DEC-WDECAREA                               
074000                                                                          
074100           IF DEC-KDSVAR-OK                                               
074200             MOVE DEC-IDEDITDATA TO WS-RETULF                             
074300             MOVE MFS-NUM-FAELT-RAETT TO MOD-RETULF-IN-ATTR               
074400             MOVE MFS-ROER-EJ-FAELT TO MOD-RETULF-IN                      
074500           ELSE                                                           
074600             MOVE MFS-NUM-FAELT-FEL TO MOD-RETULF-IN-ATTR                 
074700             MOVE NEJ TO INDATA-SW                                        
074800           END-IF                                                         
074900         ELSE                                                             
075000           MOVE MFS-NUM-FAELT-RAETT TO MOD-RETULF-IN-ATTR                 
075100         END-IF                                                           
075200                                                                          
075300         IF MID-PRKURS-IN NOT = ALL '+'                                   
075400           MOVE JA TO FORANDRING-SW                                       
075500           MOVE MID-PRKURS-IN TO DEC-IDFRIDATA                            
075600           MOVE 6           TO DEC-KVHELTAL                               
075700           MOVE 4           TO DEC-KVDECIMAL                              
075800                                                                          
075900           CALL WDECEDIT USING DEC-WDECAREA                               
076000                                                                          
076100           IF DEC-KDSVAR-OK                                               
076200             MOVE DEC-IDEDITDATA    TO WS-PRKURS                          
076300             MOVE MFS-NUM-FAELT-RAETT TO MOD-PRKURS-IN-ATTR               
076400             MOVE MFS-ROER-EJ-FAELT TO MOD-PRKURS-IN                      
076500           ELSE                                                           
076600             MOVE MFS-NUM-FAELT-FEL TO MOD-PRKURS-IN-ATTR                 
076700             MOVE NEJ TO INDATA-SW                                        
076800           END-IF                                                         
076900         ELSE                                                             
077000           MOVE MFS-NUM-FAELT-RAETT TO MOD-PRKURS-IN-ATTR                 
077100         END-IF                                                           
077200                                                                          
077300         IF MID-PRARTBEL-PR-INM NOT = ALL '+'                             
077400           MOVE JA TO FORANDRING-SW                                       
077500           MOVE MID-PRARTBEL-PR-INM TO DEC-IDFRIDATA                      
077600           MOVE 8                 TO DEC-KVHELTAL                         
077700           MOVE 3                 TO DEC-KVDECIMAL                        
077800                                                                          
077900           CALL WDECEDIT USING DEC-WDECAREA                               
078000                                                                          
078100           IF DEC-KDSVAR-OK                                               
078200             MOVE DEC-IDEDITDATA TO WS-PRARTBEL-PR                        
078300             MOVE MFS-NUM-FAELT-RAETT TO MOD-PRARTBEL-PR-ATTR             
078400             MOVE MFS-ROER-EJ-FAELT TO MOD-PRARTBEL-PR-INM                
078500           ELSE                                                           
078600             MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTBEL-PR-ATTR               
078700             MOVE NEJ TO INDATA-SW                                        
078800           END-IF                                                         
078900         ELSE                                                             
079000           MOVE MFS-NUM-FAELT-RAETT TO MOD-PRARTBEL-PR-ATTR               
079100         END-IF                                                           
079200                                                                          
079300         IF FORANDRING-PRIS                                               
079400           IF MID-KDPRBEH-INM NOT = ALL '+'                               
079500             IF MID-KDPRBEH-INM = 'N' OR 'F' OR 'J'                       
079600               IF MID-KDPRBEH-INM = 'F'                                   
079700                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRBEH-INM-ATTR        
079800                 MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRBEH-INM                
079900               ELSE                                                       
080000                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRBEH-INM-ATTR          
080100                 MOVE NEJ TO INDATA-SW                                    
080200               END-IF                                                     
080300             ELSE                                                         
080400               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRBEH-INM-ATTR            
080500               MOVE NEJ TO INDATA-SW                                      
080600             END-IF                                                       
080700           ELSE                                                           
080800             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRBEH-INM-ATTR              
080900             MOVE NEJ TO INDATA-SW                                        
081000           END-IF                                                         
081100                                                                          
081200           IF MID-KDVALISO-INM NOT = ALL '+'                              
081300             IF MID-PRKURS-IN NOT = ALL '+'                               
081400              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDVALISO-INM-ATTR          
081500              MOVE MFS-ROER-EJ-FAELT TO MOD-KDVALISO-INM                  
081600             ELSE                                                         
081700               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDVALISO-INM-ATTR           
081800               MOVE NEJ TO INDATA-SW                                      
081900             END-IF                                                       
082000           ELSE                                                           
082100             IF MID-PRKURS-IN NOT = ALL '+'                               
082200               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDVALISO-INM-ATTR           
082300               MOVE NEJ TO INDATA-SW                                      
082400             END-IF                                                       
082500           END-IF                                                         
082600                                                                          
082700           IF MID-PRARTBEL-PR-INM NOT = ALL '+'                           
082800             IF MID-TIPRLIST-INM NOT = ALL '+'                            
082900               MOVE MID-TIPRLIST-INM TO DAT-I-TIDATUM                     
083000               MOVE 'AAMMDD' TO DAT-KDDATFORM                             
083100                                                                          
083200               CALL WDATKONV USING DAT-KDDATFORM                          
083300                                   DAT-I-TIDATUM                          
083400                                   DAT-O-TIDATUM                          
083500                                   DAT-KDSVAR                             
083600                                                                          
083700               IF DAT-KDSVAR-OK                                           
083800                 MOVE MFS-NUM-FAELT-RAETT TO                              
083900                      MOD-TIPRLIST-INM-ATTR                               
084000                 MOVE DAT-TIAAMMDD TO WS-TIPRLIST                         
084100                 MOVE MFS-ROER-EJ-FAELT TO MOD-TIPRLIST-INM               
084200               ELSE                                                       
084300                 MOVE MFS-NUM-FAELT-FEL TO MOD-TIPRLIST-INM-ATTR          
084400                 MOVE NEJ TO INDATA-SW                                    
084500               END-IF                                                     
084600             ELSE                                                         
084700               MOVE MFS-NUM-FAELT-FEL TO MOD-TIPRLIST-INM-ATTR            
084800               MOVE NEJ TO INDATA-SW                                      
084900             END-IF                                                       
085000                                                                          
085100             IF MID-IDLEVNR-INM NOT = ALL '+'                             
085200               MOVE MID-IDLEVNR-INM TO WS-IDLEVNR-INM                     
085300                 MOVE MFS-NUM-FAELT-RAETT TO                              
085400                      MOD-IDLEVNR-INM-ATTR                                
085500                 MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-INM                
085600             ELSE                                                         
085700               MOVE MFS-NUM-FAELT-FEL TO MOD-IDLEVNR-INM-ATTR             
085800               MOVE NEJ TO INDATA-SW                                      
085900             END-IF                                                       
086000           END-IF                                                         
086100                                                                          
086200           IF MID-TEARTNOT-IN-UT NOT = ALL '+'                            
086300            MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-IN-UT-ATTR          
086400           END-IF                                                         
086500                                                                          
086600         ELSE                                                             
086700           IF MID-KDPRBEH-INM NOT = ALL '+'                               
086800             IF MID-KDPRBEH-INM = 'N' OR 'J'                              
086900               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRBEH-INM-ATTR          
087000             ELSE                                                         
087100               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRBEH-INM-ATTR            
087200               MOVE NEJ TO  INDATA-SW                                     
087300             END-IF                                                       
087400           END-IF                                                         
087500                                                                          
087600           IF MID-TIPRLIST-INM NOT = ALL '+'                              
087700             MOVE MID-TIPRLIST-INM TO DAT-I-TIDATUM                       
087800             MOVE 'AAMMDD' TO DAT-KDDATFORM                               
087900                                                                          
088000             CALL WDATKONV USING DAT-KDDATFORM                            
088100                                 DAT-I-TIDATUM                            
088200                                 DAT-O-TIDATUM                            
088300                                 DAT-KDSVAR                               
088400                                                                          
088500             IF DAT-KDSVAR-OK                                             
088600               MOVE MFS-NUM-FAELT-RAETT TO                                
088700                    MOD-TIPRLIST-INM-ATTR                                 
088800               MOVE DAT-TIAAMMDD TO WS-TIPRLIST                           
088900               MOVE MFS-ROER-EJ-FAELT TO MOD-TIPRLIST-INM                 
089000             ELSE                                                         
089100               MOVE MFS-NUM-FAELT-FEL TO MOD-TIPRLIST-INM-ATTR            
089200               MOVE NEJ TO INDATA-SW                                      
089300             END-IF                                                       
089400           END-IF                                                         
089500                                                                          
089600           IF MID-IDLEVNR-INM NOT = ALL '+'                               
089700             MOVE MID-IDLEVNR-INM TO WS-IDLEVNR-INM                       
089800               MOVE MFS-NUM-FAELT-RAETT TO                                
089900                    MOD-IDLEVNR-INM-ATTR                                  
090000           END-IF                                                         
090100                                                                          
090200           IF MID-TEARTNOT-IN-UT NOT = ALL '+'                            
090300            MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-IN-UT-ATTR          
090400           END-IF                                                         
090500                                                                          
090600           IF MID-KDVALISO-INM NOT = ALL '+'                              
090700             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDVALISO-INM-ATTR             
090800             MOVE MFS-NUM-FAELT-FEL TO MOD-PRKURS-IN-ATTR                 
090900             MOVE NEJ TO INDATA-SW                                        
091000           END-IF                                                         
091100         END-IF                                                           
091200                                                                          
091300         IF INDATA-OK                                                     
091400           IF FORANDRING-PRIS                                             
091500              PERFORM GA-BERAKNA-NYA-PRISER                               
091600              IF WS-PRINK-KOM-FSLAG = ZERO                                
091700                MOVE 'NOLL KR SOM NYTT PRIS '                             
091800                     TO MOD-TEMFSFEL                                      
091900                PERFORM GC-ANGRA-UPPDATERING                              
092000                MOVE JA TO ANGRA-SW                                       
092100              ELSE                                                        
092200                MOVE MFS-RENSA-FAELT TO MOD-FLSVAR-UPPDAT                 
092300                MOVE MFS-OEPPNA-ALFA-FAELT TO MOD-FLSVAR-ATTR             
092400                MOVE 'SVAR' TO MOD-SVAR-VISNING                           
092500                MOVE 'KONTROLLERA PRISÄNDRING *** BEKRÄFTA J/N'           
092600                     TO MOD-TEMFSFEL                                      
092700                IF MID-FLAGGA = NEJ OR MID-FLAGGA = SPACE                 
092800                  MOVE JA TO MOD-FLAGGA                                   
092900                             NYA-PRISER-SW                                
093000                             PF11-VARNING-SW                              
093100                  MOVE MFS-ADD-LAES-IN-FAELT TO                           
093200                       MOD-PRINK-KOM-ATTR                                 
093300                  MOVE MFS-ADD-LAES-IN-FAELT TO                           
093400                       MOD-REAENDR-INK-ATTR                               
093500                  PERFORM MFS-ROER-EJ-FAELT-IN                            
093600                END-IF                                                    
093700*               MOVE INF-PRESS-PF11 TO MED-IDMFSINF                       
093800*               CALL WMEDKONV USING MED-WMEDAREA                          
093900*               MOVE MED-MFSINF TO MOD-TEMFSFEL                           
094000              END-IF                                                      
094100           ELSE                                                           
094200             MOVE '    ' TO MOD-SVAR-VISNING                              
094300             MOVE MFS-RENSA-FAELT TO MOD-FLSVAR-UPPDAT                    
094400             MOVE MFS-STAENG-FAELT TO MOD-FLSVAR-ATTR                     
094500           END-IF                                                         
094600         END-IF                                                           
094700       END-IF                                                             
094800                                                                          
094900       IF INDATA-FEL                                                      
095000         IF PF11-VARNING-NEJ                                              
095100           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
095200           CALL WMEDKONV USING MED-WMEDAREA                               
095300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
095400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
095500           PERFORM MFS-ROER-EJ-FAELT-IN                                   
095600         ELSE                                                             
095700           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
095800           CALL WMEDKONV USING MED-WMEDAREA                               
095900           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
096000           PERFORM MFS-ROER-EJ-FAELT-UT                                   
096100           PERFORM MFS-ROER-EJ-FAELT-IN                                   
096200         END-IF                                                           
096300       END-IF                                                             
096400     END-IF                                                               
096500     .                                                                    
096600     EJECT                                                                
096700 GA-BERAKNA-NYA-PRISER SECTION.                                           
096800     SKIP2                                                                
096900     PERFORM IMS-GHU-WDC601                                               
097000                                                                          
097100     IF SEGMENT-FINNS                                                     
097200       IF MID-RETULF-IN = ALL '+'                                         
097300         MOVE PRIE-ART-RETULF TO WS-RETULF                                
097400       END-IF                                                             
097500                                                                          
097600       IF MID-PRKURS-IN = ALL '+'                                         
097700         MOVE PRIE-ART-PRKURS TO WS-PRKURS                                
097800       END-IF                                                             
097900                                                                          
098000       IF MID-PRARTBEL-PR-INM = ALL '+'                                   
098100         MOVE PRIE-ART-PRARTBEL-PR TO WS-PRARTBEL-PR                      
098200       END-IF                                                             
098300                                                                          
098400******* HÄR RÄKNAS NYTT INKPRIS FRAM (SPIS) **************                
098500                                                                          
098600       COMPUTE WS-PRINK-KOM-FSLAG ROUNDED = (WS-RETULF) *                 
098700               (WS-PRKURS) * (WS-PRARTBEL-PR)                             
098800                                                                          
098900**********************************************************                
099000                                                                          
099100******* HÄR RÄKNAS NY % SATS FRAM PÅ INKPRIS *************                
099200       COMPUTE WS-REAENDR-INK-FSLAG ROUNDED =                             
099300      (100 * WS-PRINK-KOM-FSLAG / PRIE-ART-PRINK-AKT) - 100               
099400                                                                          
099500       IF WS-REAENDR-INK-FSLAG > 10000 OR                                 
099600          WS-PRINK-KOM-FSLAG = ZERO                                       
099700         MOVE 9999 TO WS-REAENDR-INK-FSLAG                                
099800       END-IF                                                             
099900**********************************************************                
100000                                                                          
100100******* HÄR RÄKNAS NYTT STDPRIS FRAM (SPIS)  *************                
100200                                                                          
100300       COMPUTE WS-STDPRIS-FSLAG ROUNDED = WS-PRINK-KOM-FSLAG +            
100400               PRIE-ART-PRDIRLON-KOM +                                    
100500               PRIE-ART-PRDMTRL-KOM +                                     
100600               PRIE-ART-PROVRPAL-KOM                                      
100700                                                                          
100800**********************************************************                
100900                                                                          
101000******* HÄR TAS AKTUELLT STDPRIS FRAM        *************                
101100                                                                          
101200       COMPUTE WS-STDPRIS-AKT ROUNDED = PRIE-ART-PRINK-AKT +              
101300               PRIE-ART-PRDIRLON-AKT +                                    
101400               PRIE-ART-PRDMTRL-AKT +                                     
101500               PRIE-ART-PROVRPAL-AKT                                      
101600                                                                          
101700**********************************************************                
101800                                                                          
101900******* HÄR TAS NY % SATS FRAM FÖR STDPRIS   *************                
102000                                                                          
102100       COMPUTE WS-REAENDR-STD-FSLAG ROUNDED =                             
102200       ( 100 * WS-STDPRIS-FSLAG / WS-STDPRIS-AKT ) - 100                  
102300                                                                          
102400       IF WS-REAENDR-STD-FSLAG > 10000 OR                                 
102500          WS-STDPRIS-FSLAG  = ZERO                                        
102600         MOVE 9999 TO WS-REAENDR-STD-FSLAG                                
102700       END-IF                                                             
102800**********************************************************                
102900                                                                          
103000     END-IF                                                               
103100     .                                                                    
103200     EJECT                                                                
103300 GB-BEHANDLA-KOM-PRIS SECTION.                                            
103400     SKIP2                                                                
103500     MOVE MID-PRINK-KOM TO DEC-IDFRIDATA                                  
103600     MOVE 7                 TO DEC-KVHELTAL                               
103700     MOVE 2                 TO DEC-KVDECIMAL                              
103800                                                                          
103900     CALL WDECEDIT USING DEC-WDECAREA                                     
104000                                                                          
104100     IF DEC-KDSVAR-OK                                                     
104200       MOVE DEC-IDEDITDATA TO WS-PRINK-BYT                                
104300       MOVE MFS-NUM-FAELT-RAETT TO MOD-PRINK-KOM-ATTR                     
104400     ELSE                                                                 
104500       MOVE MFS-NUM-FAELT-FEL TO MOD-PRINK-KOM-ATTR                       
104600       MOVE NEJ TO INDATA-SW                                              
104700     END-IF                                                               
104800                                                                          
104900     MOVE MID-REAENDR     TO DEC-IDFRIDATA                                
105000     MOVE 4                 TO DEC-KVHELTAL                               
105100     MOVE 1                 TO DEC-KVDECIMAL                              
105200                                                                          
105300     CALL WDECEDIT USING DEC-WDECAREA                                     
105400                                                                          
105500     IF DEC-KDSVAR-OK                                                     
105600       MOVE DEC-IDEDITDATA TO WS-REAENDR-BYT                              
105700       MOVE MFS-NUM-FAELT-RAETT TO MOD-REAENDR-INK-ATTR                   
105800     ELSE                                                                 
105900       MOVE MFS-NUM-FAELT-FEL TO MOD-REAENDR-INK-ATTR                     
106000       MOVE NEJ TO INDATA-SW                                              
106100     END-IF                                                               
106200                                                                          
106300     IF MID-RETULF-IN NOT = ALL '+'                                       
106400       MOVE MID-RETULF-IN TO DEC-IDFRIDATA                                
106500       MOVE 3               TO DEC-KVHELTAL                               
106600       MOVE 4               TO DEC-KVDECIMAL                              
106700                                                                          
106800       CALL WDECEDIT USING DEC-WDECAREA                                   
106900                                                                          
107000       IF DEC-KDSVAR-OK                                                   
107100         MOVE DEC-IDEDITDATA TO WS-RETULF                                 
107200         MOVE MFS-NUM-FAELT-RAETT TO MOD-RETULF-IN-ATTR                   
107300         MOVE MFS-ROER-EJ-FAELT TO MOD-RETULF-IN                          
107400       ELSE                                                               
107500         MOVE MFS-NUM-FAELT-FEL TO MOD-RETULF-IN-ATTR                     
107600         MOVE NEJ TO INDATA-SW                                            
107700       END-IF                                                             
107800     END-IF                                                               
107900                                                                          
108000     IF MID-PRKURS-IN NOT = ALL '+'                                       
108100       MOVE MID-PRKURS-IN TO DEC-IDFRIDATA                                
108200       MOVE 6               TO DEC-KVHELTAL                               
108300       MOVE 4               TO DEC-KVDECIMAL                              
108400                                                                          
108500       CALL WDECEDIT USING DEC-WDECAREA                                   
108600                                                                          
108700       IF DEC-KDSVAR-OK                                                   
108800         MOVE DEC-IDEDITDATA        TO WS-PRKURS                          
108900         MOVE MFS-NUM-FAELT-RAETT TO MOD-PRKURS-IN-ATTR                   
109000         MOVE MFS-ROER-EJ-FAELT TO MOD-PRKURS-IN                          
109100       ELSE                                                               
109200         MOVE MFS-NUM-FAELT-FEL TO MOD-PRKURS-IN-ATTR                     
109300         MOVE NEJ TO INDATA-SW                                            
109400       END-IF                                                             
109500     END-IF                                                               
109600                                                                          
109700     IF MID-PRARTBEL-PR-INM NOT = ALL '+'                                 
109800       MOVE MID-PRARTBEL-PR-INM TO DEC-IDFRIDATA                          
109900       MOVE 8                     TO DEC-KVHELTAL                         
110000       MOVE 3                     TO DEC-KVDECIMAL                        
110100                                                                          
110200       CALL WDECEDIT USING DEC-WDECAREA                                   
110300                                                                          
110400       IF DEC-KDSVAR-OK                                                   
110500         MOVE DEC-IDEDITDATA TO WS-PRARTBEL-PR                            
110600         MOVE MFS-NUM-FAELT-RAETT TO MOD-PRARTBEL-PR-ATTR                 
110700         MOVE MFS-ROER-EJ-FAELT TO MOD-PRARTBEL-PR-INM                    
110800       ELSE                                                               
110900         MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTBEL-PR-ATTR                   
111000         MOVE NEJ TO INDATA-SW                                            
111100       END-IF                                                             
111200     END-IF                                                               
111300                                                                          
111400     MOVE NEJ TO MOD-FLAGGA                                               
111500                 PF11-VARNING-SW                                          
111600     MOVE 'U' TO NYA-PRISER-SW                                            
111700     .                                                                    
111800     EJECT                                                                
111900                                                                          
112000 GC-ANGRA-UPPDATERING SECTION.                                            
112100     SKIP2                                                                
112200     PERFORM MFS-RENSA-FAELT-IN                                           
112300     MOVE '    ' TO MOD-SVAR-VISNING                                      
112400     MOVE MFS-RENSA-FAELT TO MOD-FLSVAR-UPPDAT                            
112500     MOVE MFS-STAENG-FAELT TO MOD-FLSVAR-ATTR                             
112600     .                                                                    
112700     EJECT                                                                
112800                                                                          
112900 H-UPPDATERA SECTION.                                                     
113000                                                                          
113100     IF ANGRA-UPPDAT-NEJ                                                  
113200       PERFORM IMS-GHU-WDC601                                             
113300       IF SEGMENT-FINNS                                                   
113400         IF NYA-PRISER-UPPDAT                                             
113500           MOVE NEJ TO NYA-PRISER-SW                                      
113600           IF MID-RETULF-IN NOT = ALL '+'                                 
113700             MOVE WS-RETULF TO PRIE-ART-RETULF                            
113800             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-RETULF-IN-ATTR             
113900           ELSE                                                           
114000             MOVE MFS-ROER-EJ-FAELT TO MOD-RETULF-IN                      
114100           END-IF                                                         
114200                                                                          
114300           IF MID-PRKURS-IN NOT = ALL '+'                                 
114400             MOVE WS-PRKURS TO PRIE-ART-PRKURS                            
114500             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRKURS-IN-ATTR             
114600           ELSE                                                           
114700             MOVE MFS-ROER-EJ-FAELT TO MOD-PRKURS-IN                      
114800           END-IF                                                         
114900                                                                          
115000           IF MID-PRARTBEL-PR-INM NOT = ALL '+'                           
115100             MOVE WS-PRARTBEL-PR  TO PRIE-ART-PRARTBEL-PR                 
115200             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRARTBEL-PR-ATTR           
115300           ELSE                                                           
115400             MOVE MFS-ROER-EJ-FAELT TO MOD-PRARTBEL-PR-INM                
115500           END-IF                                                         
115600                                                                          
115700           MOVE WS-PRINK-BYT        TO PRIE-ART-PRINK-KOM                 
115800           MOVE WS-REAENDR-BYT      TO PRIE-ART-REAENDR                   
115900                                                                          
116000           IF MID-KDVALISO-INM NOT = ALL '+'                              
116100             MOVE MID-KDVALISO-INM TO PRIE-ART-KDVALISO                   
116200             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDVALISO-INM-ATTR          
116300           ELSE                                                           
116400             MOVE MFS-ROER-EJ-FAELT TO MOD-KDVALISO-INM                   
116500           END-IF                                                         
116600                                                                          
116700           IF MID-KDPRBEH-INM NOT = ALL '+'                               
116800             MOVE MID-KDPRBEH-INM TO PRIE-ART-KDPRBEH                     
116900             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDPRBEH-INM-ATTR           
117000           ELSE                                                           
117100             MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRBEH-INM                    
117200           END-IF                                                         
117300                                                                          
117400           IF MID-TIPRLIST-INM NOT = ALL '+'                              
117500             MOVE MID-TIPRLIST-INM TO PRIE-ART-TIPRLIST                   
117600             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIPRLIST-INM-ATTR          
117700           ELSE                                                           
117800             MOVE MFS-ROER-EJ-FAELT TO MOD-TIPRLIST-INM                   
117900           END-IF                                                         
118000                                                                          
118100           IF MID-IDLEVNR-INM NOT = ALL '+'                               
118200             MOVE MID-IDLEVNR-INM TO PRIE-ART-IDLEVNR                     
118300             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDLEVNR-INM-ATTR           
118400           ELSE                                                           
118500             MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-INM                    
118600           END-IF                                                         
118700                                                                          
118800           IF MID-TEARTNOT-IN-UT NOT = ALL '+'                            
118900             MOVE MID-TEARTNOT-IN-UT TO PRIE-ART-TEARTNOT                 
119000             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEARTNOT-IN-UT-ATTR        
119100           ELSE                                                           
119200             MOVE MFS-ROER-EJ-FAELT TO MOD-TEARTNOT-IN-UT                 
119300           END-IF                                                         
119400                                                                          
119500         ELSE                                                             
119600                                                                          
119700           IF MID-KDPRBEH-INM NOT = ALL '+'                               
119800             MOVE MID-KDPRBEH-INM TO PRIE-ART-KDPRBEH                     
119900             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDPRBEH-INM-ATTR           
120000           ELSE                                                           
120100             MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRBEH-INM                    
120200           END-IF                                                         
120300                                                                          
120400           IF MID-TIPRLIST-INM NOT = ALL '+'                              
120500             MOVE MID-TIPRLIST-INM TO PRIE-ART-TIPRLIST                   
120600             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIPRLIST-INM-ATTR          
120700           ELSE                                                           
120800             MOVE MFS-ROER-EJ-FAELT TO MOD-TIPRLIST-INM                   
120900           END-IF                                                         
121000                                                                          
121100           IF MID-IDLEVNR-INM NOT = ALL '+'                               
121200             MOVE MID-IDLEVNR-INM TO PRIE-ART-IDLEVNR                     
121300             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDLEVNR-INM-ATTR           
121400           ELSE                                                           
121500             MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-INM                    
121600           END-IF                                                         
121700                                                                          
121800           IF MID-TEARTNOT-IN-UT NOT = ALL '+'                            
121900             MOVE MID-TEARTNOT-IN-UT TO PRIE-ART-TEARTNOT                 
122000             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEARTNOT-IN-UT-ATTR        
122100           ELSE                                                           
122200             MOVE MFS-ROER-EJ-FAELT TO MOD-TEARTNOT-IN-UT                 
122300           END-IF                                                         
122400         END-IF                                                           
122500                                                                          
122600         PERFORM IMS-REPL-WDC6                                            
122700                                                                          
122800         MOVE INF-UPDATE-DONE TO MED-IDMFSINF                             
122900         CALL WMEDKONV USING MED-WMEDAREA                                 
123000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
123100         PERFORM MFS-FORM-ATTR                                            
123200         PERFORM MFS-RENSA-FAELT-IN                                       
123300       END-IF                                                             
123400     ELSE                                                                 
123500       MOVE 'INGEN UPPDATERING UTFÖRD ' TO MOD-TEMFSINF                   
123600       PERFORM MFS-FORM-ATTR                                              
123700       PERFORM MFS-RENSA-FAELT-IN                                         
123800* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
123900     END-IF                                                               
124000     .                                                                    
124100     EJECT                                                                
124200 MFS-RENSA-FAELT-UT SECTION.                                              
124300                                                                          
124400*    --- ALLA UTDATA-FÄLT                                                 
124500*    --- INKL. BLÄDDRINGSNYCKLAR                                          
124600     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-ENTER                            
124700                             MOD-IDLEVNR-ENTER                            
124800                             MOD-KDPRBEH-UT                               
124900                             MOD-REAENDR-UT                               
125000                             MOD-FLAGGA                                   
125100                             MOD-IDPRANSV                                 
125200                             MOD-MARKNING                                 
125300                             MOD-BEART-SVE                                
125400                             MOD-KVBEHOVAR                                
125500                             MOD-PRINK-KOM-INK                            
125600                             MOD-PRINK-AKT-INK                            
125700                             MOD-REAENDR-INK                              
125800                             MOD-KVDISP-SPIS                              
125900                             MOD-PRINK-KOM-TOT                            
126000                             MOD-PRINK-AKT-TOT                            
126100                             MOD-REAENDR-TOT                              
126200                             MOD-RETULF-UT                                
126300                             MOD-PRINK-KOM-BES                            
126400                             MOD-PRARTBES                                 
126500                             MOD-PRKURS-UT                                
126600                             MOD-PRINK-KOM-SJK                            
126700                             MOD-PRARTSJK                                 
126800                             MOD-FLIART                                   
126900                             MOD-PRDIRLON-KOM                             
127000                             MOD-PRDIRLON-AKT                             
127100                             MOD-IDLEVNR-HUV                              
127200                             MOD-PROVRPAL-KOM                             
127300                             MOD-PROVRPAL-AKT                             
127400                             MOD-KDPRODSL                                 
127500                             MOD-PRDMTRL-KOM                              
127600                             MOD-PRDMTRL-AKT                              
127700                             MOD-TIPRLIST-UTM                             
127800                             MOD-IDLEVNR-UTM                              
127900                             MOD-PRARTBEL-PR-UTM                          
128000                             MOD-KDVALISO-UTM                             
128100                             MOD-KDSTASPIS                                
128200                             MOD-KDPRBEH-UTM                              
128300                             MOD-REDIRLEV                                 
128400                             MOD-FLAPC                                    
128500                             MOD-FLPRFIL                                  
128600                             MOD-TEARTNOT-IN-UT                           
128700     .                                                                    
128800     SKIP3                                                                
128900 MFS-RENSA-FAELT-IN SECTION.                                              
129000                                                                          
129100*    --- ALLA INDATA-FÄLT                                                 
129200     MOVE MFS-RENSA-FAELT TO MOD-RETULF-IN                                
129300                             MOD-PRKURS-IN                                
129400                             MOD-TIPRLIST-INM                             
129500                             MOD-IDLEVNR-INM                              
129600                             MOD-PRARTBEL-PR-INM                          
129700                             MOD-KDVALISO-INM                             
129800                             MOD-KDPRBEH-INM                              
129900                             MOD-TEARTNOT-IN-UT                           
130000     .                                                                    
130100     EJECT                                                                
130200 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
130300                                                                          
130400*    --- ALLA UTDATA-FÄLT                                                 
130500*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
130600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-ENTER                          
130700                               MOD-IDLEVNR-ENTER                          
130800                               MOD-KDPRBEH-UT                             
130900                               MOD-REAENDR-UT                             
131000                               MOD-FLAGGA                                 
131100                               MOD-IDPRANSV                               
131200                               MOD-MARKNING                               
131300                               MOD-BEART-SVE                              
131400                               MOD-KVBEHOVAR                              
131500                               MOD-PRINK-KOM-INK                          
131600                               MOD-PRINK-AKT-INK                          
131700                               MOD-REAENDR-INK                            
131800                               MOD-KVDISP-SPIS                            
131900                               MOD-PRINK-KOM-TOT                          
132000                               MOD-PRINK-AKT-TOT                          
132100                               MOD-REAENDR-TOT                            
132200                               MOD-RETULF-UT                              
132300                               MOD-PRINK-KOM-BES                          
132400                               MOD-PRARTBES                               
132500                               MOD-PRKURS-UT                              
132600                               MOD-PRINK-KOM-SJK                          
132700                               MOD-PRARTSJK                               
132800                               MOD-FLIART                                 
132900                               MOD-PRDIRLON-KOM                           
133000                               MOD-PRDIRLON-AKT                           
133100                               MOD-IDLEVNR-HUV                            
133200                               MOD-PROVRPAL-KOM                           
133300                               MOD-PROVRPAL-AKT                           
133400                               MOD-KDPRODSL                               
133500                               MOD-PRDMTRL-KOM                            
133600                               MOD-PRDMTRL-AKT                            
133700                               MOD-TIPRLIST-UTM                           
133800                               MOD-IDLEVNR-UTM                            
133900                               MOD-PRARTBEL-PR-UTM                        
134000                               MOD-KDVALISO-UTM                           
134100                               MOD-KDSTASPIS                              
134200                               MOD-KDPRBEH-UTM                            
134300                               MOD-REDIRLEV                               
134400                               MOD-FLAPC                                  
134500                               MOD-FLPRFIL                                
134600                               MOD-TEARTNOT-IN-UT                         
134700     SKIP2                                                                
134800     .                                                                    
134900     SKIP3                                                                
135000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
135100                                                                          
135200*    --- ALLA INDATA-FÄLT                                                 
135300     MOVE MFS-ROER-EJ-FAELT TO MOD-RETULF-IN                              
135400                               MOD-PRKURS-IN                              
135500                               MOD-TIPRLIST-INM                           
135600                               MOD-IDLEVNR-INM                            
135700                               MOD-PRARTBEL-PR-INM                        
135800                               MOD-KDVALISO-INM                           
135900                               MOD-KDPRBEH-INM                            
136000                               MOD-TEARTNOT-IN-UT                         
136100     .                                                                    
136200     EJECT                                                                
136300 MFS-FORM-ATTR SECTION.                                                   
136400                                                                          
136500*    --- ALLA INDATA-FÄLT                                                 
136600     MOVE MFS-FORMATETS-ATTR TO MOD-RETULF-IN-ATTR                        
136700                                MOD-PRKURS-IN-ATTR                        
136800                                MOD-TIPRLIST-INM-ATTR                     
136900                                MOD-IDLEVNR-INM-ATTR                      
137000                                MOD-PRARTBEL-PR-ATTR                      
137100                                MOD-KDVALISO-INM-ATTR                     
137200                                MOD-KDPRBEH-INM-ATTR                      
137300                                MOD-TEARTNOT-IN-UT-ATTR                   
137400                                MOD-PRINK-KOM-ATTR                        
137500                                MOD-REAENDR-INK-ATTR                      
137600     .                                                                    
137700     EJECT                                                                
137800* --- IMS SEKTIONER ---                                                   
137900     SKIP3                                                                
138000 IMS-GET-MSG SECTION.                                                     
138100                                                                          
138200     MOVE '  QC' TO GODK-STATUSKODER                                      
138300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
138400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
138500     PERFORM IMS-STATUSKONTROLL                                           
138600     .                                                                    
138700     SKIP3                                                                
138800 IMS-INSERT-MSG SECTION.                                                  
138900                                                                          
139000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
139100       MOVE '0' TO MFS-KDHUVOMR                                           
139200     END-IF                                                               
139300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
139400     MOVE SPACE TO GODK-STATUSKODER                                       
139500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
139600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
139700     PERFORM IMS-STATUSKONTROLL                                           
139800     .                                                                    
139900     EJECT                                                                
140000 IMS-GHU-WDC601 SECTION.                                                  
140100                                                                          
140200     STRING 'WLPRIE01(IDARTNR  =' W-IDARTNR-X ')'                         
140300          DELIMITED BY SIZE INTO SSA1                                     
140400     MOVE '  GE' TO GODK-STATUSKODER                                      
140500     CALL CBLTDLI USING GHU PRIE-PCB DLI-IO-AREA SSA1                     
140600     MOVE PRIE-STATUS-CODE TO STATUS-WS                                   
140700     PERFORM IMS-STATUSKONTROLL                                           
140800     .                                                                    
140900     SKIP3                                                                
141000 IMS-REPL-WDC6 SECTION.                                                   
141100                                                                          
141200     MOVE '  ' TO GODK-STATUSKODER                                        
141300     CALL CBLTDLI USING REPL PRIE-PCB DLI-IO-AREA                         
141400     MOVE PRIE-STATUS-CODE TO STATUS-WS                                   
141500     PERFORM IMS-STATUSKONTROLL                                           
141600     .                                                                    
141700     EJECT                                                                
141800 IMS-GU-WDD3 SECTION.                                                     
141900                                                                          
142000     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-WDD3-X ')'                    
142100          DELIMITED BY SIZE INTO SSA1                                     
142200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
142300          DELIMITED BY SIZE INTO SSA2                                     
142400     MOVE '  GE' TO GODK-STATUSKODER                                      
142500     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA2 SSA1 SSA2                
142600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
142700     PERFORM IMS-STATUSKONTROLL                                           
142800     .                                                                    
142900     SKIP2                                                                
143000 IMS-STATUSKONTROLL SECTION.                                              
143100                                                                          
143200     SET STATUS-IX TO 1                                                   
143300     SEARCH GODK-STATUS                                                   
143400       AT END                                                             
143500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
143600         DELIMITED BY SIZE INTO FELTEXT                                   
143700         CALL FELLOG                                                      
143800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
143900         CONTINUE                                                         
144000     END-SEARCH                                                           
144100     .                                                                    
