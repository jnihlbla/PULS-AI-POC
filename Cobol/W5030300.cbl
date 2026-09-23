000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.    W5030300.                                                 
000400 AUTHOR.        K J HANSSON.                                              
000500 DATE-WRITTEN.  MAJ 1986.                                                 
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION.                                                            
000900*        FRÅGEPROGRAM FÖR INVENTERINGEN.                                  
001000*        SAMMANSTÄLLNING AV INVENTERINGSKÖN                               
001100*        'EJ INVENTERADE ARTIKLAR', PER VOLYM-                            
001200*        VÄRDESKLASS OCH OMRÅDE.                                          
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W5T303                                              
001600*        MID:         W5I30301                                            
001700*    UTDATA.                                                              
001800*        MOD:         W5O30301                                            
001900*    SUBPROGRAM.                                                          
002000*        FELLOG                                                           
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP3                                                                
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                   PIC X(8)    VALUE 'W5030300'.                
003100 77      JA                  PIC X       VALUE 'J'.                       
003200 77      NEJ                 PIC X       VALUE 'N'.                       
003300 77      IX                  PIC S9(3)   VALUE +1        COMP-3.          
003400 77      IX1                 PIC S9(3)   VALUE ZERO      COMP-3.          
003500 77      IX2                 PIC S9(3)   VALUE ZERO      COMP-3.          
003600 77      TAB-MAX             PIC S9(3)   VALUE ZERO      COMP-3.          
003700 77      TAB-IX              PIC S9(3)   VALUE ZERO      COMP-3.          
003800 77      SUM-UTSKR           PIC S9(5)   VALUE ZERO.                      
003900 77      SUM-EJ-UTSKR        PIC S9(5)   VALUE ZERO.                      
004000 77      MAX-MOD-LAENGD      PIC S9(4)   VALUE +374  COMP SYNC.           
004100                                                                          
004200 01  DYNAMISKA-SUBPROGRAM.                                                
004300     03  CBLTDLI             PIC X(8)   VALUE 'CBLTDLI '.                 
004400     03  FELLOG              PIC X(8)   VALUE 'FELLOG  '.                 
004500                                                                          
004600 01  SPAR-FALF.                                                           
004700     03  SOK-ADLAGOMR-X      PIC X(3).                                    
004800     03  FILLER REDEFINES SOK-ADLAGOMR-X.                                 
004900         05  FILLER          PIC 9.                                       
005000         05  SOK-ADLAGOMR    PIC 9(2).                                    
005100     03  SOK-KDVVKL          PIC 9(1)   VALUE ZERO.                       
005200     EJECT                                                                
005300 01  W-WDH1A1KY-MIN.                                                      
005400     03  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                     
005500     03  FILLER              PIC X(21)   VALUE LOW-VALUE.                 
005600                                                                          
005700 01  W-WDH1A1KY-MAX.                                                      
005800     03  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                     
005900     03  FILLER              PIC X(21)   VALUE HIGH-VALUE.                
006000                                                                          
006100 01  W-IDDC-B6-X.                                                         
006200     03 W-IDDC-B6            PIC X(2).                                    
006300                                                                          
006400     EJECT                                                                
006500 01  FILLER                  PIC X(16)   VALUE ALL '1'.                   
006600                                                                          
006700 01  TABELL.                                                              
006800     03  TABELLRAD          OCCURS 250.                                   
006900         05  KDVVKL          PIC S9                      COMP-3.          
007000         05  ADLAGOMR        PIC S9(3)                   COMP-3.          
007100         05  KVANTAL         PIC S9(7)                   COMP-3.          
007200     EJECT                                                                
007300 01  INDEX-FALT.                                                          
007400                                                                          
007500     03  TYP                 PIC S9(9)   VALUE +1    COMP SYNC.           
007600                                                                          
007700*    --- VALID IDDC CODES                                                 
007800*01 -COPY WWDCKONS                                                        
007900                                                                          
008000*01  -COPY WWPRODSL                                                       
008100                                                                          
008200 01  GENERELLA-SUBPROGRAM.                                                
008300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008500*01 -COPY WMSGINIT                                                        
008600     EJECT                                                                
008700 01  FELMEDDELANDE.                                                       
008800                                                                          
008900     03  FEL1.                                                            
009000         05  FILLER PIC X(40)                                             
009100             VALUE 'INGA INVENTERINGAR FINNS UPPLAGDA      '.             
009200         05  FILLER PIC X(40)                                             
009300             VALUE 'NO STOCKTAKINGS EXISTS                 '.             
009400     03  FILLER REDEFINES FEL1.                                           
009500         05  FEL-1  PIC X(40)  OCCURS 2.                                  
009600                                                                          
009700 01  MEDDELANDE.                                                          
009800                                                                          
009900     03  MED1.                                                            
010000         05  FILLER PIC X(40)                                             
010100             VALUE 'FLER SIDOR FINNS, TRYCK PF8            '.             
010200         05  FILLER PIC X(40)                                             
010300             VALUE 'MORE PAGES EXISTS, PRESS PF8           '.             
010400     03  FILLER REDEFINES MED1.                                           
010500         05  MED-1  PIC X(40)  OCCURS 2.                                  
010600     EJECT                                                                
010700****************************************************************          
010800*                                                                         
010900*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
011000*                                                                         
011100 01      FILLER          PIC X(8)    VALUE 'MFS-WS  '.                    
011200     SKIP3                                                                
011300*01      MID -COPY W5I30301                                               
011400     EJECT                                                                
011500*01      -COPY WMSGAREA                                                   
011600     EJECT                                                                
011700*  03    W5O30301 -COPY W5O30301  -RED MSG-AREA.                          
011800     EJECT                                                                
011900*01      -COPY WMFSAREA                                                   
012000     EJECT                                                                
012100*****************************************************************         
012200*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
012300*                                                                         
012400 01      IMS-WS.                                                          
012500   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
012600     SKIP3                                                                
012700*                            *** STATUSKOD FRÅN IMS                       
012800   03    STATUS-WS       PIC XX.                                          
012900     88  SEGMENT-FINNS               VALUE '  '.                          
013000     88  SEGMENT-SAKNAS              VALUE 'GE'.                          
013100     88  BASEN-SLUT                  VALUE 'GB'.                          
013200     SKIP3                                                                
013300   03    GODK-STATUSKODER.                                                
013400     05  GODK-STATUS OCCURS 5    INDEXED BY STATUS-IX PIC XX.             
013500     SKIP3                                                                
013600 01      SSA1            PIC X(128).                                      
013700     EJECT                                                                
013800*                            *** IMS FUNKTIONSKODER                       
013900*01      -COPY W0003                                                      
014000     EJECT                                                                
014100*                            *** WDH1 AREA             *******            
014200 01  FILLER                  PIC X(16)   VALUE 'WDH1A1 AREA  '.           
014300     SKIP2                                                                
014400*01  WDH1A1    -COPY WDH1A1   -PRE INV-                                   
014500                                                                          
014600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
014700 01   DLI-IO-AREA-B601.                                                   
014800*     03  -COPY WDB601                                                    
014900                                                                          
015000     EJECT                                                                
015100 LINKAGE SECTION.                                                         
015200*01      -COPY W0009     -PRE MSG-                                        
015300    EJECT                                                                 
015400*01      -COPY W0008     -PRE USEA-                                       
015500                                                                          
015600      05 FILLER          PIC X.                                           
015700*01      -COPY W0008     -PRE INV-                                        
015800                                                                          
015900      05 FILLER          PIC X.                                           
016000*01      -COPY W0008     -PRE WDB6-                                       
016100                                                                          
016200      05 FILLER          PIC X.                                           
016300     EJECT                                                                
016400 PROCEDURE DIVISION USING MSG-PCB USEA-PCB INV-PCB WDB6-PCB.              
016500 MAIN SECTION.                                                            
016600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB INV-PCB WDB6-PCB.             
016700                                                                          
016800     PERFORM IMS-GET-MSG                                                  
016900                                                                          
017000     IF  SEGMENT-FINNS                                                    
017100       PERFORM A-INIT-SPARA-INPUT                                         
017200                                                                          
017300       PERFORM B-NOLLA-TABELLER                                           
017400       PERFORM C-LAS-BAS-LAGRA-TABELL                                     
017500                                                                          
017600       IF TAB-MAX > ZERO                                                  
017700                                                                          
017800         EVALUATE TRUE                                                    
017900           WHEN MFS-IDPFK = '8'                                           
018000             MOVE MID-ADLAGOMR-NASTA   TO SOK-ADLAGOMR                    
018100             MOVE MID-KDVVKL-NASTA     TO SOK-KDVVKL                      
018200           WHEN OTHER                                                     
018300             MOVE ZERO                 TO SOK-ADLAGOMR-X                  
018400                                          SOK-KDVVKL                      
018500         END-EVALUATE                                                     
018600                                                                          
018700         IF DCS-CDC                                                       
018800           PERFORM D-LAGG-UT-NY-SIDA                                      
018900         ELSE                                                             
019000           PERFORM DA-LAGG-UT-NY-SIDA                                     
019100         END-IF                                                           
019200       ELSE                                                               
019300         MOVE FEL-1(TYP)            TO MOD-TEMFSFEL                       
019400       END-IF                                                             
019500     END-IF                                                               
019600     MOVE MAX-MOD-LAENGD    TO MSG-KVLL                                   
019700     PERFORM IMS-ISRT-MSG                                                 
019800                                                                          
019900     MOVE ZERO TO RETURN-CODE                                             
020000     GOBACK                                                               
020100     .                                                                    
020200     EJECT                                                                
020300 A-INIT-SPARA-INPUT SECTION.                                              
020400                                                                          
020500     IF  MSG-DUBBLA-TRANSKODER                                            
020600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I30301                 
020700       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
020800       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
020900       MOVE MSG-IDPFK                     TO MFS-IDPFK                    
021000     ELSE                                                                 
021100       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W5I30301                 
021200       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
021300       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
021400       MOVE SPACE                         TO MFS-IDPFK                    
021500     END-IF                                                               
021600                                                                          
021700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
021800     MOVE '001'             TO MSGI-KDCALL                                
021900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
022000     MOVE '5303'            TO MSGI-IDTRANS                               
022100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
022200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
022300                                                                          
022400     IF MID-IDDC-IN = ALL '+'                                             
022500       MOVE MID-IDDC-UT TO W-IDDC-B6                                      
022600     ELSE                                                                 
022700       MOVE MID-IDDC-IN TO W-IDDC-B6                                      
022800     END-IF                                                               
022900     PERFORM IMS-GU-WDB601                                                
023000                                                                          
023100     IF DCS-KDDC = SPACE OR DCS-DDC                                       
023200       MOVE MSGI-IDDC      TO W-IDDC-B6                                   
023300       PERFORM IMS-GU-WDB601                                              
023400     END-IF                                                               
023500                                                                          
023600     MOVE W-IDDC-B6  TO W-IDDC-MIN                                        
023700     MOVE W-IDDC-B6  TO W-IDDC-MAX                                        
023800                                                                          
023900     MOVE LOW-VALUE          TO MSG-AREA                                  
024000     MOVE 'W5O30301'         TO MFS-IDMOD                                 
024100     MOVE '5303'             TO MOD-IDTRANS                               
024200                                                                          
024300     MOVE W-IDDC-B6  TO MOD-IDDC-UT                                       
024400     INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                  
024500                                                                          
024600     IF  ENGLISH-TEXT                                                     
024700       MOVE +2             TO TYP                                         
024800     ELSE                                                                 
024900       MOVE +1             TO TYP                                         
025000     END-IF                                                               
025100     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
025200                                                                          
025300     PERFORM MFS-RENSA-MOD-FAELT                                          
025400     .                                                                    
025500     EJECT                                                                
025600 B-NOLLA-TABELLER SECTION.                                                
025700                                                                          
025800     MOVE +1 TO IX                                                        
025900     PERFORM UNTIL IX > 250                                               
026000       MOVE ZERO              TO KDVVKL  (IX)                             
026100                                 ADLAGOMR(IX)                             
026200                                 KVANTAL (IX)                             
026300       ADD +1 TO IX                                                       
026400     END-PERFORM                                                          
026500     .                                                                    
026600     EJECT                                                                
026700 C-LAS-BAS-LAGRA-TABELL  SECTION.                                         
026800                                                                          
026900     PERFORM IMS-LAS-INV                                                  
027000     PERFORM UNTIL NOT SEGMENT-FINNS                                      
027100       IF (INV-SEQA-KDINVKAT = 50 OR 60 OR 61 OR 99)                      
027200          OR INV-SEQA-FLINVBEH  = JA                                      
027300         CONTINUE                                                         
027400       ELSE                                                               
027500         MOVE INV-SEQA-KDPRODSL  TO TEST-KDPRODSL                         
027600         IF INV-SEQA-FLINVSKR = JA                                        
027700           IF DCS-IDDC = WC-SDC-NL                                        
027800             IF (KDPRODSL-VOLVO-BYTES OR                                  
027900                 KDPRODSL-WHEELS      OR                                  
028000                 KDPRODSL-TOOLS )     AND                                 
028100                ((INV-SEQA-ADLAGOMR >= 60 AND <= 69) OR                   
028200                 (INV-SEQA-ADLAGOMR = 80 OR 81 OR 86 OR 87))              
028300               IF  MSGI-IDRT-KEY = 'ET'                                   
028400                  ADD +1   TO SUM-UTSKR                                   
028500               END-IF                                                     
028600             ELSE                                                         
028700               IF MSGI-IDRT-KEY NOT = 'ET'                                
028800                 ADD +1    TO SUM-UTSKR                                   
028900               END-IF                                                     
029000             END-IF                                                       
029100           ELSE                                                           
029200             ADD +1        TO SUM-UTSKR                                   
029300           END-IF                                                         
029400         ELSE                                                             
029500           IF DCS-IDDC = WC-SDC-NL                                        
029600             IF (KDPRODSL-VOLVO-BYTES OR                                  
029700                 KDPRODSL-WHEELS      OR                                  
029800                 KDPRODSL-TOOLS )     AND                                 
029900                ((INV-SEQA-ADLAGOMR >= 60 AND <= 69) OR                   
030000                 (INV-SEQA-ADLAGOMR = 80 OR 81 OR 86 OR 87))              
030100               IF MSGI-IDRT-KEY = 'ET'                                    
030200                 PERFORM CB-LAGG-IN-I-TABELL                              
030300                 ADD +1    TO SUM-EJ-UTSKR                                
030400               END-IF                                                     
030500             ELSE                                                         
030600               IF MSGI-IDRT-KEY NOT = 'ET'                                
030700                 PERFORM CB-LAGG-IN-I-TABELL                              
030800                 ADD +1    TO SUM-EJ-UTSKR                                
030900               END-IF                                                     
031000             END-IF                                                       
031100           ELSE                                                           
031200             IF DCS-CDC                                                   
031300               PERFORM CA-LAGG-IN-I-TABELL                                
031400             ELSE                                                         
031500               PERFORM CB-LAGG-IN-I-TABELL                                
031600             END-IF                                                       
031700             ADD +1        TO SUM-EJ-UTSKR                                
031800           END-IF                                                         
031900         END-IF                                                           
032000       END-IF                                                             
032100       PERFORM IMS-LAS-INV                                                
032200     END-PERFORM                                                          
032300     MOVE SUM-UTSKR        TO MOD-KVANTAL-UTSKR                           
032400     MOVE SUM-EJ-UTSKR     TO MOD-KVANTAL-EJUTSKR                         
032500     .                                                                    
032600     EJECT                                                                
032700 CA-LAGG-IN-I-TABELL  SECTION.                                            
032800                                                                          
032900     MOVE +1 TO TAB-IX                                                    
033000     PERFORM UNTIL (ADLAGOMR(TAB-IX) >= INV-SEQA-ADLAGOMR) OR             
033100                   (TAB-IX > TAB-MAX)                                     
033200       ADD +1 TO TAB-IX                                                   
033300     END-PERFORM                                                          
033400                                                                          
033500     PERFORM UNTIL (ADLAGOMR(TAB-IX) NOT = INV-SEQA-ADLAGOMR) OR          
033600                   (KDVVKL(TAB-IX) >= INV-SEQA-KDVVKL) OR                 
033700                   (TAB-IX > TAB-MAX)                                     
033800       ADD +1 TO TAB-IX                                                   
033900     END-PERFORM                                                          
034000                                                                          
034100     IF TAB-MAX < TAB-IX                                                  
034200**** LÄGG TILL NY TABELLRAD I SLUTET AV TABELLEN                          
034300       ADD +1                    TO TAB-MAX                               
034400       MOVE INV-SEQA-KDVVKL      TO KDVVKL  (TAB-MAX)                     
034500       MOVE INV-SEQA-ADLAGOMR    TO ADLAGOMR(TAB-MAX)                     
034600       ADD  +1                   TO KVANTAL (TAB-MAX)                     
034700                                                                          
034800     ELSE                                                                 
034900       IF KDVVKL(TAB-IX) = INV-SEQA-KDVVKL AND                            
035000       ADLAGOMR(TAB-IX) = INV-SEQA-ADLAGOMR                               
035100****  ADDERA TILL BEFINTLIG TABELLRAD                                     
035200         ADD +1                TO KVANTAL(TAB-IX)                         
035300                                                                          
035400       ELSE                                                               
035500         MOVE TAB-MAX          TO IX1                                     
035600         ADD +1                TO TAB-MAX                                 
035700         MOVE TAB-MAX          TO IX2                                     
035800         PERFORM UNTIL                                                    
035900          ( IX1 < TAB-IX )                                                
036000           MOVE KDVVKL  (IX1) TO KDVVKL  (IX2)                            
036100           MOVE ADLAGOMR(IX1) TO ADLAGOMR(IX2)                            
036200           MOVE KVANTAL (IX1) TO KVANTAL (IX2)                            
036300           SUBTRACT +1        FROM IX1                                    
036400                                   IX2                                    
036500         END-PERFORM                                                      
036600         MOVE INV-SEQA-KDVVKL   TO KDVVKL  (IX2)                          
036700         MOVE INV-SEQA-ADLAGOMR TO ADLAGOMR(IX2)                          
036800         MOVE +1                TO KVANTAL (IX2)                          
036900       END-IF                                                             
037000     END-IF                                                               
037100     .                                                                    
037200     EJECT                                                                
037300 CB-LAGG-IN-I-TABELL  SECTION.                                            
037400                                                                          
037500     MOVE +1 TO TAB-IX                                                    
037600     PERFORM UNTIL (ADLAGOMR(TAB-IX) >= INV-SEQA-ADLAGOMR) OR             
037700                   (TAB-IX > TAB-MAX)                                     
037800       ADD +1 TO TAB-IX                                                   
037900     END-PERFORM                                                          
038000                                                                          
038100     IF TAB-MAX < TAB-IX                                                  
038200**** LÄGG TILL NY TABELLRAD I SLUTET AV TABELLEN                          
038300       ADD +1                    TO TAB-MAX                               
038400       MOVE ZERO                 TO KDVVKL  (TAB-MAX)                     
038500       MOVE INV-SEQA-ADLAGOMR    TO ADLAGOMR(TAB-MAX)                     
038600       ADD  +1                   TO KVANTAL (TAB-MAX)                     
038700                                                                          
038800     ELSE                                                                 
038900       IF ADLAGOMR(TAB-IX) = INV-SEQA-ADLAGOMR                            
039000****  ADDERA TILL BEFINTLIG TABELLRAD                                     
039100         ADD +1                TO KVANTAL(TAB-IX)                         
039200                                                                          
039300       ELSE                                                               
039400         MOVE TAB-MAX          TO IX1                                     
039500         ADD +1                TO TAB-MAX                                 
039600         MOVE TAB-MAX          TO IX2                                     
039700         PERFORM UNTIL                                                    
039800          ( IX1 < TAB-IX )                                                
039900           MOVE KDVVKL  (IX1) TO KDVVKL  (IX2)                            
040000           MOVE ADLAGOMR(IX1) TO ADLAGOMR(IX2)                            
040100           MOVE KVANTAL (IX1) TO KVANTAL (IX2)                            
040200           SUBTRACT +1        FROM IX1                                    
040300                                   IX2                                    
040400         END-PERFORM                                                      
040500         MOVE ZERO              TO KDVVKL  (IX2)                          
040600         MOVE INV-SEQA-ADLAGOMR TO ADLAGOMR(IX2)                          
040700         MOVE +1                TO KVANTAL (IX2)                          
040800       END-IF                                                             
040900     END-IF                                                               
041000     .                                                                    
041100     EJECT                                                                
041200 D-LAGG-UT-NY-SIDA SECTION.                                               
041300                                                                          
041400     MOVE +1 TO IX1                                                       
041500     IF SOK-KDVVKL NUMERIC AND SOK-ADLAGOMR NUMERIC                       
041600       PERFORM UNTIL (KDVVKL(IX1) >= SOK-KDVVKL) AND                      
041700                     (ADLAGOMR(IX1) >= SOK-ADLAGOMR) OR                   
041800                     (IX1 >= TAB-MAX)                                     
041900         ADD +1 TO IX1                                                    
042000       END-PERFORM                                                        
042100     END-IF                                                               
042200     MOVE KDVVKL(IX1)           TO MOD-KDVVKL-FORSTA                      
042300     MOVE ADLAGOMR(IX1)         TO SOK-ADLAGOMR-X                         
042400     MOVE SOK-ADLAGOMR          TO MOD-ADLAGOMR-FORSTA                    
042500                                                                          
042600     MOVE +1 TO IX2                                                       
042700                                                                          
042800     PERFORM UNTIL IX2 = 28 OR IX1 > TAB-MAX                              
042900       MOVE KDVVKL  (IX1)     TO MOD-KDVVKL  (IX2)                        
043000       MOVE ADLAGOMR(IX1)     TO MOD-ADLAGOMR(IX2)                        
043100       MOVE KVANTAL (IX1)     TO MOD-KVANTAL (IX2)                        
043200            ADD +1 TO IX1                                                 
043300                      IX2                                                 
043400     END-PERFORM                                                          
043500                                                                          
043600     IF IX1 <= TAB-MAX                                                    
043700       MOVE KDVVKL  (IX1)     TO MOD-KDVVKL-NASTA                         
043800       MOVE ADLAGOMR(IX1)     TO SOK-ADLAGOMR-X                           
043900       MOVE SOK-ADLAGOMR      TO MOD-ADLAGOMR-NASTA                       
044000       MOVE MED-1(TYP)        TO MOD-TEMFSINF                             
044100     ELSE                                                                 
044200       MOVE ZERO              TO MOD-ADLAGOMR-NASTA                       
044300                                 MOD-KDVVKL-NASTA                         
044400       MOVE SPACE             TO MOD-TEMFSINF                             
044500     END-IF                                                               
044600     .                                                                    
044700     EJECT                                                                
044800 DA-LAGG-UT-NY-SIDA SECTION.                                              
044900                                                                          
045000     MOVE +1 TO IX1                                                       
045100     IF SOK-KDVVKL NUMERIC AND SOK-ADLAGOMR NUMERIC                       
045200       PERFORM UNTIL (ADLAGOMR(IX1) >= SOK-ADLAGOMR) OR                   
045300                     IX1 > TAB-MAX                                        
045400         ADD +1 TO IX1                                                    
045500       END-PERFORM                                                        
045600     END-IF                                                               
045700     MOVE ZERO                  TO MOD-KDVVKL-FORSTA                      
045800     MOVE ADLAGOMR(IX1)         TO SOK-ADLAGOMR-X                         
045900     MOVE SOK-ADLAGOMR          TO MOD-ADLAGOMR-FORSTA                    
046000                                                                          
046100     MOVE +1 TO IX2                                                       
046200     PERFORM UNTIL IX2 = 28 OR IX1 > TAB-MAX                              
046300       MOVE ZERO              TO MOD-KDVVKL  (IX2)                        
046400       MOVE ADLAGOMR(IX1)     TO MOD-ADLAGOMR(IX2)                        
046500       MOVE KVANTAL (IX1)     TO MOD-KVANTAL (IX2)                        
046600            ADD +1 TO IX1                                                 
046700                      IX2                                                 
046800     END-PERFORM                                                          
046900                                                                          
047000     IF IX1 <= TAB-MAX                                                    
047100       MOVE ZERO              TO MOD-KDVVKL-NASTA                         
047200       MOVE ADLAGOMR(IX1)     TO SOK-ADLAGOMR-X                           
047300       MOVE SOK-ADLAGOMR      TO MOD-ADLAGOMR-NASTA                       
047400       MOVE MED-1(TYP)        TO MOD-TEMFSINF                             
047500     ELSE                                                                 
047600       MOVE ZERO              TO MOD-ADLAGOMR-NASTA                       
047700                                 MOD-KDVVKL-NASTA                         
047800       MOVE SPACE             TO MOD-TEMFSINF                             
047900     END-IF                                                               
048000     .                                                                    
048100     EJECT                                                                
048200 MFS-RENSA-MOD-FAELT SECTION.                                             
048300     SKIP3                                                                
048400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
048500                             MOD-TEMFSINF                                 
048600                                                                          
048700     MOVE +1 TO IX1                                                       
048800     PERFORM UNTIL IX1 = 28                                               
048900       MOVE MFS-RENSA-FAELT TO MOD-KDVVKL  (IX1)                          
049000                               MOD-ADLAGOMR(IX1)                          
049100                               MOD-KVANTAL (IX1)                          
049200       ADD +1 TO IX1                                                      
049300     END-PERFORM                                                          
049400     .                                                                    
049500     EJECT                                                                
049600* IMS SECTIONER                                                           
049700     SKIP3                                                                
049800 IMS-GET-MSG SECTION.                                                     
049900     MOVE '  QC'             TO GODK-STATUSKODER                          
050000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
050100     MOVE MSG-STATUS-CODE    TO STATUS-WS                                 
050200     PERFORM IMS-STATUS-KONTROLL                                          
050300     .                                                                    
050400     SKIP3                                                                
050500 IMS-ISRT-MSG SECTION.                                                    
050600     IF ENGLISH-TEXT                                                      
050700       MOVE 'N' TO MFS-KDHUVOMR                                           
050800     END-IF                                                               
050900     MOVE LOW-VALUE          TO MSG-KDZ1 MSG-KDZ2                         
051000     MOVE SPACE              TO GODK-STATUSKODER                          
051100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
051200     MOVE MSG-STATUS-CODE    TO STATUS-WS                                 
051300     PERFORM IMS-STATUS-KONTROLL                                          
051400     .                                                                    
051500     EJECT                                                                
051600 IMS-LAS-INV    SECTION.                                                  
051700                                                                          
051800     STRING 'WDH1A1  (WDH1A1KY >' W-WDH1A1KY-MIN                          
051900                    '&WDH1A1KY <' W-WDH1A1KY-MAX ')'                      
052000     DELIMITED BY SIZE INTO SSA1                                          
052100     MOVE '  GE'             TO GODK-STATUSKODER                          
052200     CALL CBLTDLI USING GN INV-PCB INV-SEQA-WDH1A1 SSA1                   
052300     MOVE INV-STATUS-CODE    TO STATUS-WS                                 
052400     PERFORM IMS-STATUS-KONTROLL                                          
052500     .                                                                    
052600     EJECT                                                                
052700 IMS-GU-WDB601    SECTION.                                                
052800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
052900          DELIMITED BY SIZE INTO SSA1                                     
053000     MOVE '  GE' TO GODK-STATUSKODER                                      
053100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
053200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
053300     PERFORM IMS-STATUS-KONTROLL                                          
053400     IF SEGMENT-SAKNAS                                                    
053500         MOVE SPACE TO DCS-KDDC                                           
053600     END-IF                                                               
053700     .                                                                    
053800 IMS-STATUS-KONTROLL SECTION.                                             
053900     SET STATUS-IX TO 1                                                   
054000     SEARCH GODK-STATUS                                                   
054100       AT END                                                             
054200         CALL FELLOG                                                      
054300     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
054400       CONTINUE                                                           
054500     END-SEARCH                                                           
054600     .                                                                    
