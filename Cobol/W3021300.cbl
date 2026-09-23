000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3021300.                                                
000400 AUTHOR.         S. K.                                                    
000500     DATE-WRITTEN.   AUG   89.                                            
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        ARTIKELSTATESTIK                                                 
001100*                                                                         
001200*        SÖKNING OCH UPPDATERING AV                                       
001300*        SÄDSONGSKURVA                                                    
001400*                                                                         
001500*        COBOL II                                                         
001600*                                                                         
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W3T213                                              
002000*        MID:         W3I21301                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W3O21301                                            
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900*                                                                         
003000****************************************************************          
003100*         WORKING-STORAGE SECTION                                         
003101                                                                          
003110*    -- CHECKED BY WY2000                                                 
003200****************************************************************          
003300*                                                                         
003400 WORKING-STORAGE SECTION.                                                 
003500*                                                                         
003600 01  FILLER                  PIC X(16)        VALUE '77-OR'.              
003700*                                                                         
003800 77  PROGRAM-NAMN            PIC X(8)         VALUE 'W3021300'.           
003900 77  JA                      PIC X            VALUE 'J'.                  
004000 77  NEJ                     PIC X            VALUE 'N'.                  
004100 77  WS-IDSKURVA             PIC X(2)         VALUE SPACE.                
004200 77  WS-NUM-UPPACK           PIC S9(2)        VALUE ZERO.                 
004300 77  ANTAL-TOMMA-INFALT      PIC S9(2)        VALUE 0 COMP-3.             
004400 77  ANTAL-GODKAENDA-PUNKTER PIC S9(3)        VALUE 0 COMP-3.             
004500 77  PUNKT-SUMMA             PIC S9(3)V9(1)   VALUE 0 COMP-3.             
004600 77  PUNKT-IX                PIC S9(3)        VALUE +1 COMP-3.            
004700 77  RAD-IX                  PIC S9(3)        VALUE +1 COMP-3.            
004800 77  MAX-PUNKT-IX            PIC S9(2)        VALUE 13.                   
004900 77  MAX-RAD-IX              PIC S9(2)        VALUE 11.                   
005000 77  MAX-MOD-LAENGD          PIC S9(4)        VALUE +661 COMP-3.          
005100 77  WS-IDTRANS              PIC X(04).                                   
005200     88 EGEN-BILD                             VALUE '3213'.               
005300   EJECT                                                                  
005400*                                                                         
005500******************************************************************        
005600*          SUBPROGRAM                                                     
005700******************************************************************        
005800*                                                                         
005900 01  FILLER                  PIC X(16)   VALUE 'SUBPGM'.                  
006000*                                                                         
006100 01  SUBPGM.                                                              
006200   03  WDECEDIT              PIC X(8)    VALUE 'WDECEDIT'.                
006300   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
006400   03  FELLOG                PIC X(8)    VALUE 'FELLOG '.                 
006500   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
006600   EJECT                                                                  
006700*                                                                         
006800******************************************************************        
006900*          NYCKLAR TILL DLI                                               
007000******************************************************************        
007100*                                                                         
007200 01    FILLER                  PIC X(16) VALUE 'DLI-NYCKLAR'.             
007300*                                                                         
007400 01    NYCKLAR-TILL-DLI.                                                  
007500*                                IDSKURVA ROT                             
007600   03    W-3133-X.                                                        
007700     05    W-3133              PIC X(4)  VALUE '3133'.                    
007800     05    FILLER              PIC X(26) VALUE LOW-VALUE.                 
007900     SKIP3                                                                
008000*                                IDSKURVA SEGMENT                         
008100   03    W-IDSKURVA-X.                                                    
008200     05    W-IDSKURVA          PIC S9(3) VALUE ZERO  COMP-3.              
008300     05    FILLER              PIC X(13) VALUE LOW-VALUE.                 
008400     SKIP3                                                                
008500                                                                          
008600******************************************************************        
008700*            SWITCHAR                                                     
008800******************************************************************        
008900*                                                                         
009000 01   FILLER                   PIC X(16)   VALUE 'SWITCHAR'.              
009100*                                                                         
009200 01   SWITCHAR.                                                           
009300    03 FELFLAGGA               PIC X(01)   VALUE 'N'.                     
009400    03 ISRT-FLAGGA             PIC X(01)   VALUE 'N'.                     
009500    03 REPL-FLAGGA             PIC X(01)   VALUE 'N'.                     
009600    03 FEL-1-FLAGGA            PIC X(01)   VALUE 'N'.                     
009700                                                                          
009800   EJECT                                                                  
009900*                                                                         
010000******************************************************************        
010100*            ARBETSFÄLTT                                                  
010200******************************************************************        
010300*                                                                         
010400 01   FILLER                   PIC X(16)   VALUE 'ARBETSFÄLT'.            
010500                                                                          
010600 01  ARBETSFAELT.                                                         
010700                                                                          
010800    03 INRAD-FAELT              OCCURS 12 TIMES.                          
010900       05 WS-REFSGSIX             PIC S9(2)V9(1)   COMP-3.                
011000                                                                          
011100                                                                          
011200    03 FLYTTAL-FAELT            OCCURS 12 TIMES.                          
011300       05 FLYTTAL-REFSGSIX        PIC S9(2)V9(1)  COMP-3.                 
011400                                                                          
011500    03 DAGENS-DATUM                    PIC X(6) VALUE ZERO.               
011600                                                                          
011700*                                                                         
011800******************************************************************        
011900*            DIV SPARFÄLT                                                 
012000******************************************************************        
012100*                                                                         
012200 01   FILLER                   PIC X(16)   VALUE 'DIV SPARFÄLT'.          
012300*                                                                         
012400 01   SPARFAELT.                                                          
012500    03 SPAR-DIVERSE.                                                      
012600       05 SPAR-TEXT-IND           PIC 9(01)   VALUE ZERO.                 
012700                                                                          
012800    03 SPARA-KDSVAR            OCCURS 12 TIMES.                           
012900       05 KDSVAR-SPAR              PIC X(1).                              
013000*                                                                         
013100******************************************************************        
013200*                COPYTEXTER                                               
013300******************************************************************        
013400*                                                                         
013500                                                                          
013600 01   FILLER                PIC X(16)   VALUE 'WDECAREA'.                 
013700*01  -COPY WDECAREA.                                                      
013800*++INCLUDE WDECAREAC0                                                     
013900     EJECT                                                                
014000                                                                          
014100 01   FILLER                PIC X(16)   VALUE 'WMEDAREA'.                 
014200*01  -COPY WMEDAREA.                                                      
014300*++INCLUDE WMEDAREAC0                                                     
014400     EJECT                                                                
014500*                                                                         
014600******************************************************************        
014700*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
014800******************************************************************        
014900*                                                                         
015000 01    FILLER                 PIC X(16)   VALUE 'MIDCOPYTEXT'.            
015100     SKIP3                                                                
015200*01    MID -COPY W3I21301.                                                
015300*++INCLUDE W3I21301C0                                                     
015400     EJECT                                                                
015500 01    FILLER                 PIC X(16)   VALUE 'MSG-IO-AREA'.            
015600*01    -COPY WMSGAREA                                                     
015700*++INCLUDE WMSGAREAC0                                                     
015800     EJECT                                                                
015900*  03    MOD -COPY W3O21301  -RED MSG-AREA.                               
016000*++INCLUDE W3O21301C0                                                     
016100     EJECT                                                                
016200*01    -COPY WMFSAREA                                                     
016300*++INCLUDE WMFSAREAC0                                                     
016400     EJECT                                                                
016500******************************************************************        
016600                                                                          
016700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016800******************************************************************        
016900*                                                                         
017000 01    IMS-WS.                                                            
017100   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
017200     SKIP3                                                                
017300*                        **** STATUS-KOD FRÅN IMS                         
017400   03    STATUS-WS               PIC XX.                                  
017500     88    SEGMENT-FINNS                     VALUE '  '.                  
017600     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
017700     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
017800     SKIP3                                                                
017900   03    GODK-STATUSKODER.                                                
018000     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
018100     SKIP3                                                                
018200 01    SSA1                      PIC X(64).                               
018300 01    SSA2                      PIC X(64).                               
018400     EJECT                                                                
018500*                            IMS FUNKTIONSKODER                           
018600*01    -COPY W0003                                                        
018700 ++INCLUDE W0003CCCC0                                                     
018800     EJECT                                                                
018900*                            DLI INPUT-OUTPUT AREA                        
019000 01    DLI-IO-AREA.                                                       
019100   03    IO-AREA                 PIC X(100)  VALUE SPACE.                 
019200*  WLXXCH01 INGEN COPYTEXT - TOM ROT MÅSTE FINNAS                         
019300*  03  WLXXCH11   -COPY WDGX3134         -RED IO-AREA.                    
019400*++INCLUDE WDGX3134C0                                                     
019500     EJECT                                                                
019600 LINKAGE SECTION.                                                         
019700*01    -COPY W0009     -PRE MSG-                                          
019800 ++INCLUDE W0009CCCC0                                                     
019900     EJECT                                                                
020000*01    -COPY W0008     -PRE XXCH-                                         
020100 ++INCLUDE W0008CCCC0                                                     
020200     05  FILLER                  PIC X.                                   
020300     EJECT                                                                
020400 PROCEDURE DIVISION USING MSG-PCB XXCH-PCB.                               
020500                                                                          
020600     ENTRY 'DLITCBL' USING MSG-PCB XXCH-PCB.                              
020700                                                                          
020800 STYR SECTION.                                                            
020900     PERFORM IMS-GET-MSG                                                  
021000     IF SEGMENT-FINNS                                                     
021100        PERFORM A-INIT-SPARA-INPUT                                        
021200        PERFORM B-LAES-ROT-I-DATABAS                                      
021300        IF MFS-UPDATE                                                     
021400           PERFORM C-KONTROLLERA-INDATA                                   
021500           IF FELFLAGGA = NEJ                                             
021600              PERFORM D-UPPDATERA                                         
021700              PERFORM E-VISA-RIKTIG-BILD                                  
021800           ELSE                                                           
021900              PERFORM F-VISA-FELBILD-UPPDATERING                          
022000           END-IF                                                         
022100        ELSE                                                              
022200           PERFORM G-INRAD-KONTROLL                                       
022300           IF FELFLAGGA = NEJ                                             
022400              PERFORM E-VISA-RIKTIG-BILD                                  
022500*             PERFORM I-LAES-SEGMENT-I-DATABAS                            
022600          ELSE                                                            
022700              PERFORM H-VISA-FELBILD-SOEKNING                             
022800           END-IF                                                         
022900        END-IF                                                            
023000        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
023100        PERFORM IMS-INSERT-MSG                                            
023200     END-IF                                                               
023300     MOVE ZERO TO RETURN-CODE                                             
023400     GOBACK.                                                              
023500    EJECT                                                                 
023600                                                                          
023700 A-INIT-SPARA-INPUT SECTION.                                              
023800     IF MSG-DUBBLA-TRANSKODER                                             
023900        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I21301                
024000        MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                 
024100        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
024200        MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                          
024300        MOVE MSG-IDPFK TO MFS-IDPFK                                       
024400     ELSE                                                                 
024500        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I21301                 
024600        MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                 
024700        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
024800        MOVE ' ' TO MFS-KDTRTYP                                           
024900        MOVE ' ' TO MFS-IDPFK                                             
025000     END-IF                                                               
025100                                                                          
025200     IF MFS-IDTRANS = '3213'                                              
025300        CONTINUE                                                          
025400     ELSE                                                                 
025500        MOVE ALL '+'            TO MID-INRAD-IDSKURVA                     
025600        MOVE +1                 TO PUNKT-IX                               
025700        PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                             
025800           MOVE ALL '+'         TO MID-INRAD-REFSGSIX(PUNKT-IX)           
025900           ADD 1                TO PUNKT-IX                               
026000        END-PERFORM                                                       
026100     END-IF                                                               
026200                                                                          
026300     IF MFS-IDPFK = 7 OR MFS-IDPFK = 8                                    
026400        MOVE ' ' TO MFS-IDPFK                                             
026500     END-IF                                                               
026600                                                                          
026700     MOVE LOW-VALUE TO MSG-AREA                                           
026800     MOVE 'W3O213N1' TO MFS-IDMOD                                         
026900     MOVE '3213' TO MOD-IDTRANS                                           
027000                                                                          
027100     MOVE MFS-RENSA-FAELT            TO MOD-INRAD-IDSKURVA                
027200     MOVE +1                         TO PUNKT-IX                          
027300     PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                                
027400        MOVE MFS-RENSA-FAELT                                              
027500                   TO MOD-INRAD-REFSGSIX(PUNKT-IX)                        
027600        ADD 1                        TO PUNKT-IX                          
027700     END-PERFORM                                                          
027800                                                                          
027900     MOVE MFS-RENSA-FAELT            TO MOD-TEMFSINF                      
028000     MOVE MFS-RENSA-FAELT            TO MOD-TEMFSFEL                      
028100                                                                          
028200     ACCEPT DAGENS-DATUM              FROM DATE                           
028300     IF SWEDISH-TEXT                                                      
028400        MOVE 1                       TO SPAR-TEXT-IND                     
028500     ELSE                                                                 
028600        MOVE 2                       TO SPAR-TEXT-IND                     
028700     END-IF.                                                              
028800    EJECT                                                                 
028900                                                                          
029000                                                                          
029100 B-LAES-ROT-I-DATABAS SECTION.                                            
029200     PERFORM IMS-GU-WLXXCH01-IDSKURVA-ROT.                                
029300    EJECT                                                                 
029400                                                                          
029500                                                                          
029600 C-KONTROLLERA-INDATA SECTION.                                            
029700                                                                          
029800                                                                          
029900     PERFORM CA-TOM-INRAD-PF11-KONTROLL                                   
030000     IF FELFLAGGA = NEJ                                                   
030100        PERFORM CB-FORMELL-INDATAKONTROLL                                 
030200        PERFORM CC-UPPDATERINGSTYP-RELKOLL                                
030300     ELSE                                                                 
030400        MOVE MFS-NUM-FAELT-FEL  TO MOD-INRAD-IDSKURVA-ATTR                
030500        MOVE +1 TO PUNKT-IX                                               
030600        PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                             
030700           MOVE MFS-RENSA-FAELT                                           
030800                        TO MOD-INRAD-REFSGSIX(PUNKT-IX)                   
030900           ADD 1 TO PUNKT-IX                                              
031000        END-PERFORM                                                       
031100     END-IF.                                                              
031200    EJECT                                                                 
031300                                                                          
031400 CA-TOM-INRAD-PF11-KONTROLL SECTION.                                      
031500                                                                          
031600*          SEKTIONEN GÅR IGENOM INDATARADEN FÖR ATT TA REDA PÅ            
031700*          OM SAMTLIGA 13 INDATAFÄLT (1 IDSKURVA + 12 KURVPUNKTER)        
031800*          ÄR OIFYLLDA. ÄR INDATARADEN TOM VID PF11-TRYCKNING             
031900*          BLIR FELFLAGGA = JA. DÄREFTER VISAS SPARAD BILD                
032000*          MED MEDDEALANDE "EJ PF11 OCH TOM INDATARAD".                   
032100                                                                          
032200                                                                          
032300     MOVE 0              TO ANTAL-TOMMA-INFALT                            
032400     IF MOD-INRAD-IDSKURVA = ALL '+'                                      
032500        ADD 1                         TO ANTAL-TOMMA-INFALT               
032600     ELSE                                                                 
032700        CONTINUE                                                          
032800     END-IF                                                               
032900     MOVE +1                          TO PUNKT-IX                         
033000     PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                                
033100        IF MID-INRAD-REFSGSIX(PUNKT-IX) = ALL '+'                         
033200           ADD 1                     TO ANTAL-TOMMA-INFALT                
033300        ELSE                                                              
033400           CONTINUE                                                       
033500        END-IF                                                            
033600        ADD 1 TO PUNKT-IX                                                 
033700     END-PERFORM                                                          
033800     IF ANTAL-TOMMA-INFALT = MAX-PUNKT-IX                                 
033900        MOVE JA                     TO FELFLAGGA                          
034000        MOVE JA                     TO FEL-1-FLAGGA                       
034100        MOVE MFS-NUM-FAELT-FEL      TO MOD-INRAD-IDSKURVA-ATTR            
034200        MOVE +1                     TO PUNKT-IX                           
034300        PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                             
034400           MOVE MFS-RENSA-FAELT  TO MOD-INRAD-REFSGSIX(PUNKT-IX)          
034500           ADD 1 TO PUNKT-IX                                              
034600        END-PERFORM                                                       
034700     ELSE                                                                 
034800        CONTINUE                                                          
034900     END-IF.                                                              
035000   EJECT                                                                  
035100                                                                          
035200 CB-FORMELL-INDATAKONTROLL SECTION.                                       
035300                                                                          
035400*          SEKTIONEN KONTROLLERAR INDATARADEN OCH SLÅR PÅ                 
035500*          FELFLAGGA OM IDSKURVA ÄR OIFYYLLD ELLER OM VÄRDET I            
035600*          IDSKURVA INTE LIGGER MELLAN 1 OCH 10. FELFLAGGA                
035700*          SLÅS OCKSÅ PÅ OM NÅGON KURVPUNKT HAR ETT VÄRDE SOM             
035800*          INTE LIGGER MELLAN 0.0 OCH 99.9. OIFYLLD KURVPUNKT             
035900*          ACCEPTERAS.                                                    
036000*          VID PÅSLAGEN FELFLAGGA LÄGGS SPARAD BILD UT MED                
036100*          FELTEXT "UPPLYSTA FÄLT FEL".                                   
036200                                                                          
036300                                                                          
036400*                                             IDSKURVA                    
036500                                                                          
036600     IF MID-INRAD-IDSKURVA = ALL '+'                                      
036700         MOVE JA                  TO FELFLAGGA                            
036800         MOVE MFS-RENSA-FAELT     TO MOD-INRAD-IDSKURVA                   
036900         MOVE MFS-NUM-FAELT-FEL   TO MOD-INRAD-IDSKURVA-ATTR              
037000     ELSE                                                                 
037100                                                                          
037200        IF MID-INRAD-IDSKURVA NUMERIC                                     
037300            IF MID-INRAD-IDSKURVA > 0 AND < 11                            
037400               MOVE MID-INRAD-IDSKURVA    TO WS-IDSKURVA                  
037500               MOVE MFS-NUM-FAELT-RAETT                                   
037600                 TO MOD-INRAD-IDSKURVA-ATTR                               
037700            ELSE                                                          
037800               MOVE JA             TO FELFLAGGA                           
037900               MOVE MFS-NUM-FAELT-FEL                                     
038000                  TO MOD-INRAD-IDSKURVA-ATTR                              
038100            END-IF                                                        
038200        ELSE                                                              
038300            MOVE JA                   TO FELFLAGGA                        
038400            MOVE MFS-NUM-FAELT-FEL                                        
038500                  TO MOD-INRAD-IDSKURVA-ATTR                              
038600        END-IF                                                            
038700        MOVE MFS-ROER-EJ-FAELT                                            
038800                  TO MOD-INRAD-IDSKURVA                                   
038900     END-IF                                                               
039000                                                                          
039100*                                             KURVPUNKTER                 
039200                                                                          
039300     MOVE +1                          TO PUNKT-IX                         
039400     MOVE 0                         TO ANTAL-GODKAENDA-PUNKTER            
039500     PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                                
039600        IF MID-INRAD-REFSGSIX(PUNKT-IX) = ALL '+'                         
039700            MOVE MFS-RENSA-FAELT                                          
039800                     TO MOD-INRAD-REFSGSIX(PUNKT-IX)                      
039900            MOVE MFS-NUM-FAELT-RAETT                                      
040000                    TO MOD-INRAD-REFSGSIX-ATTR(PUNKT-IX)                  
040100        ELSE                                                              
040200           MOVE MID-INRAD-REFSGSIX(PUNKT-IX)                              
040300                                         TO DEC-IDFRIDATA                 
040400           MOVE 2             TO DEC-KVHELTAL                             
040500           MOVE 1             TO DEC-KVDECIMAL                            
040600           CALL WDECEDIT USING DEC-IDFRIDATA                              
040700                               DEC-IDEDITDATA                             
040800                               DEC-KVHELTAL                               
040900                               DEC-KVDECIMAL                              
041000                               DEC-KDSVAR                                 
041100           MOVE DEC-KDSVAR         TO KDSVAR-SPAR(PUNKT-IX)               
041200           IF DEC-KDSVAR-OK                                               
041300              MOVE DEC-IDEDITDATA                                         
041400                          TO FLYTTAL-REFSGSIX(PUNKT-IX)                   
041500               IF FLYTTAL-REFSGSIX(PUNKT-IX) > -0.1                       
041600                  MOVE MFS-NUM-FAELT-RAETT                                
041700                      TO MOD-INRAD-REFSGSIX-ATTR(PUNKT-IX)                
041800                  COMPUTE ANTAL-GODKAENDA-PUNKTER =                       
041900                          ANTAL-GODKAENDA-PUNKTER + 1                     
042000               ELSE                                                       
042100                  MOVE JA             TO FELFLAGGA                        
042200                  MOVE MFS-NUM-FAELT-FEL                                  
042300                      TO MOD-INRAD-REFSGSIX-ATTR(PUNKT-IX)                
042400               END-IF                                                     
042500           ELSE                                                           
042600               MOVE JA                   TO FELFLAGGA                     
042700               MOVE MFS-NUM-FAELT-FEL                                     
042800                     TO MOD-INRAD-REFSGSIX-ATTR(PUNKT-IX)                 
042900           END-IF                                                         
043000           MOVE MFS-ROER-EJ-FAELT                                         
043100                        TO MOD-INRAD-REFSGSIX(PUNKT-IX)                   
043200        END-IF                                                            
043300        ADD 1 TO PUNKT-IX                                                 
043400     END-PERFORM.                                                         
043500    EJECT                                                                 
043600                                                                          
043700                                                                          
043800 CC-UPPDATERINGSTYP-RELKOLL SECTION.                                      
043900                                                                          
044000                                                                          
044100*          I SEKTIONEN KONTROLLERAS INDATARADEN MED AVSEENDE PÅ           
044200*          UPPDATERINGSTYP OCH INBÖRDES RELATIONER.                       
044300*          FÖRST KONTROLLERAS HURUVIDA NÅGON IDSKURVA MED ANGIVET         
044400*          NUMMER FINNS I BASEN ELLER EJ. OM SEGMENT INTE FINNS           
044500*          (NYREGISTRERING) KONTROLLERAS ATT ALLA 12 KURVPUNKTERNA        
044600*          ÄR GODKÄNDA, DÄREFTER KONTROLLERAS ATT PUNKTERNAS              
044700*          SUMMA 100.0.                                                   
044800*          UNDERSTIGER ANTALET IFYLLDA PUNKTER 12 VISAS SPARAD            
044900*          BILD MED FELTEXT.                                              
045000*          ÄR KURVPUNKTERNAS SUMMA INTE 100.0 VISAS SPARAD BILD           
045100*          MED FELTEXT.                                                   
045200*          FINNS SEGMENT (ÄNDRING) KONTROLLERAS ATT FLER ÄN EN            
045300*          KURVPUNKT ÄR IFYLLD OCH ATT PUNKTER SOM SKALL FÖR-             
045400*          ÄNDRAS OCH PUNKTER SOM SKALL VARA KVAR TILLSAMMANS ÄR          
045500*          100.0                                                          
045600*          VID FEL VISAS SPARAD BILD, SAMMA FELTEXTER SOM VID             
045700*          NYREGISTRERING.                                                
045800                                                                          
045900                                                                          
046000     MOVE MID-INRAD-IDSKURVA          TO W-IDSKURVA                       
046100     PERFORM IMS-GHNP-WLXXCH11-HAEMTA                                     
046200     IF SEGMENT-SAKNAS                                                    
046300*                                        **NYREGISTRERING**               
046400        IF ANTAL-GODKAENDA-PUNKTER = 12                                   
046500           PERFORM CCA-FLYTTA-FLYTTAL-WS                                  
046600           PERFORM CCB-SUMMERA-PUNKTVARDE                                 
046700           IF PUNKT-SUMMA = 100.0                                         
046800              MOVE JA                 TO ISRT-FLAGGA                      
046900           ELSE                                                           
047000              MOVE JA                 TO FELFLAGGA                        
047100              MOVE '112'              TO MED-IDMFSINF                     
047200              CALL WMEDKONV           USING MED-WMEDAREA                  
047300              MOVE MED-MFSINF         TO MOD-TEMFSINF                     
047400              MOVE +1                 TO PUNKT-IX                         
047500              PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                       
047600                 MOVE MFS-NUM-FAELT-FEL                                   
047700                        TO MOD-INRAD-REFSGSIX-ATTR(PUNKT-IX)              
047800                 ADD 1 TO PUNKT-IX                                        
047900              END-PERFORM                                                 
048000           END-IF                                                         
048100        ELSE                                                              
048200           MOVE JA                     TO FELFLAGGA                       
048300           MOVE +1                     TO PUNKT-IX                        
048400           PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                          
048500              IF MID-INRAD-REFSGSIX(PUNKT-IX) = ALL '+'                   
048600                 MOVE MFS-NUM-FAELT-FEL                                   
048700                      TO MOD-INRAD-REFSGSIX-ATTR(PUNKT-IX)                
048800              END-IF                                                      
048900              ADD 1 TO PUNKT-IX                                           
049000           END-PERFORM                                                    
049100        END-IF                                                            
049200     ELSE                                                                 
049300*                                     **ÄNDRING**                         
049400        IF ANTAL-GODKAENDA-PUNKTER > 1                                    
049500           PERFORM CCC-FLYTTA-BAS-WS                                      
049600           PERFORM CCA-FLYTTA-FLYTTAL-WS                                  
049700           PERFORM CCB-SUMMERA-PUNKTVARDE                                 
049800           IF PUNKT-SUMMA = 100.0                                         
049900              MOVE JA                 TO REPL-FLAGGA                      
050000           ELSE                                                           
050100              MOVE JA                 TO FELFLAGGA                        
050200              MOVE '112'              TO MED-IDMFSINF                     
050300              CALL WMEDKONV           USING MED-WMEDAREA                  
050400              MOVE MED-MFSINF         TO MOD-TEMFSINF                     
050500              MOVE +1                 TO PUNKT-IX                         
050600              PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                       
050700                 IF MID-INRAD-REFSGSIX(PUNKT-IX)                          
050800                                  NOT = ALL '+'                           
050900                    MOVE MFS-NUM-FAELT-FEL                                
051000                        TO MOD-INRAD-REFSGSIX-ATTR(PUNKT-IX)              
051100                 END-IF                                                   
051200                 ADD 1 TO PUNKT-IX                                        
051300              END-PERFORM                                                 
051400           END-IF                                                         
051500        ELSE                                                              
051600           MOVE JA                     TO FELFLAGGA                       
051700           MOVE +1                     TO PUNKT-IX                        
051800           PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                          
051900              IF MID-INRAD-REFSGSIX(PUNKT-IX) = ALL '+'                   
052000                 MOVE MFS-NUM-FAELT-FEL                                   
052100                    TO MOD-INRAD-REFSGSIX-ATTR(PUNKT-IX)                  
052200                 MOVE MFS-RENSA-FAELT                                     
052300                        TO MOD-INRAD-REFSGSIX(PUNKT-IX)                   
052400              END-IF                                                      
052500              ADD 1 TO PUNKT-IX                                           
052600           END-PERFORM                                                    
052700        END-IF                                                            
052800     END-IF.                                                              
052900    EJECT                                                                 
053000                                                                          
053100                                                                          
053200 CCA-FLYTTA-FLYTTAL-WS SECTION.                                           
053300                                                                          
053400                                                                          
053500*          FLYTTAR KONTROLLERAT INDATA TILL WS-FÄLT.                      
053600*          VID ÄNDRING SAMMANFÖRS HÄR NYTT INDATA                         
053700*          MED DATA FRÅN BASEN, GAMMALT DATA ÖVERSKRIVES.                 
053800                                                                          
053900                                                                          
054000     MOVE MID-INRAD-IDSKURVA       TO WS-IDSKURVA                         
054100     MOVE +1                          TO PUNKT-IX                         
054200     PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                                
054300        IF MID-INRAD-REFSGSIX(PUNKT-IX)= ALL '+'                          
054400                              OR KDSVAR-SPAR(PUNKT-IX) = 'F'              
054500           CONTINUE                                                       
054600        ELSE                                                              
054700           MOVE FLYTTAL-REFSGSIX(PUNKT-IX)                                
054800                             TO WS-REFSGSIX(PUNKT-IX)                     
054900        END-IF                                                            
055000     ADD 1 TO PUNKT-IX                                                    
055100     END-PERFORM.                                                         
055200    EJECT                                                                 
055300                                                                          
055400                                                                          
055500 CCB-SUMMERA-PUNKTVARDE SECTION.                                          
055600                                                                          
055700*          SUMMERAR PUNKTVÄRDEN.                                          
055800                                                                          
055900                                                                          
056000     MOVE +1 TO PUNKT-IX                                                  
056100     PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                                
056200        COMPUTE PUNKT-SUMMA                                               
056300                = PUNKT-SUMMA + WS-REFSGSIX(PUNKT-IX)                     
056400     ADD 1 TO PUNKT-IX                                                    
056500     END-PERFORM.                                                         
056600    EJECT                                                                 
056700                                                                          
056800                                                                          
056900 CCC-FLYTTA-BAS-WS SECTION.                                               
057000                                                                          
057100*          VID ÄNDRING FLYTTTAS DATA FRÅN BASEN TILL WS-FÄLT              
057200*          FÖR KONTROLL AV ATT ÄNDRADE OCH OÄNDRADE POSTER                
057300*          TILLSAMMANS BLIR 100.0.                                        
057400                                                                          
057500                                                                          
057600     MOVE 3134-IDSKURVA               TO WS-IDSKURVA                      
057700     MOVE +1                          TO PUNKT-IX                         
057800     PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                                
057900        MOVE 3134-REFSGSIX(PUNKT-IX)                                      
058000                                TO WS-REFSGSIX(PUNKT-IX)                  
058100     ADD 1 TO PUNKT-IX                                                    
058200     END-PERFORM.                                                         
058300   EJECT                                                                  
058400                                                                          
058500                                                                          
058600                                                                          
058700 D-UPPDATERA SECTION.                                                     
058800                                                                          
058900*          BASEN UPPDATERAS OCH MEDDELANDE "IDSKURVA                      
059000*          NYREGISTREAD" ELLER "IDSKURVA ÄNDRAD" VISAS.                   
059100                                                                          
059200     IF ISRT-FLAGGA = JA                                                  
059300        PERFORM DA-FLYTTA-WS-BAS                                          
059400        PERFORM IMS-ISRT-WLXXCH11                                         
059500        MOVE '101'              TO MED-IDMFSINF                           
059600        CALL WMEDKONV           USING MED-WMEDAREA                        
059700        MOVE MED-MFSINF         TO MOD-TEMFSINF                           
059800     END-IF                                                               
059900                                                                          
060000     IF REPL-FLAGGA = JA                                                  
060100        PERFORM DA-FLYTTA-WS-BAS                                          
060200        PERFORM IMS-REPL-WLXXCH11                                         
060300        MOVE '101'              TO MED-IDMFSINF                           
060400        CALL WMEDKONV           USING MED-WMEDAREA                        
060500        MOVE MED-MFSINF         TO MOD-TEMFSINF                           
060600     END-IF                                                               
060700                                                                          
060800     PERFORM DB-BLANKA-INRAD.                                             
060900                                                                          
061000    EJECT                                                                 
061100                                                                          
061200                                                                          
061300 DA-FLYTTA-WS-BAS SECTION.                                                
061400                                                                          
061500                                                                          
061600*          FLYTTAR GODKÄNT INDATA TILL I/O AREAN.                         
061700                                                                          
061800                                                                          
061900     MOVE W-IDSKURVA           TO 3134-IDSKURVA                           
062000     MOVE LOW-VALUE            TO 3134-LOW-VALUE                          
062100     MOVE +1                   TO PUNKT-IX                                
062200     PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                                
062300        MOVE WS-REFSGSIX(PUNKT-IX)                                        
062400                  TO 3134-REFSGSIX(PUNKT-IX)                              
062500        ADD 1 TO PUNKT-IX                                                 
062600     END-PERFORM                                                          
062700     MOVE DAGENS-DATUM        TO 3134-TIUPPDAT.                           
062800    EJECT                                                                 
062900                                                                          
063000                                                                          
063100 DB-BLANKA-INRAD SECTION.                                                 
063200                                                                          
063300                                                                          
063400*          BLANKAR UT INDATARAD EFTER UTFÖRD UPPDATERING.                 
063500                                                                          
063600                                                                          
063700                                                                          
063800     MOVE MFS-RENSA-FAELT      TO MOD-INRAD-IDSKURVA                      
063900     MOVE +1                   TO PUNKT-IX                                
064000     PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                                
064100        MOVE MFS-RENSA-FAELT                                              
064200                  TO MOD-INRAD-REFSGSIX(PUNKT-IX)                         
064300        ADD 1 TO PUNKT-IX                                                 
064400     END-PERFORM.                                                         
064500    EJECT                                                                 
064600                                                                          
064700                                                                          
064800 E-VISA-RIKTIG-BILD SECTION.                                              
064900                                                                          
065000                                                                          
065100*          VISAR NYLÄST BILD EFTER SÖKNING ELLER UTFÖRD                   
065200*          UPPDATERING. VID UPPDATERING MEDDELANDE OM                     
065300*          UPPDATERINGSTYP.                                               
065400                                                                          
065500     MOVE ZERO                   TO W-IDSKURVA                            
065600     PERFORM IMS-GNP-WLXXCH11-IDSKURVA-F                                  
065700     MOVE +1 TO RAD-IX                                                    
065800     PERFORM UNTIL RAD-IX = MAX-RAD-IX OR SEGMENT-SAKNAS                  
065900        MOVE 3134-IDSKURVA TO MOD-UTRAD-IDSKURVA(RAD-IX)                  
066000        MOVE +1                   TO PUNKT-IX                             
066100        PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                             
066200           MOVE 3134-REFSGSIX(PUNKT-IX)                                   
066300                   TO MOD-UTRAD-REFSGSIX(RAD-IX, PUNKT-IX)                
066400           ADD 1 TO PUNKT-IX                                              
066500        END-PERFORM                                                       
066600        PERFORM IMS-GNP-WLXXCH11-IDSKURVA                                 
066700        ADD 1 TO RAD-IX                                                   
066800     END-PERFORM                                                          
066900     IF SEGMENT-SAKNAS                                                    
067000        PERFORM UNTIL RAD-IX = MAX-RAD-IX                                 
067100           MOVE MFS-RENSA-FAELT    TO MOD-UTRAD-IDSKURVA(RAD-IX)          
067200           MOVE +1                   TO PUNKT-IX                          
067300           PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                          
067400              MOVE MFS-RENSA-FAELT                                        
067500                   TO MOD-UTRAD-REFSGSIX(RAD-IX PUNKT-IX)                 
067600              ADD 1 TO PUNKT-IX                                           
067700           END-PERFORM                                                    
067800           ADD 1 TO RAD-IX                                                
067900        END-PERFORM                                                       
068000     END-IF.                                                              
068100  EJECT                                                                   
068200                                                                          
068300                                                                          
068400                                                                          
068500 F-VISA-FELBILD-UPPDATERING SECTION.                                      
068600                                                                          
068700*          VID ICKE GODKÄNT INDATA VISAS SPARAD BILD OCH                  
068800*          SPARAD INDATARAD MED FELAKTIGA FÄLT UPPLYSTA.                  
068900*          FELTEXT "UPPLYSTA FÄLT FEL" SAMT EV MEDDELANDEN.               
069000                                                                          
069100                                                                          
069200     IF FEL-1-FLAGGA = JA                                                 
069300        MOVE '011'               TO MED-IDMFSFEL                          
069400        CALL WMEDKONV            USING MED-WMEDAREA                       
069500        MOVE MED-MFSFEL          TO MOD-TEMFSFEL                          
069600     ELSE                                                                 
069700        MOVE '001'               TO MED-IDMFSFEL                          
069800        CALL WMEDKONV            USING MED-WMEDAREA                       
069900        MOVE MED-MFSFEL          TO MOD-TEMFSFEL                          
070000     END-IF                                                               
070100     MOVE +1 TO RAD-IX                                                    
070200     PERFORM UNTIL RAD-IX = MAX-RAD-IX                                    
070300        MOVE MFS-ROER-EJ-FAELT                                            
070400                   TO MOD-UTRAD-IDSKURVA(RAD-IX)                          
070500        MOVE +1                   TO PUNKT-IX                             
070600        PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                             
070700           MOVE MFS-ROER-EJ-FAELT                                         
070800                   TO MOD-UTRAD-REFSGSIX(RAD-IX, PUNKT-IX)                
070900           ADD 1 TO PUNKT-IX                                              
071000        END-PERFORM                                                       
071100        ADD 1 TO RAD-IX                                                   
071200     END-PERFORM.                                                         
071300                                                                          
071400   EJECT                                                                  
071500                                                                          
071600                                                                          
071700 G-INRAD-KONTROLL SECTION.                                                
071800                                                                          
071900                                                                          
072000*          KONTROLLERAR OM INDARATARAD ÄR IFYLLD VID                      
072100*          ENTERTRYCKNING, I SÅ FALL SLÅS FELFLAGGA PÅ.                   
072200                                                                          
072300                                                                          
072400     IF MID-INRAD-IDSKURVA = ALL '+'                                      
072500        CONTINUE                                                          
072600     ELSE                                                                 
072700        MOVE JA                       TO FELFLAGGA                        
072800        MOVE '003'               TO MED-IDMFSFEL                          
072900        CALL WMEDKONV            USING MED-WMEDAREA                       
073000        MOVE MED-MFSFEL          TO MOD-TEMFSFEL                          
073100     END-IF                                                               
073200     MOVE +1                          TO PUNKT-IX                         
073300     PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                                
073400        IF MID-INRAD-REFSGSIX(PUNKT-IX) = ALL '+'                         
073500           CONTINUE                                                       
073600        ELSE                                                              
073700           MOVE JA                  TO FELFLAGGA                          
073800           MOVE '003'               TO MED-IDMFSFEL                       
073900           CALL WMEDKONV            USING MED-WMEDAREA                    
074000           MOVE MED-MFSFEL          TO MOD-TEMFSFEL                       
074100        END-IF                                                            
074200        ADD 1 TO PUNKT-IX                                                 
074300     END-PERFORM.                                                         
074400    EJECT                                                                 
074500                                                                          
074600                                                                          
074700 H-VISA-FELBILD-SOEKNING SECTION.                                         
074800                                                                          
074900                                                                          
075000*          VID ENTER OCH IFYLLDA FÄLT PÅ INRADEN LÄGGS                    
075100*          SPARAD BILD UT MED FELTEXT "TRYCK PF11 FÖR                     
075200*          UPPDATERING"                                                   
075300                                                                          
075400                                                                          
075500     IF MID-INRAD-IDSKURVA = ALL '+'                                      
075600        MOVE MFS-RENSA-FAELT        TO MOD-INRAD-IDSKURVA                 
075700     ELSE                                                                 
075800        MOVE MFS-ADD-LAES-IN-FAELT                                        
075900                   TO MOD-INRAD-IDSKURVA-ATTR                             
076000        MOVE MFS-ROER-EJ-FAELT      TO MOD-INRAD-IDSKURVA                 
076100     END-IF                                                               
076200     MOVE +1                          TO PUNKT-IX                         
076300     PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                                
076400        IF MID-INRAD-REFSGSIX(PUNKT-IX) = ALL '+'                         
076500           MOVE MFS-RENSA-FAELT                                           
076600                            TO MOD-INRAD-REFSGSIX(PUNKT-IX)               
076700        ELSE                                                              
076800           MOVE MFS-ADD-LAES-IN-FAELT                                     
076900                            TO MOD-INRAD-REFSGSIX-ATTR(PUNKT-IX)          
077000           MOVE MFS-ROER-EJ-FAELT                                         
077100                            TO MOD-INRAD-REFSGSIX(PUNKT-IX)               
077200        END-IF                                                            
077300        ADD 1 TO PUNKT-IX                                                 
077400     END-PERFORM.                                                         
077500                                                                          
077600     MOVE 1 TO RAD-IX                                                     
077700     PERFORM UNTIL RAD-IX = MAX-RAD-IX                                    
077800        MOVE MFS-ROER-EJ-FAELT                                            
077900                                 TO MOD-UTRAD-IDSKURVA(RAD-IX)            
078000        MOVE +1                  TO PUNKT-IX                              
078100        PERFORM UNTIL PUNKT-IX = MAX-PUNKT-IX                             
078200           MOVE MFS-ROER-EJ-FAELT                                         
078300                   TO MOD-UTRAD-REFSGSIX(RAD-IX, PUNKT-IX)                
078400           ADD 1 TO PUNKT-IX                                              
078500        END-PERFORM                                                       
078600        ADD 1 TO RAD-IX                                                   
078700     END-PERFORM.                                                         
078800 EJECT                                                                    
078900                                                                          
079000                                                                          
079100*                                                                         
079200******************************************************************        
079300* IMS SEKTIONER                                                           
079400******************************************************************        
079500*                                                                         
079600    SKIP3                                                                 
079700 IMS-GET-MSG SECTION.                                                     
079800                                                                          
079900*          HÄMTA IN BILD                                                  
080000                                                                          
080100     MOVE '  QC' TO GODK-STATUSKODER                                      
080200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
080300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
080400     PERFORM IMS-STATUSKONTROLL.                                          
080500   EJECT                                                                  
080600                                                                          
080700 IMS-INSERT-MSG SECTION.                                                  
080800                                                                          
080900*          LÄGGA UT BILD PÅ SKÄRMEN                                       
081000                                                                          
081010     IF NOT ENGLISH-TEXT                                                  
081020       MOVE '0' TO MFS-KDHUVOMR                                           
081030     END-IF                                                               
081100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
081200     MOVE SPACE TO GODK-STATUSKODER                                       
081300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
081400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
081500     PERFORM IMS-STATUSKONTROLL.                                          
081600    EJECT                                                                 
081700                                                                          
081800                                                                          
081900 IMS-GU-WLXXCH01-IDSKURVA-ROT SECTION.                                    
082000                                                                          
082100*          SÖKA IDSKURVA ROT                                              
082200                                                                          
082300     STRING 'WLXXCH01(WDGXKEY  =' W-3133-X  ')'                           
082400            DELIMITED BY SIZE INTO SSA1                                   
082500     MOVE '  ' TO GODK-STATUSKODER                                        
082600     CALL CBLTDLI USING GU XXCH-PCB DLI-IO-AREA SSA1                      
082700     MOVE XXCH-STATUS-CODE TO STATUS-WS                                   
082800     PERFORM IMS-STATUSKONTROLL.                                          
082900   EJECT                                                                  
083000                                                                          
083100 IMS-GNP-WLXXCH11-IDSKURVA SECTION.                                       
083200                                                                          
083300*          SÖKA IDSKURVA SEGMENT                                          
083400                                                                          
083500     STRING 'WLXXCH11(WDGXKEY  >' W-IDSKURVA-X ')'                        
083600            DELIMITED BY SIZE INTO SSA1                                   
083700     MOVE '  GE' TO GODK-STATUSKODER                                      
083800     CALL CBLTDLI USING GNP XXCH-PCB DLI-IO-AREA SSA1                     
083900     MOVE XXCH-STATUS-CODE TO STATUS-WS                                   
084000     PERFORM IMS-STATUSKONTROLL.                                          
084100   EJECT                                                                  
084200                                                                          
084300 IMS-GNP-WLXXCH11-IDSKURVA-F SECTION.                                     
084400                                                                          
084500*          SÖKA IDSKURVA PÅNYTT                                           
084600                                                                          
084700     STRING 'WLXXCH11*F(WDGXKEY  >' W-IDSKURVA-X ')'                      
084800            DELIMITED BY SIZE INTO SSA1                                   
084900     MOVE '  GE' TO GODK-STATUSKODER                                      
085000     CALL CBLTDLI USING GNP XXCH-PCB DLI-IO-AREA SSA1                     
085100     MOVE XXCH-STATUS-CODE TO STATUS-WS                                   
085200     PERFORM IMS-STATUSKONTROLL.                                          
085300   EJECT                                                                  
085400                                                                          
085500 IMS-GHNP-WLXXCH11-HAEMTA SECTION.                                        
085600                                                                          
085700*          HÄMTA FÖR UPPDATERING                                          
085800                                                                          
085900     STRING 'WLXXCH11(WDGXKEY  =' W-IDSKURVA-X ')'                        
086000            DELIMITED BY SIZE INTO SSA1                                   
086100     MOVE '  GE' TO GODK-STATUSKODER                                      
086200     CALL CBLTDLI USING GHNP XXCH-PCB DLI-IO-AREA SSA1                    
086300     MOVE XXCH-STATUS-CODE TO STATUS-WS                                   
086400     PERFORM IMS-STATUSKONTROLL.                                          
086500    EJECT                                                                 
086600                                                                          
086700                                                                          
086800 IMS-ISRT-WLXXCH11 SECTION.                                               
086900                                                                          
087000*          "INSERTA"                                                      
087100                                                                          
087200     STRING 'WLXXCH01(WDGXKEY  =' W-3133-X ')'                            
087300            DELIMITED BY SIZE INTO SSA1                                   
087400     MOVE   'WLXXCH11 ' TO SSA2                                           
087500     MOVE '  ' TO GODK-STATUSKODER                                        
087600     CALL CBLTDLI USING ISRT XXCH-PCB DLI-IO-AREA SSA1 SSA2               
087700     MOVE XXCH-STATUS-CODE TO STATUS-WS                                   
087800     PERFORM IMS-STATUSKONTROLL.                                          
087900     EJECT                                                                
088000                                                                          
088100                                                                          
088200                                                                          
088300 IMS-REPL-WLXXCH11 SECTION.                                               
088400                                                                          
088500*          "REPLACA"                                                      
088600                                                                          
088700     MOVE '  ' TO GODK-STATUSKODER                                        
088800     CALL CBLTDLI USING REPL XXCH-PCB DLI-IO-AREA                         
088900     MOVE XXCH-STATUS-CODE TO STATUS-WS                                   
089000     PERFORM IMS-STATUSKONTROLL.                                          
089100     EJECT                                                                
089200                                                                          
089300                                                                          
089400 IMS-STATUSKONTROLL SECTION.                                              
089500     SKIP2                                                                
089600     SET STATUS-IX TO 1                                                   
089700     SEARCH GODK-STATUS AT END CALL FELLOG                                
089800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
089900     END-SEARCH.                                                          
090000   EJECT                                                                  
