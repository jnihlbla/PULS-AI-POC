000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1010800.                                                
000300*AUTHOR.         KENETH GOUDE.                                            
000400*DATE-COMPILED.                                                           
000500*DATE-WRITTEN.   APR 1980.                                                
000600*    FUNCTION.                                                            
000700*         TP-FRÅGE-PROGRAM                                                
000800*         CROSSINDEX PER VOLVO ART-NR.                                    
000900     SKIP2                                                                
001000*    INDATA.                                                              
001100*        TRANSAKTION: W1T108                                              
001200*        MID:         W1I10801                                            
001300*    UTDATA.                                                              
001400*        MOD:         W1O10801                                            
001500*    SUBPROGRAM.                                                          
001600*        FELLOG                                                           
001700     SKIP2                                                                
001800*                                                                         
001900*   ÄNDRINGAR:                                                            
002000*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002100*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002200*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002300*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002400*                                                                         
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM               PIC X(8)    VALUE 'W1010800'.                    
003300 77  IDARTNR-WS          PIC X(9).                                        
003400 77  WS-IDLEVNR-8        PIC X(8)    VALUE SPACE.                         
003500 77  MAX-MOD-LENGD       PIC S9(4)   VALUE +674      COMP SYNC.           
003600 77  IX                  PIC S9(9)   VALUE ZERO      COMP SYNC.           
003700 77  SPIND               PIC S9(9)   VALUE +0        COMP SYNC.           
003800 77  MAX-RADER           PIC S9(3)   VALUE +14       COMP-3.              
003900 77  JA                  PIC X       VALUE 'J'.                           
004000 77  NEJ                 PIC X       VALUE 'N'.                           
004100     SKIP2                                                                
004200 77  NYCKEL-SW           PIC X.                                           
004300     88  NYCKLAR-OK                  VALUE 'J'.                           
004400     SKIP2                                                                
004500 77  RADER-SW            PIC X.                                           
004600     88  FLER-RADER                  VALUE 'J'.                           
004700     SKIP2                                                                
004800 77  ARTIKEL-SW          PIC X.                                           
004900     88  NY-ARTIKEL                  VALUE 'J'.                           
005000     SKIP2                                                                
005100                                                                          
005200 01  DYNAMISKA-SUBPROGRAM.                                                
005300   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
005400   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
005500   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
005600                                                                          
005700 01  NYCKLAR-TILL-DLI.                                                    
005800     03  W-IDARTNR-X.                                                     
005900         05  W-IDARTNR   PIC S9(9)   VALUE ZERO COMP-3.                   
006000     03  W-IDLEVNR-X.                                                     
006100         05  W-IDLEVNR   PIC X(5)    VALUE LOW-VALUE.                     
006200     EJECT                                                                
006300 01  MEDDELANDEN.                                                         
006400   03  W-FEL-1.                                                           
006500     05  FILLER          PIC X(34)                                        
006600               VALUE 'ARTIKELN FINNS EJ I CROSS-INDEX   '.                
006700     05  FILLER          PIC X(34)                                        
006800               VALUE 'THIS ARTICLE IS NOT IN CROSS-INDEX'.                
006900   03  FILLER REDEFINES W-FEL-1.                                          
007000     05  FEL-1            PIC X(34)  OCCURS 2.                            
007100                                                                          
007200   03  W-FEL-2.                                                           
007300     05  FILLER          PIC X(26)                                        
007400               VALUE 'ARTIKELNUMRET EJ NUMERISKT'.                        
007500     05  FILLER          PIC X(26)                                        
007600               VALUE 'PARTNUMBER IS NOT NUMERIC '.                        
007700   03  FILLER REDEFINES W-FEL-2.                                          
007800     05  FEL-2           PIC X(26)   OCCURS 2.                            
007900     SKIP2                                                                
008000   03  W-MED-1.                                                           
008100     05  FILLER          PIC X(17)                                        
008200               VALUE 'FLER RADER FINNS '.                                 
008300     05  FILLER          PIC X(17)                                        
008400               VALUE 'MORE LINES       '.                                 
008500   03  FILLER REDEFINES W-MED-1.                                          
008600     05  MED-1           PIC X(17)  OCCURS 2.                             
008700                                                                          
008800   03    W-INF-1.                                                         
008900     05  FILLER        PIC X(40)                                          
009000                           VALUE 'BEGRÄNSAD INFO VISAS'.                  
009100     05  FILLER        PIC X(40)                                          
009200                           VALUE 'SECURITY RESTRICTIONS APPLIED'.         
009300   03   FILLER REDEFINES W-INF-1.                                         
009400     05  INF-1         PIC X(40) OCCURS 2.                                
009500     SKIP2                                                                
009600     EJECT                                                                
009700*                        ****    PARAMETRAR TILL W005INIT                 
009800*01  -COPY WMSGINIT                                                       
009900     EJECT                                                                
010000*                        ****    TP-AREOR                                 
010100 01  FILLER              PIC X(16)   VALUE '    TP-AREAOR   '.            
010200     SKIP2                                                                
010300*01      MID -COPY W1I10801 -PRE MID-.                                    
010400     EJECT                                                                
010500*01      -COPY WMSGAREA.                                                  
010600     EJECT                                                                
010700*  03    MOD -COPY W1O10801 -PRE MOD- -RED MSG-AREA.                      
010800     EJECT                                                                
010900*01      -COPY WMFSAREA.                                                  
011000     EJECT                                                                
011100******************************************************************        
011200*****                                                                     
011300*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011400*****                                                                     
011500 01  IMS-WS.                                                              
011600     03  FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
011700     SKIP2                                                                
011800*****                    **** STATUS-KOD FRÅN IMS                         
011900     03  STATUS-WS       PIC XX.                                          
012000         88  SEGMENT-FINNS           VALUE '  '.                          
012100         88  SEGMENT-SAKNAS          VALUE 'GE'.                          
012200     SKIP2                                                                
012300     03  GODK-STATUSKODER.                                                
012400         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
012500     SKIP2                                                                
012600 01  SSA1                PIC X(32).                                       
012700 01  SSA2                PIC X(32).                                       
012800     EJECT                                                                
012900*                        *** IMS FUNKTIONSKODER ***                       
013000*01          -COPY   W0003                                                
013100     EJECT                                                                
013200*            *** DLI INPUT-OUTPUT AREA ***                                
013300 01  DLI-IO-AREA         PIC X(150)  VALUE SPACE.                         
013400     EJECT                                                                
013500*01  WDF501     -COPY WDF501 -RED DLI-IO-AREA.                            
013600     EJECT                                                                
013700*01  WDF502     -COPY WDF502 -RED DLI-IO-AREA.                            
013800     EJECT                                                                
013900 LINKAGE SECTION.                                                         
014000*01               -COPY W0009  -PRE MSG-                                  
014100     EJECT                                                                
014200*01               -COPY W0008  -PRE USEA-.                                
014300         05  FILLER      PIC X.                                           
014400     EJECT                                                                
014500*01               -COPY W0008  -PRE WDF5-.                                
014600         05  FILLER      PIC X.                                           
014700     EJECT                                                                
014800 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
014900                                  WDF5-PCB.                               
015000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
015100                                   WDF5-PCB.                              
015200                                                                          
015300*****************************************                                 
015400**   W101AE  CROSS-INDEX         WDF5  **                                 
015500*****************************************                                 
015600                                                                          
015700     PERFORM IMS-GET-MSG                                                  
015800     IF SEGMENT-FINNS                                                     
015900       PERFORM A-KOLLA-NYCKLAR                                            
016000       IF NYCKLAR-OK                                                      
016100         PERFORM IMS-GET-WDF501-ARTIKEL                                   
016200         IF SEGMENT-FINNS                                                 
016300           IF NY-ARTIKEL                                                  
016400             PERFORM B-REDIGERA-WDF501-ARTIKEL                            
016500             PERFORM C-REDIGERA-WDF502-INFO                               
016600           ELSE                                                           
016700             PERFORM D-SPARA-GRUNDBILD                                    
016800             IF FLER-RADER                                                
016900               PERFORM C-REDIGERA-WDF502-INFO                             
017000             ELSE                                                         
017100               PERFORM E-SPARA-GAMLA-RADER                                
017200             END-IF                                                       
017300           END-IF                                                         
017400         ELSE                                                             
017500           MOVE FEL-1 (SPIND)  TO MOD-MESSAGE-RAD1                        
017600         END-IF                                                           
017700       END-IF                                                             
017800       PERFORM IMS-INSERT-MSG                                             
017900     END-IF                                                               
018000     MOVE ZERO TO RETURN-CODE                                             
018100     GOBACK                                                               
018200     CONTINUE.                                                            
018300     EJECT                                                                
018400 A-KOLLA-NYCKLAR   SECTION.                                               
018500     SKIP2                                                                
018600     MOVE JA TO NYCKEL-SW                                                 
018700     MOVE NEJ TO ARTIKEL-SW RADER-SW                                      
018800     SKIP2                                                                
018900     IF MSG-DUBBLA-TRANSKODER                                             
019000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I10801                 
019100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
019200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019300       MOVE JA TO ARTIKEL-SW                                              
019400     ELSE                                                                 
019500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I10801                  
019600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
019700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
019800       MOVE ' ' TO MFS-KDTRTYP                                            
019900     END-IF                                                               
020000     SKIP2                                                                
020100     MOVE ALL '+' TO MSGI-WMSGINIT                                        
020200     MOVE '001'             TO MSGI-KDCALL                                
020300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020500     MOVE '1108'            TO MSGI-IDTRANS                               
020600     IF MFS-IDTRANS = '1108'                                              
020700     OR (MID-IDARTNR-IN NUMERIC                                           
020800     AND MID-IDARTNR-IN > ZERO)                                           
020900         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
021000     END-IF                                                               
021100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
021200     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
021300     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
021400                                                                          
021500     IF MID-IDARTNR-IN NOT = ALL '+'                                      
021600       MOVE ZERO TO MID-RADER-SKIP                                        
021700       MOVE ' ' TO MFS-KDTRTYP                                            
021800       MOVE JA TO ARTIKEL-SW                                              
021900     END-IF                                                               
022000     SKIP2                                                                
022100     IF MFS-IDTRANS NOT = '1108'                                          
022200       MOVE ' ' TO MFS-KDTRTYP                                            
022300       MOVE JA TO ARTIKEL-SW                                              
022400       MOVE NEJ TO RADER-SW                                               
022500       MOVE ZERO TO MID-RADER-SKIP                                        
022600     END-IF                                                               
022700     SKIP2                                                                
022800     IF MID-RADER-SKIP NOT NUMERIC                                        
022900       MOVE ZERO TO MID-RADER-SKIP                                        
023000     END-IF                                                               
023100     IF MID-RADER-SKIP NOT = ZERO                                         
023200       MOVE JA TO RADER-SW                                                
023300     ELSE                                                                 
023400       MOVE JA TO ARTIKEL-SW                                              
023500     END-IF                                                               
023600     SKIP2                                                                
023700     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
023800       MOVE +1 TO SPIND                                                   
023900     ELSE                                                                 
024000       MOVE +2 TO SPIND                                                   
024100     END-IF                                                               
024200     SKIP2                                                                
024300     MOVE LOW-VALUE TO MSG-AREA                                           
024400     MOVE 'W1O108N1' TO MFS-IDMOD                                         
024500     MOVE '1108' TO MOD-IDTRANS                                           
024600                                                                          
024700     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
024800     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
024900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
025000                             MOD-MESSAGE-RAD1                             
025100                             MOD-MESSAGE-RAD23                            
025200     SKIP2                                                                
025300     IF IDARTNR-WS NOT NUMERIC                                            
025400       MOVE NEJ TO NYCKEL-SW                                              
025500       MOVE FEL-2 (SPIND) TO MOD-MESSAGE-RAD1                             
025600     ELSE                                                                 
025700       MOVE IDARTNR-WS TO W-IDARTNR                                       
025800     END-IF                                                               
025900     SKIP2                                                                
026000     MOVE MAX-MOD-LENGD TO MSG-KVLL                                       
026100     CONTINUE.                                                            
026200     EJECT                                                                
026300 B-REDIGERA-WDF501-ARTIKEL   SECTION.                                     
026400     SKIP2                                                                
026500*                                                                         
026600     MOVE XART-REKSIFFR   TO MOD-REKSIFFR                                 
026700     MOVE XART-KDFTAG     TO MOD-KDFTAG                                   
026800     MOVE XART-TIV-UPPDAT TO MOD-TIV-UPPDAT                               
026900     .                                                                    
027000     EJECT                                                                
027100 C-REDIGERA-WDF502-INFO   SECTION.                                        
027200     SKIP2                                                                
027300*                                                                         
027400     MOVE MID-RADER-SKIP TO IX                                            
027500     PERFORM IMS-GET-WDF502-LEV-NUMMER                                    
027600     PERFORM UNTIL                                                        
027700      NOT ( SEGMENT-FINNS AND IX > 0 )                                    
027800       SUBTRACT +1 FROM IX                                                
027900       PERFORM IMS-GET-WDF502-LEV-NUMMER                                  
028000     END-PERFORM                                                          
028100     IF SEGMENT-FINNS                                                     
028200       MOVE +1 TO IX                                                      
028300       PERFORM UNTIL SEGMENT-SAKNAS OR  IX > MAX-RADER                    
028400                                                                          
028500         MOVE XLEV-IDLEVNR         TO WS-IDLEVNR-8                        
028600         IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                        
028700         OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                  
028800                                                                          
028900*          -- VISAR LEV-INFO FÖR BEHÖRIG USER                             
029000           MOVE XLEV-IDLEVNR TO MOD-IDLEVNR (IX)                          
029100           MOVE XLEV-FLTLVM  TO MOD-FLTLVM (IX)                           
029200           MOVE XLEV-IDBENR  TO MOD-IDBENR (IX)                           
029300           MOVE XLEV-BELEVART TO MOD-BELEV (IX)                           
029400           ADD +1 TO IX                                                   
029500           ADD +1 TO MID-RADER-SKIP                                       
029600         ELSE                                                             
029700*          -- VISAR INTE DENNA LEV-INFO FÖR OBEHÖRIG USER                 
029800           MOVE INF-1 (SPIND)  TO MOD-MESSAGE-RAD1                        
029900         END-IF                                                           
030000                                                                          
030100         PERFORM IMS-GET-WDF502-LEV-NUMMER                                
030200       END-PERFORM                                                        
030300                                                                          
030400       IF IX = MAX-RADER + 1                                              
030500         MOVE MED-1 (SPIND) TO MOD-MESSAGE-RAD23                          
030600         MOVE MID-RADER-SKIP TO MOD-RADER-SKIP                            
030700       ELSE                                                               
030800         IF IX = +1                                                       
030900*           -- VISAR INTE DENNA LEV-INFO FÖR OBEHÖRIG USER                
031000            MOVE MFS-RENSA-FAELT TO MOD-KDFTAG                            
031100                                    MOD-TIV-UPPDAT                        
031200         END-IF                                                           
031300         MOVE ZERO TO MOD-RADER-SKIP                                      
031400       END-IF                                                             
031500     ELSE                                                                 
031600       MOVE ZERO TO MOD-RADER-SKIP                                        
031700     END-IF                                                               
031800     CONTINUE.                                                            
031900     EJECT                                                                
032000 D-SPARA-GRUNDBILD   SECTION.                                             
032100     SKIP2                                                                
032200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                             
032300                               MOD-KDFTAG                                 
032400                               MOD-TIV-UPPDAT                             
032500     CONTINUE.                                                            
032600     SKIP3                                                                
032700     SKIP3                                                                
032800 E-SPARA-GAMLA-RADER   SECTION.                                           
032900     SKIP2                                                                
033000     MOVE MFS-ROER-EJ-FAELT TO MOD-MESSAGE-RAD23                          
033100     MOVE +1 TO IX                                                        
033200     PERFORM UNTIL  IX > MAX-RADER                                        
033300       MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR (IX)                         
033400                                 MOD-FLTLVM (IX)                          
033500                                 MOD-IDBENR (IX)                          
033600                                 MOD-BELEV (IX)                           
033700       ADD +1 TO IX                                                       
033800     END-PERFORM                                                          
033900     .                                                                    
034000     EJECT                                                                
034100 IMS-GET-MSG SECTION.                                                     
034200     MOVE '  QC' TO GODK-STATUSKODER                                      
034300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
034400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
034500     PERFORM IMS-STATUS-KONTROLL                                          
034600     CONTINUE.                                                            
034700     SKIP2                                                                
034800 IMS-INSERT-MSG SECTION.                                                  
034900     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
035000       MOVE '0' TO MFS-KDHUVOMR                                           
035100     END-IF                                                               
035200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
035300     MOVE SPACE TO GODK-STATUSKODER                                       
035400     CALL CBLTDLI USING ISRT MSG-PCB                                      
035500                          MSG-IO-AREA MFS-IDMOD                           
035600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
035700     PERFORM IMS-STATUS-KONTROLL                                          
035800     CONTINUE.                                                            
035900     EJECT                                                                
036000 IMS-GET-WDF501-ARTIKEL  SECTION.                                         
036100     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
036200            DELIMITED BY SIZE INTO SSA1                                   
036300     MOVE '  GE' TO GODK-STATUSKODER                                      
036400     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA SSA1                      
036500     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
036600     PERFORM IMS-STATUS-KONTROLL                                          
036700     CONTINUE.                                                            
036800     SKIP2                                                                
036900 IMS-GET-WDF502-LEV-NUMMER  SECTION.                                      
037000     MOVE 'WDF502  ' TO SSA1                                              
037100     MOVE '  GE' TO GODK-STATUSKODER                                      
037200     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-AREA SSA1                     
037300     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
037400     PERFORM IMS-STATUS-KONTROLL                                          
037500     CONTINUE.                                                            
037600     SKIP2                                                                
037700 IMS-STATUS-KONTROLL SECTION.                                             
037800     SET STATUS-IX TO 1                                                   
037900     SEARCH GODK-STATUS                                                   
038000       AT END                                                             
038100         CALL FELLOG                                                      
038200     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
038300     END-SEARCH                                                           
038400     CONTINUE                                                             
038500            CONTINUE.                                                     
038600 IMS-STATUS-KONTROLL-EXIT. EXIT.                                          
038700     CONTINUE.                                                            
