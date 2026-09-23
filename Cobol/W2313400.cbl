000100*                  * CONVERTED BY VILMAII *                               
000200*                  * TO PURE COBOLCODE    *                               
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.                 W2313400.                                    
000500*              PROGRAM CONVERTED BY                                       
000600*              COBOL CONVERSION AID PO 5785-ABJ                           
000700*              CONVERSION DATE 05/25/91 19:49:21.                         
000800*AUTHOR.                     IDK, GÖTEBORG.                               
000900*DATE-COMPILED.                                                           
001000*DATE-WRITTEN.               MAJ 1979.                                    
001100*REMARKS.                                                                 
001200*        PROGRAMMET LÄSER OCH MATCHAR POSTFILEN W23133 OCH                
001300*        REGISTERFILEN W23135.                                            
001400*        W23133 KAN INNEHÅLLA FLERA POSTER PER ARTIKEL.                   
001500*        UTDATA ÄR NYTT REGISTER (W23135) OCH EN TRANSFIL                 
001600*        W23136. DESSA BÅDA SKRIVS SAMTIDIGT OCH SKALL                    
001700*        ALLTSÅ INNEHÅLLA LIKA MÅNGA POSTER.                              
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200*                            *** STATISTIKPOST, LAGERFÖRÄNDRING           
002300*                            *** INPUT                                    
002400     SELECT W23133  ASSIGN   UT-S-W23134D1.                               
002500*                            *** REGISTER, STAT. LAGERFÖRÄNDRING          
002600*                            *** INPUT                                    
002700     SELECT W23135I ASSIGN   UT-S-W23134D2.                               
002800*                            *** REGISTER, STAT. LAGERFÖRÄNDING           
002900*                            *** OUTPUT                                   
003000     SELECT W23135U ASSIGN   UT-S-W23134D3.                               
003100*                            *** TRANS LAGERRÖRELSER I PERIOD             
003200*                            *** OUTPUT                                   
003300     SELECT W23136  ASSIGN   UT-S-W23134D4.                               
003400     SKIP3                                                                
003500 DATA DIVISION.                                                           
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W23133                                                               
003900     RECORDING MODE F                                                     
004000     BLOCK CONTAINS 0 RECORDS.                                            
004100     SKIP3                                                                
004200*01  -COPY W231229    -PRE I33POST- -L.                                   
004400     SKIP3                                                                
004500 FD  W23135I                                                              
004600     RECORDING MODE F                                                     
004700     BLOCK CONTAINS 0 RECORDS.                                            
004800     SKIP3                                                                
004900*01  -COPY W231215    -PRE I35REG- -L.                                    
005100     SKIP3                                                                
005200 FD  W23135U                                                              
005300     RECORDING MODE F                                                     
005400     BLOCK CONTAINS 0 RECORDS.                                            
005500     SKIP3                                                                
005600*01  POST   -COPY W231215    -PRE U35REG- -L.                             
005800     SKIP3                                                                
005900 FD  W23136                                                               
006000     RECORDING MODE F                                                     
006100     BLOCK CONTAINS 0 RECORDS.                                            
006200     SKIP3                                                                
006300*01  POST   -COPY W231214    -PRE U36TR- -L.                              
006500     EJECT                                                                
006600 WORKING-STORAGE SECTION.                                                 
006610                                                                          
006700*    -- CHECKED BY WY2000                                                 
006800 77  INDENT-I PIC X(40) VALUE                                             
006900     'W2313400 91/05/25 TIME 12.54 VILMAII'.                              
007000***  STATEMENT ABOVE GENERATED BY VILMAII CONVERTER                       
007100*                                                                         
007200     SKIP3                                                                
007300 01  W.                                                                   
007400*----------------------------------------- KONSTANTER                     
007500     03  JA              PIC X       VALUE 'J'.                           
007600     03  NEJ             PIC X       VALUE 'N'.                           
007700     03  MAX-ANT-PERIODER                                                 
007800                         PIC S9(9)   VALUE +12       COMP SYNC.           
007900     03  MAX-ANT-CLAGER  PIC S9(9)   VALUE +2        COMP SYNC.           
008000     SKIP1                                                                
008100*----------------------------------------- ARBETSAREOR                    
008200 01  W.                                                                   
008400     03  IX                  PIC S9(9)               COMP-3.              
008500     03  IY                  PIC S9(9)               COMP-3.              
009200     03  W-PERIOD-AARP       PIC 9(4).                                    
009300     03  W-PER               REDEFINES W-PERIOD-AARP.                     
009400         05  W-PERIOD-AA     PIC 9(2).                                    
009500         05  W-PERIOD-RP     PIC 9(2).                                    
009600     SKIP1                                                                
009700     03  W-PERIOD-AKTUELL-AARP                                            
009800                             PIC S9(5)               COMP-3.              
009900     SKIP3                                                                
009910 01  WS-DAGENS-AAVV          PIC 9(4).                                    
009920 01  FILLER REDEFINES WS-DAGENS-AAVV.                                     
009930     03 WS-DAGENS-AA         PIC 9(2).                                    
009940     03 WS-DAGENS-VV         PIC 9(2).                                    
009950     SKIP3                                                                
010000 01  SWITCHAR.                                                            
010100     03  SW-ALLA-ANTAL-ZERO  PIC X(1)    VALUE 'J'.                       
010200     SKIP1                                                                
010300 01  EOF-SWITCHAR.                                                        
010400     03  W23133-EOF          PIC X(1)    VALUE 'N'.                       
010500     03  W23135-EOF          PIC X(1)    VALUE 'N'.                       
010600     SKIP3                                                                
010700 01  DYNAMISKA-SUBPROGRAM.                                                
010800     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
010900     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
011000     03  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
011010     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
011100     EJECT                                                                
011200*                            *** PARAMETRAR TILL POSTSUM                  
011300     SKIP3                                                                
011400*01  -COPY W0005        -PRE POSTSUM-.                                    
011600     EJECT                                                                
011700*                            *** PARAMETRAR TILL DATUMKORT                
011800 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W23134'.                  
011900 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
012000     SKIP3                                                                
012100*01  -COPY WDATKORT.                                                      
012300     EJECT                                                                
012320*03  -COPY WDATAREA                                                       
012330     EJECT                                                                
012400*                            *************************************        
012500*                            *** STAT.POST LAGERFÖRÄNDRING     ***        
012600*                            *************************************        
012700     SKIP1                                                                
012800 01  POST-AREA.                                                           
012900     03  POST-IDPTYP         PIC X(3).                                    
013000     03  POST-ID             PIC X(5).                                    
013100     03  FILLER              PIC X(8).                                    
013200     SKIP3                                                                
013300*01  AREA  -COPY W231229    -PRE I33POST-  -RED POST-AREA.                
013500     EJECT                                                                
013600*                            *************************************        
013700*                            *** STATISTIK LAGERFÖRÄNDRING  IN ***        
013800*                            *************************************        
013900     SKIP1                                                                
014000 01  REGIN-AREA.                                                          
014100     03  REGIN-IDPTYP        PIC X(3).                                    
014200     03  REGIN-ID            PIC X(5).                                    
014300     03  FILLER              PIC X(208).                                  
014400     SKIP3                                                                
014500*01  AREA  -COPY W231215    -PRE I35REG-  -RED REGIN-AREA.                
014700     EJECT                                                                
014800*                            *************************************        
014900*                            *** STATISTIK LAGERFÖRÄNDRING  UT ***        
015000*                            *************************************        
015100     SKIP1                                                                
015200 01  REGUT-AREA.                                                          
015300     03  REGUT-IDPTYP        PIC X(3).                                    
015400     03  REGUT-ID            PIC X(5).                                    
015500     03  FILLER              PIC X(208).                                  
015600     SKIP3                                                                
015700*01  AREA  -COPY W231215    -PRE U35REG-  -RED REGUT-AREA.                
015900     EJECT                                                                
016000*                            *************************************        
016100*                            *** TRANS LAGERRÖRELSE I PERIOD   ***        
016200*                            *************************************        
016300     SKIP1                                                                
016400*01  AREA  -COPY W231214    -PRE U36TR-.                                  
016600     EJECT                                                                
016700 PROCEDURE DIVISION.                                                      
016800     SKIP3                                                                
016900     PERFORM A-INITIERA                                                   
017000     PERFORM S01-LAES-JUST-REG-W23135                                     
017100     PERFORM S03-LAES-POST-W23133                                         
017200     SKIP1                                                                
017300     PERFORM UNTIL                                                        
017400      NOT ( W23133-EOF = NEJ OR W23135-EOF = NEJ )                        
017500       SKIP1                                                              
017600       IF  POST-ID < REGIN-ID                                             
017700         SKIP1                                                            
017800         PERFORM B-INIT-UTPOSTER-FRAN-POST                                
017900         PERFORM UNTIL                                                    
018000          NOT ( POST-ID = REGUT-ID )                                      
018100           PERFORM D-UPPDAT-UTPOSTER                                      
018200           PERFORM S03-LAES-POST-W23133                                   
018300         END-PERFORM                                                      
018400         PERFORM E-BERAKNA-SUMMAFAELT-I-POST                              
018500         PERFORM F-ANTALSTEST                                             
018600         IF  SW-ALLA-ANTAL-ZERO = NEJ                                     
018700           PERFORM S04-SKRIV-UTPOSTER                                     
018800         END-IF                                                           
018900       ELSE                                                               
019000         SKIP1                                                            
019100         IF POST-ID = REGIN-ID                                            
019200           SKIP1                                                          
019300           PERFORM C-INIT-UTPOSTER-FRAN-REG                               
019400           PERFORM UNTIL                                                  
019500            NOT ( POST-ID = REGUT-ID )                                    
019600             PERFORM D-UPPDAT-UTPOSTER                                    
019700             PERFORM S03-LAES-POST-W23133                                 
019800           END-PERFORM                                                    
019900           PERFORM E-BERAKNA-SUMMAFAELT-I-POST                            
020000           PERFORM F-ANTALSTEST                                           
020100           IF  SW-ALLA-ANTAL-ZERO = NEJ                                   
020200             PERFORM S04-SKRIV-UTPOSTER                                   
020300           END-IF                                                         
020400           PERFORM S01-LAES-JUST-REG-W23135                               
020500         ELSE                                                             
020600           SKIP1                                                          
020700           IF  POST-ID > REGIN-ID                                         
020800             SKIP1                                                        
020900             PERFORM C-INIT-UTPOSTER-FRAN-REG                             
021000             PERFORM E-BERAKNA-SUMMAFAELT-I-POST                          
021100             PERFORM F-ANTALSTEST                                         
021200             IF  SW-ALLA-ANTAL-ZERO = NEJ                                 
021300               PERFORM S04-SKRIV-UTPOSTER                                 
021400             END-IF                                                       
021500             PERFORM S01-LAES-JUST-REG-W23135                             
021600           END-IF                                                         
021700         END-IF                                                           
021800       END-IF                                                             
021900     END-PERFORM                                                          
022000     SKIP1                                                                
022100     PERFORM Z-AVSLUTA                                                    
022200     MOVE ZERO TO RETURN-CODE                                             
022300     GOBACK                                                               
022400     CONTINUE.                                                            
022500     EJECT                                                                
022600 A-INITIERA SECTION.                                                      
022700                                                                          
022800     OPEN INPUT  W23133                                                   
022900                 W23135I                                                  
023000          OUTPUT W23135U                                                  
023100                 W23136                                                   
023200                                                                          
023300     MOVE '214' TO U36TR-IDPTYP                                           
023400     MOVE '215' TO U35REG-IDPTYP                                          
023500     MOVE 'W23134' TO POSTSUM-PROGNAMN                                    
023501                                                                          
023700     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
023910     MOVE D-AAR   TO  WS-DAGENS-AA                                        
023920     MOVE D-VECKA TO  WS-DAGENS-VV                                        
023921                                                                          
023922     MOVE 'AAVV  '       TO DAT-KDDATFORM                                 
023923     MOVE WS-DAGENS-AAVV TO DAT-I-TIDATUM                                 
023924     CALL WDATKONV USING DAT-KDDATFORM                                    
023925                         DAT-I-TIDATUM                                    
023926                         DAT-O-TIDATUM                                    
023927                         DAT-KDSVAR                                       
023928     MOVE DAT-TIAARP     TO W-PERIOD-AKTUELL-AARP                         
023929     .                                                                    
024200     EJECT                                                                
024300 B-INIT-UTPOSTER-FRAN-POST SECTION.                                       
024400                                                                          
024500     MOVE I33POST-IDARTNR TO U35REG-IDARTNR                               
024600                             U36TR-IDARTNR                                
024700                                                                          
024800     MOVE W-PERIOD-AKTUELL-AARP TO U35REG-TIAARP(1)                       
024900     MOVE 1 TO IX                                                         
025000     PERFORM UNTIL (IX > MAX-ANT-PERIODER)                                
025200       MOVE 1 TO IY                                                       
025300       PERFORM UNTIL                                                      
025400        ( IY > MAX-ANT-CLAGER )                                           
025500         MOVE ZERO TO U35REG-KVANTAL-UTLEV (IX, IY)                       
025600                              U35REG-KVANTAL-TILL  (IX, IY)               
025700                              U35REG-KVANTAL-FRAN  (IX, IY)               
025800         ADD 1 TO IY                                                      
025900       END-PERFORM                                                        
026000                                                                          
026100       IF  IX < MAX-ANT-PERIODER                                          
026200         MOVE U35REG-TIAARP(IX) TO W-PERIOD-AARP                          
026300         SUBTRACT 1 FROM W-PERIOD-AARP                                    
026400         IF  W-PERIOD-RP < 1                                              
026500           MOVE 12 TO W-PERIOD-RP                                         
026600           SUBTRACT 1 FROM W-PERIOD-AA                                    
026700         END-IF                                                           
026800         ADD 1 TO IX                                                      
026900         MOVE W-PERIOD-AARP TO U35REG-TIAARP(IX)                          
027000       ELSE                                                               
027100         ADD 1 TO IX                                                      
027200       END-IF                                                             
027300     END-PERFORM                                                          
027400                                                                          
027500     PERFORM S05-INIT-U36TR                                               
027600     .                                                                    
027700     EJECT                                                                
027800 C-INIT-UTPOSTER-FRAN-REG SECTION.                                        
027900     SKIP1                                                                
028000     MOVE I35REG-AREA TO U35REG-AREA                                      
028100     SKIP1                                                                
028200     MOVE I35REG-IDARTNR TO U36TR-IDARTNR                                 
028300     PERFORM S05-INIT-U36TR                                               
028400     CONTINUE.                                                            
028500     EJECT                                                                
028600 D-UPPDAT-UTPOSTER SECTION.                                               
028700******************************************************************        
028800*                                                                *        
028900*   UPPDATERING AV REGISTER W23135 OCH TRANS W23136              *        
029000*                                                                *        
029100******************************************************************        
029200     SKIP1                                                                
029300     MOVE I33POST-KDCLAGER TO IX                                          
029400     SKIP1                                                                
029500     IF  I33POST-FRAN-LAGER                                               
029600       IF I33POST-KUNDORDER OR I33POST-SKROTNING                          
029700         ADD I33POST-KVANTAL TO U35REG-KVANTAL-FRAN (1, IX)               
029800         ADD I33POST-KVANTAL                                              
029900                           TO U35REG-KVANTAL-UTLEV (1, IX)                
030000       END-IF                                                             
030100     ELSE                                                                 
030200       ADD I33POST-KVANTAL TO U35REG-KVANTAL-TILL (1, IX)                 
030300     END-IF                                                               
030400     SKIP3                                                                
030500     IF I33POST-CLEARING                                                  
030600       IF  I33POST-FRAN-LAGER                                             
030700         ADD I33POST-KVANTAL TO U36TR-KVANTAL-UTCLEAR (IX)                
030800       ELSE                                                               
030900         ADD I33POST-KVANTAL TO U36TR-KVANTAL-INCLEAR (IX)                
031000       END-IF                                                             
031100     ELSE                                                                 
031200       IF  I33POST-INVENTERING                                            
031300         IF  I33POST-FRAN-LAGER                                           
031400           ADD I33POST-KVANTAL TO U36TR-KVANTAL-NEDINV (IX)               
031500         ELSE                                                             
031600           ADD I33POST-KVANTAL TO U36TR-KVANTAL-UPPINV (IX)               
031700         END-IF                                                           
031800       ELSE                                                               
031900         IF I33POST-PLAN-INLEV                                            
032000           ADD I33POST-KVANTAL TO U36TR-KVANTAL-PLANINL  (IX)             
032100         ELSE                                                             
032200           IF  I33POST-OPLAN-INLEV                                        
032300             ADD I33POST-KVANTAL TO U36TR-KVANTAL-OPLANINL (IX)           
032400           ELSE                                                           
032500             IF  I33POST-LAN-FR-LAGER                                     
032600               ADD I33POST-KVANTAL TO U36TR-KVANTAL-LAN (IX)              
032700             ELSE                                                         
032800               IF  I33POST-RETUR-FR-LAGER                                 
032900                 ADD I33POST-KVANTAL TO U36TR-KVANTAL-RETUR (IX)          
033000               ELSE                                                       
033100                 IF I33POST-SKROTNING                                     
033200                   ADD I33POST-KVANTAL TO U36TR-KVANTAL-SKROT             
033300                   (IX)                                                   
033400                 ELSE                                                     
033500                   IF  I33POST-KUNDORDER                                  
033600                     ADD I33POST-KVANTAL                                  
033700                     TO U36TR-KVANTAL-K-ORDER (IX)                        
033800                   END-IF                                                 
033900                 END-IF                                                   
034000               END-IF                                                     
034100             END-IF                                                       
034200           END-IF                                                         
034300         END-IF                                                           
034400       END-IF                                                             
034500     END-IF                                                               
034600     CONTINUE.                                                            
034700     EJECT                                                                
034800 E-BERAKNA-SUMMAFAELT-I-POST SECTION.                                     
034900     SKIP3                                                                
035000     MOVE 1 TO IX                                                         
035100     PERFORM UNTIL                                                        
035200      NOT ( IX < MAX-ANT-CLAGER )                                         
035300       MOVE ZERO TO U36TR-KVANTAL-UTLEV-12P (IX)                          
035400                        U36TR-KVANTAL-TILL-AR (IX)                        
035500                        U36TR-KVANTAL-FRAN-12P (IX)                       
035600                        U36TR-KVANTAL-FRAN-AR (IX)                        
035700       ADD 1 TO IX                                                        
035800     END-PERFORM                                                          
035900                                                                          
036000     MOVE 1 TO IX                                                         
036100     PERFORM UNTIL                                                        
036200      ( IX > MAX-ANT-PERIODER )                                           
036300       MOVE 1 TO IY                                                       
036400       MOVE U35REG-TIAARP(IX) TO W-PERIOD-AARP                            
036500       PERFORM UNTIL                                                      
036600        ( IY > MAX-ANT-CLAGER )                                           
036700         ADD U35REG-KVANTAL-UTLEV (IX, IY)                                
036800                                    TO U36TR-KVANTAL-UTLEV-12P            
036900         (IY)                                                             
037000         ADD U35REG-KVANTAL-FRAN (IX, IY)                                 
037100                                    TO U36TR-KVANTAL-FRAN-12P (IY)        
037200         IF  D-AAR = W-PERIOD-AA                                          
037300           ADD U35REG-KVANTAL-TILL (IX, IY)                               
037400                                      TO U36TR-KVANTAL-TILL-AR            
037500           (IY)                                                           
037600           ADD U35REG-KVANTAL-FRAN (IX, IY)                               
037700                                      TO U36TR-KVANTAL-FRAN-AR            
037800           (IY)                                                           
037900         END-IF                                                           
038000         ADD 1 TO IY                                                      
038100       END-PERFORM                                                        
038200       ADD 1 TO IX                                                        
038300     END-PERFORM                                                          
038400     .                                                                    
038500     EJECT                                                                
038600 F-ANTALSTEST SECTION.                                                    
038700     SKIP3                                                                
038800     MOVE JA TO SW-ALLA-ANTAL-ZERO                                        
038900     MOVE 1 TO IX                                                         
039000     PERFORM UNTIL                                                        
039100      NOT ( IX NOT > MAX-ANT-PERIODER AND SW-ALLA-ANTAL-ZERO              
039200        = JA )                                                            
039300       MOVE 1 TO IY                                                       
039400       PERFORM UNTIL                                                      
039500        ( IY > MAX-ANT-CLAGER )                                           
039600         IF  U35REG-KVANTAL-UTLEV (IX, IY) > ZERO                         
039700         OR  U35REG-KVANTAL-TILL  (IX, IY) > ZERO                         
039800         OR  U35REG-KVANTAL-FRAN  (IX, IY) > ZERO                         
039900           MOVE NEJ TO SW-ALLA-ANTAL-ZERO                                 
040000         END-IF                                                           
040100         ADD 1 TO IY                                                      
040200       END-PERFORM                                                        
040300       ADD 1 TO IX                                                        
040400     END-PERFORM                                                          
040500     SKIP1                                                                
040600     MOVE 1 TO IY                                                         
040700     PERFORM UNTIL                                                        
040800      NOT ( IY NOT > 2 AND SW-ALLA-ANTAL-ZERO = JA )                      
040900       IF  U36TR-KVANTAL-UTLEV-12P   (IY) > ZERO                          
041000       OR  U36TR-KVANTAL-TILL-AR     (IY) > ZERO                          
041100       OR  U36TR-KVANTAL-FRAN-AR     (IY) > ZERO                          
041200       OR  U36TR-KVANTAL-FRAN-12P    (IY) > ZERO                          
041300       OR  U36TR-KVANTAL-UTCLEAR     (IY) > ZERO                          
041400       OR  U36TR-KVANTAL-INCLEAR     (IY) > ZERO                          
041500       OR  U36TR-KVANTAL-UPPINV      (IY) > ZERO                          
041600       OR  U36TR-KVANTAL-NEDINV      (IY) > ZERO                          
041700       OR  U36TR-KVANTAL-PLANINL     (IY) > ZERO                          
041800       OR  U36TR-KVANTAL-OPLANINL    (IY) > ZERO                          
041900       OR  U36TR-KVANTAL-LAN         (IY) > ZERO                          
042000       OR  U36TR-KVANTAL-RETUR       (IY) > ZERO                          
042100       OR  U36TR-KVANTAL-SKROT       (IY) > ZERO                          
042200       OR  U36TR-KVANTAL-K-ORDER     (IY) > ZERO                          
042300         MOVE NEJ TO SW-ALLA-ANTAL-ZERO                                   
042400       END-IF                                                             
042500       ADD 1 TO IY                                                        
042600     END-PERFORM                                                          
042700     CONTINUE.                                                            
042800     EJECT                                                                
042900 Z-AVSLUTA SECTION.                                                       
043000     SKIP3                                                                
043100     CLOSE W23133                                                         
043200           W23135I                                                        
043300           W23135U                                                        
043400           W23136                                                         
043500     SKIP1                                                                
043600     MOVE 'S' TO POSTSUM-OPKOD                                            
043700     CALL POSTSUM USING POSTSUM-PARM                                      
043800     CONTINUE.                                                            
043900     EJECT                                                                
044000 S01-LAES-JUST-REG-W23135 SECTION.                                        
044100     SKIP3                                                                
044200     READ W23135I      INTO I35REG-AREA                                   
044300          AT END                                                          
044400             MOVE HIGH-VALUE TO REGIN-ID                                  
044500             MOVE JA TO W23135-EOF                                        
044600     END-READ                                                             
044700     SKIP1                                                                
044800     IF  W23135-EOF = NEJ                                                 
044900       MOVE 'W23135' TO POSTSUM-FDNAMN                                    
045000       MOVE 'W23134D1' TO POSTSUM-DDNAMN2                                 
045100       MOVE I35REG-IDPTYP TO POSTSUM-TRANSTYP                             
045200       SKIP1                                                              
045300       CALL POSTSUM USING POSTSUM-PARM                                    
045400       PERFORM S02-JUST-REG-W23135                                        
045500     END-IF                                                               
045600     CONTINUE.                                                            
045700     EJECT                                                                
045800 S02-JUST-REG-W23135 SECTION.                                             
045900     SKIP3                                                                
046000     MOVE MAX-ANT-PERIODER TO IX                                          
046100                              IY                                          
046200     SUBTRACT 1 FROM IY                                                   
046300     PERFORM UNTIL                                                        
046400      NOT ( IY > ZERO )                                                   
046500       MOVE I35REG-W231215-NY-001-GRP(IY)                                 
046510                      TO I35REG-W231215-NY-001-GRP(IX)                    
046600       SUBTRACT 1 FROM IX                                                 
046700                       IY                                                 
046800     END-PERFORM                                                          
046900     SKIP1                                                                
047000     MOVE W-PERIOD-AKTUELL-AARP TO I35REG-TIAARP(1)                       
047100     MOVE ZERO TO I35REG-KVANTAL-UTLEV (1, 1)                             
047200                  I35REG-KVANTAL-UTLEV (1, 2)                             
047300                  I35REG-KVANTAL-TILL  (1, 1)                             
047400                  I35REG-KVANTAL-TILL  (1, 2)                             
047500                  I35REG-KVANTAL-FRAN  (1, 1)                             
047600                  I35REG-KVANTAL-FRAN  (1, 2)                             
047700     CONTINUE.                                                            
047800     EJECT                                                                
047900 S03-LAES-POST-W23133 SECTION.                                            
048000     SKIP3                                                                
048100     READ W23133       INTO I33POST-AREA                                  
048200          AT END                                                          
048300             MOVE HIGH-VALUE TO POST-ID                                   
048400             MOVE JA TO W23133-EOF                                        
048500     END-READ                                                             
048600     SKIP1                                                                
048700     IF  W23133-EOF = NEJ                                                 
048800       MOVE 'W23133' TO POSTSUM-FDNAMN                                    
048900       MOVE 'W23134D2' TO POSTSUM-DDNAMN2                                 
049000       MOVE I33POST-IDPTYP TO POSTSUM-TRANSTYP                            
049100       SKIP1                                                              
049200       CALL POSTSUM USING POSTSUM-PARM                                    
049300     END-IF                                                               
049400     CONTINUE.                                                            
049500     EJECT                                                                
049600 S04-SKRIV-UTPOSTER SECTION.                                              
049700     SKIP3                                                                
049800     WRITE U35REG-POST FROM U35REG-AREA                                   
049900     SKIP1                                                                
050000     MOVE 'W23135' TO POSTSUM-FDNAMN                                      
050100     MOVE 'W23134D3' TO POSTSUM-DDNAMN2                                   
050200     MOVE U35REG-IDPTYP TO POSTSUM-TRANSTYP                               
050300     SKIP1                                                                
050400     CALL POSTSUM USING POSTSUM-PARM                                      
050500     SKIP3                                                                
050600     WRITE U36TR-POST FROM U36TR-AREA                                     
050700     SKIP1                                                                
050800     MOVE 'W23136' TO POSTSUM-FDNAMN                                      
050900     MOVE 'W23134D4' TO POSTSUM-DDNAMN2                                   
051000     MOVE U36TR-IDPTYP TO POSTSUM-TRANSTYP                                
051100     SKIP1                                                                
051200     CALL POSTSUM USING POSTSUM-PARM                                      
051300     CONTINUE.                                                            
051400     EJECT                                                                
051500 S05-INIT-U36TR SECTION.                                                  
051600     SKIP3                                                                
051700     MOVE 1 TO IY                                                         
051800     PERFORM UNTIL                                                        
051900      ( IY > MAX-ANT-CLAGER )                                             
052000       MOVE ZERO TO U36TR-KVANTAL-UTLEV-12P (IY)                          
052100                        U36TR-KVANTAL-TILL-AR   (IY)                      
052200                        U36TR-KVANTAL-FRAN-AR   (IY)                      
052300                        U36TR-KVANTAL-FRAN-12P  (IY)                      
052400                        U36TR-KVANTAL-UTCLEAR   (IY)                      
052500                        U36TR-KVANTAL-INCLEAR   (IY)                      
052600                        U36TR-KVANTAL-UPPINV    (IY)                      
052700                        U36TR-KVANTAL-NEDINV    (IY)                      
052800                        U36TR-KVANTAL-PLANINL   (IY)                      
052900                        U36TR-KVANTAL-OPLANINL  (IY)                      
053000                        U36TR-KVANTAL-LAN       (IY)                      
053100                        U36TR-KVANTAL-RETUR     (IY)                      
053200                        U36TR-KVANTAL-SKROT     (IY)                      
053300                        U36TR-KVANTAL-K-ORDER   (IY)                      
053400       ADD 1 TO IY                                                        
053500     END-PERFORM                                                          
053600     .                                                                    
