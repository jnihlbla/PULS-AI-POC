000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W222PBTO                                                 
000400 AUTHOR.         STEFAN ANDREASSON.                                       
000500 DATE-WRITTEN.   AUGUSTI 1997.                                            
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION.                                                            
000900*        SUBPROGRAM FÖR ATT BERÄKNA PROGNOSBEHOVET TOTALT                 
001000*        (PB-PLAN) SAMT RÄKNA UT SÄSONGSINDEX                             
001100*                                                                         
001200*    INDATA.                                                              
001300*        CALL-PARAMETRAR FRÅN KALLANDE PROGRAM.                           
001400*          W222PBTO                                                       
001500*          ARTC-PCB                                                       
001600*          WDK7-PCB                                                       
001700*          ARTM-PCB                                                       
001800*          2501-PCB                                                       
001900*                                                                         
002000*    UTDATA.                                                              
002100*        W222PBTO                                                         
002200                                                                          
002300                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(8)    VALUE 'W222PBTO'.            
003200 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003300 01  DAGENS-DATUM                PIC 9(6).                                
003400 01  FILLER                      REDEFINES DAGENS-DATUM.                  
003500   03 DAGENS-AA                  PIC 9(2).                                
003600   03 DAGENS-MM                  PIC 9(2).                                
003700   03 DAGENS-DD                  PIC 9(2).                                
003800 01  DAGENS-VECKA                PIC 9(2)    VALUE ZERO.                  
003900 01  KONSTANTER.                                                          
004000   03 JA                         PIC X       VALUE 'J'.                   
004100   03 NEJ                        PIC X       VALUE 'N'.                   
004200   03 PB-TOTAL                   PIC X(2)    VALUE '20'.                  
004300 01  WS-ARBETSFAELT.                                                      
004400   03 WS-NOLL                    PIC 9       VALUE ZERO.                  
004500   03 WS-TIME-START              PIC 9(10)   VALUE ZERO.                  
004600   03 WS-TIME-SLUT               PIC 9(10)   VALUE ZERO.                  
004700   03 WS-AAPP                    PIC 9(4)    VALUE ZERO.                  
004800   03 FILLER REDEFINES           WS-AAPP.                                 
004900    05 WS-AA                     PIC 9(2).                                
005000    05 WS-PP                     PIC 9(2).                                
005100   03 WS-PER                     PIC  9(2)    VALUE ZERO.                 
005200   03 WS-VV                      PIC  9(2)   VALUE ZERO.                  
005300   03 WS-TIAAVV-NUM              PIC   9(4)  VALUE ZERO.                  
005400   03 WS-ANTAL-VECKOR            PIC   9(3)   VALUE ZERO COMP-3.          
005500   03 WS-ARSTOTAL                PIC S9(9)V9(2)                           
005600                                             COMP-3  VALUE ZERO.          
005700   03 WS-KVPB                    PIC S9(9)V9(2)                           
005800                                             COMP-3  VALUE ZERO.          
005900   03 WS-KVBEHOV-PER             PIC S9(7)V9(2)                           
006000                                             COMP-3  VALUE ZERO.          
006100   03 WS-KVBEHOV-PER-TEST        OCCURS 12                                
006200                                 PIC S9(7)V9(2)                           
006300                                             COMP-3  VALUE ZERO.          
006400   03 WS-JUSTERA                 PIC S9V9(2) COMP-3  VALUE ZERO.          
006500   03 WS-RESEASON-TOT            PIC S9(2)V9(2)                           
006600                                             COMP-3  VALUE ZERO.          
006700   03 WS-TABELL    OCCURS 12.                                             
006800    05 WS-FORSTA-V               PIC  9(2)   VALUE ZERO.                  
006900    05 WS-SISTA-V                PIC  9(2)   VALUE ZERO.                  
007000    05 WS-KVVIPER                PIC 9       VALUE ZERO.                  
007100                                                                          
007200                                                                          
007300   03 WS-KVBEHOV-TABELL.                                                  
007400     05 WS-KVBEHOV-VECKA         OCCURS 52                                
007500                                 PIC S9(7)V9(2)                           
007600                                             COMP-3  VALUE ZERO.          
007700   03 WS-RESEASON-TABELL.                                                 
007800     05 WS-RESEASON              OCCURS 12                                
007900                                 PIC S9(2)V9(4)                           
008000                                             COMP-3  VALUE ZERO.          
008100   03 WS-RESEASON-AVR-TABELL.                                             
008200     05 WS-RESEASON-AVR          OCCURS 12                                
008300                                 PIC S9(2)V9(2)                           
008400                                             COMP-3  VALUE ZERO.          
008500   03 WS-NOLLA-KVBEHOV.                                                   
008600     05 FILLER                   OCCURS 52                                
008700                                 PIC S9(7)V9(2)                           
008800                                             COMP-3  VALUE ZERO.          
008900   03 WS-NOLLA-RESEASON.                                                  
009000     05 FILLER                   OCCURS 12                                
009100                                 PIC S9(2)V9(4)                           
009200                                             COMP-3  VALUE ZERO.          
009300   03 WS-NOLLA-PARM.                                                      
009400     07 FILLER                   OCCURS 12 TIMES                          
009500                                 PIC S9V9(2)         COMP-3.              
009600*                                 SÄSONGSINDEX                            
009700     07 FILLER                   PIC S9(6)V9(1)      COMP-3.              
009800*                                 PERIODBEHOV (PROGNOS)                   
009900 01  IX                          PIC S9(3)           VALUE ZERO.          
010000 01  IX-FRAN                     PIC S9(3)           VALUE ZERO.          
010100 01  IX-TILL                     PIC S9(3)           VALUE ZERO.          
010200 01  IX-PER                      PIC S9(3)           VALUE ZERO.          
010300 01  IX-VECKA                    PIC S9(3)           VALUE ZERO.          
010400                                                                          
010500     SKIP3                                                                
010600 01  DYNAMISKA-SUBPROGRAM.                                                
010700   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
010800   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
010900   03  W22222                    PIC X(8)    VALUE 'W22222'.              
011000   03  W009VADD                  PIC X(8)    VALUE 'W009VADD'.            
011100     EJECT                                                                
011200*    *************************************                                
011300*    **  LINK-AREA                      **                                
011400*    **  BEHOVSTABELL                   **                                
011500*    *************************************                                
011600*01  AREA  -COPY W222L222   -PRE LINK-.                                   
011700     EJECT                                                                
011800*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
011900*01  -COPY WDATAREA                                                       
012000     EJECT                                                                
012100 LINKAGE SECTION.                                                         
012200*01  -COPY W222PBTO                                                       
012300     EJECT                                                                
012400 01  W222-WDK6-PCB               PIC X.                                   
012500 01  W222-WDK7-PCB               PIC X.                                   
012600 01  W222-ARTM-PCB               PIC X.                                   
012700 01  W222-2501-PCB               PIC X.                                   
012800 01  W222-WDB6R-PCB              PIC X.                                   
012900 01  W222-WDK7R-PCB              PIC X.                                   
013000 01  W222-WDB6-PCB               PIC X.                                   
013100 01  W222-WDD7-PCB               PIC X.                                   
013200 01  W222-WDK7E-PCB              PIC X.                                   
013300 01  W222-UTIL-WDK6-PCB          PIC X.                                   
013400 01  W222-UTIL-WDK7-PCB          PIC X.                                   
013500 01  W222-UTIL-WDB6-PCB          PIC X.                                   
013600 01  W222-UTUP-WDK7-PCB          PIC X.                                   
013700 01  W222-UTUP-WDB6-PCB          PIC X.                                   
013800 01  W222-UTUP-UTIL-WDK6-PCB     PIC X.                                   
013900 01  W222-UTUP-UTIL-WDK7-PCB     PIC X.                                   
014000 01  W222-UTUP-UTIL-WDB6-PCB     PIC X.                                   
014100     EJECT                                                                
014200 PROCEDURE DIVISION USING  PBTO-W222PBTO                                  
014300                           W222-WDK6-PCB  W222-WDK7-PCB                   
014400                           W222-ARTM-PCB  W222-2501-PCB                   
014500                           W222-WDB6R-PCB W222-WDK7R-PCB                  
014600                           W222-WDB6-PCB  W222-WDD7-PCB                   
014700                           W222-WDK7E-PCB                                 
014800                           W222-UTIL-WDK6-PCB                             
014900                           W222-UTIL-WDK7-PCB                             
015000                           W222-UTIL-WDB6-PCB                             
015100                           W222-UTUP-WDK7-PCB                             
015200                           W222-UTUP-WDB6-PCB                             
015300                           W222-UTUP-UTIL-WDK6-PCB                        
015400                           W222-UTUP-UTIL-WDK7-PCB                        
015500                           W222-UTUP-UTIL-WDB6-PCB                        
015600                           .                                              
015700 STYR SECTION.                                                            
015800                                                                          
015900     PERFORM A-INIT                                                       
016000     PERFORM B-BEARBETA                                                   
016100                                                                          
016200     ACCEPT WS-TIME-SLUT  FROM TIME                                       
016300                                                                          
016400     MOVE ZERO TO RETURN-CODE                                             
016500     GOBACK                                                               
016600     .                                                                    
016700     EJECT                                                                
016800 A-INIT SECTION.                                                          
016900                                                                          
017000     ACCEPT DAGENS-DATUM FROM DATE                                        
017100     ACCEPT WS-TIME-START FROM TIME                                       
017200                                                                          
017300     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
017400     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
017500                                                                          
017600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
017700                     DAT-O-TIDATUM DAT-KDSVAR                             
017800                                                                          
017900     IF DAT-KDSVAR-OK                                                     
018000                                                                          
018100       MOVE DAT-TIVV         TO DAGENS-VECKA                              
018200       MOVE 1                TO WS-ANTAL-VECKOR                           
018300                                                                          
018400       MOVE DAT-TIAAVV-GRP   TO WS-TIAAVV-NUM                             
018500       MOVE WS-TIAAVV-NUM    TO LINK-TIAAVV-AKTUELL                       
018600                                LINK-TIBEHOV-START                        
018700       CALL W009VADD USING      LINK-TIBEHOV-START                        
018800                                WS-ANTAL-VECKOR                           
018900                                                                          
019000       IF (DAT-TIMM = 01 AND DAT-TIDD = 01)                               
019100       OR (DAT-TIMM = 12 AND DAT-TIDD = 31)                               
019200         MOVE 6 TO DAT-TID                                                
019300       END-IF                                                             
019400       MOVE DAT-TID          TO LINK-TID-AKTUELL                          
019500                                                                          
019600       MOVE SPACE            TO LINK-IDDC                                 
019700                                                                          
019800     ELSE                                                                 
019900         STRING ' FEL FRÅN DATUMRUTIN WDATKONV '                          
020000         DELIMITED BY SIZE INTO FELTEXT                                   
020100         CALL FELLOG                                                      
020200     END-IF                                                               
020300     PERFORM AA-HAMTA-VV-I-PER                                            
020400                                                                          
020500     MOVE PBTO-IDARTNR       TO LINK-IDARTNR                              
020600                                                                          
020700     MOVE ZERO               TO WS-ARSTOTAL                               
020800                                IX-FRAN                                   
020900                                IX-TILL                                   
021000                                IX-PER                                    
021100     MOVE WS-NOLLA-KVBEHOV   TO WS-KVBEHOV-TABELL                         
021200     MOVE WS-NOLLA-RESEASON  TO WS-RESEASON-TABELL                        
021300                                WS-RESEASON-AVR-TABELL                    
021400     .                                                                    
021500     EJECT                                                                
021600 AA-HAMTA-VV-I-PER SECTION.                                               
021700                                                                          
021800*    --- FYLL I VECKONR FÖR PERIODERNA                                    
021900     MOVE DAGENS-DATUM(1:2)  TO WS-AAPP(1:2)                              
022000     MOVE 01                 TO WS-AAPP(3:2)                              
022100     MOVE WS-AAPP            TO DAT-I-TIDATUM                             
022200     MOVE 'AARP'             TO DAT-KDDATFORM                             
022300     MOVE +1                 TO WS-PP                                     
022400                                                                          
022500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
022600                     DAT-O-TIDATUM DAT-KDSVAR                             
022700                                                                          
022800     IF DAT-KDSVAR-OK                                                     
022900                                                                          
023000       MOVE DAT-KVVIPER      TO WS-KVVIPER(WS-PP)                         
023100       MOVE 1                TO WS-FORSTA-V(WS-PP)                        
023200                                                                          
023300     ELSE                                                                 
023400         STRING ' FEL FRÅN DATUMRUTIN WDATKONV '                          
023500         DELIMITED BY SIZE INTO FELTEXT                                   
023600         CALL FELLOG                                                      
023700     END-IF                                                               
023800                                                                          
023900     PERFORM UNTIL WS-PP      >  12                                       
024000       ADD +1                 TO WS-PP                                    
024100       IF WS-PP = +13                                                     
024200         MOVE 52             TO WS-SISTA-V(12)                            
024300       ELSE                                                               
024400                                                                          
024500         MOVE WS-AAPP         TO DAT-I-TIDATUM                            
024600         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
024700                             DAT-O-TIDATUM DAT-KDSVAR                     
024800         IF DAT-KDSVAR-OK                                                 
024900             COMPUTE WS-SISTA-V(WS-PP - 1) = DAT-TIVV - 1                 
025000             IF WS-PP > +1                                                
025100               MOVE DAT-TIVV  TO WS-FORSTA-V(WS-PP)                       
025200               MOVE DAT-KVVIPER TO WS-KVVIPER(WS-PP)                      
025300             END-IF                                                       
025400         ELSE                                                             
025500           MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                   
025600           CALL FELLOG                                                    
025700         END-IF                                                           
025800       END-IF                                                             
025900     END-PERFORM                                                          
026000     .                                                                    
026100     EJECT                                                                
026200 B-BEARBETA SECTION.                                                      
026300                                                                          
026400     PERFORM BA-HAMTA-BEHOV                                               
026500                                                                          
026600     IF LINK-ANROP-OK                                                     
026700                                                                          
026800       PERFORM BB-BERAKNA-PBTOTAL                                         
026900       PERFORM BC-BERAKNA-SASONG                                          
027000       PERFORM BD-FLYTTA-TILL-PARM                                        
027100       MOVE JA               TO PBTO-KDSVAR                               
027200                                                                          
027300     ELSE                                                                 
027400                                                                          
027500       MOVE WS-NOLLA-PARM    TO PBTO-UTDATA                               
027600       MOVE NEJ              TO PBTO-KDSVAR                               
027700                                                                          
027800     END-IF                                                               
027900                                                                          
028000                                                                          
028100***** TVINGA FRAM EN DUMP FÖR ATT SE TESTRESULTAT   START                 
028200*    DIVIDE WS-KVPB BY WS-NOLL GIVING WS-KVPB                             
028300***** TVINGA FRAM EN DUMP FÖR ATT SE TESTRESULTAT   SLUT                  
028400     .                                                                    
028500     EJECT                                                                
028600****                                                                      
028700****                                                                      
028800****                                                                      
028900 BA-HAMTA-BEHOV SECTION.                                                  
029000                                                                          
029100     MOVE 52                 TO LINK-KVVECKOR-BEHOV                       
029200     MOVE PB-TOTAL           TO LINK-KDBEHOV                              
029300     MOVE NEJ                TO LINK-FLINKLDIRLEV                         
029400     CALL W22222 USING LINK-AREA W222-WDK6-PCB  W222-WDK7-PCB             
029500                                 W222-ARTM-PCB  W222-2501-PCB             
029600                                 W222-WDB6R-PCB W222-WDK7R-PCB            
029700                                 W222-WDB6-PCB  W222-WDD7-PCB             
029800                                 W222-WDK7E-PCB                           
029900                                 W222-UTIL-WDK6-PCB                       
030000                                 W222-UTIL-WDK7-PCB                       
030100                                 W222-UTIL-WDB6-PCB                       
030200                                 W222-UTUP-WDK7-PCB                       
030300                                 W222-UTUP-WDB6-PCB                       
030400                                 W222-UTUP-UTIL-WDK6-PCB                  
030500                                 W222-UTUP-UTIL-WDK7-PCB                  
030600                                 W222-UTUP-UTIL-WDB6-PCB                  
030700     .                                                                    
030800     EJECT                                                                
030900 BB-BERAKNA-PBTOTAL SECTION.                                              
031000                                                                          
031100****                                                                      
031200**** FLYTTA IN RESULTATET FRÅN BEHOVSMODULEN TILL EN                      
031300**** ARBETSAREA DÄR VECKOBEHOVEN LIGGER FROM VECKA 1 TOM 52               
031400****                                                                      
031500                                                                          
031600     COMPUTE IX-TILL = DAGENS-VECKA + 1                                   
031700                                                                          
031800     MOVE +1                 TO IX-FRAN                                   
031900     PERFORM UNTIL IX-TILL   >  52                                        
032000        ADD  LINK-KVBEHOV-VECKA(IX-FRAN)                                  
032100                             TO WS-KVBEHOV-VECKA (IX-TILL)                
032200                                WS-ARSTOTAL                               
032300        ADD +1               TO IX-FRAN                                   
032400                                IX-TILL                                   
032500     END-PERFORM                                                          
032600                                                                          
032700     MOVE +1                 TO IX-TILL                                   
032800     PERFORM UNTIL IX-FRAN   >  52                                        
032900        ADD  LINK-KVBEHOV-VECKA(IX-FRAN)                                  
033000                             TO WS-KVBEHOV-VECKA (IX-TILL)                
033100                                WS-ARSTOTAL                               
033200        ADD +1               TO IX-FRAN                                   
033300                                IX-TILL                                   
033400     END-PERFORM                                                          
033500                                                                          
033600****                                                                      
033700**** PROGNOSBEHOVET SPEGLAR BEHOVET FÖR EN 6-VECKORSPERIOD                
033800****                                                                      
033900                                                                          
034000     COMPUTE WS-KVPB ROUNDED = WS-ARSTOTAL / 12                           
034100     .                                                                    
034200     EJECT                                                                
034300 BC-BERAKNA-SASONG SECTION.                                               
034400                                                                          
034500     MOVE ZERO               TO WS-RESEASON-TOT                           
034600                                                                          
034700     MOVE +1                 TO IX-FRAN                                   
034800                                IX-PER                                    
034900     PERFORM UNTIL IX-PER > +12                                           
035000                                                                          
035100       MOVE WS-FORSTA-V (IX-PER)                                          
035200                             TO IX-VECKA                                  
035300       MOVE ZERO             TO WS-KVBEHOV-PER                            
035400                                                                          
035500       PERFORM UNTIL IX-VECKA > WS-SISTA-V (IX-PER)                       
035600                                                                          
035700         ADD WS-KVBEHOV-VECKA (IX-FRAN)                                   
035800                             TO WS-KVBEHOV-PER                            
035900         ADD +1              TO IX-VECKA                                  
036000                                IX-FRAN                                   
036100       END-PERFORM                                                        
036200                                                                          
036300****                                                                      
036400****   RÄKNA UT SÄSONGSINDEX                                              
036500****                                                                      
036600                                                                          
036700       COMPUTE WS-RESEASON (IX-PER) ROUNDED =                             
036800               WS-KVBEHOV-PER / WS-KVPB                                   
036900                ON SIZE ERROR                                             
037000                    MOVE ZERO     TO WS-RESEASON (IX-PER)                 
037100       END-COMPUTE                                                        
037200       COMPUTE WS-RESEASON-AVR (IX-PER) ROUNDED =                         
037300               WS-RESEASON (IX-PER) * +1                                  
037400       ADD WS-RESEASON-AVR (IX-PER)                                       
037500                             TO WS-RESEASON-TOT                           
037600****   WS-KVBEHOV-PER-TEST ANVÄNDS ENBART I TESTSYFTE                     
037700       MOVE WS-KVBEHOV-PER   TO WS-KVBEHOV-PER-TEST (IX-PER)              
037800       ADD +1                TO IX-PER                                    
037900                                                                          
038000     END-PERFORM                                                          
038100                                                                          
038200     IF WS-RESEASON-TOT = ZERO                                            
038300       MOVE +1               TO IX-PER                                    
038400       PERFORM UNTIL IX-PER > +12                                         
038500         MOVE +1             TO WS-RESEASON-AVR (IX-PER)                  
038600         ADD +1              TO IX-PER                                    
038700       END-PERFORM                                                        
038800     END-IF                                                               
038900                                                                          
039000     IF WS-RESEASON-TOT > +12.00                                          
039100       MOVE -0.01            TO WS-JUSTERA                                
039200     ELSE                                                                 
039300       MOVE +0.01            TO WS-JUSTERA                                
039400     END-IF                                                               
039500                                                                          
039600     PERFORM UNTIL WS-RESEASON-TOT = +12.00                               
039700     OR            WS-RESEASON-TOT = ZERO                                 
039800                                                                          
039900       MOVE +1               TO IX-PER                                    
040000       PERFORM UNTIL WS-RESEASON-TOT = +12.00                             
040100       OR IX-PER > +12                                                    
040200         IF WS-RESEASON-AVR (IX-PER) > +0.01                              
040300           ADD WS-JUSTERA    TO WS-RESEASON-AVR (IX-PER)                  
040400                                WS-RESEASON-TOT                           
040500         END-IF                                                           
040600         ADD +1              TO IX-PER                                    
040700       END-PERFORM                                                        
040800     END-PERFORM                                                          
040900     .                                                                    
041000     EJECT                                                                
041100 BD-FLYTTA-TILL-PARM SECTION.                                             
041200                                                                          
041300****                                                                      
041400****   FLYTTA RESULTATET TILL PARAMETERAREA                               
041500****                                                                      
041600     MOVE WS-KVPB            TO PBTO-KVPB-PLAN                            
041700     MOVE +1                 TO IX-PER                                    
041800     PERFORM UNTIL IX-PER > +12                                           
041900                                                                          
042000       MOVE WS-RESEASON-AVR (IX-PER)                                      
042100                             TO PBTO-RESEASON-PLAN (IX-PER)               
042200       ADD +1                TO IX-PER                                    
042300                                                                          
042400     END-PERFORM                                                          
042500     .                                                                    
