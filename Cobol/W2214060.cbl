000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.         W2214060.                                            
000400 AUTHOR.             IDK, GÖTEBORG.                                       
000500 DATE-WRITTEN.       FEB 1979.                                            
000600     SKIP2                                                                
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION.                                                            
001000*            PROGRAMMET ÄR ETT SUBPROGRAM                                 
001100*            VILKET UTFÖR KONTROLL AV GÄLLANDE PLAN GENTEMOT              
001200*            FÖRBRUKNING OCH GÄLLANDE LAGERNIVÅ EFTER                     
001300*            FRYSTIDEN.                                                   
001400*    SUBPROGRAM.                                                          
001500*            W009VADD    ADD AV VECKOR TILL DATUM                         
001600*            W221LPAD    UPPDATERING AV TABELL KDLPORS-TAB                
001700*            W2214010    IMS-SUBPROGRAM                                   
001800*                                                                         
001801*    ÄNDRINGAR:                                                           
001810*    2015-04-22  E'TRACKER 10130993                                       
001820*                REDUCE NUMBER OF DELIVERY SCHEDULES                      
001830*                                                                         
001900 ENVIRONMENT DIVISION.                                                    
002000 DATA DIVISION.                                                           
002100     EJECT                                                                
002200 WORKING-STORAGE SECTION.                                                 
002300     SKIP2                                                                
002400*    -COPY WY2000W3                                                       
002500     SKIP3                                                                
002600*    -COPY WY2000W9                                                       
002700     SKIP3                                                                
002800 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
002900     SKIP3                                                                
003000 01  KONSTANTER.                                                          
003100     03  JA                  PIC X       VALUE 'J'.                       
003200     03  NEJ                 PIC X       VALUE 'N'.                       
003300     03  MIN-AVROPSVARDE-VVKL1-2                                          
003400                             PIC S9(9)   VALUE +1000 COMP-3.              
003500     SKIP1                                                                
003600     03  LAES-AVROP-FIRST    PIC S9(3)   VALUE +420  COMP-3.              
003700     03  LAES-AVROP-NEXT     PIC S9(3)   VALUE +421  COMP-3.              
003800     SKIP3                                                                
003810*    --- VARIABLER TILL SUBPROGRAM W221LPAD                               
003820 01  W-W221LP-CTX            PIC X(08) VALUE 'W221LP01'.                  
003830 01  W-KDLPORS-GRP.                                                       
003840     03 W-KDLPORS-TAB     OCCURS 4 PIC 9(3).                              
003850     SKIP3                                                                
003900 01  W-ARBETSAREOR.                                                       
004000     SKIP1                                                                
004100     03  IX                  PIC S9(9)               COMP SYNC.           
004200     03  IX-SLUTVARDE        PIC S9(9)               COMP SYNC.           
004300     SKIP1                                                                
004400     03  W-FRYSTIDP          PIC S9(5)               COMP-3.              
004500     03  W-TIDPUNKT          PIC S9(5)               COMP-3.              
004600     03  W-TIDP-VVKL1-2      PIC S9(5)               COMP-3.              
004700     03  W-TIDP-VVKL3-5      PIC S9(5)               COMP-3.              
004800     03  W-ANTAL-VECKOR      PIC S9(3)               COMP-3.              
004900     03  W-TILLG             PIC S9(7)V99            COMP-3.              
005000     03  W-UNDRE-GRANS       PIC S9(7)V99            COMP-3.              
005100     03  W-OVRE-GRANS        PIC S9(7)V99            COMP-3.              
005200     03  W-DIFF              PIC S9(7)               COMP-3.              
005300     03  W-DIFF-AA           PIC S9(3)               COMP-3.              
005400     03  W-VECKO-DIFFERENS   PIC S9(7)               COMP-3.              
005500     03  W-ARTIKEL-ANTAL     PIC S9(7)               COMP-3.              
005600     03  W-VARDE             PIC S9(11)              COMP-3.              
005700     03  W-IDLEVNR           PIC X(5).                                    
005800     SKIP1                                                                
005900     03  W-DATUM-FROM        PIC S9(5)               COMP-3.              
006000     03  W-DATUM-TOM         PIC S9(5)               COMP-3.              
006100     03  W-DATUM-AAVV-FROM   PIC 9(4).                                    
006200     03  W-DAT-AAVV-FROM     REDEFINES W-DATUM-AAVV-FROM.                 
006300         05  W-DATUM-AA-FROM PIC 9(2).                                    
006400         05  W-DATUM-VV-FROM PIC 9(2).                                    
006500     03  W-DATUM-AAVV-TOM    PIC 9(4).                                    
006600     03  W-DAT-AAVV-TOM      REDEFINES W-DATUM-AAVV-TOM.                  
006700         05  W-DATUM-AA-TOM  PIC 9(2).                                    
006800         05  W-DATUM-VV-TOM  PIC 9(2).                                    
006900     03  W-KDLPORS           PIC 9(3).                                    
007000     03  W-TIFINLV           PIC 9(5).                                    
007100     03  FILLER REDEFINES W-TIFINLV.                                      
007200         05  W-TIFINLV-1-4   PIC 9(4).                                    
007300         05  FILLER          PIC 9.                                       
007400     03  W-KVVECKOR-FT       PIC S9(3)               COMP-3.              
007500     SKIP3                                                                
007600 01  SWITCHAR.                                                            
007700     03  SW-INLEV-UNDER-PERIOD                                            
007800                             PIC X       VALUE 'N'.                       
007900     03  SW-OMSPEC-ARTIKEL   PIC X       VALUE 'N'.                       
008000     SKIP3                                                                
008100*----------------------------------------- TILLGÅNGSTABELL                
008200 01  TILLGANGSTABELL.                                                     
008300     03  TILLGTAB-MAX        PIC S9(9)   VALUE +156  COMP SYNC.           
008400     03  TILLGTAB-IX         PIC S9(9)   VALUE +0    COMP SYNC.           
008500     SKIP1                                                                
008600     03  TILLGTAB.                                                        
008700         05  TILLGTAB-INGANG OCCURS 156.                                  
008800             10  TILLGTAB-ANTAL                                           
008900                             PIC S9(7)V99            COMP-3.              
009000     EJECT                                                                
009100 01  DYNAMISKA-SUBPROGRAM.                                                
009200     03  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
009300     03  W221LPAD            PIC X(8)    VALUE 'W221LPAD'.                
009400     03  W2214010            PIC X(8)    VALUE 'W2214010'.                
009500     EJECT                                                                
009600*                            *************************************        
009700*                            ** LINK3-AREA                      **        
009800*                            ** LÄSNING WDD9 -AVROP             **        
009900*                            *************************************        
010000*01  AREA  -COPY W221L402   -PRE LINK3-.                                  
010100     EJECT                                                                
010200 LINKAGE SECTION.                                                         
010300     SKIP3                                                                
010400     EJECT                                                                
010500*                            *************************************        
010600*                            ** LINK-AREA                       **        
010700*                            ** KOMMUNIKATION MED HUVUDPROGRAM  **        
010800*                            *************************************        
010900*01  AREA  -COPY W221L401   -PRE LINK-.                                   
011000     EJECT                                                                
011100*                            *************************************        
011200*                            ** LNK2-AREA                       **        
011300*                            ** BERÄKNADE VECKO-BEHOV           **        
011400*                            *************************************        
011500*01  AREA  -COPY W222L222   -PRE LNK2-.                                   
011600     EJECT                                                                
011700*01  -COPY W0008  -PRE ARTC-.                                             
011800     05  FILLER              PIC X.                                       
011900     EJECT                                                                
012000*01  -COPY W0008  -PRE INLB-.                                             
012100     05  FILLER              PIC X.                                       
012200     EJECT                                                                
012300*01  -COPY W0008  -PRE ARTM-.                                             
012400     05  FILLER              PIC X.                                       
012500     EJECT                                                                
012600 PROCEDURE DIVISION USING  LINK-AREA LNK2-AREA ARTC-PCB INLB-PCB          
012700                           ARTM-PCB.                                      
012800     SKIP3                                                                
012900     PERFORM A-INITIERA                                                   
013000     SKIP1                                                                
013100     PERFORM B-BERAKNA-AKTUELL-TILLGANG                                   
013200     PERFORM C-SKAPA-TILLGANGSTABELL                                      
013300     PERFORM D-KONTROLL-KORRGRANSER                                       
013400     SKIP1                                                                
013500     MOVE ZERO TO RETURN-CODE                                             
013600     GOBACK                                                               
013700     .                                                                    
013800     EJECT                                                                
013900 A-INITIERA SECTION.                                                      
014000     SKIP3                                                                
014100     MOVE LINK-TIAAVV-AKT TO W-FRYSTIDP                                   
014200     MOVE LINK-KVVECKOR-FT TO W-ANTAL-VECKOR                              
014300     CALL W009VADD USING W-FRYSTIDP W-ANTAL-VECKOR                        
014400     SKIP1                                                                
014500     MOVE LINK-TIAAVV-AKT TO W-TIDP-VVKL1-2                               
014600     MOVE LINK-KVVECKOR-FT TO W-ANTAL-VECKOR                              
014700*FIX ADD 5 TO W-ANTAL-VECKOR                                              
014800     ADD 20 TO W-ANTAL-VECKOR                                             
014900     CALL W009VADD USING W-TIDP-VVKL1-2 W-ANTAL-VECKOR                    
015000     SKIP1                                                                
015100     MOVE LINK-TIAAVV-AKT TO W-TIDP-VVKL3-5                               
015200     MOVE LINK-KVVECKOR-FT TO W-ANTAL-VECKOR                              
015300*FIX ADD 8 TO W-ANTAL-VECKOR                                              
015400     ADD 20 TO W-ANTAL-VECKOR                                             
015500     CALL W009VADD USING W-TIDP-VVKL3-5 W-ANTAL-VECKOR                    
015600     MOVE NEJ TO SW-OMSPEC-ARTIKEL                                        
015700                 SW-INLEV-UNDER-PERIOD                                    
015800     SKIP1                                                                
015900     MOVE 1 TO IX                                                         
016000     PERFORM UNTIL NOT(                                                   
016100        IX NOT > TILLGTAB-MAX)                                            
016200         MOVE ZERO TO TILLGTAB-ANTAL (IX)                                 
016300         ADD 1 TO IX                                                      
016400     END-PERFORM                                                          
016500     .                                                                    
016600     EJECT                                                                
016700 B-BERAKNA-AKTUELL-TILLGANG SECTION.                                      
016800******************************************************************        
016900*                                                                *        
017000*    BERAKNING AV TILLGÅNG I AKTUELL VECKA                       *        
017100*                                                                *        
017200******************************************************************        
017300     SKIP1                                                                
017400     COMPUTE W-TILLG =                                                    
017500                   LINK-KVLS   (1)                                        
017600               -  (LINK-KVRESS (1)     )                                  
017700               -  (LINK-KVROS  (1)     )                                  
017800               -  (LINK-KVOKS-BULK (1) + LINK-KVOKS-DAG (1) +             
017900                                         LINK-KVOKS-VOR (1))              
018000               +   LINK-KVAKS  (1)                                        
018110               +   LINK-KVLAAN                                            
018200               +   LINK-KVLS-SDC-OVER                                     
018300     SUBTRACT LNK2-KVBEHOV-DESSUTOM                                       
018400                             FROM W-TILLG                                 
018500     .                                                                    
018600     EJECT                                                                
018700 C-SKAPA-TILLGANGSTABELL SECTION.                                         
018800******************************************************************        
018900*                                                                *        
019000*    TABELL MED INLEVERANSER PLACERADE I RESPEKTIVE VECKA        *        
019100*                                                                *        
019200******************************************************************        
019300     SKIP1                                                                
019400     IF  LINK-KDVVKL < 3                                                  
019500         MOVE W-TIDP-VVKL1-2 TO W-TIDPUNKT                                
019600     ELSE                                                                 
019700         MOVE W-TIDP-VVKL3-5 TO W-TIDPUNKT                                
019800     END-IF                                                               
019900     SKIP1                                                                
020000     MOVE LINK-IDARTNR TO LINK3-IDARTNR                                   
020100     MOVE 2 TO LINK3-KDAVROP                                              
020200     MOVE LAES-AVROP-FIRST TO LINK3-KDCALL                                
020300     CALL W2214010 USING LINK3-AREA  ARTC-PCB INLB-PCB ARTM-PCB           
020400     SKIP1                                                                
020500     PERFORM UNTIL NOT(                                                   
020600        LINK3-ANROP-OK)                                                   
020700     MOVE LINK3-TIAVROP-DISP   TO TMP1-YYWW                               
020800     MOVE W-TIDPUNKT           TO TMP2-YYWW                               
020900     PERFORM WY2000P3                                                     
021000     IF  TMP1-YYWW <= TMP2-YYWW                                           
021100     SKIP1                                                                
021200         MOVE LINK3-TIAVROP-DISP   TO TMP1-YYWW                           
021300         MOVE LINK-TIAAVV-AKT      TO TMP2-YYWW                           
021400         PERFORM WY2000P3                                                 
021500         IF TMP1-YYWW <= TMP2-YYWW                                        
021600             ADD LINK3-KVAVROP TO W-TILLG                                 
021700         ELSE                                                             
021800             MOVE LINK3-TIAVROP-DISP TO W-DATUM-TOM                       
021900             MOVE LINK-TIAAVV-AKT TO W-DATUM-FROM                         
022000             PERFORM S01-BERAKNA-VECKODIFFERENS                           
022100             IF W-VECKO-DIFFERENS > ZERO                                  
022200                 MOVE W-VECKO-DIFFERENS TO TILLGTAB-IX                    
022300             ELSE                                                         
022400                 MOVE +1            TO TILLGTAB-IX                        
022500                 DISPLAY 'ARTNR   ' LINK3-IDARTNR                         
022600             END-IF                                                       
022700             IF  TILLGTAB-IX < TILLGTAB-MAX                               
022800                 ADD LINK3-KVAVROP                                        
022900                            TO TILLGTAB-ANTAL (TILLGTAB-IX)               
023000               IF  TILLGTAB-IX > LINK-KVVECKOR-FT                         
023100               AND LINK3-IDLEVNR = LINK-IDLEVNR                           
023200                     MOVE JA TO SW-INLEV-UNDER-PERIOD                     
023300               END-IF                                                     
023400             END-IF                                                       
023500         END-IF                                                           
023600     END-IF                                                               
023700         MOVE LAES-AVROP-NEXT  TO LINK3-KDCALL                            
023800         CALL W2214010 USING LINK3-AREA ARTC-PCB INLB-PCB ARTM-PCB        
023900     END-PERFORM                                                          
024000     .                                                                    
024100     EJECT                                                                
024200 D-KONTROLL-KORRGRANSER SECTION.                                          
024300******************************************************************        
024400*                                                                *        
024500*    ÖVRE OCH UNDRE LAGERGRÄNSER BERÄKNAS                        *        
024600*    BERÄKNAD TILLGÅNG KONTROLLERAS MOT GRÄNSER                  *        
024700*    TILLGÅNG UTANFÖR GRÄNSER MEDFÖR BEGÄRAN AV OMSPEC           *        
024800*                                                                *        
024900*                                                                *        
025000******************************************************************        
025100     SKIP1                                                                
025200     MOVE ZERO TO W-ARTIKEL-ANTAL                                         
025300     COMPUTE W-UNDRE-GRANS =                                              
025400                      LINK-KVSLAGER (1) -                                 
025500                    ((LINK-KVPB-SEP (1) ) / 4.33)                         
025600     IF  LINK-KDVVKL < 3                                                  
025700         COMPUTE W-OVRE-GRANS =                                           
025800                          LINK-KVSLAGER (1)                               
025900                       +  LINK-KVQ                                        
026000                       +  2 * ( LINK-KVPB-SEP (1)                         
026100                             +  LINK-KVPB-SATS (1) )                      
026200     ELSE                                                                 
026300         MOVE LINK-KVOEKORR TO W-OVRE-GRANS                               
026400     END-IF                                                               
026500     SKIP1                                                                
026600     MOVE LINK-TIFINLV TO W-TIFINLV                                       
026700     MOVE W-TIFINLV-1-4     TO TMP1-YYWW                                  
026800     MOVE LINK-TIAAVV-AKT   TO TMP2-YYWW                                  
026900     PERFORM WY2000P3                                                     
027000     IF TMP1-YYWW > TMP2-YYWW                                             
027100         MOVE W-TIFINLV-1-4 TO W-DATUM-TOM                                
027200         MOVE LINK-TIAAVV-AKT TO W-DATUM-FROM                             
027300         PERFORM S01-BERAKNA-VECKODIFFERENS                               
027400         COMPUTE W-KVVECKOR-FT = W-VECKO-DIFFERENS - 1                    
027500     ELSE                                                                 
027600         MOVE LINK-KVVECKOR-FT TO W-KVVECKOR-FT                           
027700     END-IF                                                               
027800     SKIP2                                                                
027900     MOVE 1 TO IX                                                         
028000     PERFORM UNTIL NOT(                                                   
028100        IX NOT > W-KVVECKOR-FT AND < +157)                                
028200         ADD TILLGTAB-ANTAL (IX) TO W-TILLG                               
028300         SUBTRACT LNK2-KVBEHOV-VECKA (IX)                                 
028400                                 FROM W-TILLG                             
028500         IF  LINK-KDVVKL < 3                                              
028600             ADD TILLGTAB-ANTAL (IX) TO W-ARTIKEL-ANTAL                   
028700         END-IF                                                           
028800         ADD 1 TO IX                                                      
028900     END-PERFORM                                                          
029000     SKIP1                                                                
029100     IF  LINK-KDVVKL < 3                                                  
029200         MOVE LINK-KVVECKOR-FT TO IX-SLUTVARDE                            
029300*FIX     ADD 5 TO IX-SLUTVARDE                                            
029400         ADD 20 TO IX-SLUTVARDE                                           
029500     ELSE                                                                 
029600         MOVE LINK-KVVECKOR-FT TO IX-SLUTVARDE                            
029700         MOVE LINK-TIAAVV-AKT   TO TMP1-YYWW                              
029800         MOVE 9431              TO TMP2-YYWW                              
029900         PERFORM WY2000P3                                                 
030000         IF TMP1-YYWW < TMP2-YYWW                                         
030100            ADD 4 TO IX-SLUTVARDE                                         
030200         ELSE                                                             
030300*FIX        ADD 8 TO IX-SLUTVARDE                                         
030400            ADD 20 TO IX-SLUTVARDE                                        
030500         END-IF                                                           
030600     END-IF                                                               
030700     IF IX-SLUTVARDE > TILLGTAB-MAX                                       
030800         DISPLAY 'SLUTVDE ' IX-SLUTVARDE                                  
030900         MOVE TILLGTAB-MAX TO IX-SLUTVARDE                                
031000         DISPLAY 'TOO LONG ' LINK-IDARTNR                                 
031100     END-IF                                                               
031200     PERFORM UNTIL NOT(                                                   
031300        IX NOT > IX-SLUTVARDE AND SW-OMSPEC-ARTIKEL = NEJ)                
031400         ADD TILLGTAB-ANTAL (IX) TO W-TILLG                               
031500         SUBTRACT LNK2-KVBEHOV-VECKA (IX)                                 
031600                                 FROM W-TILLG                             
031700         IF  LINK-KDVVKL < 3                                              
031800             ADD TILLGTAB-ANTAL (IX) TO W-ARTIKEL-ANTAL                   
031900         END-IF                                                           
032000     SKIP1                                                                
032100         PERFORM DA-TESTA-GRANSER                                         
032200         ADD 1 TO IX                                                      
032300     END-PERFORM                                                          
032400     SKIP1                                                                
032500     IF SW-OMSPEC-ARTIKEL   = JA                                          
033300         MOVE 22 TO W-KDLPORS                                             
033500         PERFORM DB-UPPDAT-ORSAKSTABELL                                   
033600     END-IF                                                               
033700     .                                                                    
033800     EJECT                                                                
033900 DA-TESTA-GRANSER SECTION.                                                
034000******************************************************************        
034100*                                                                *        
034200*    TEST AV BERÄKNAD VECKOTILLGÅNG MOT ACCEPTABLA               *        
034300*    ÖVRE- OCH UNDRE LAGERNIVÅER                                 *        
034400*                                                                *        
034500******************************************************************        
034600     SKIP1                                                                
034700     IF  W-TILLG < W-UNDRE-GRANS                                          
034800        MOVE JA TO SW-OMSPEC-ARTIKEL                                      
034900     ELSE                                                                 
035000       IF  (W-TILLG > W-OVRE-GRANS                                        
035100             AND SW-INLEV-UNDER-PERIOD = JA)                              
035200     SKIP1                                                                
035300         IF  LINK-KDVVKL < 3                                              
035400**********   COMPUTE W-VARDE =                                            
035500**********           W-ARTIKEL-ANTAL * LINK-PRARTSTD                      
035600**********   IF  W-VARDE > MIN-AVROPSVARDE-VVKL1-2                        
035700                 MOVE JA TO SW-OMSPEC-ARTIKEL                             
035800**********   END-IF                                                       
035900         ELSE                                                             
036000             MOVE JA TO SW-OMSPEC-ARTIKEL                                 
036100        END-IF                                                            
036200      END-IF                                                              
036300     END-IF                                                               
036400*FIX                                                                      
036500     MOVE JA TO SW-OMSPEC-ARTIKEL                                         
036600     .                                                                    
036700     EJECT                                                                
036800 DB-UPPDAT-ORSAKSTABELL SECTION.                                          
036900     SKIP3                                                                
037010     MOVE LINK-KDLPORS-TAB (01)     TO W-KDLPORS-TAB (01)                 
037020     MOVE LINK-KDLPORS-TAB (02)     TO W-KDLPORS-TAB (02)                 
037030     MOVE LINK-KDLPORS-TAB (03)     TO W-KDLPORS-TAB (03)                 
037040     MOVE W-KDLPORS                 TO W-KDLPORS-TAB (04)                 
037048                                                                          
037050     CALL W221LPAD USING W-W221LP-CTX W-KDLPORS-GRP                       
037051                                                                          
037060     MOVE W-KDLPORS-TAB (01)        TO LINK-KDLPORS-TAB (01)              
037070     MOVE W-KDLPORS-TAB (02)        TO LINK-KDLPORS-TAB (02)              
037080     MOVE W-KDLPORS-TAB (03)        TO LINK-KDLPORS-TAB (03)              
037100     .                                                                    
037200     EJECT                                                                
037300 S01-BERAKNA-VECKODIFFERENS SECTION.                                      
037400     SKIP3                                                                
037500     MOVE W-DATUM-TOM     TO W-DATUM-AAVV-TOM                             
037600     MOVE W-DATUM-FROM    TO W-DATUM-AAVV-FROM                            
037700     MOVE W-DATUM-AA-TOM  TO TMP1-YY                                      
037800     MOVE W-DATUM-AA-FROM TO TMP2-YY                                      
037900     PERFORM WY2000P9                                                     
038000     COMPUTE W-VECKO-DIFFERENS =                                          
038100        (TMP1-YY * 52 + W-DATUM-VV-TOM) -                                 
038200        (TMP2-YY * 52 + W-DATUM-VV-FROM)                                  
038300     IF W-DATUM-AA-FROM = 4 AND W-DATUM-AA-TOM > 4                        
038400        ADD +1 TO W-VECKO-DIFFERENS                                       
038500     END-IF                                                               
038600     .                                                                    
038700     EJECT                                                                
038800*    -COPY WY2000P3                                                       
038900     EJECT                                                                
039000*    -COPY WY2000P9                                                       
