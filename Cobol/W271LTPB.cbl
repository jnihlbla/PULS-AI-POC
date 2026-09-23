000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W271LTPB.                                                
000500 AUTHOR.         STEFAN KIHLBERG.                                         
000600 DATE-WRITTEN.   96/09/06.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*    FUNKTION:                                                            
001000*        BERÄKNAR BEHOV UNDER BÅTLEDTID ELLER FRAM TILL ANGIVET           
001100*        ETA-DATUM                                                        
001200*                                                                         
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001710*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP2                                                                
002800 FILE SECTION.                                                            
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -COPY WY2000W1                                                       
003300     SKIP3                                                                
003400 77  IDPGM                         PIC X(8)    VALUE 'W271LTPB'.          
003500 77  JA                            PIC X       VALUE 'J'.                 
003600 77  NEJ                           PIC X       VALUE 'N'.                 
003700     EJECT                                                                
003800 01  DAGENS-DATUM                  PIC 9(6)    VALUE ZERO.                
003900 01  FILLER REDEFINES DAGENS-DATUM.                                       
004000     03  DAGENS-DATUM-AAR          PIC 9(2).                              
004100     03  DAGENS-DATUM-MAANAD       PIC 9(2).                              
004200     03  DAGENS-DATUM-DAG          PIC 9(2).                              
004300     EJECT                                                                
004400*      --- VALID IDDC CODES                                               
004500*                                                                         
004600*01    -COPY WWDCKONS                                                     
004700       EJECT                                                              
004800 01  ARBETSFAELT.                                                         
004900*                                                                         
005000*                                                                         
005600     03  WS-PER-I-START-TIAAMMDD   PIC 9(6)    VALUE ZERO.                
005700     03  WS-PER-II-START-TIAAMMDD  PIC 9(6)    VALUE ZERO.                
005800     03  WS-PER-III-START-TIAAMMDD PIC 9(6)    VALUE ZERO.                
005900     03  WS-PER-IV-START-TIAAMMDD  PIC 9(6)    VALUE ZERO.                
006000     03  WS-PER-V-START-TIAAMMDD   PIC 9(6)    VALUE ZERO.                
006100     03  WS-PER-VI-START-TIAAMMDD  PIC 9(6)    VALUE ZERO.                
006200     03  WS-PER-VII-START-TIAAMMDD PIC 9(6)    VALUE ZERO.                
006300                                                                          
006400     03  WS-PER-I-TIAARP           PIC 9(4).                              
006500     03  FILLER REDEFINES WS-PER-I-TIAARP.                                
006600         05 WS-PER-I-TIAA          PIC 9(2).                              
006700         05 WS-PER-I-TIRP          PIC 9(2).                              
006800                                                                          
006900     03  WS-PER-II-TIAARP          PIC 9(4).                              
007000     03  FILLER REDEFINES WS-PER-II-TIAARP.                               
007100         05 WS-PER-II-TIAA         PIC 9(2).                              
007200         05 WS-PER-II-TIRP         PIC 9(2).                              
007300                                                                          
007400     03  WS-PER-III-TIAARP         PIC 9(4).                              
007500     03  FILLER REDEFINES WS-PER-III-TIAARP.                              
007600         05 WS-PER-III-TIAA        PIC 9(2).                              
007700         05 WS-PER-III-TIRP        PIC 9(2).                              
007800                                                                          
007900     03  WS-PER-IV-TIAARP          PIC 9(4).                              
008000     03  FILLER REDEFINES WS-PER-IV-TIAARP.                               
008100         05 WS-PER-IV-TIAA         PIC 9(2).                              
008200         05 WS-PER-IV-TIRP         PIC 9(2).                              
008300                                                                          
008400     03  WS-PER-V-TIAARP           PIC 9(4).                              
008500     03  FILLER REDEFINES WS-PER-V-TIAARP.                                
008600         05 WS-PER-V-TIAA          PIC 9(2).                              
008700         05 WS-PER-V-TIRP          PIC 9(2).                              
008800                                                                          
008900     03  WS-PER-VI-TIAARP          PIC 9(4).                              
009000     03  FILLER REDEFINES WS-PER-VI-TIAARP.                               
009100         05 WS-PER-VI-TIAA         PIC 9(2).                              
009200         05 WS-PER-VI-TIRP         PIC 9(2).                              
009300                                                                          
009400     03  WS-PER-VII-TIAARP         PIC 9(4).                              
009500     03  FILLER REDEFINES WS-PER-VII-TIAARP.                              
009600         05 WS-PER-VII-TIAA        PIC 9(2).                              
009700         05 WS-PER-VII-TIRP        PIC 9(2).                              
009800                                                                          
009900     03  WS-BINNDAY-TIAARP     PIC 9(4)    VALUE ZERO.                    
010000     03  FILLER REDEFINES WS-BINNDAY-TIAARP.                              
010100         05 WS-BINNDAY-TIAA    PIC 9(2).                                  
010200         05 WS-BINNDAY-TIRP    PIC 9(2).                                  
010300                                                                          
010400                                                                          
010500*                                                                         
010600     EJECT                                                                
010700 01  DYNAMISKA-SUBPROGRAM.                                                
010800*                                                                         
010900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011300     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
011400     EJECT                                                                
011500*    --- PARAMETRAR TILL POSTSUM                                          
011600     EJECT                                                                
011700     EJECT                                                                
012100*    ---PARAMETRAR TILL DATKONV                                           
012200*01  -COPY WDATAREA                                                       
012300     EJECT                                                                
012400*    ---PARAMETRAR TILL DAGKONV                                           
012500*01  -COPY WDAGAREA                                                       
012600     EJECT                                                                
012700*    --- PARAMETRAR TILL ABEND                                            
012800                                                                          
012900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013100     SKIP2                                                                
013200 01  FELTEXT.                                                             
013300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013500     EJECT                                                                
013600 LINKAGE SECTION.                                                         
013700                                                                          
013800*01  -COPY W271LTPB                                                       
013900                                                                          
014000     EJECT                                                                
014100 PROCEDURE DIVISION  USING W271LTPB-W271LTPB.                             
014200                                                                          
014300     PERFORM A-INIT                                                       
014400     PERFORM B-NOLLSTALL                                                  
014500     IF W271LTPB-BINNDAY-TIAAMMDD = ZERO                                  
014600        PERFORM C-BINNDATUM-NDC                                           
014700     END-IF                                                               
014800     PERFORM D-ARBETSDAGAR-U-LEDTID-NDC                                   
014900     MOVE ZERO TO RETURN-CODE                                             
015000     GOBACK                                                               
015100     .                                                                    
015200     EJECT                                                                
015300                                                                          
015400                                                                          
015500 A-INIT SECTION.                                                          
015600                                                                          
015700     ACCEPT DAGENS-DATUM FROM DATE                                        
015800     .                                                                    
015900     EJECT                                                                
016000                                                                          
016100                                                                          
016200                                                                          
016300 B-NOLLSTALL SECTION.                                                     
016400                                                                          
016500                                                                          
016600     MOVE ZERO    TO  WS-PER-I-TIAARP                                     
016700                      WS-PER-II-TIAARP                                    
016800                      WS-PER-III-TIAARP                                   
016900                      WS-PER-IV-TIAARP                                    
017000                      WS-PER-V-TIAARP                                     
017100                      WS-PER-VI-TIAARP                                    
017200                      WS-PER-VII-TIAARP                                   
017300                      WS-PER-I-START-TIAAMMDD                             
017400                      WS-PER-II-START-TIAAMMDD                            
017500                      WS-PER-III-START-TIAAMMDD                           
017600                      WS-PER-IV-START-TIAAMMDD                            
017700                      WS-PER-V-START-TIAAMMDD                             
017800                      WS-PER-VI-START-TIAAMMDD                            
017900                      WS-PER-VII-START-TIAAMMDD                           
018300                                                                          
018400     .                                                                    
018500     EJECT                                                                
018600                                                                          
018700                                                                          
018801 C-BINNDATUM-NDC    SECTION.                                              
018900                                                                          
019000     MOVE W271LTPB-KVDLTID-TOT                                            
019100                                TO DAG-KVKALDAG                           
019200*    ADD 1                      TO DAG-KVKALDAG                           
019300                                                                          
019400     IF W271LTPB-START-DATUM = ZERO                                       
019500       MOVE DAGENS-DATUM        TO DAG-TIAAMMDD-FOM                       
019600     ELSE                                                                 
019700       MOVE W271LTPB-START-DATUM                                          
019800                                TO DAG-TIAAMMDD-FOM                       
019900     END-IF                                                               
020100     MOVE 002                   TO DAG-KDCALL                             
020200     CALL WDAGKONV USING DAG-KDCALL                                       
020300                         DAG-DATUM-AREA                                   
020400                         DAG-KDSVAR                                       
020500                                                                          
020600     IF DAG-KDSVAR = SPACE                                                
020700        MOVE DAG-TIAAMMDD-TOM     TO  W271LTPB-BINNDAY-TIAAMMDD           
020800     ELSE                                                                 
020900        MOVE 'FEL FRÅN WDAGKONV 1, I C-SECTION I W271LTPB'                
021000                                 TO   FELTEXT-STR                         
021100        DISPLAY FELTEXT                                                   
021200        PERFORM S99-ABEND                                                 
021300     END-IF                                                               
021400     .                                                                    
021500     EJECT                                                                
030700                                                                          
030800 D-ARBETSDAGAR-U-LEDTID-NDC SECTION.                                      
030900                                                                          
031000*  TRANSPORTTID HYLLA CDC => HYLLA NDC  KAN STRÄCKA SIG ÖVER              
031100*  TRE PERIODER, BEHOVET UNDER LEDTIDEN MÅSTE SÄSONGSANPASSAS.            
031200*  I SEKTIONEN BERÄKNAS HUR MÅNGA ARBETSDAGAR(FÖRBRUKNINGSDAGAR)          
031300*  SOM LIGGER I RESP PERIOD FRAM TILL BINNDAGEN.                          
031400*                                                                         
031500*  DÅ LEDTIDEN FÖR LOKALA ARTIKLAR I NA KAN VARA UPP TILL 99              
031600*  DAGAR BERÄKNAS LEDTID FÖR 7 PERIODER                                   
031700*  I SEKTIONEN KALLAS DE TRE PERIODERNA "PERIOD-I",                       
031800*  "PERIOD-II" OCH "PERIOD-III", OAVSETT PERIODERNAS 'RIKTIGA'            
031900*  NAMN.                                                                  
032000*  PERIOD-VII ANVÄNDS FÖR ATT SKAPA EN BORTRE GRÄNS                       
032100                                                                          
032200*  1. VILKEN PERIOD ÄR DET NU (PERIOD I)                                  
032300     MOVE 'AAMMDD'         TO DAT-KDDATFORM                               
032400     IF W271LTPB-START-DATUM = ZERO                                       
032500       MOVE DAGENS-DATUM   TO DAT-I-TIDATUM                               
032600     ELSE                                                                 
032700       MOVE W271LTPB-START-DATUM                                          
032800                           TO DAT-I-TIDATUM                               
032900     END-IF                                                               
033000     CALL WDATKONV USING   DAT-KDDATFORM                                  
033100                           DAT-I-TIDATUM                                  
033200                           DAT-O-TIDATUM                                  
033300                           DAT-KDSVAR                                     
033400                                                                          
033500     IF DAT-KDSVAR-OK                                                     
033600        MOVE DAT-TIAARP    TO WS-PER-I-TIAARP                             
033703                              W271LTPB-PER-I-TIAARP                       
033800        MOVE DAT-TIRP      TO W271LTPB-PER-I-TIRP                         
033900     ELSE                                                                 
034000        MOVE 'FEL FRÅN WDATKONV I  I D SECTION I W271LTPB' TO             
034100                                    FELTEXT-STR                           
034200        DISPLAY FELTEXT                                                   
034300        PERFORM S99-ABEND                                                 
034400     END-IF                                                               
034500                                                                          
034600                                                                          
034700*  2. VILKA ÄR PERIODERNA OCH NÄR BÖRJAR DOM                              
034800                                                                          
034900**    VILKEN PERIOD ÄR PERIOD-II                                          
035303     MOVE WS-PER-I-TIAARP     TO WS-PER-II-TIAARP                         
035403     IF WS-PER-I-TIRP = 12                                                
035503        MOVE 01 TO WS-PER-II-TIRP                                         
035603        ADD 1 TO WS-PER-II-TIAA                                           
035703     ELSE                                                                 
035803        ADD 1 TO WS-PER-II-TIRP                                           
035903     END-IF                                                               
036003                                                                          
036100     MOVE WS-PER-II-TIRP TO W271LTPB-PER-II-TIRP                          
036203     MOVE WS-PER-II-TIAARP TO W271LTPB-PER-II-TIAARP                      
036303                                                                          
036403**  NÄR BÖRJAR PERIOD-II?                                                 
036503     MOVE 'AARP  '         TO DAT-KDDATFORM                               
036603     MOVE WS-PER-II-TIAARP TO DAT-I-TIDATUM                               
036703     CALL WDATKONV USING   DAT-KDDATFORM                                  
036803                           DAT-I-TIDATUM                                  
036903                           DAT-O-TIDATUM                                  
037003                           DAT-KDSVAR                                     
037103                                                                          
037203     IF DAT-KDSVAR-OK                                                     
037303        MOVE DAT-TIAAMMDD  TO WS-PER-II-START-TIAAMMDD                    
037403     ELSE                                                                 
037503        MOVE 'FEL FRÅN WDATKONV II I D  SECTION I W271LTPB' TO            
037603                                    FELTEXT-STR                           
037703        DISPLAY FELTEXT                                                   
037803        PERFORM S99-ABEND                                                 
037903     END-IF                                                               
038003                                                                          
038103                                                                          
038203                                                                          
038303**    VILKEN PERIOD ÄR PERIOD-III                                         
038703     MOVE WS-PER-II-TIAARP     TO WS-PER-III-TIAARP                       
038803     IF WS-PER-II-TIRP = 12                                               
038903        MOVE 01 TO WS-PER-III-TIRP                                        
039003        ADD 1 TO WS-PER-III-TIAA                                          
039103     ELSE                                                                 
039203        ADD 1 TO WS-PER-III-TIRP                                          
039303     END-IF                                                               
039503                                                                          
039603     MOVE WS-PER-III-TIRP TO W271LTPB-PER-III-TIRP                        
039703     MOVE WS-PER-III-TIAARP TO W271LTPB-PER-III-TIAARP                    
039803                                                                          
039903**  NÄR BÖRJAR PERIOD-III                                                 
040003     MOVE 'AARP  '           TO DAT-KDDATFORM                             
040103     MOVE WS-PER-III-TIAARP  TO DAT-I-TIDATUM                             
040203     CALL WDATKONV USING     DAT-KDDATFORM                                
040303                             DAT-I-TIDATUM                                
040403                             DAT-O-TIDATUM                                
040503                             DAT-KDSVAR                                   
040603     IF DAT-KDSVAR-OK                                                     
040703        MOVE DAT-TIAAMMDD  TO WS-PER-III-START-TIAAMMDD                   
040803     ELSE                                                                 
040903        MOVE 'FEL FRÅN WDATKONV III I D SECTION I W271LTPB'               
041003                                 TO  FELTEXT-STR                          
041103        DISPLAY FELTEXT                                                   
041203        PERFORM S99-ABEND                                                 
041303     END-IF                                                               
041403                                                                          
041503                                                                          
041603                                                                          
041703*PER IV                                                                   
041803**    VILKEN PERIOD ÄR PERIOD-IV                                          
042203     MOVE WS-PER-III-TIAARP TO WS-PER-IV-TIAARP                           
042303     IF WS-PER-III-TIRP = 12                                              
042403        MOVE 01 TO WS-PER-IV-TIRP                                         
042503        ADD 1 TO WS-PER-IV-TIAA                                           
042603     ELSE                                                                 
042703        ADD 1 TO WS-PER-IV-TIRP                                           
042803     END-IF                                                               
043003                                                                          
043103     MOVE WS-PER-IV-TIRP TO W271LTPB-PER-IV-TIRP                          
043203     MOVE WS-PER-IV-TIAARP TO W271LTPB-PER-IV-TIAARP                      
043303                                                                          
043403**  NÄR BÖRJAR PERIOD-IV                                                  
043503     MOVE 'AARP  '           TO DAT-KDDATFORM                             
043603     MOVE WS-PER-IV-TIAARP   TO DAT-I-TIDATUM                             
043703     CALL WDATKONV USING     DAT-KDDATFORM                                
043803                             DAT-I-TIDATUM                                
043903                             DAT-O-TIDATUM                                
044003                             DAT-KDSVAR                                   
044103     IF DAT-KDSVAR-OK                                                     
044203        MOVE DAT-TIAAMMDD  TO WS-PER-IV-START-TIAAMMDD                    
044303     ELSE                                                                 
044403        MOVE 'FEL FRÅN WDATKONV IV  I D SECTION I W271LTPB' TO            
044503                                    FELTEXT-STR                           
044603        DISPLAY FELTEXT                                                   
044703        PERFORM S99-ABEND                                                 
044803     END-IF                                                               
044903                                                                          
045003                                                                          
045103*PER V                                                                    
045203**    VILKEN PERIOD ÄR PERIOD-V                                           
045603     MOVE WS-PER-IV-TIAARP TO WS-PER-V-TIAARP                             
045703     IF WS-PER-IV-TIRP = 12                                               
045803        MOVE 01 TO WS-PER-V-TIRP                                          
045903        ADD 1 TO WS-PER-V-TIAA                                            
046003     ELSE                                                                 
046103        ADD 1 TO WS-PER-V-TIRP                                            
046203     END-IF                                                               
046403                                                                          
046503     MOVE WS-PER-V-TIRP TO W271LTPB-PER-V-TIRP                            
046603     MOVE WS-PER-V-TIAARP TO W271LTPB-PER-V-TIAARP                        
046703                                                                          
046803                                                                          
046903**  NÄR BÖRJAR PERIOD-V                                                   
047003     MOVE 'AARP  '           TO DAT-KDDATFORM                             
047103     MOVE WS-PER-V-TIAARP    TO DAT-I-TIDATUM                             
047203     CALL WDATKONV USING     DAT-KDDATFORM                                
047303                             DAT-I-TIDATUM                                
047403                             DAT-O-TIDATUM                                
047503                             DAT-KDSVAR                                   
047603     IF DAT-KDSVAR-OK                                                     
047703        MOVE DAT-TIAAMMDD  TO WS-PER-V-START-TIAAMMDD                     
047803     ELSE                                                                 
047903        MOVE 'FEL FRÅN WDATKONV V  I D SECTION I W271LTPB' TO             
048003                                    FELTEXT-STR                           
048103        DISPLAY FELTEXT                                                   
048203        PERFORM S99-ABEND                                                 
048303     END-IF                                                               
048403                                                                          
048503                                                                          
048603*PER IV                                                                   
048703**    VILKEN PERIOD ÄR PERIOD-VI                                          
049103     MOVE WS-PER-V-TIAARP TO WS-PER-VI-TIAARP                             
049203     IF WS-PER-V-TIRP = 12                                                
049303        MOVE 01 TO WS-PER-VI-TIRP                                         
049403        ADD 1 TO WS-PER-VI-TIAA                                           
049503     ELSE                                                                 
049603        ADD 1 TO WS-PER-VI-TIRP                                           
049703     END-IF                                                               
049903                                                                          
050003     MOVE WS-PER-VI-TIRP TO W271LTPB-PER-VI-TIRP                          
050103     MOVE WS-PER-VI-TIAARP TO W271LTPB-PER-VI-TIAARP                      
050203                                                                          
050303                                                                          
050403**  NÄR BÖRJAR PERIOD-VI                                                  
050503     MOVE 'AARP  '           TO DAT-KDDATFORM                             
050603     MOVE WS-PER-VI-TIAARP   TO DAT-I-TIDATUM                             
050703     CALL WDATKONV USING     DAT-KDDATFORM                                
050803                             DAT-I-TIDATUM                                
050903                             DAT-O-TIDATUM                                
051003                             DAT-KDSVAR                                   
051103     IF DAT-KDSVAR-OK                                                     
051203        MOVE DAT-TIAAMMDD  TO WS-PER-VI-START-TIAAMMDD                    
051303     ELSE                                                                 
051403        MOVE 'FEL FRÅN WDATKONV VI  I D SECTION I W271LTPB' TO            
051503                                    FELTEXT-STR                           
051603        DISPLAY FELTEXT                                                   
051703        PERFORM S99-ABEND                                                 
051803     END-IF                                                               
051903                                                                          
052003**    VILKEN PERIOD ÄR PERIOD-VII                                         
052403     MOVE WS-PER-VI-TIAARP TO WS-PER-VII-TIAARP                           
052503     IF WS-PER-VI-TIRP = 12                                               
052603        MOVE 01 TO WS-PER-VII-TIRP                                        
052703        ADD 1 TO WS-PER-VII-TIAA                                          
052803     ELSE                                                                 
052903        ADD 1 TO WS-PER-VII-TIRP                                          
053003     END-IF                                                               
053203                                                                          
053303*PER VII                                                                  
053403**  NÄR BÖRJAR PERIOD-VII                                                 
053503     MOVE 'AARP  '           TO DAT-KDDATFORM                             
053603     MOVE WS-PER-VII-TIAARP  TO DAT-I-TIDATUM                             
053703     CALL WDATKONV USING     DAT-KDDATFORM                                
053803                             DAT-I-TIDATUM                                
053903                             DAT-O-TIDATUM                                
054003                             DAT-KDSVAR                                   
054103     IF DAT-KDSVAR-OK                                                     
054203        MOVE DAT-TIAAMMDD  TO WS-PER-VII-START-TIAAMMDD                   
054303     ELSE                                                                 
054403        MOVE 'FEL FRÅN WDATKONV VII  I D SECTION I W271LTPB' TO           
054503                                    FELTEXT-STR                           
054603        DISPLAY FELTEXT                                                   
054703        PERFORM S99-ABEND                                                 
054803     END-IF                                                               
054903                                                                          
055003                                                                          
055103                                                                          
055203*  3. I VILKEN PERIOD INFALLER BINNINGDATUM                               
055303                                                                          
055403     MOVE W271LTPB-BINNDAY-TIAAMMDD   TO TMP1-YYMMDD                      
055503     MOVE WS-PER-II-START-TIAAMMDD    TO TMP2-YYMMDD                      
055603     PERFORM WY2000P1                                                     
055703     IF TMP1-YYMMDD < TMP2-YYMMDD                                         
055803        PERFORM DA-KVDAGAR-BINN-PER-I                                     
055903     ELSE                                                                 
056003        MOVE W271LTPB-BINNDAY-TIAAMMDD   TO TMP1-YYMMDD                   
056103        MOVE WS-PER-III-START-TIAAMMDD   TO TMP2-YYMMDD                   
056203        PERFORM WY2000P1                                                  
056303        IF TMP1-YYMMDD < TMP2-YYMMDD                                      
056403           PERFORM DB-KVDAGAR-BINN-PER-II                                 
056503        ELSE                                                              
056603           MOVE W271LTPB-BINNDAY-TIAAMMDD   TO TMP1-YYMMDD                
056703           MOVE WS-PER-IV-START-TIAAMMDD    TO TMP2-YYMMDD                
056803           PERFORM WY2000P1                                               
056903           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
057003              PERFORM DC-KVDAGAR-BINN-PER-III                             
057103           ELSE                                                           
057203              MOVE W271LTPB-BINNDAY-TIAAMMDD   TO TMP1-YYMMDD             
057303              MOVE WS-PER-V-START-TIAAMMDD     TO TMP2-YYMMDD             
057403              PERFORM WY2000P1                                            
057503              IF TMP1-YYMMDD < TMP2-YYMMDD                                
057603                 PERFORM DD-KVDAGAR-BINN-PER-IV                           
057703              ELSE                                                        
057803                 MOVE W271LTPB-BINNDAY-TIAAMMDD                           
057903                                      TO TMP1-YYMMDD                      
058003                 MOVE WS-PER-VI-START-TIAAMMDD                            
058103                                      TO TMP2-YYMMDD                      
058203                 PERFORM WY2000P1                                         
058303                 IF TMP1-YYMMDD < TMP2-YYMMDD                             
058403                    PERFORM DE-KVDAGAR-BINN-PER-V                         
058503                 ELSE                                                     
058603                    PERFORM DF-KVDAGAR-BINN-PER-VI                        
058703                 END-IF                                                   
058803              END-IF                                                      
058903           END-IF                                                         
059003        END-IF                                                            
059103     END-IF                                                               
059203                                                                          
059303                                                                          
059403     .                                                                    
059503     EJECT                                                                
059603                                                                          
059703                                                                          
059803 DA-KVDAGAR-BINN-PER-I SECTION.                                           
059903                                                                          
060003*PERI                                                                     
060103     IF W271LTPB-START-DATUM = ZERO                                       
060203       MOVE DAGENS-DATUM        TO DAG-TIAAMMDD-FOM                       
060303     ELSE                                                                 
060403       MOVE W271LTPB-START-DATUM                                          
060503                                TO DAG-TIAAMMDD-FOM                       
060603     END-IF                                                               
060703     MOVE W271LTPB-BINNDAY-TIAAMMDD   TO DAG-TIAAMMDD-TOM                 
060803     MOVE 001                         TO DAG-KDCALL                       
060902     CALL WDAGKONV USING DAG-KDCALL                                       
061002                         DAG-DATUM-AREA                                   
061102                         DAG-KDSVAR                                       
061202                                                                          
061302     IF DAG-KDSVAR = SPACE                                                
061402        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-I                       
061403        COMPUTE W271LTPB-KVDAGAR-PER-I =                                  
061404                W271LTPB-KVDAGAR-PER-I                                    
061500     ELSE                                                                 
061602        MOVE 'FEL FRÅN WDAGKONV 1 I DA SECTION I W271LTPB'                
061700                                 TO   FELTEXT-STR                         
061800        DISPLAY FELTEXT                                                   
061900        PERFORM S99-ABEND                                                 
062000     END-IF                                                               
062100                                                                          
062200     MOVE ZERO            TO W271LTPB-KVDAGAR-PER-II                      
062300                             W271LTPB-KVDAGAR-PER-III                     
062400                             W271LTPB-KVDAGAR-PER-IV                      
062500                             W271LTPB-KVDAGAR-PER-V                       
062600                             W271LTPB-KVDAGAR-PER-VI                      
062700                                                                          
062800     MOVE WS-PER-I-TIRP   TO W271LTPB-BINNDAY-TIRP                        
062900     .                                                                    
063000     EJECT                                                                
063100                                                                          
063200                                                                          
063302 DB-KVDAGAR-BINN-PER-II SECTION.                                          
063400                                                                          
063500*PERI                                                                     
063600     IF W271LTPB-START-DATUM = ZERO                                       
063702       MOVE DAGENS-DATUM        TO DAG-TIAAMMDD-FOM                       
063800     ELSE                                                                 
063900       MOVE W271LTPB-START-DATUM                                          
064002                                TO DAG-TIAAMMDD-FOM                       
064100     END-IF                                                               
064202     MOVE WS-PER-II-START-TIAAMMDD TO DAG-TIAAMMDD-TOM                    
064302     MOVE 001                      TO DAG-KDCALL                          
064402     CALL WDAGKONV USING DAG-KDCALL                                       
064502                         DAG-DATUM-AREA                                   
064602                         DAG-KDSVAR                                       
064702     IF DAG-KDSVAR = SPACE                                                
064802        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-I                       
064803        COMPUTE W271LTPB-KVDAGAR-PER-I =                                  
064804                W271LTPB-KVDAGAR-PER-I - 1                                
064900     ELSE                                                                 
065002        MOVE 'FEL FRÅN WDAGKONV 1 I DB SECTION I W271LTPB'                
065100                                    TO FELTEXT-STR                        
065200        DISPLAY FELTEXT                                                   
065300        PERFORM S99-ABEND                                                 
065400     END-IF                                                               
065500                                                                          
065600*PERII                                                                    
065702     MOVE WS-PER-II-START-TIAAMMDD   TO DAG-TIAAMMDD-FOM                  
065802     MOVE W271LTPB-BINNDAY-TIAAMMDD  TO DAG-TIAAMMDD-TOM                  
065902     MOVE 001                        TO DAG-KDCALL                        
066002     CALL WDAGKONV USING DAG-KDCALL                                       
066102                         DAG-DATUM-AREA                                   
066202                         DAG-KDSVAR                                       
066302     IF DAG-KDSVAR = SPACE                                                
066402        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-II                      
066500     ELSE                                                                 
066602        MOVE 'FEL FRÅN WDAGKONV 2 I DB SECTION I W271LTPB'                
066700                                    TO FELTEXT-STR                        
066800        DISPLAY FELTEXT                                                   
066900        PERFORM S99-ABEND                                                 
067000     END-IF                                                               
067100                                                                          
067200     MOVE ZERO             TO W271LTPB-KVDAGAR-PER-III                    
067300                              W271LTPB-KVDAGAR-PER-IV                     
067400                              W271LTPB-KVDAGAR-PER-V                      
067500                              W271LTPB-KVDAGAR-PER-VI                     
067600                                                                          
067700     MOVE WS-PER-II-TIRP   TO W271LTPB-BINNDAY-TIRP                       
067800                                                                          
067900     .                                                                    
068000     EJECT                                                                
068100                                                                          
068200                                                                          
068300 DC-KVDAGAR-BINN-PER-III SECTION.                                         
068400                                                                          
068500*PERI                                                                     
068600     IF W271LTPB-START-DATUM = ZERO                                       
068702       MOVE DAGENS-DATUM        TO DAG-TIAAMMDD-FOM                       
068800     ELSE                                                                 
068900       MOVE W271LTPB-START-DATUM                                          
069002                                TO DAG-TIAAMMDD-FOM                       
069100     END-IF                                                               
069202     MOVE WS-PER-II-START-TIAAMMDD TO DAG-TIAAMMDD-TOM                    
069302     MOVE 001                      TO DAG-KDCALL                          
069502     CALL WDAGKONV USING DAG-KDCALL                                       
069602                         DAG-DATUM-AREA                                   
069702                         DAG-KDSVAR                                       
069802     IF DAG-KDSVAR = SPACE                                                
069902        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-I                       
069903        COMPUTE W271LTPB-KVDAGAR-PER-I =                                  
069904                W271LTPB-KVDAGAR-PER-I - 1                                
070000     ELSE                                                                 
070102        MOVE 'FEL FRÅN WDAGKONV 1 I DC SECTION I W271LTPB'                
070200                                   TO FELTEXT-STR                         
070300        DISPLAY FELTEXT                                                   
070400        PERFORM S99-ABEND                                                 
070500     END-IF                                                               
070600                                                                          
070700*PERII                                                                    
070802     MOVE WS-PER-II-START-TIAAMMDD  TO DAG-TIAAMMDD-FOM                   
070902     MOVE WS-PER-III-START-TIAAMMDD TO DAG-TIAAMMDD-TOM                   
071002     MOVE 001                       TO DAG-KDCALL                         
071202     CALL WDAGKONV USING DAG-KDCALL                                       
071302                         DAG-DATUM-AREA                                   
071402                         DAG-KDSVAR                                       
071502     IF DAG-KDSVAR = SPACE                                                
071602        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-II                      
071700        COMPUTE W271LTPB-KVDAGAR-PER-II =                                 
071800                W271LTPB-KVDAGAR-PER-II - 1                               
071900     ELSE                                                                 
072002        MOVE 'FEL FRÅN WDAGKONV 2 I DC SECTION I W271LTPB'                
072100                                    TO FELTEXT-STR                        
072200        DISPLAY FELTEXT                                                   
072300        PERFORM S99-ABEND                                                 
072400     END-IF                                                               
072500                                                                          
072600*PER III                                                                  
072702     MOVE WS-PER-III-START-TIAAMMDD  TO DAG-TIAAMMDD-FOM                  
072802     MOVE W271LTPB-BINNDAY-TIAAMMDD  TO DAG-TIAAMMDD-TOM                  
072902     MOVE 001                        TO DAG-KDCALL                        
073102     CALL WDAGKONV USING DAG-KDCALL                                       
073202                         DAG-DATUM-AREA                                   
073302                         DAG-KDSVAR                                       
073402     IF DAG-KDSVAR = SPACE                                                
073502        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-III                     
073600     ELSE                                                                 
073702        MOVE 'FEL FRÅN WDAGKONV 3 I DC SECTION I W271LTPB'                
073800                                    TO FELTEXT-STR                        
073900        DISPLAY FELTEXT                                                   
074000        PERFORM S99-ABEND                                                 
074100     END-IF                                                               
074200                                                                          
074300     MOVE ZERO             TO W271LTPB-KVDAGAR-PER-IV                     
074400                              W271LTPB-KVDAGAR-PER-V                      
074500                              W271LTPB-KVDAGAR-PER-VI                     
074600                                                                          
074700     MOVE WS-PER-III-TIRP   TO W271LTPB-BINNDAY-TIRP                      
074800     .                                                                    
074900     EJECT                                                                
075000                                                                          
075100                                                                          
075200 DD-KVDAGAR-BINN-PER-IV SECTION.                                          
075300                                                                          
075400*PER I                                                                    
075500     IF W271LTPB-START-DATUM = ZERO                                       
075602       MOVE DAGENS-DATUM        TO DAG-TIAAMMDD-FOM                       
075700     ELSE                                                                 
075800       MOVE W271LTPB-START-DATUM                                          
075902                                TO DAG-TIAAMMDD-FOM                       
076000     END-IF                                                               
076102     MOVE WS-PER-II-START-TIAAMMDD TO DAG-TIAAMMDD-TOM                    
076202     MOVE 001                      TO DAG-KDCALL                          
076402     CALL WDAGKONV USING DAG-KDCALL                                       
076502                         DAG-DATUM-AREA                                   
076602                         DAG-KDSVAR                                       
076702     IF DAG-KDSVAR = SPACE                                                
076802        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-I                       
076803        COMPUTE W271LTPB-KVDAGAR-PER-I =                                  
076804                W271LTPB-KVDAGAR-PER-I - 1                                
076900     ELSE                                                                 
077002        MOVE 'FEL FRÅN WDAGKONV 1 I DC SECTION I W271LTPB'                
077100                                   TO FELTEXT-STR                         
077200        DISPLAY FELTEXT                                                   
077300        PERFORM S99-ABEND                                                 
077400     END-IF                                                               
077500                                                                          
077600*PER II                                                                   
077702     MOVE WS-PER-II-START-TIAAMMDD  TO DAG-TIAAMMDD-FOM                   
077802     MOVE WS-PER-III-START-TIAAMMDD TO DAG-TIAAMMDD-TOM                   
077902     MOVE 001                       TO DAG-KDCALL                         
078102     CALL WDAGKONV USING DAG-KDCALL                                       
078202                         DAG-DATUM-AREA                                   
078302                         DAG-KDSVAR                                       
078402     IF DAG-KDSVAR = SPACE                                                
078502        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-II                      
078600        COMPUTE W271LTPB-KVDAGAR-PER-II =                                 
078700                W271LTPB-KVDAGAR-PER-II - 1                               
078800     ELSE                                                                 
078902        MOVE 'FEL FRÅN WDAGKONV 2 I DC SECTION I W271LTPB'                
079000                                    TO FELTEXT-STR                        
079100        DISPLAY FELTEXT                                                   
079200        PERFORM S99-ABEND                                                 
079300     END-IF                                                               
079400                                                                          
079500*PER III                                                                  
079602     MOVE WS-PER-III-START-TIAAMMDD TO DAG-TIAAMMDD-FOM                   
079702     MOVE WS-PER-IV-START-TIAAMMDD  TO DAG-TIAAMMDD-TOM                   
079802     MOVE 001                       TO DAG-KDCALL                         
080002     CALL WDAGKONV USING DAG-KDCALL                                       
080102                         DAG-DATUM-AREA                                   
080202                         DAG-KDSVAR                                       
080302     IF DAG-KDSVAR = SPACE                                                
080402        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-III                     
080500        COMPUTE W271LTPB-KVDAGAR-PER-III =                                
080600                W271LTPB-KVDAGAR-PER-III - 1                              
080700     ELSE                                                                 
080802        MOVE 'FEL FRÅN WDAGKONV 3 I DD SECTION I W271LTPB'                
080900                                    TO FELTEXT-STR                        
081000        DISPLAY FELTEXT                                                   
081100        PERFORM S99-ABEND                                                 
081200     END-IF                                                               
081300                                                                          
081400*PER IV                                                                   
081502     MOVE WS-PER-IV-START-TIAAMMDD   TO DAG-TIAAMMDD-FOM                  
081602     MOVE W271LTPB-BINNDAY-TIAAMMDD  TO DAG-TIAAMMDD-TOM                  
081702     MOVE 001                        TO DAG-KDCALL                        
081902     CALL WDAGKONV USING DAG-KDCALL                                       
082002                         DAG-DATUM-AREA                                   
082102                         DAG-KDSVAR                                       
082202     IF DAG-KDSVAR = SPACE                                                
082302        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-IV                      
082400     ELSE                                                                 
082502        MOVE 'FEL FRÅN WDAGKONV 4 I DD SECTION I W271LTPB'                
082600                                    TO FELTEXT-STR                        
082700        DISPLAY FELTEXT                                                   
082800        PERFORM S99-ABEND                                                 
082900     END-IF                                                               
083000                                                                          
083100     MOVE ZERO             TO W271LTPB-KVDAGAR-PER-V                      
083200                              W271LTPB-KVDAGAR-PER-VI                     
083300                                                                          
083400     MOVE WS-PER-IV-TIRP   TO W271LTPB-BINNDAY-TIRP                       
083500     .                                                                    
083600     EJECT                                                                
083700                                                                          
083800                                                                          
083900                                                                          
084000 DE-KVDAGAR-BINN-PER-V SECTION.                                           
084100                                                                          
084200*PER I                                                                    
084300     IF W271LTPB-START-DATUM = ZERO                                       
084402       MOVE DAGENS-DATUM        TO DAG-TIAAMMDD-FOM                       
084500     ELSE                                                                 
084600       MOVE W271LTPB-START-DATUM                                          
084702                                TO DAG-TIAAMMDD-FOM                       
084800     END-IF                                                               
084902     MOVE WS-PER-II-START-TIAAMMDD TO DAG-TIAAMMDD-TOM                    
085002     MOVE 001                      TO DAG-KDCALL                          
085202     CALL WDAGKONV USING DAG-KDCALL                                       
085302                         DAG-DATUM-AREA                                   
085402                         DAG-KDSVAR                                       
085502     IF DAG-KDSVAR = SPACE                                                
085602        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-I                       
085603        COMPUTE W271LTPB-KVDAGAR-PER-I =                                  
085604                W271LTPB-KVDAGAR-PER-I - 1                                
085700     ELSE                                                                 
085802        MOVE 'FEL FRÅN WDAGKONV 1 I DE SECTION I W271LTPB'                
085900                                   TO FELTEXT-STR                         
086000        DISPLAY FELTEXT                                                   
086100        PERFORM S99-ABEND                                                 
086200     END-IF                                                               
086300                                                                          
086400*PER II                                                                   
086502     MOVE WS-PER-II-START-TIAAMMDD  TO DAG-TIAAMMDD-FOM                   
086602     MOVE WS-PER-III-START-TIAAMMDD TO DAG-TIAAMMDD-TOM                   
086702     MOVE 001                       TO DAG-KDCALL                         
086902     CALL WDAGKONV USING DAG-KDCALL                                       
087002                         DAG-DATUM-AREA                                   
087102                         DAG-KDSVAR                                       
087202     IF DAG-KDSVAR = SPACE                                                
087302        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-II                      
087400        COMPUTE W271LTPB-KVDAGAR-PER-II =                                 
087500                W271LTPB-KVDAGAR-PER-II - 1                               
087600     ELSE                                                                 
087702        MOVE 'FEL FRÅN WDAGKONV 2 I DE SECTION I W271LTPB'                
087800                                    TO FELTEXT-STR                        
087900        DISPLAY FELTEXT                                                   
088000        PERFORM S99-ABEND                                                 
088100     END-IF                                                               
088200                                                                          
088300*PER III                                                                  
088402     MOVE WS-PER-III-START-TIAAMMDD TO DAG-TIAAMMDD-FOM                   
088502     MOVE WS-PER-IV-START-TIAAMMDD  TO DAG-TIAAMMDD-TOM                   
088602     MOVE 001                       TO DAG-KDCALL                         
088802     CALL WDAGKONV USING DAG-KDCALL                                       
088902                         DAG-DATUM-AREA                                   
089002                         DAG-KDSVAR                                       
089102     IF DAG-KDSVAR = SPACE                                                
089202        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-III                     
089300        COMPUTE W271LTPB-KVDAGAR-PER-III =                                
089400                W271LTPB-KVDAGAR-PER-III - 1                              
089500     ELSE                                                                 
089602        MOVE 'FEL FRÅN WDAGKONV 3 I DE SECTION I W271LTPB'                
089700                                    TO FELTEXT-STR                        
089800        DISPLAY FELTEXT                                                   
089900        PERFORM S99-ABEND                                                 
090000     END-IF                                                               
090100                                                                          
090200*PER IV                                                                   
090302     MOVE WS-PER-IV-START-TIAAMMDD  TO DAG-TIAAMMDD-FOM                   
090402     MOVE WS-PER-V-START-TIAAMMDD   TO DAG-TIAAMMDD-TOM                   
090502     MOVE 001                       TO DAG-KDCALL                         
090702     CALL WDAGKONV USING DAG-KDCALL                                       
090802                         DAG-DATUM-AREA                                   
090902                         DAG-KDSVAR                                       
091002     IF DAG-KDSVAR = SPACE                                                
091102        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-IV                      
091200        COMPUTE W271LTPB-KVDAGAR-PER-IV =                                 
091300                W271LTPB-KVDAGAR-PER-IV - 1                               
091400     ELSE                                                                 
091502        MOVE 'FEL FRÅN WDAGKONV 4 I DE SECTION I W271LTPB'                
091600                                    TO FELTEXT-STR                        
091700        DISPLAY FELTEXT                                                   
091800        PERFORM S99-ABEND                                                 
091900     END-IF                                                               
092000                                                                          
092100*PER V                                                                    
092202     MOVE WS-PER-V-START-TIAAMMDD    TO DAG-TIAAMMDD-FOM                  
092302     MOVE W271LTPB-BINNDAY-TIAAMMDD  TO DAG-TIAAMMDD-TOM                  
092402     MOVE 001                        TO DAG-KDCALL                        
092602     CALL WDAGKONV USING DAG-KDCALL                                       
092702                         DAG-DATUM-AREA                                   
092802                         DAG-KDSVAR                                       
092902     IF DAG-KDSVAR = SPACE                                                
093002        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-V                       
093100     ELSE                                                                 
093202        MOVE 'FEL FRÅN WDAGKONV 5 I DE SECTION I W271LTPB'                
093300                                    TO FELTEXT-STR                        
093400        DISPLAY FELTEXT                                                   
093500        PERFORM S99-ABEND                                                 
093600     END-IF                                                               
093700                                                                          
093800     MOVE ZERO             TO W271LTPB-KVDAGAR-PER-VI                     
093900                                                                          
094000     MOVE WS-PER-V-TIRP   TO W271LTPB-BINNDAY-TIRP                        
094100     .                                                                    
094200     EJECT                                                                
094300                                                                          
094400                                                                          
094500 DF-KVDAGAR-BINN-PER-VI SECTION.                                          
094600                                                                          
094700*PER I                                                                    
094800     IF W271LTPB-START-DATUM = ZERO                                       
094902       MOVE DAGENS-DATUM        TO DAG-TIAAMMDD-FOM                       
095000     ELSE                                                                 
095100       MOVE W271LTPB-START-DATUM                                          
095202                                TO DAG-TIAAMMDD-FOM                       
095300     END-IF                                                               
095402     MOVE WS-PER-II-START-TIAAMMDD TO DAG-TIAAMMDD-TOM                    
095502     MOVE 001                      TO DAG-KDCALL                          
095702     CALL WDAGKONV USING DAG-KDCALL                                       
095802                         DAG-DATUM-AREA                                   
095902                         DAG-KDSVAR                                       
096002     IF DAG-KDSVAR = SPACE                                                
096102        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-I                       
096103        COMPUTE W271LTPB-KVDAGAR-PER-I =                                  
096104                W271LTPB-KVDAGAR-PER-I - 1                                
096200     ELSE                                                                 
096302        MOVE 'FEL FRÅN WDAGKONV 1 I DF SECTION I W271LTPB'                
096400                                   TO FELTEXT-STR                         
096500        DISPLAY FELTEXT                                                   
096600        PERFORM S99-ABEND                                                 
096700     END-IF                                                               
096800                                                                          
096900*PER II                                                                   
097002     MOVE WS-PER-II-START-TIAAMMDD  TO DAG-TIAAMMDD-FOM                   
097102     MOVE WS-PER-III-START-TIAAMMDD TO DAG-TIAAMMDD-TOM                   
097202     MOVE 001                       TO DAG-KDCALL                         
097402     CALL WDAGKONV USING DAG-KDCALL                                       
097502                         DAG-DATUM-AREA                                   
097602                         DAG-KDSVAR                                       
097702     IF DAG-KDSVAR = SPACE                                                
097802        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-II                      
097900        COMPUTE W271LTPB-KVDAGAR-PER-II =                                 
098000                W271LTPB-KVDAGAR-PER-II - 1                               
098100     ELSE                                                                 
098202        MOVE 'FEL FRÅN WDAGKONV 2 I DF SECTION I W271LTPB'                
098300                                    TO FELTEXT-STR                        
098400        DISPLAY FELTEXT                                                   
098500        PERFORM S99-ABEND                                                 
098600     END-IF                                                               
098700                                                                          
098800*PER III                                                                  
098902     MOVE WS-PER-III-START-TIAAMMDD TO DAG-TIAAMMDD-FOM                   
099002     MOVE WS-PER-IV-START-TIAAMMDD  TO DAG-TIAAMMDD-TOM                   
099102     MOVE 001                       TO DAG-KDCALL                         
099302     CALL WDAGKONV USING DAG-KDCALL                                       
099402                         DAG-DATUM-AREA                                   
099502                         DAG-KDSVAR                                       
099602     IF DAG-KDSVAR = SPACE                                                
099702        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-III                     
099800        COMPUTE W271LTPB-KVDAGAR-PER-III =                                
099900                W271LTPB-KVDAGAR-PER-III - 1                              
100000     ELSE                                                                 
100102        MOVE 'FEL FRÅN WDAGKONV 3 I DF SECTION I W271LTPB'                
100200                                    TO FELTEXT-STR                        
100300        DISPLAY FELTEXT                                                   
100400        PERFORM S99-ABEND                                                 
100500     END-IF                                                               
100600                                                                          
100700*PER IV                                                                   
100802     MOVE WS-PER-IV-START-TIAAMMDD  TO DAG-TIAAMMDD-FOM                   
100902     MOVE WS-PER-V-START-TIAAMMDD   TO DAG-TIAAMMDD-TOM                   
101002     MOVE 001                       TO DAG-KDCALL                         
101202     CALL WDAGKONV USING DAG-KDCALL                                       
101302                         DAG-DATUM-AREA                                   
101402                         DAG-KDSVAR                                       
101502     IF DAG-KDSVAR = SPACE                                                
101602        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-IV                      
101700        COMPUTE W271LTPB-KVDAGAR-PER-IV =                                 
101800                W271LTPB-KVDAGAR-PER-IV - 1                               
101900     ELSE                                                                 
102002        MOVE 'FEL FRÅN WDAGKONV 4 I DF SECTION I W271LTPB'                
102100                                    TO FELTEXT-STR                        
102200        DISPLAY FELTEXT                                                   
102300        PERFORM S99-ABEND                                                 
102400     END-IF                                                               
102500                                                                          
102600*PER V                                                                    
102702     MOVE WS-PER-V-START-TIAAMMDD    TO DAG-TIAAMMDD-FOM                  
102802     MOVE WS-PER-VI-START-TIAAMMDD   TO DAG-TIAAMMDD-TOM                  
102902     MOVE 001                        TO DAG-KDCALL                        
103102     CALL WDAGKONV USING DAG-KDCALL                                       
103202                         DAG-DATUM-AREA                                   
103302                         DAG-KDSVAR                                       
103402     IF DAG-KDSVAR = SPACE                                                
103502        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-V                       
103600        COMPUTE W271LTPB-KVDAGAR-PER-V =                                  
103700                W271LTPB-KVDAGAR-PER-V - 1                                
103800     ELSE                                                                 
103902        MOVE 'FEL FRÅN WDAGKONV 5 I DF SECTION I W271LTPB'                
104000                                    TO FELTEXT-STR                        
104100        DISPLAY FELTEXT                                                   
104200        PERFORM S99-ABEND                                                 
104300     END-IF                                                               
104400                                                                          
104500*PER IV                                                                   
104602     MOVE WS-PER-VI-START-TIAAMMDD     TO DAG-TIAAMMDD-FOM                
104700     MOVE W271LTPB-BINNDAY-TIAAMMDD    TO TMP1-YYMMDD                     
104800     MOVE WS-PER-VII-START-TIAAMMDD    TO TMP2-YYMMDD                     
104900     PERFORM WY2000P1                                                     
105000     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
105102        MOVE WS-PER-VII-START-TIAAMMDD TO DAG-TIAAMMDD-TOM                
105200     ELSE                                                                 
105302        MOVE W271LTPB-BINNDAY-TIAAMMDD    TO DAG-TIAAMMDD-TOM             
105400     END-IF                                                               
105502     MOVE 001                          TO  DAG-KDCALL                     
105702     CALL WDAGKONV USING DAG-KDCALL                                       
105802                         DAG-DATUM-AREA                                   
105902                         DAG-KDSVAR                                       
106002     IF DAG-KDSVAR = SPACE                                                
106102        MOVE DAG-KVKALDAG TO W271LTPB-KVDAGAR-PER-VI                      
106200     ELSE                                                                 
106302        MOVE 'FEL FRÅN WDAGKONV 7 I DF SECTION I W271LTPB'                
106400                                    TO FELTEXT-STR                        
106500        DISPLAY FELTEXT                                                   
106600        PERFORM S99-ABEND                                                 
106700     END-IF                                                               
106800                                                                          
106900     MOVE WS-PER-VI-TIRP   TO W271LTPB-BINNDAY-TIRP                       
107000     .                                                                    
107100     EJECT                                                                
107200                                                                          
107300 S99-ABEND SECTION.                                                       
107400                                                                          
107500     SKIP2                                                                
107600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
107700     .                                                                    
107800     EJECT                                                                
107900                                                                          
108000                                                                          
108100     EJECT                                                                
109000*    -COPY WY2000P1                                                       
