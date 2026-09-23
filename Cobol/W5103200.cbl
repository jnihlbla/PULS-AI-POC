000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5103200.                                                
000400 AUTHOR.         KARL JOHAN HANSSON.                                      
000500 DATE-WRITTEN.   96/11/18.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*  FUNKTION:  - FORMERAR KREDITERINGENS POSTER PÅ FIL FÖR LAB.            
000900*             - ALLA POSTER HAR 720-POSTENS UTSEENDE OCH COPYTEXT,        
001000*               DOCK ÄR ENDAST FÄLT AVSEDDA FÖR AKTUELL POSTTYP           
001100*               IFYLLDA, ÖVRIGA ÄR NOLLADE.                               
001200*             - POSTERNA SORTERAS PER KREDITNOTA OCH IDDC,                
001300*               SUMMERA SAMMAN KREDITNOTARADER (722) TILL KREDIT-         
001400*               HUVUDPOSTER(721), BESTÄMMER TOTAL FÖR KREDITNOTA,         
001500*               TOTAL MED TILLÄGG SAMT DESS UTLÄNDSKA BELOPP.             
001600*               DESSA POSTER HAR TYP 720 IN, ÖVRIGA POSTER                
001700*               HAR TYP 72C OCH 72D IN.                                   
001800*               POSTER UT HAR TYP A04, A05, L05 OCH A12                   
001900*                                                                         
002000*        ÄNDRAD FÖR ETRACKER NO 1475589, INSTALLERAD 2004-10-15           
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700                                                                          
002800*          --- TRANSAR FRÅN KREDITERINGEN TYP 720, 728 O 729              
002900     SELECT W41833                     ASSIGN TO W51032D1.                
003000                                                                          
003100*          --- KREDITTRANSAR TILL LAB  TYP 721,722,728,729                
003200     SELECT W51033                     ASSIGN TO W51032D2.                
003300                                                                          
003400     SELECT SORTFIL                    ASSIGN TO W51032DS.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W41833                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  -COPY W41833       -L.                                               
004500     SKIP3                                                                
004600 FD  W51033                                                               
004700     RECORDING       V                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  POST -COPY W510A04 -PRE  UA04-  -L.                                  
005100                                                                          
005200*01  POST -COPY W510A05 -PRE  UA05-  -L.                                  
005300                                                                          
005400*01  POST -COPY W510A05 -PRE  UL05-  -L.                                  
005500                                                                          
005600*01  POST -COPY W510A12 -PRE  UA12-  -L.                                  
005700     SKIP3                                                                
005800 SD  SORTFIL.                                                             
005900                                                                          
006000 01  SRT-POST.                                                            
006100*    03  -COPY W41833   -PRE SRT-                                         
006200     EJECT                                                                
006300 WORKING-STORAGE SECTION.                                                 
006400                                                                          
006500*    -- CHECKED BY WY2000                                                 
006600 77  IDPGM                       PIC X(8)      VALUE 'W5103200'.          
006700 77  JA                          PIC X         VALUE 'J'.                 
006800 77  NEJ                         PIC X         VALUE 'N'.                 
006900                                                                          
007000 77  SORTFIL-EOF-SW              PIC X         VALUE 'N'.                 
007100                                                                          
007200 77  MINST-EN-K-NOTA-SW          PIC X         VALUE 'N'.                 
007300     88  MINST-EN-K-NOTA                       VALUE 'J'.                 
007400                                                                          
007500 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
007600                                                                          
007700 01  DYNAMISKA-SUBPROGRAM.                                                
007800     03  ABEND                   PIC X(8)      VALUE 'ABEND'.             
007900     03  POSTSUM                 PIC X(8)      VALUE 'POSTSUM'.           
008000     SKIP2                                                                
008100*    --- PARAMETRAR TILL ABEND                                            
008200*01  -COPY WWDC99                                                         
008300*01  -COPY WWDCKONS                                                       
008400*    --- PARAMETRAR TILL ABEND                                            
008500                                                                          
008600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008700     SKIP2                                                                
008800 01  SPAR-AREA.                                                           
008900     03  W-HUV-PRLANDCO          PIC S9(7)V9(2)  VALUE +0 COMP-3.         
009000     03  W-HUV-PRMOMS            PIC S9(7)V9(2)  VALUE +0 COMP-3.         
009100     03  W-HUV-SUKRENTO          PIC S9(11)V9(2) VALUE +0 COMP-3.         
009200 01  FELTEXT.                                                             
009300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009500     EJECT                                                                
009600 01  TEST-IDDISTR        PIC 9(5)        COMP-3.                          
009700                                                                          
009800*01  FILLER  -COPY WWDIST07  -RED TEST-IDDISTR.                           
009900                                                                          
010000*01  FILLER  -COPY WWDIST18  -RED TEST-IDDISTR.                           
010100                                                                          
010200*01  FILLER  -COPY WWDIST35  -RED TEST-IDDISTR.                           
010300     EJECT                                                                
010400 01  FILLER                      PIC X(24)   VALUE 'POSTSUM  '.           
010500*                                                                         
010600*01  -COPY W0005   -PRE  POSTSUM-                                         
010700     EJECT                                                                
010800 01  FILLER                      PIC X(24)   VALUE 'SORT-AREA'.           
010900                                                                          
011000 01  SORT-AREA.                                                           
011100*    03  AREA -COPY W41833   -PRE 720-                                    
011200     EJECT                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'HUV-AREA'.            
011400                                                                          
011500*01  AREA -COPY W41833   -PRE HUV-                                        
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'A04-AREA'.            
011800                                                                          
011900*01  AREA -COPY W510A04  -PRE A04-                                        
012000     EJECT                                                                
012100 01  FILLER                      PIC X(16)   VALUE 'A05-AREA'.            
012200                                                                          
012300*01  AREA -COPY W510A05  -PRE A05-                                        
012400     EJECT                                                                
012500 01  FILLER                      PIC X(16)   VALUE 'L05-AREA'.            
012600                                                                          
012700*01  AREA -COPY W510A05  -PRE L05-                                        
012800     EJECT                                                                
012900 01  FILLER                      PIC X(16)   VALUE 'A12-AREA'.            
013000                                                                          
013100*01  AREA -COPY W510A12  -PRE A12-                                        
013200     EJECT                                                                
013300 PROCEDURE DIVISION.                                                      
013400                                                                          
013500                                                                          
013600     PERFORM A-INIT                                                       
013700                                                                          
013800     SORT SORTFIL ASCENDING  KEY SRT-IDPTYP                               
013900                                 SRT-IDDC                                 
014000                                 SRT-IDKNOTNR                             
014100                                 SRT-IDARTNR                              
014200                                                                          
014300             USING W41833                                                 
014400             OUTPUT PROCEDURE B-SKAPA-LAB-TRANSAR                         
014500                                                                          
014600     IF SORT-RETURN NOT = 0                                               
014700       MOVE SORT-RETURN TO SORT-RETURN-X                                  
014800       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
014900           DELIMITED BY SIZE                                              
015000           INTO FELTEXT-STR                                               
015100       DISPLAY FELTEXT                                                    
015200       PERFORM S99-ABEND                                                  
015300     END-IF                                                               
015400                                                                          
015500     PERFORM Z-FINIT                                                      
015600                                                                          
015700     MOVE ZERO TO RETURN-CODE                                             
015800     GOBACK                                                               
015900     .                                                                    
016000     SKIP3                                                                
016100 A-INIT SECTION.                                                          
016200                                                                          
016300     OPEN OUTPUT W51033                                                   
016400                                                                          
016500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016600                                                                          
016700     INITIALIZE HUV-W41833                                                
016800     MOVE ZERO                TO W-HUV-PRLANDCO                           
016900                                 W-HUV-PRMOMS                             
017000                                 W-HUV-SUKRENTO                           
017100     .                                                                    
017200     EJECT                                                                
017300 B-SKAPA-LAB-TRANSAR SECTION.                                             
017400                                                                          
017500     PERFORM S01-LAES-SORTFIL                                             
017600     PERFORM UNTIL SORTFIL-EOF-SW = JA                                    
017700                                                                          
017800       IF 720-IDPTYP = '720'                                              
017900         SET MINST-EN-K-NOTA TO TRUE                                      
018000                                                                          
018100         IF HUV-IDKNOTNR NOT = 720-IDKNOTNR AND                           
018200            HUV-IDKNOTNR NOT = 0                                          
018300*** SKRIV UT FÖREGÅENDE KREDITNOTA-HUVUDPOST                              
018400           PERFORM BA-SKAPA-KNOTA-HUV-POST                                
018500         END-IF                                                           
018600         PERFORM BC-ADDERA-SKRIV-KNOTA-RADER                              
018700       END-IF                                                             
018800                                                                          
018900       IF 720-IDPTYP = '72C' OR '72D'                                     
019000         MOVE 720-IDDISTR       TO TEST-IDDISTR                           
019100         IF DIST07-USA-RETAILER OR DIST07-CAN-RETAILER                    
019200           PERFORM BD-SKAPA-A12-USA-CAN-RETAILER                          
019300         END-IF                                                           
019400       END-IF                                                             
019500                                                                          
019600       PERFORM S01-LAES-SORTFIL                                           
019700     END-PERFORM                                                          
019800     .                                                                    
019900     EJECT                                                                
020000 BA-SKAPA-KNOTA-HUV-POST SECTION.                                         
020100                                                                          
020200     COMPUTE HUV-SUKRENOT = W-HUV-SUKRENTO +                              
020300             W-HUV-PRLANDCO + HUV-PRFRAKT  + HUV-PRLEGKST +               
020400             HUV-PRFOERS  + W-HUV-PRMOMS                                  
020500                                                                          
020600     COMPUTE HUV-SUKREUTL ROUNDED = HUV-SUKRENOT / HUV-PRKURS             
020700       ON SIZE ERROR MOVE ZERO TO HUV-SUKREUTL                            
020800     END-COMPUTE                                                          
020900                                                                          
021000     MOVE HUV-IDDISTR          TO TEST-IDDISTR                            
021100                                                                          
021200     EVALUATE TRUE                                                        
021300       WHEN DIST35-REFILL-NA                                              
021400         PERFORM BAA-SKAPA-A04-REFILL                                     
021500       WHEN DIST35-NA-CDC-RETURN                                          
021600         PERFORM BAB-SKAPA-A04-NA-RETURN                                  
021700       WHEN DIST35-REFILL-NA-JAP                                          
021800         PERFORM BAC-SKAPA-A04-REFILL-JAP                                 
021900       WHEN DIST07-USA-RET-DISCR OR DIST07-CAN-RET-DISCR                  
022000         PERFORM BAD-SKAPA-A04-USA-CAN-RETAILER                           
022100       WHEN DIST18-SCRAP-NDC-QUAL                                         
022200         PERFORM BAE-SKAPA-A04-SCRAP-NDC-QUAL                             
022300     END-EVALUATE                                                         
022400                                                                          
022500     MOVE ZERO                TO W-HUV-PRLANDCO                           
022600                                 W-HUV-PRMOMS                             
022700                                 W-HUV-SUKRENTO                           
022800     .                                                                    
022900     EJECT                                                                
023000 BAA-SKAPA-A04-REFILL SECTION.                                            
023100                                                                          
023200     MOVE 'A04'               TO A04-IDPTYP                               
023300     MOVE 'T10'               TO A04-KDEKOHT                              
023400     MOVE 53                  TO A04-IDFTG                                
023500     MOVE WC-CDC-SE           TO A04-IDDC-SEND                            
023600     EVALUATE TRUE                                                        
023700       WHEN DIST35-CDC-NDC41-REFILL                                       
023800         MOVE WC-NDC-US-RU    TO A04-IDDC-REC                             
024100       WHEN DIST35-CDC-NDC43-REFILL                                       
024200         MOVE WC-NDC-US-LA    TO A04-IDDC-REC                             
024300       WHEN DIST35-CDC-NDC44-REFILL                                       
024310         MOVE WC-NDC-US-SE    TO A04-IDDC-REC                             
024320       WHEN DIST35-CDC-NDC45-REFILL                                       
024330         MOVE WC-NDC-US-CH    TO A04-IDDC-REC                             
024340       WHEN DIST35-CDC-NDC46-REFILL                                       
024350         MOVE WC-NDC-US-JA    TO A04-IDDC-REC                             
024360       WHEN DIST35-CDC-NDC47-REFILL                                       
024370         MOVE WC-NDC-US-DA    TO A04-IDDC-REC                             
024400       WHEN DIST35-CDC-NDC51-REFILL                                       
024500         MOVE 54              TO A04-IDFTG                                
024600         MOVE WC-NDC-CA       TO A04-IDDC-REC                             
024700     END-EVALUATE                                                         
024800     MOVE HUV-IDDISTR         TO A04-IDDISTR                              
024900     MOVE HUV-IDKUNDNR        TO A04-IDKUNDNR                             
025000     MOVE HUV-IDKNOTNR        TO A04-IDKNOTNR                             
025100     MOVE HUV-DAKRENOT        TO A04-DAKRENOT                             
025200     MOVE HUV-IDRAPPNR        TO A04-IDRAPPNR                             
025300     MOVE W-HUV-SUKRENTO      TO A04-SUKRENTO                             
025400     MOVE W-HUV-PRLANDCO      TO A04-PRLANDCO                             
025500     MOVE ZERO                TO A04-PREMBHNT                             
025600     MOVE HUV-PRFOERS         TO A04-PRFOERS                              
025700     MOVE HUV-PRFRAKT         TO A04-PRFRAKT                              
025800     MOVE W-HUV-PRMOMS        TO A04-PRMOMS                               
025900     MOVE HUV-PRLEGKST        TO A04-PRLEGKST                             
026000     MOVE HUV-SUKRENOT        TO A04-SUKRENOT                             
026100     MOVE HUV-SUKREUTL        TO A04-SUKREUTL                             
026200     MOVE HUV-PRKURS          TO A04-PRKURS                               
026300                                                                          
026400     PERFORM S04-SKRIV-W51033-A04                                         
026500     .                                                                    
026600     EJECT                                                                
026700 BAB-SKAPA-A04-NA-RETURN SECTION.                                         
026800                                                                          
026900     MOVE 'A04'               TO A04-IDPTYP                               
027000     MOVE 'T20'               TO A04-KDEKOHT                              
027100     MOVE HUV-IDKUNDNR        TO A04-IDKUNDNR                             
027200     IF HUV-IDKUNDNR = +51                                                
027300       MOVE '54'              TO A04-IDFTG                                
027400     ELSE                                                                 
027500       MOVE '53'              TO A04-IDFTG                                
027600     END-IF                                                               
027700     MOVE A04-IDKUNDNR(5:2)   TO A04-IDDC-SEND                            
027800     MOVE WC-CDC-SE           TO A04-IDDC-REC                             
027900     MOVE HUV-IDDISTR         TO A04-IDDISTR                              
028000     MOVE HUV-IDKNOTNR        TO A04-IDKNOTNR                             
028100     MOVE HUV-DAKRENOT        TO A04-DAKRENOT                             
028200     MOVE HUV-IDRAPPNR        TO A04-IDRAPPNR                             
028300     MOVE W-HUV-SUKRENTO      TO A04-SUKRENTO                             
028400     MOVE W-HUV-PRLANDCO      TO A04-PRLANDCO                             
028500     MOVE ZERO                TO A04-PREMBHNT                             
028600     MOVE HUV-PRFOERS         TO A04-PRFOERS                              
028700     MOVE HUV-PRFRAKT         TO A04-PRFRAKT                              
028800     MOVE W-HUV-PRMOMS        TO A04-PRMOMS                               
028900     MOVE HUV-PRLEGKST        TO A04-PRLEGKST                             
029000     MOVE HUV-SUKRENOT        TO A04-SUKRENOT                             
029100     MOVE HUV-SUKREUTL        TO A04-SUKREUTL                             
029200     MOVE HUV-PRKURS          TO A04-PRKURS                               
029300                                                                          
029400     PERFORM S04-SKRIV-W51033-A04                                         
029500     .                                                                    
029600     EJECT                                                                
029700 BAC-SKAPA-A04-REFILL-JAP SECTION.                                        
029800                                                                          
029900     MOVE 'A04'               TO A04-IDPTYP                               
030000     MOVE 'I20'               TO A04-KDEKOHT                              
030100     MOVE 53                  TO A04-IDFTG                                
030200     MOVE WC-CDC-SE           TO A04-IDDC-SEND                            
030300     EVALUATE TRUE                                                        
030400       WHEN DIST35-JAP-NDC41-REFILL                                       
030500         MOVE WC-NDC-US-RU    TO A04-IDDC-REC                             
030800       WHEN DIST35-JAP-NDC43-REFILL                                       
030900         MOVE WC-NDC-US-LA    TO A04-IDDC-REC                             
030910       WHEN DIST35-JAP-NDC44-REFILL                                       
030920         MOVE WC-NDC-US-SE    TO A04-IDDC-REC                             
031000       WHEN DIST35-JAP-NDC51-REFILL                                       
031100         MOVE 54              TO A04-IDFTG                                
031200         MOVE WC-NDC-CA       TO A04-IDDC-REC                             
031300     END-EVALUATE                                                         
031400     MOVE HUV-IDDISTR         TO A04-IDDISTR                              
031500     MOVE HUV-IDKUNDNR        TO A04-IDKUNDNR                             
031600     MOVE HUV-IDKNOTNR        TO A04-IDKNOTNR                             
031700     MOVE HUV-DAKRENOT        TO A04-DAKRENOT                             
031800     MOVE HUV-IDRAPPNR        TO A04-IDRAPPNR                             
031900     MOVE W-HUV-SUKRENTO      TO A04-SUKRENTO                             
032000     MOVE W-HUV-PRLANDCO      TO A04-PRLANDCO                             
032100     MOVE ZERO                TO A04-PREMBHNT                             
032200     MOVE HUV-PRFOERS         TO A04-PRFOERS                              
032300     MOVE HUV-PRFRAKT         TO A04-PRFRAKT                              
032400     MOVE W-HUV-PRMOMS        TO A04-PRMOMS                               
032500     MOVE HUV-PRLEGKST        TO A04-PRLEGKST                             
032600     MOVE HUV-SUKRENOT        TO A04-SUKRENOT                             
032700     MOVE HUV-SUKREUTL        TO A04-SUKREUTL                             
032800     MOVE HUV-PRKURS          TO A04-PRKURS                               
032900                                                                          
033000     PERFORM S04-SKRIV-W51033-A04                                         
033100     .                                                                    
033200     EJECT                                                                
033300 BAD-SKAPA-A04-USA-CAN-RETAILER SECTION.                                  
033400                                                                          
033500     MOVE 'A04'               TO A04-IDPTYP                               
033600     MOVE 'O20'               TO A04-KDEKOHT                              
033700     IF DIST07-USA-RET-DISCR                                              
033800       MOVE 53                TO A04-IDFTG                                
033900       MOVE WC-NDC-US-RU      TO A04-IDDC-REC                             
034000     ELSE                                                                 
034100       MOVE 54                TO A04-IDFTG                                
034200       MOVE WC-NDC-CA         TO A04-IDDC-REC                             
034300     END-IF                                                               
034400     MOVE WC-CDC-SE           TO A04-IDDC-SEND                            
034500     MOVE HUV-IDDISTR         TO A04-IDDISTR                              
034600     MOVE HUV-IDKUNDNR        TO A04-IDKUNDNR                             
034700     MOVE HUV-IDKNOTNR        TO A04-IDKNOTNR                             
034800     MOVE HUV-DAKRENOT        TO A04-DAKRENOT                             
034900     MOVE HUV-IDRAPPNR        TO A04-IDRAPPNR                             
035000     MOVE W-HUV-SUKRENTO      TO A04-SUKRENTO                             
035100     MOVE W-HUV-PRLANDCO      TO A04-PRLANDCO                             
035200     MOVE ZERO                TO A04-PREMBHNT                             
035300     MOVE HUV-PRFOERS         TO A04-PRFOERS                              
035400     MOVE HUV-PRFRAKT         TO A04-PRFRAKT                              
035500     MOVE W-HUV-PRMOMS        TO A04-PRMOMS                               
035600     MOVE HUV-PRLEGKST        TO A04-PRLEGKST                             
035700     MOVE HUV-SUKRENOT        TO A04-SUKRENOT                             
035800     MOVE HUV-SUKREUTL        TO A04-SUKREUTL                             
035900     MOVE HUV-PRKURS          TO A04-PRKURS                               
036000                                                                          
036100     PERFORM S04-SKRIV-W51033-A04                                         
036200     .                                                                    
036300     EJECT                                                                
036400 BAE-SKAPA-A04-SCRAP-NDC-QUAL SECTION.                                    
036500                                                                          
036600     MOVE 'A04'               TO A04-IDPTYP                               
036700     MOVE 'O71'               TO A04-KDEKOHT                              
036800     MOVE HUV-IDKUNDNR        TO A04-IDKUNDNR                             
036900     IF HUV-IDKUNDNR = +51                                                
037000       MOVE 54                TO A04-IDFTG                                
037100     ELSE                                                                 
037200       MOVE 53                TO A04-IDFTG                                
037300     END-IF                                                               
037400     MOVE A04-IDKUNDNR(5:2)   TO A04-IDDC-SEND                            
037500     MOVE WC-CDC-SE           TO A04-IDDC-REC                             
037600     MOVE HUV-IDDISTR         TO A04-IDDISTR                              
037700     MOVE HUV-IDKNOTNR        TO A04-IDKNOTNR                             
037800     MOVE HUV-DAKRENOT        TO A04-DAKRENOT                             
037900     MOVE HUV-IDRAPPNR        TO A04-IDRAPPNR                             
038000     MOVE W-HUV-SUKRENTO      TO A04-SUKRENTO                             
038100     MOVE W-HUV-PRLANDCO      TO A04-PRLANDCO                             
038200     MOVE ZERO                TO A04-PREMBHNT                             
038300     MOVE HUV-PRFOERS         TO A04-PRFOERS                              
038400     MOVE HUV-PRFRAKT         TO A04-PRFRAKT                              
038500     MOVE W-HUV-PRMOMS        TO A04-PRMOMS                               
038600     MOVE HUV-PRLEGKST        TO A04-PRLEGKST                             
038700     MOVE HUV-SUKRENOT        TO A04-SUKRENOT                             
038800     MOVE HUV-SUKREUTL        TO A04-SUKREUTL                             
038900     MOVE HUV-PRKURS          TO A04-PRKURS                               
039000                                                                          
039100     PERFORM S04-SKRIV-W51033-A04                                         
039200     .                                                                    
039300     EJECT                                                                
039400 BC-ADDERA-SKRIV-KNOTA-RADER SECTION.                                     
039500                                                                          
039600     MOVE 720-W41833          TO HUV-W41833                               
039700                                                                          
039800*** ADDERA SAMMAN RADVÄRDEN FÖR KREDITNOTA-HUVUDET                        
039900     COMPUTE W-HUV-SUKRENTO = W-HUV-SUKRENTO +                            
040000             720-KVKREANT * 720-PRARTNTO                                  
040100                                                                          
040200     COMPUTE W-HUV-PRLANDCO = W-HUV-PRLANDCO + 720-PRLANDCO               
040300                                                                          
040400     COMPUTE W-HUV-PRMOMS ROUNDED = W-HUV-PRMOMS +                        
040500                     720-REVAT * 720-PRARTNTO * 720-KVKREANT              
040600                                                                          
040700     MOVE 720-IDDISTR         TO TEST-IDDISTR                             
040800*** SKAPA KREDITNOTA-RADPOSTER                                            
040900     EVALUATE TRUE                                                        
041000       WHEN DIST35-REFILL-NA                                              
041100         PERFORM BCH-SKAPA-A05-REFILL                                     
041200       WHEN DIST35-NA-CDC-RETURN                                          
041300         PERFORM BCI-SKAPA-A05-NA-RETURN                                  
041400       WHEN DIST35-REFILL-NA-JAP                                          
041500         PERFORM BCJ-SKAPA-A05-REFILL-JAP                                 
041600       WHEN DIST07-USA-RET-DISCR OR DIST07-CAN-RET-DISCR                  
041700         PERFORM BCK-SKAPA-A05-USA-CAN-RETAILER                           
041800       WHEN DIST18-SCRAP-NDC-QUAL                                         
041900         PERFORM BCL-SKAPA-A05-SCRAP-NDC-QUAL                             
042000       WHEN DIST07-USA-RETAILER OR DIST07-CAN-RETAILER OR                 
042100            DIST35-US-CAN-TRANSFER OR                                     
042200            DIST35-CAN-US-TRANSFER                                        
042300         IF 720-FLLSBOK = JA                                              
042400            PERFORM BCM-SKAPA-L05-LVRAPPOST                               
042500         END-IF                                                           
042600     END-EVALUATE                                                         
042700     .                                                                    
042800     EJECT                                                                
042900 BCH-SKAPA-A05-REFILL SECTION.                                            
043000                                                                          
043100     MOVE 'A05'               TO A05-IDPTYP                               
043200     MOVE 'T10'               TO A05-KDEKOHT                              
043300     MOVE 53                  TO A05-IDFTG                                
043400     MOVE WC-CDC-SE           TO A05-IDDC-SEND                            
043500     EVALUATE TRUE                                                        
043600       WHEN DIST35-CDC-NDC41-REFILL                                       
043700         MOVE WC-NDC-US-RU    TO A05-IDDC-REC                             
044000       WHEN DIST35-CDC-NDC43-REFILL                                       
044100         MOVE WC-NDC-US-LA    TO A05-IDDC-REC                             
044110       WHEN DIST35-CDC-NDC44-REFILL                                       
044120         MOVE WC-NDC-US-SE    TO A05-IDDC-REC                             
044130       WHEN DIST35-CDC-NDC45-REFILL                                       
044140         MOVE WC-NDC-US-CH    TO A05-IDDC-REC                             
044150       WHEN DIST35-CDC-NDC46-REFILL                                       
044160         MOVE WC-NDC-US-JA    TO A05-IDDC-REC                             
044170       WHEN DIST35-CDC-NDC47-REFILL                                       
044180         MOVE WC-NDC-US-DA    TO A05-IDDC-REC                             
044200       WHEN DIST35-CDC-NDC51-REFILL                                       
044300         MOVE 54              TO A05-IDFTG                                
044400         MOVE WC-NDC-CA       TO A05-IDDC-REC                             
044500     END-EVALUATE                                                         
044600     MOVE 720-IDDISTR         TO A05-IDDISTR                              
044700     MOVE 720-IDKUNDNR        TO A05-IDKUNDNR                             
044800     MOVE 720-IDKNOTNR        TO A05-IDKNOTNR                             
044900     MOVE 720-DAKRENOT        TO A05-DAKRENOT                             
045000     MOVE 720-IDARTNR         TO A05-IDARTNR                              
045100     MOVE 720-KDPRODSL        TO A05-KDPRODSL                             
045200     MOVE 720-KDPSLLOC        TO A05-KDPSLLOC                             
045300     MOVE 720-KDANMORS        TO A05-KDANMORS                             
045400     MOVE 720-KVKREANT        TO A05-KVKREANT                             
045500     MOVE 720-PRARTNTO        TO A05-PRARTNTO                             
045600                                                                          
045700     PERFORM S05-SKRIV-W51033-A05                                         
045800     .                                                                    
045900     EJECT                                                                
046000 BCI-SKAPA-A05-NA-RETURN SECTION.                                         
046100                                                                          
046200     MOVE 'A05'               TO A05-IDPTYP                               
046300     MOVE 'T20'               TO A05-KDEKOHT                              
046400     MOVE 720-IDKUNDNR        TO A05-IDKUNDNR                             
046500     IF 720-IDKUNDNR = +51                                                
046600       MOVE '54'              TO A05-IDFTG                                
046700     ELSE                                                                 
046800       MOVE '53'              TO A05-IDFTG                                
046900     END-IF                                                               
047000     MOVE A05-IDKUNDNR(5:2)   TO A05-IDDC-SEND                            
047100     MOVE WC-CDC-SE           TO A05-IDDC-REC                             
047200     MOVE 720-IDDISTR         TO A05-IDDISTR                              
047300     MOVE 720-IDKNOTNR        TO A05-IDKNOTNR                             
047400     MOVE 720-DAKRENOT        TO A05-DAKRENOT                             
047500     MOVE 720-IDARTNR         TO A05-IDARTNR                              
047600     MOVE 720-KDPRODSL        TO A05-KDPRODSL                             
047700     MOVE 720-KDPSLLOC        TO A05-KDPSLLOC                             
047800     MOVE 720-KDANMORS        TO A05-KDANMORS                             
047900     MOVE 720-KVKREANT        TO A05-KVKREANT                             
048000     MOVE 720-PRARTNTO        TO A05-PRARTNTO                             
048100                                                                          
048200     PERFORM S05-SKRIV-W51033-A05                                         
048300     .                                                                    
048400     EJECT                                                                
048500 BCJ-SKAPA-A05-REFILL-JAP SECTION.                                        
048600                                                                          
048700     MOVE 'A05'               TO A05-IDPTYP                               
048800     MOVE 'I20'               TO A05-KDEKOHT                              
048900     MOVE 53                  TO A05-IDFTG                                
049000     MOVE WC-CDC-SE           TO A05-IDDC-SEND                            
049100     EVALUATE TRUE                                                        
049200       WHEN DIST35-JAP-NDC41-REFILL                                       
049300         MOVE WC-NDC-US-RU    TO A05-IDDC-REC                             
049600       WHEN DIST35-JAP-NDC43-REFILL                                       
049700         MOVE WC-NDC-US-LA    TO A05-IDDC-REC                             
049710       WHEN DIST35-JAP-NDC44-REFILL                                       
049720         MOVE WC-NDC-US-SE    TO A05-IDDC-REC                             
049800       WHEN DIST35-JAP-NDC51-REFILL                                       
049900         MOVE 54              TO A05-IDFTG                                
050000         MOVE WC-NDC-CA       TO A05-IDDC-REC                             
050100     END-EVALUATE                                                         
050200     MOVE 720-IDDISTR         TO A05-IDDISTR                              
050300     MOVE 720-IDKUNDNR        TO A05-IDKUNDNR                             
050400     MOVE 720-IDKNOTNR        TO A05-IDKNOTNR                             
050500     MOVE 720-DAKRENOT        TO A05-DAKRENOT                             
050600     MOVE 720-IDARTNR         TO A05-IDARTNR                              
050700     MOVE 720-KDPRODSL        TO A05-KDPRODSL                             
050800     MOVE 720-KDPSLLOC        TO A05-KDPSLLOC                             
050900     MOVE 720-KDANMORS        TO A05-KDANMORS                             
051000     MOVE 720-KVKREANT        TO A05-KVKREANT                             
051100     MOVE 720-PRARTNTO        TO A05-PRARTNTO                             
051200                                                                          
051300     PERFORM S05-SKRIV-W51033-A05                                         
051400     .                                                                    
051500     EJECT                                                                
051600 BCK-SKAPA-A05-USA-CAN-RETAILER SECTION.                                  
051700                                                                          
051800     MOVE 'A05'               TO A05-IDPTYP                               
051900     MOVE 'O20'               TO A05-KDEKOHT                              
052000     IF DIST07-USA-RET-DISCR                                              
052100       MOVE 53                TO A05-IDFTG                                
052200       MOVE WC-NDC-US-RU      TO A05-IDDC-REC                             
052300     ELSE                                                                 
052400       MOVE 54                TO A05-IDFTG                                
052500       MOVE WC-NDC-CA         TO A05-IDDC-REC                             
052600     END-IF                                                               
052700     MOVE WC-CDC-SE           TO A05-IDDC-SEND                            
052800     MOVE 720-IDDISTR         TO A05-IDDISTR                              
052900     MOVE 720-IDKUNDNR        TO A05-IDKUNDNR                             
053000     MOVE 720-IDKNOTNR        TO A05-IDKNOTNR                             
053100     MOVE 720-DAKRENOT        TO A05-DAKRENOT                             
053200     MOVE 720-IDARTNR         TO A05-IDARTNR                              
053300     MOVE 720-KDPRODSL        TO A05-KDPRODSL                             
053400     MOVE 720-KDPSLLOC        TO A05-KDPSLLOC                             
053500     MOVE 720-KDANMORS        TO A05-KDANMORS                             
053600     MOVE 720-KVKREANT        TO A05-KVKREANT                             
053700     MOVE 720-PRARTNTO        TO A05-PRARTNTO                             
053800                                                                          
053900     PERFORM S05-SKRIV-W51033-A05                                         
054000     .                                                                    
054100     EJECT                                                                
054200 BCL-SKAPA-A05-SCRAP-NDC-QUAL SECTION.                                    
054300                                                                          
054400     MOVE 'A05'               TO A05-IDPTYP                               
054500     MOVE 'O71'               TO A05-KDEKOHT                              
054600     MOVE 720-IDKUNDNR        TO A05-IDKUNDNR                             
054700     IF 720-IDKUNDNR = +51                                                
054800       MOVE 54                TO A05-IDFTG                                
054900     ELSE                                                                 
055000       MOVE 53                TO A05-IDFTG                                
055100     END-IF                                                               
055200     MOVE A05-IDKUNDNR(5:2)   TO A05-IDDC-SEND                            
055300     MOVE WC-CDC-SE           TO A05-IDDC-REC                             
055400     MOVE 720-IDDISTR         TO A05-IDDISTR                              
055500     MOVE 720-IDKNOTNR        TO A05-IDKNOTNR                             
055600     MOVE 720-DAKRENOT        TO A05-DAKRENOT                             
055700     MOVE 720-IDARTNR         TO A05-IDARTNR                              
055800     MOVE 720-KDPRODSL        TO A05-KDPRODSL                             
055900     MOVE 720-KDPSLLOC        TO A05-KDPSLLOC                             
056000     MOVE 720-KDANMORS        TO A05-KDANMORS                             
056100     MOVE 720-KVKREANT        TO A05-KVKREANT                             
056200     MOVE 720-PRARTNTO        TO A05-PRARTNTO                             
056300                                                                          
056400     PERFORM S05-SKRIV-W51033-A05                                         
056500     .                                                                    
056600     EJECT                                                                
056700 BCM-SKAPA-L05-LVRAPPOST SECTION.                                         
056800                                                                          
056900     MOVE 'L05'               TO L05-IDPTYP                               
057000     MOVE 'O10'               TO L05-KDEKOHT                              
057100     MOVE 720-IDDC            TO L05-IDDC-SEND                            
057200                                 L05-IDDC-REC                             
057300     IF DIST35-CDC-NDC51-REFILL OR                                        
057400        DIST35-CAN-US-TRANSFER                                            
057500       MOVE 54                TO L05-IDFTG                                
057600     ELSE                                                                 
057700       MOVE 53                TO L05-IDFTG                                
057800     END-IF                                                               
057900     MOVE 720-IDDISTR         TO L05-IDDISTR                              
058000     MOVE 720-IDKUNDNR        TO L05-IDKUNDNR                             
058100     MOVE 720-IDKNOTNR        TO L05-IDKNOTNR                             
058200     MOVE 720-DAKRENOT        TO L05-DAKRENOT                             
058300     MOVE 720-IDARTNR         TO L05-IDARTNR                              
058400     MOVE 720-KDPRODSL        TO L05-KDPRODSL                             
058500     MOVE 720-KDPSLLOC        TO L05-KDPSLLOC                             
058600     MOVE 720-KDANMORS        TO L05-KDANMORS                             
058700     MOVE 720-KVKREANT        TO L05-KVKREANT                             
058800     MOVE 720-PRARTNTO        TO L05-PRARTNTO                             
058900                                                                          
059000     PERFORM S07-SKRIV-W51033-L05                                         
059100     .                                                                    
059200     EJECT                                                                
059300 BD-SKAPA-A12-USA-CAN-RETAILER SECTION.                                   
059400                                                                          
059500     MOVE 'A12'               TO A12-IDPTYP                               
059600     MOVE 'O10'               TO A12-KDEKOHT                              
059700     MOVE 720-IDDC            TO A12-IDDC-SEND                            
059800                                 A12-IDDC-REC                             
059900                                 WS-IDDC                                  
060000     IF NDC-CA                                                            
060100       MOVE 54                TO A12-IDFTG                                
060200     ELSE                                                                 
060300       MOVE 53                TO A12-IDFTG                                
060400     END-IF                                                               
060500     MOVE 720-IDRAPPNR        TO A12-IDRAPPNR                             
060600     MOVE 720-DAKRENOT        TO A12-DAJUSTDA                             
060700     MOVE 720-IDARTNR         TO A12-IDARTNR                              
060800     MOVE 720-IDDISTR         TO A12-IDDISTR                              
060900     MOVE 720-IDKUNDNR        TO A12-IDKUNDNR                             
061000     MOVE 720-KDPRODSL        TO A12-KDPRODSL                             
061100     MOVE 720-KDPSLLOC        TO A12-KDPSLLOC                             
061200     MOVE 720-KVKREANT        TO A12-KVJUSTKV                             
061300     MOVE 720-PRAVCOST        TO A12-PRAVCOST                             
061400     MOVE 720-KDANMORS        TO A12-KDANMORS                             
061500                                                                          
061600     PERFORM S06-SKRIV-W51033-A12                                         
061700     .                                                                    
061800     EJECT                                                                
061900 Z-FINIT SECTION.                                                         
062000                                                                          
062100     IF MINST-EN-K-NOTA                                                   
062200       PERFORM BA-SKAPA-KNOTA-HUV-POST                                    
062300     END-IF                                                               
062400                                                                          
062500     CLOSE W51033                                                         
062600                                                                          
062700     MOVE 'S' TO POSTSUM-OPKOD                                            
062800     CALL POSTSUM USING POSTSUM-PARM                                      
062900     .                                                                    
063000     EJECT                                                                
063100 S01-LAES-SORTFIL SECTION.                                                
063200                                                                          
063300     RETURN SORTFIL INTO SORT-AREA                                        
063400     AT END                                                               
063500        MOVE JA TO SORTFIL-EOF-SW                                         
063600     NOT AT END                                                           
063700        MOVE 'SORT '     TO POSTSUM-FDNAMN                                
063800        MOVE 'W51032DS'  TO POSTSUM-DDNAMN2                               
063900        MOVE 720-IDPTYP  TO POSTSUM-TRANSTYP                              
064000        CALL POSTSUM USING POSTSUM-PARM                                   
064100     END-RETURN                                                           
064200     .                                                                    
064300     EJECT                                                                
064400 S04-SKRIV-W51033-A04 SECTION.                                            
064500                                                                          
064600     WRITE UA04-POST FROM A04-AREA                                        
064700                                                                          
064800     MOVE 'A04'       TO POSTSUM-TRANSTYP                                 
064900     MOVE 'W51033'    TO POSTSUM-FDNAMN                                   
065000     MOVE 'W51032D2'  TO POSTSUM-DDNAMN2                                  
065100     CALL POSTSUM USING POSTSUM-PARM                                      
065200     .                                                                    
065300     SKIP2                                                                
065400 S05-SKRIV-W51033-A05 SECTION.                                            
065500                                                                          
065600     WRITE UA05-POST FROM A05-AREA                                        
065700                                                                          
065800     MOVE 'A05'       TO POSTSUM-TRANSTYP                                 
065900     MOVE 'W51033'    TO POSTSUM-FDNAMN                                   
066000     MOVE 'W51032D2'  TO POSTSUM-DDNAMN2                                  
066100     CALL POSTSUM USING POSTSUM-PARM                                      
066200     .                                                                    
066300     SKIP2                                                                
066400 S06-SKRIV-W51033-A12 SECTION.                                            
066500                                                                          
066600     WRITE UA12-POST FROM A12-AREA                                        
066700                                                                          
066800     MOVE 'A12'       TO POSTSUM-TRANSTYP                                 
066900     MOVE 'W51033'    TO POSTSUM-FDNAMN                                   
067000     MOVE 'W51032D2'  TO POSTSUM-DDNAMN2                                  
067100     CALL POSTSUM USING POSTSUM-PARM                                      
067200     .                                                                    
067300     SKIP2                                                                
067400 S07-SKRIV-W51033-L05 SECTION.                                            
067500                                                                          
067600     WRITE UL05-POST FROM L05-AREA                                        
067700                                                                          
067800     MOVE 'L05'       TO POSTSUM-TRANSTYP                                 
067900     MOVE 'W51033'    TO POSTSUM-FDNAMN                                   
068000     MOVE 'W51032D2'  TO POSTSUM-DDNAMN2                                  
068100     CALL POSTSUM USING POSTSUM-PARM                                      
068200     .                                                                    
068300     SKIP3                                                                
068400 S99-ABEND SECTION.                                                       
068500                                                                          
068600     MOVE 'S' TO POSTSUM-OPKOD                                            
068700     CALL POSTSUM USING POSTSUM-PARM                                      
068800     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
068900     .                                                                    
