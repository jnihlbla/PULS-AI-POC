000200 ID DIVISION.                                                             
000300 PROGRAM-ID.                 W4189700.                                    
000900 AUTHOR.                     BERT ANDERSSON.                              
001100 DATE-WRITTEN.               JULI    2002.                                
001200 DATE-COMPILED.                                                           
001500*                                                                         
001530*  POGRAMMET ÄR EN KOPIA AV PGM W4189200.                                 
001540*                                                                         
001600*  FUNKTION:                                                              
001700*  PROGRAMMET ÄR ETT  BATCH - PROGRAM SOM                                 
001800*  - LÄSER EN FIL OCH                                                     
001900*  - SKRIVER KREDITNOTOR TILL LDC-2A  WAKEFIELD                           
002600*                                                                         
004100*  INDATA: W41897                                                         
004200*                                                                         
004210*  POSTTYPER:                                                             
004400*  78A      BILDAR KREDITNOTAHUVUD                                        
004700*  78B      BILDAR KREDITNOTARAD                                          
005800*                                                                         
007100*  UTDATA: LISTA1  W41897-001 KREDITNOTA-LDC-2A                           
007200*        : LISTA2  W41897-002 PÅ FIL TILL FICHE                           
007300*        : LISTA3  W41897-003 PÅ FIL TILL ONDEMAND                        
008700*                                                                         
008800*  SUBPROGR:   DATKORT                                                    
009500*                                                                         
009800     EJECT                                                                
009900 ENVIRONMENT DIVISION.                                                    
010300 INPUT-OUTPUT SECTION.                                                    
010500 FILE-CONTROL.                                                            
010600     SELECT   W41897   ASSIGN TO  W41897D1.                               
010900     SELECT   LISTA1   ASSIGN TO  W41897D2.                               
011000     SELECT   LISTA2   ASSIGN TO  W41897D3.                               
011100     SELECT   LISTA3   ASSIGN TO  W41897D4.                               
011300 DATA DIVISION.                                                           
011500 FILE SECTION.                                                            
011700 FD  W41897                                                               
011701     RECORDING V                                                          
011710     BLOCK 0.                                                             
012200 01  W41897-POST.                                                         
012400*    03  -COPY W41878A   -PRE IN-.                                        
013000                                                                          
015200 FD  LISTA1                                                               
015210     RECORDING F                                                          
015220     BLOCK 0.                                                             
015500 01  LISTA1-POST         PIC X(83).                                       
015700                                                                          
015800 FD  LISTA2                                                               
015900     RECORDING F                                                          
016000     BLOCK 0.                                                             
016100 01  LISTA2-POST         PIC X(83).                                       
016110                                                                          
016120 FD  LISTA3                                                               
016130     RECORDING F                                                          
016140     BLOCK 0.                                                             
016150 01  LISTA3-POST         PIC X(83).                                       
016200                                                                          
017900 WORKING-STORAGE SECTION.                                                 
017910*    -- CHECKED BY WY2000                                                 
018000     SKIP3                                                                
018200 77  FLTA                PIC X(6)       VALUE 'W41897'.                   
018300 77  FLTB                PIC X(6)       VALUE 'WDATUM'.                   
018301 77  JA                  PIC X(1)       VALUE 'J'.                        
018302 77  NEJ                 PIC X(1)       VALUE 'N'.                        
018310 77  WS-TEXT1            PIC X(16)      VALUE 'INTERNUPPACKNING'.         
018400                                                                          
021301     EJECT                                                                
021320 01  DATUMKORT.                                                           
021400     03  FILLER          PIC X(11).                                       
021500     03  DATUM           PIC 9(6).                                        
021600     03  FILLER          PIC X(63).                                       
021700     EJECT                                                                
021800 01  SUBPROGRAM.                                                          
022000     03  DATKORT           PIC X(8)         VALUE 'DATKORT'.              
022500     EJECT                                                                
023100 01  FELTEXT.                                                             
023200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT '.            
023300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
023400     SKIP2                                                                
025700 01  SWITCHAR.                                                            
025800     03  EOF-SW            PIC X            VALUE 'N'.                    
025900         88  EOF                            VALUE 'J'.                    
025940     03  SW-FIRST-TIME-78A PIC X            VALUE 'J'.                    
025950         88  FIRST-TIME-78A                 VALUE 'J'.                    
025960     03  INTERNUPPACK-SW   PIC X            VALUE 'N'.                    
025970         88  INTERNUPPACKNING               VALUE 'J'.                    
027300     EJECT                                                                
030700 01  IFYLLDA-FALT.                                                        
030800     03  W-ADVANCE          PIC S9(9)      COMP SYNC.                     
030810     03  W-SIDR-C1          PIC S9(5)      COMP-3   VALUE ZERO.           
031000     03  W-RADR-C1          PIC S9(5)      COMP-3   VALUE ZERO.           
031700     03  W-TOTPRFRAKT       PIC S9(8)V99   COMP-3   VALUE ZERO.           
031800     03  W-TOTPRHAMNAV      PIC S9(8)V99   COMP-3   VALUE ZERO.           
031900     03  W-TOTPRLEGKST      PIC S9(8)V99   COMP-3   VALUE ZERO.           
032000     03  W-TOTPRFOERS       PIC S9(8)V99   COMP-3   VALUE ZERO.           
034120     EJECT                                                                
034130*                                                                         
034140 01  FILLER                 PIC X(16) VALUE 'TEST-IDDISTRIKT '.           
034150 01  TEST-IDDISTR           PIC 9(5)   COMP-3.                            
034160*01  FILLER  -COPY WWDIST03 -RED TEST-IDDISTR.                            
034170     EJECT                                                                
034200                                                                          
058110 01  RADER-TILL-KNOTA-LEVANM.                                             
058120     03  RUBRAD3-78A.                                                     
058130         05  FILLER           PIC X(45)    VALUE SPACE.                   
058140         05  DATUM-78A        PIC 9(06).                                  
058150         05  FILLER           PIC X(02)    VALUE SPACE.                   
058160         05  IDDISTR-78A      PIC Z(04)9.                                 
058170         05  IDKUNDNR-78A     PIC Z(07).                                  
058180         05  FILLER           PIC X(01)    VALUE SPACE.                   
058190         05  IDDC-78A         PIC X(02).                                  
058191         05  FILLER           PIC X(02)    VALUE ' C'.                    
058192         05  IDKNOTNR-78A     PIC 9(07).                                  
058193         05  FILLER           PIC X(01)    VALUE SPACE.                   
058194         05  SIDR-78A         PIC Z(04)9.                                 
058196     03  RUBRAD5-78A.                                                     
058197         05  FILLER           PIC X(09)    VALUE SPACE.                   
058198         05  BEKOPARE-1-78A   PIC X(27).                                  
058199         05  FILLER           PIC X(47)    VALUE SPACE.                   
058200     03  RUBRAD6-78A.                                                     
058201         05  FILLER           PIC X(09)    VALUE SPACE.                   
058202         05  BEKOPARE-2-78A   PIC X(27).                                  
058203         05  FILLER           PIC X(47)    VALUE SPACE.                   
058210     03  RUBRAD7-78A.                                                     
058220         05  FILLER           PIC X(09)    VALUE SPACE.                   
058230         05  ADKOPARE-1-78A   PIC X(27).                                  
058231         05  FILLER           PIC X(47)    VALUE SPACE.                   
058240     03  RUBRAD8-78A.                                                     
058250         05  FILLER           PIC X(09)    VALUE SPACE.                   
058260         05  ADKOPARE-2-78A   PIC X(27).                                  
058261         05  FILLER           PIC X(09)    VALUE SPACE.                   
058270         05  IDRAPPNR-78A     PIC Z(07).                                  
058271         05  FILLER           PIC X(02)    VALUE SPACE.                   
058280         05  TILEVANM-78A     PIC 9(06).                                  
058291         05  FILLER           PIC X(14)    VALUE SPACE.                   
058293         05  TIRETILL-78A     PIC 9(06) BLANK WHEN ZERO.                  
058294         05  FILLER           PIC X(03)    VALUE SPACE.                   
058295     03  RUBRAD9-78A.                                                     
058296         05  FILLER           PIC X(09)    VALUE SPACE.                   
058297         05  FILLER           PIC X(30)                                   
058298             VALUE 'VAT REGISTRATION NO CUSTOMER: '.                      
058299         05  IDVAT-78A        PIC X(17).                                  
058300         05  FILLER           PIC X(27)    VALUE SPACE.                   
058301     EJECT                                                                
058302     03  DETRAD-78B.                                                      
058303         05  FILLER           PIC X(09)    VALUE SPACE.                   
058304         05  KDFAKTYP-78B     PIC X(01).                                  
058305         05  IDFAKT-78B       PIC Z(07).                                  
058307         05  TIFAKT-78B       PIC 9(06) BLANK WHEN ZERO.                  
058308         05  FILLER           PIC X(01)    VALUE SPACE.                   
058309         05  IDARTNR-78B      PIC Z(08)9.                                 
058310         05  FILLER           PIC X(01)    VALUE SPACE.                   
058311         05  BEART-78B        PIC X(15).                                  
058312         05  FILLER           PIC X(02)    VALUE SPACE.                   
058313         05  KVLEVANM-78B     PIC Z(05)9.                                 
058314         05  KVKREANT-78B     PIC Z(05)9.                                 
058315         05  PRARTBTO-78B     PIC Z(06).Z(02).                            
058316         05  RADBELOPP-78B    PIC Z(08).Z(02).                            
058317     EJECT                                                                
058318     03  TOTRAD51-78B.                                                    
058319         05  FILLER           PIC X(72)   VALUE SPACE.                    
058320         05  TOTBELOPP-1-78B  PIC Z(08).Z(02).                            
058321     03  TOTRAD52-78B.                                                    
058322         05  FILLER           PIC X(09)   VALUE SPACE.                    
058323         05  TEXT1-78B        PIC X(16)   VALUE SPACE.                    
058324         05  FILLER           PIC X(47)   VALUE SPACE.                    
058325         05  PRLANDCO-78B     PIC Z(08).Z(02).                            
058326     03  TOTRAD53-78B.                                                    
058327         05  FILLER           PIC X(72)   VALUE SPACE.                    
058328         05  PREMBHNT-78B     PIC Z(08).Z(02).                            
058329     03  TOTRAD54-78B.                                                    
058330         05  FILLER           PIC X(72)   VALUE SPACE.                    
058331         05  PRFRAKT-78B      PIC Z(08).Z(02).                            
058332     03  TOTRAD55-78B.                                                    
058333         05  FILLER           PIC X(82)   VALUE SPACE.                    
058334     03  TOTRAD56-78B.                                                    
058335         05  FILLER           PIC X(72)   VALUE SPACE.                    
058336         05  PRLEGKST-78B     PIC Z(08).Z(02).                            
058337     03  TOTRAD57-78B.                                                    
058338         05  FILLER           PIC X(72)   VALUE SPACE.                    
058339         05  PRFOERS-78B      PIC Z(08).Z(02).                            
058340     03  TOTRAD58-78B.                                                    
058341         05  FILLER           PIC X(82)   VALUE SPACE.                    
058342     03  TOTRAD60-78B.                                                    
058343         05  FILLER           PIC X(72)   VALUE SPACE.                    
058344         05  PRMOMS-78B       PIC Z(08).Z(02).                            
058347     03  TOTRAD61-78B.                                                    
058351         05  FILLER           PIC X(72)   VALUE SPACE.                    
058352         05  TOTBELOPP-2-78B  PIC Z(08).Z(02).                            
058360     03  TOTRAD62-78B.                                                    
058361         05  FILLER           PIC X(09)   VALUE SPACE.                    
058362         05  FORSAKRAN-1-78A  PIC X(38).                                  
058363         05  FILLER           PIC X(36)   VALUE SPACE.                    
058364     03  TOTRAD63-78B.                                                    
058365         05  FILLER           PIC X(09)   VALUE SPACE.                    
058366         05  FORSAKRAN-2-78A  PIC X(34).                                  
058367         05  FILLER           PIC X(09)   VALUE SPACE.                    
058368         05  KDVALISO-78A     PIC X(03).                                  
058369         05  FILLER           PIC X(01)   VALUE SPACE.                    
058370         05  PRKURS-78A       PIC Z(04)9.9(05) BLANK WHEN ZERO.           
058371         05  KDVALUT-78A      PIC Z(03).                                  
058372         05  FILLER           PIC X(02)   VALUE SPACE.                    
058373         05  TOTBELOPP-3-78B  PIC Z(08).Z(02) BLANK WHEN ZERO.            
058374     EJECT                                                                
061983 01  BLANKRADER.                                                          
061984     03  BLANKRAD.                                                        
061985         05  FILLER           PIC X(83)    VALUE SPACE.                   
062012     EJECT                                                                
062020*            * * * * * * * * * * *                                        
062100*            *   I N F I L E N   *                                        
062200*            * * * * * * * * * * *                                        
062300******************************************************************        
062310 01  FILLER                     PIC X(05) VALUE '*78A*'.                  
062400*01  POST         -COPY W41878A     -PRE  78A-                            
062500******************************************************************        
062501     EJECT                                                                
062510 01  FILLER                     PIC X(05) VALUE '*78B*'.                  
062600*01  POST         -COPY W41878B     -PRE  78B-                            
062700******************************************************************        
065700     EJECT                                                                
065800 LINKAGE SECTION.                                                         
065900                                                                          
066200     EJECT                                                                
155800 PROCEDURE DIVISION.                                                      
156000                                                                          
256500     PERFORM A-INIT                                                       
256600     PERFORM S01-LAES-INFIL                                               
256610                                                                          
256700     PERFORM UNTIL EOF                                                    
256800       MOVE SPACE                          TO TEXT1-78B                   
257500       PERFORM B-BEHANDLA                                                 
257600       PERFORM S01-LAES-INFIL                                             
257800     END-PERFORM                                                          
257900                                                                          
257901     IF NOT FIRST-TIME-78A                                                
257902        PERFORM CA-BYGG-SIDSLUT-78A                                       
257903     END-IF                                                               
257904                                                                          
257910     PERFORM Z-FINIT                                                      
258000                                                                          
258200     MOVE ZERO                             TO RETURN-CODE                 
258300     GOBACK                                                               
258400     .                                                                    
258500     EJECT                                                                
258600 A-INIT  SECTION.                                                         
258700                                                                          
258800     CALL DATKORT USING FLTA FLTB DATUMKORT                               
258900                                                                          
259000     OPEN INPUT   W41897                                                  
259100          OUTPUT  LISTA1                                                  
259110                  LISTA2                                                  
259120                  LISTA3                                                  
259200                                                                          
259500     PERFORM AA-INIT-AV-AREOR                                             
259600     .                                                                    
259700     EJECT                                                                
260900 AA-INIT-AV-AREOR     SECTION.                                            
261000     SKIP3                                                                
261110     MOVE NEJ                              TO INTERNUPPACK-SW             
261120     MOVE +1                               TO W-SIDR-C1                   
261200     MOVE +18                              TO W-RADR-C1                   
261203     MOVE 'J'                              TO SW-FIRST-TIME-78A           
261332     MOVE ZERO                             TO W-TOTPRFRAKT                
261333                                              W-TOTPRLEGKST               
261334                                              W-TOTPRHAMNAV               
261335                                              W-TOTPRFOERS                
273600     .                                                                    
273700     EJECT                                                                
273800 B-BEHANDLA SECTION.                                                      
274200                                                                          
274300     EVALUATE IN-IDPTYP                                                   
274400        WHEN '78A'                                                        
274800          IF FIRST-TIME-78A                                               
274802             MOVE 'N'                      TO SW-FIRST-TIME-78A           
274804          ELSE                                                            
274810             PERFORM CA-BYGG-SIDSLUT-78A                                  
274841          END-IF                                                          
274850          MOVE W41897-POST                 TO 78A-POST                    
275001          MOVE +1                          TO W-SIDR-C1                   
275002          MOVE +18                         TO W-RADR-C1                   
275008          PERFORM CB-BYGG-HUVUD-78A                                       
275700        WHEN '78B'                                                        
275710          MOVE W41897-POST                 TO 78B-POST                    
275820          IF W-RADR-C1 > 48                                               
275821             ADD   +1                      TO W-SIDR-C1                   
275830             PERFORM CB-BYGG-HUVUD-78A                                    
275851             MOVE +18                      TO W-RADR-C1                   
275860          END-IF                                                          
275900          PERFORM CC-BYGG-RAD-78B                                         
276000          ADD   +1                         TO W-RADR-C1                   
291300     END-EVALUATE                                                         
291400     .                                                                    
291500     EJECT                                                                
291501 CA-BYGG-SIDSLUT-78A SECTION.                                             
291502                                                                          
291503     IF INTERNUPPACKNING                                                  
291504        MOVE WS-TEXT1                      TO TEXT1-78B                   
291505     END-IF                                                               
291506                                                                          
291527     MOVE 78A-SUKRENTO                     TO TOTBELOPP-1-78B             
291536     MOVE 78A-PRLANDCO                     TO PRLANDCO-78B                
291545     MOVE 78A-PREMBHNT                     TO PREMBHNT-78B                
291546     MOVE W-TOTPRFRAKT                     TO PRFRAKT-78B                 
291547     MOVE W-TOTPRLEGKST                    TO PRLEGKST-78B                
291548     MOVE W-TOTPRFOERS                     TO PRFOERS-78B                 
291552                                                                          
291565     MOVE 78A-PRMOMS                        TO PRMOMS-78B                 
291574                                                                          
291576     MOVE 78A-SUKRENOT                     TO TOTBELOPP-2-78B             
291577     MOVE 78A-FORSAKRAN-1                  TO FORSAKRAN-1-78A             
291578     MOVE 78A-FORSAKRAN-2                  TO FORSAKRAN-2-78A             
291579     MOVE 78A-KDVALISO                     TO KDVALISO-78A                
291580     MOVE 78A-PRKURS-LOC                   TO PRKURS-78A                  
291581     MOVE 78A-KDVALUT                      TO KDVALUT-78A                 
291582                                                                          
291583     IF 78A-KDVALISO = 'SEK'                                              
291584        MOVE SPACE                         TO TOTRAD63-78B                
291592     END-IF                                                               
291593                                                                          
291595     MOVE 78A-SUKREUTL                     TO TOTBELOPP-3-78B             
291596***** 78A-SUKREUTL = 78A-SUKRENOT ( SUMMA KREDITNOT.BRUTTO. )             
291597***** VID 78A-KDVALISO = 'SEK'                                            
291598***** ANNARS OMRÄKN.I UTL. VALUTA                                         
291599                                                                          
291600     COMPUTE W-ADVANCE = 50 - W-RADR-C1 + 2                               
291601                                                                          
291602     WRITE LISTA1-POST FROM TOTRAD51-78B AFTER W-ADVANCE                  
291603     WRITE LISTA1-POST FROM TOTRAD52-78B AFTER 1                          
291604     WRITE LISTA1-POST FROM TOTRAD53-78B AFTER 1                          
291605     WRITE LISTA1-POST FROM TOTRAD54-78B AFTER 1                          
291606     WRITE LISTA1-POST FROM TOTRAD55-78B AFTER 1                          
291607     WRITE LISTA1-POST FROM TOTRAD56-78B AFTER 1                          
291608     WRITE LISTA1-POST FROM TOTRAD57-78B AFTER 1                          
291609     WRITE LISTA1-POST FROM TOTRAD58-78B AFTER 1                          
291610     WRITE LISTA1-POST FROM TOTRAD60-78B AFTER 1                          
291611     WRITE LISTA1-POST FROM TOTRAD61-78B AFTER 1                          
291612     WRITE LISTA1-POST FROM TOTRAD62-78B AFTER 2                          
291613                                                                          
291614     WRITE LISTA2-POST FROM TOTRAD51-78B AFTER W-ADVANCE                  
291615     WRITE LISTA2-POST FROM TOTRAD52-78B AFTER 1                          
291616     WRITE LISTA2-POST FROM TOTRAD53-78B AFTER 1                          
291617     WRITE LISTA2-POST FROM TOTRAD54-78B AFTER 1                          
291618     WRITE LISTA2-POST FROM TOTRAD55-78B AFTER 1                          
291619     WRITE LISTA2-POST FROM TOTRAD56-78B AFTER 1                          
291620     WRITE LISTA2-POST FROM TOTRAD57-78B AFTER 1                          
291621     WRITE LISTA2-POST FROM TOTRAD58-78B AFTER 1                          
291622     WRITE LISTA2-POST FROM TOTRAD60-78B AFTER 1                          
291623     WRITE LISTA2-POST FROM TOTRAD61-78B AFTER 1                          
291624     WRITE LISTA2-POST FROM TOTRAD62-78B AFTER 2                          
291625                                                                          
291626     WRITE LISTA3-POST FROM TOTRAD51-78B AFTER W-ADVANCE                  
291627     WRITE LISTA3-POST FROM TOTRAD52-78B AFTER 1                          
291628     WRITE LISTA3-POST FROM TOTRAD53-78B AFTER 1                          
291629     WRITE LISTA3-POST FROM TOTRAD54-78B AFTER 1                          
291630     WRITE LISTA3-POST FROM TOTRAD55-78B AFTER 1                          
291631     WRITE LISTA3-POST FROM TOTRAD56-78B AFTER 1                          
291632     WRITE LISTA3-POST FROM TOTRAD57-78B AFTER 1                          
291633     WRITE LISTA3-POST FROM TOTRAD58-78B AFTER 1                          
291634     WRITE LISTA3-POST FROM TOTRAD60-78B AFTER 1                          
291635     WRITE LISTA3-POST FROM TOTRAD61-78B AFTER 1                          
291636     WRITE LISTA3-POST FROM TOTRAD62-78B AFTER 2                          
291637                                                                          
291639     WRITE LISTA1-POST FROM TOTRAD63-78B AFTER 1                          
291640     WRITE LISTA2-POST FROM TOTRAD63-78B AFTER 1                          
291641     WRITE LISTA3-POST FROM TOTRAD63-78B AFTER 1                          
291643                                                                          
291644     PERFORM CAA-NOLLST-78B                                               
291645     .                                                                    
291646     EJECT                                                                
291647 CAA-NOLLST-78B    SECTION.                                               
291648                                                                          
291649     MOVE NEJ                              TO INTERNUPPACK-SW             
291650     MOVE ZERO                             TO W-TOTPRFRAKT                
291651                                              W-TOTPRLEGKST               
291652                                              W-TOTPRFOERS                
291653     .                                                                    
291660     EJECT                                                                
291699 CB-BYGG-HUVUD-78A SECTION.                                               
291700                                                                          
291701     MOVE 78A-DATUM                        TO DATUM-78A                   
291703     MOVE 78A-IDDISTR                      TO IDDISTR-78A                 
291705     MOVE 78A-IDKUNDNR                     TO IDKUNDNR-78A                
291707     MOVE 78A-IDDC                         TO IDDC-78A                    
291708     MOVE 78A-IDKNOTNR                     TO IDKNOTNR-78A                
291712     MOVE 78A-IDVAT                        TO IDVAT-78A                   
291715     MOVE W-SIDR-C1                        TO SIDR-78A                    
291720     MOVE 78A-BEKOPARE-1                   TO BEKOPARE-1-78A              
291723     MOVE 78A-BEKOPARE-2                   TO BEKOPARE-2-78A              
291725     MOVE 78A-ADKOPARE-1                   TO ADKOPARE-1-78A              
291727     MOVE 78A-ADKOPARE-2                   TO ADKOPARE-2-78A              
291728     MOVE 78A-IDRAPPNR                     TO IDRAPPNR-78A                
291729     MOVE 78A-TILEVANM                     TO TILEVANM-78A                
291731     MOVE 78A-TIRETILL                     TO TIRETILL-78A                
291734     MOVE 78A-PRFOERS                      TO W-TOTPRFOERS                
291735     MOVE 78A-PRFRAKT                      TO W-TOTPRFRAKT                
291736     MOVE 78A-PRLEGKST                     TO W-TOTPRLEGKST               
291737                                                                          
291738     WRITE LISTA1-POST FROM BLANKRAD       AFTER PAGE                     
291739     WRITE LISTA1-POST FROM RUBRAD3-78A AFTER 2                           
291740     WRITE LISTA1-POST FROM RUBRAD5-78A AFTER 2                           
291741     WRITE LISTA1-POST FROM RUBRAD6-78A AFTER 1                           
291742     WRITE LISTA1-POST FROM RUBRAD7-78A AFTER 1                           
291743     WRITE LISTA1-POST FROM RUBRAD8-78A AFTER 1                           
291745                                                                          
291746     WRITE LISTA2-POST FROM BLANKRAD       AFTER PAGE                     
291747     WRITE LISTA2-POST FROM RUBRAD3-78A AFTER 2                           
291748     WRITE LISTA2-POST FROM RUBRAD5-78A AFTER 2                           
291749     WRITE LISTA2-POST FROM RUBRAD6-78A AFTER 1                           
291750     WRITE LISTA2-POST FROM RUBRAD7-78A AFTER 1                           
291751     WRITE LISTA2-POST FROM RUBRAD8-78A AFTER 1                           
291752                                                                          
291753     WRITE LISTA3-POST FROM BLANKRAD       AFTER PAGE                     
291754     WRITE LISTA3-POST FROM RUBRAD3-78A AFTER 2                           
291755     WRITE LISTA3-POST FROM RUBRAD5-78A AFTER 2                           
291756     WRITE LISTA3-POST FROM RUBRAD6-78A AFTER 1                           
291757     WRITE LISTA3-POST FROM RUBRAD7-78A AFTER 1                           
291758     WRITE LISTA3-POST FROM RUBRAD8-78A AFTER 1                           
291759                                                                          
291763     WRITE LISTA1-POST FROM RUBRAD9-78A AFTER 5                           
291764     WRITE LISTA2-POST FROM RUBRAD9-78A AFTER 5                           
291765     WRITE LISTA3-POST FROM RUBRAD9-78A AFTER 5                           
291767     .                                                                    
291768     EJECT                                                                
291769 CC-BYGG-RAD-78B SECTION.                                                 
291770                                                                          
291771     MOVE 78B-KDFAKTYP                     TO KDFAKTYP-78B                
291772     MOVE 78B-IDFAKT                       TO IDFAKT-78B                  
291773     MOVE 78B-TIFAKT                       TO TIFAKT-78B                  
291774     MOVE 78B-IDARTNR                      TO IDARTNR-78B                 
291775     MOVE 78B-BEART                        TO BEART-78B                   
291776     MOVE 78B-KVLEVANM                     TO KVLEVANM-78B                
291777     MOVE 78B-KVKREANT                     TO KVKREANT-78B                
291778     MOVE 78B-PRARTBTO                     TO PRARTBTO-78B                
291779                                                                          
291780     MOVE 78B-SUKRENTO-RAD                 TO RADBELOPP-78B               
291781                                                                          
291782     IF W-RADR-C1 = 18                                                    
291788        WRITE LISTA1-POST FROM DETRAD-78B AFTER 5                         
291789        WRITE LISTA2-POST FROM DETRAD-78B AFTER 5                         
291790        WRITE LISTA3-POST FROM DETRAD-78B AFTER 5                         
291792     ELSE                                                                 
291793        WRITE LISTA1-POST FROM DETRAD-78B AFTER 1                         
291794        WRITE LISTA2-POST FROM DETRAD-78B AFTER 1                         
291795        WRITE LISTA3-POST FROM DETRAD-78B AFTER 1                         
291800     END-IF                                                               
291802                                                                          
291803     IF 78B-KDANMORS = '97'                                               
291804        MOVE JA                             TO INTERNUPPACK-SW            
291805     END-IF                                                               
291810     .                                                                    
291900     EJECT                                                                
299100 Z-FINIT SECTION.                                                         
299110                                                                          
299200     SKIP3                                                                
300000     CLOSE  W41897 LISTA1 LISTA2 LISTA3                                   
300100     .                                                                    
300200     EJECT                                                                
300300 S01-LAES-INFIL SECTION.                                                  
300400                                                                          
300500     READ  W41897                                                         
300600                     AT END     MOVE 'J' TO EOF-SW                        
300700     END-READ                                                             
300800     .                                                                    
