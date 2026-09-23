000100 ID DIVISION.                                                             
000300 PROGRAM-ID.    W5128400.                                                 
000400                                                                          
000500*    AUTHOR.        BARSHARANI BISHOYE.                                   
000600*    DATE-WRITTEN   NOVEMBER 2019.                                        
000700*                                                                         
000800*    FUNKTION:                                                            
000900*               SKAPAR LISTPOSTER KURANSGRUPPERADE EL. NEDAN:             
001100*             - ARTIKLAR MED KDERS > 20     HAMNAR I KURANS 6             
001110*             - ARTIKLAR MED PUBLICERINGSVECKA YNGRE ÄN                   
001200*               KÖRNINGSDATUM - 2 ÅR        HAMNAR I KURANS 1             
001300*             - ARTIKLAR UTAN ORDERINGÅNG   HAMNAR I KURANS 5             
001400*                                                                         
001500*             - ARTIKLAR MED KDERS > 21     HAMNAR I KURANS 5             
001600*    UNDANTAG - ARTIKLAR MED KDERS = 29  HANTERAS ENLIGT SHELFLIFE        
001700*                                                                         
001800*               SKAPAR LISTA KURANSGRUPPERING MED SHELFLIFEMETODEN        
001900*               ÅRSBEHOV TAS FRAM M.H.A. ORDERINGÅNG W51294B.             
002000*               SULEVANT  FÖR 53 VECKOR                                   
002100*               TOT. LAGERSALDO HÄMTAS FRÅN LAGERBANDET W01184.           
002200*               KVLS + KVEFRS                                             
002300*               ÅRSBEHOV STÄLLS I RELATION TILL TOT.LAGERSALDO.           
002400*               ALLT UPP TILL   1  ÅRSBEHOV HAMNAR I KURANS 1             
002500*               FRÅN 1 UPP TILL 3  ÅRSBEHOV HAMNAR I KURANS 2             
002600*               FRÅN 3 UPP TILL 5  ÅRSBEHOV HAMNAR I KURANS 3             
002700*               FRÅN 5 UPP TILL 10 ÅRSBEHOV HAMNAR I KURANS 4             
002800*               FRÅN 10 ÅRBEHOV OCH UPPÅT   HAMNAR I KURANS 5             
002900*                                                                         
003000     EJECT                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200                                                                          
003300 INPUT-OUTPUT SECTION.                                                    
003400 FILE-CONTROL.                                                            
003500                                                                          
003600     SELECT W5128PA ASSIGN       TO W51284D1.                             
003700                                                                          
003800     SELECT W51294B  ASSIGN       TO W51284D2.                            
003900                                                                          
004200     SELECT W51284   ASSIGN       TO W51284D3.                            
004300                                                                          
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600 FILE SECTION.                                                            
004700                                                                          
004800 FD  W5128PA                                                              
004900     RECORDING F                                                          
005000     BLOCK CONTAINS 0.                                                    
005100                                                                          
005200 01  INPOST1.                                                             
005300*    03   -COPY W5128P    -L                                              
005400     SKIP2                                                                
005500                                                                          
005600 FD  W51294B                                                              
005700     RECORDING F                                                          
005800     BLOCK CONTAINS 0.                                                    
005900                                                                          
006000 01  INPOST2.                                                             
006100*    03   -COPY W51295   -L                                               
006200     SKIP2                                                                
006300                                                                          
007200 FD  W51284                                                               
007897     RECORDING F                                                          
007898     BLOCK CONTAINS 0.                                                    
007899                                                                          
007900*01  UTPOST      -COPY W51284    -L                                       
007901     SKIP2                                                                
007902                                                                          
007910 WORKING-STORAGE SECTION.                                                 
008000     SKIP3                                                                
008100 77  IDPGM                   PIC X(8)      VALUE 'W5128400'.              
008200 77  JA                      PIC X         VALUE 'J'.                     
008300 77  NEJ                     PIC X         VALUE 'N'.                     
008400 77  EOF-W5128PA             PIC X         VALUE 'N'.                     
008500 77  EOF-W51294B             PIC X         VALUE 'N'.                     
008700 77  WS-IX                   PIC S9(3)   COMP-3 VALUE ZERO.               
008800 77  MAX-WEEK                PIC S9(3)   COMP-3 VALUE +53.                
008900                                                                          
009000 01  W-TIAAVVD               PIC S9(5).                                   
009100 01  W-TIYYAAVVD             PIC S9(7).                                   
009200 01  W-TIAAVVD-P             PIC S9(5)   VALUE ZERO COMP-3.               
009300                                                                          
009400 01  WS-SULEVANT-TOT         PIC S9(9)   COMP-3 VALUE ZERO.               
009500 01  WS-KDERS                PIC 9(3)    VALUE ZERO.                      
009600 01  WS-KDPSLLOC             PIC 9(2)    VALUE ZERO.                      
009700 01  WS-TIFINLV              PIC S9(5)   COMP-3 VALUE ZERO.               
009800 01  WS-TIFINLV2             PIC S9(7)   COMP-3 VALUE ZERO.               
009900 01  WS-CHECK                PIC S9(7)   COMP-3 VALUE ZERO.               
010000 01  WS-STOCK-KURANS         PIC S9(7)   COMP-3 VALUE ZERO.               
010100 01  WS-STOCK-KURANS1        PIC S9(7)   COMP-3 VALUE ZERO.               
010200 01  WS-STOCK-KURANS2        PIC S9(7)   COMP-3 VALUE ZERO.               
010300 01  WS-STOCK-KURANS3        PIC S9(7)   COMP-3 VALUE ZERO.               
010400 01  WS-STOCK-KURANS4        PIC S9(7)   COMP-3 VALUE ZERO.               
010500 01  WS-STOCK-KURANS5        PIC S9(7)   COMP-3 VALUE ZERO.               
010600 01  WS-STOCK-KURANS6        PIC S9(7)   COMP-3 VALUE ZERO.               
010700 01  WS-KURANS               PIC S9(1)   COMP-3 VALUE ZERO.               
010710 01  WS-KVPB                 PIC S9(6)V9(1) COMP-3.                       
010800                                                                          
010900 01  WDATUM                  PIC X(6)    VALUE 'WDATUM'.                  
011000                                                                          
011100 01  SUBPROGRAM.                                                          
011200     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
011300     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
011400                                                                          
011500 01  W5128PA-TRANSID.                                                     
011600     03  FILLER              PIC X(7) VALUE 'W5128PA'.                    
011700     03  FILLER              PIC X(8) VALUE 'W51284D1'.                   
011800     03  FILLER              PIC X(4) VALUE '1184'.                       
011900                                                                          
012000 01  W51294B-TRANSID.                                                     
012100     03  FILLER              PIC X(7) VALUE 'W51294B'.                    
012200     03  FILLER              PIC X(8) VALUE 'W51284D2'.                   
012300     03  FILLER              PIC X(5) VALUE '1296B'.                      
012400                                                                          
013000 01  W51284-TRANSID.                                                      
013100     03  FILLER              PIC X(6) VALUE 'W51284'.                     
013200     03  FILLER              PIC X(8) VALUE 'W51284D3'.                   
013300     03  FILLER              PIC X(4) VALUE '  UT'.                       
013400                                                                          
013500     EJECT                                                                
013600*   -COPY W0005  -PRE POSTSUM-                                            
013700     EJECT                                                                
013800*   -COPY WDATKORT                                                        
013900     EJECT                                                                
014000                                                                          
014010*01  -COPY WWDC99                                                         
014020     EJECT                                                                
014030                                                                          
014100 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
014200 01  INAREA1.                                                             
014300*    03  -COPY W5128P -PRE IN-                                            
014400     EJECT                                                                
014500                                                                          
014600 01  FILLER                  PIC X(16)   VALUE 'IN-AREA2   '.             
014700 01  INAREA2.                                                             
014800*    03  -COPY W51295 -PRE SALES-                                         
014900     EJECT                                                                
015000                                                                          
015500 01  FILLER                  PIC X(16)   VALUE 'UT-AREA1    '.            
015600 01  UTAREA1.                                                             
015700*    03  -COPY W51284 -PRE UT-                                            
015995     EJECT                                                                
015996                                                                          
016000 PROCEDURE DIVISION.                                                      
016100 MAIN SECTION.                                                            
016200                                                                          
016210     PERFORM A-INIT                                                       
016220                                                                          
016230     PERFORM S01-READ-W5128PA-POST                                        
016240     PERFORM S02-READ-W51294B-POST                                        
016250                                                                          
016260     PERFORM UNTIL EOF-W5128PA = JA                                       
016270       MOVE IN-IDDC     TO WS-IDDC                                        
016280       PERFORM UNTIL EOF-W51294B = JA                                     
016290         EVALUATE TRUE                                                    
016291           WHEN IN-IDARTNR = SALES-IDARTNR                                
016292             IF IN-IDDC = SALES-IDDC                                      
016293               PERFORM B-READ-STOCK                                       
016294               PERFORM C-REDIGERA-PRODUCT-GROUP                           
016295               PERFORM D-CREATE-KURANS-FILE                               
016296               PERFORM S01-READ-W5128PA-POST                              
016297               PERFORM S02-READ-W51294B-POST                              
016298             ELSE                                                         
016299               IF IN-IDDC > SALES-IDDC                                    
016300                 PERFORM S02-READ-W51294B-POST                            
016301               ELSE                                                       
016302                 IF IN-IDDC < SALES-IDDC                                  
016303                   MOVE +0   TO WS-SULEVANT-TOT                           
016307                   PERFORM C-REDIGERA-PRODUCT-GROUP                       
016308                   PERFORM D-CREATE-KURANS-FILE                           
016309                   PERFORM S01-READ-W5128PA-POST                          
016310                 END-IF                                                   
016311               END-IF                                                     
016312             END-IF                                                       
016313           WHEN IN-IDARTNR < SALES-IDARTNR                                
016314             MOVE +0    TO WS-SULEVANT-TOT                                
016318             PERFORM C-REDIGERA-PRODUCT-GROUP                             
016319             PERFORM E-CREATE-KURANS-FILE                                 
016320             PERFORM S01-READ-W5128PA-POST                                
016321           WHEN IN-IDARTNR > SALES-IDARTNR                                
016322             PERFORM S02-READ-W51294B-POST                                
016323         END-EVALUATE                                                     
016324       END-PERFORM                                                        
016325       MOVE +0    TO WS-SULEVANT-TOT                                      
016329       PERFORM C-REDIGERA-PRODUCT-GROUP                                   
016330       PERFORM E-CREATE-KURANS-FILE                                       
016331       PERFORM S01-READ-W5128PA-POST                                      
016332     END-PERFORM                                                          
016333                                                                          
016334     PERFORM Z-END                                                        
016335                                                                          
016336     MOVE ZERO TO RETURN-CODE                                             
016337     GOBACK                                                               
016338     .                                                                    
016339     EJECT                                                                
016340                                                                          
016341 A-INIT SECTION.                                                          
016342     OPEN INPUT  W5128PA                                                  
016343                 W51294B                                                  
016344          OUTPUT W51284                                                   
016345                                                                          
016346     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
016347                                                                          
016348     CALL DATKORT  USING IDPGM WDATUM DATUMKORT                           
016349     MOVE D-AAR           TO W-TIAAVVD(1:2)                               
016350     MOVE D-VECKA         TO W-TIAAVVD(3:2)                               
016351     MOVE 1               TO W-TIAAVVD(5:1)                               
016352     COMPUTE W-TIYYAAVVD = 2000000 + W-TIAAVVD                            
016353     .                                                                    
016354     EJECT                                                                
016355                                                                          
016356 B-READ-STOCK SECTION.                                                    
016357     IF SALES-SULEVANT < +0                                               
016358       COMPUTE WS-SULEVANT-TOT ROUNDED = WS-SULEVANT-TOT                  
016362     ELSE                                                                 
016365       COMPUTE WS-SULEVANT-TOT ROUNDED = SALES-SULEVANT                   
016370     END-IF                                                               
016371*    IF SALES-SULEVANT < +0                                               
016372*      COMPUTE WS-SULEVANT-TOT = WS-SULEVANT-TOT + 0 + 0                  
016373*    ELSE                                                                 
016374*      COMPUTE WS-SULEVANT-TOT = SALES-SULEVANT + 0 + 0                   
016375*    END-IF                                                               
016376     .                                                                    
016377     EJECT                                                                
016378                                                                          
016379 C-REDIGERA-PRODUCT-GROUP SECTION.                                        
016380     MOVE IN-KDERS      TO WS-KDERS                                       
016381     MOVE IN-IDDC     TO WS-IDDC                                          
016382     IF NDC-NA                                                            
016383       MOVE IN-KDPSLLOC TO WS-KDPSLLOC                                    
016384       COMPUTE WS-KVPB = IN-KVPB-REF                                      
016385     ELSE                                                                 
016386       MOVE IN-KDPRODSL TO WS-KDPSLLOC                                    
016387       COMPUTE WS-KVPB = IN-KVPB-REF                                      
016388     END-IF                                                               
016389     MOVE IN-TIFINLV    TO WS-TIFINLV                                     
016390     .                                                                    
016391     EJECT                                                                
016392                                                                          
016393 D-CREATE-KURANS-FILE SECTION.                                            
016394     COMPUTE WS-STOCK-KURANS = IN-KVEFRS + IN-KVLS                        
016395     PERFORM S04-GIVE-KURANS-FIRST                                        
016396     PERFORM S05-GIVE-KURANS-SHELFLIFE                                    
016397     PERFORM S06-CREATE-FILE-W51284                                       
016398     .                                                                    
016399     EJECT                                                                
016400                                                                          
016401 E-CREATE-KURANS-FILE SECTION.                                            
016402     COMPUTE WS-STOCK-KURANS = IN-KVEFRS + IN-KVLS                        
016403     PERFORM S04-GIVE-KURANS-FIRST                                        
016404     PERFORM S07-GIVE-KURANS-SHELFLIFE                                    
016405     PERFORM S06-CREATE-FILE-W51284                                       
016406     .                                                                    
016407     EJECT                                                                
016408                                                                          
016409 Z-END SECTION.                                                           
016410     CLOSE W5128PA                                                        
016411           W51294B                                                        
016412           W51284                                                         
016413                                                                          
016414     MOVE 'S'        TO POSTSUM-OPKOD                                     
016415     CALL POSTSUM USING POSTSUM-PARM                                      
016416     .                                                                    
016417     EJECT                                                                
016418                                                                          
016419 S01-READ-W5128PA-POST SECTION.                                           
016420     READ W5128PA INTO INAREA1                                            
016421     AT END                                                               
016422       MOVE JA TO EOF-W5128PA                                             
016423     NOT AT END                                                           
016424       MOVE W5128PA-TRANSID TO POSTSUM-TRANSID                            
016425       CALL POSTSUM USING POSTSUM-PARM                                    
016426     END-READ                                                             
016427     .                                                                    
016428     EJECT                                                                
016429                                                                          
016430 S02-READ-W51294B-POST SECTION.                                           
016431     READ W51294B INTO INAREA2                                            
016432     AT END                                                               
016433       MOVE JA TO EOF-W51294B                                             
016434     NOT AT END                                                           
016435       MOVE W51294B-TRANSID TO POSTSUM-TRANSID                            
016436       CALL POSTSUM USING POSTSUM-PARM                                    
016437     END-READ                                                             
016438     .                                                                    
016439     SKIP2                                                                
016440                                                                          
016441 S04-GIVE-KURANS-FIRST SECTION.                                           
016442     MOVE +0 TO WS-KURANS                                                 
016443     MOVE +0 TO WS-STOCK-KURANS1                                          
016444     MOVE +0 TO WS-STOCK-KURANS2                                          
016445     MOVE +0 TO WS-STOCK-KURANS3                                          
016446     MOVE +0 TO WS-STOCK-KURANS4                                          
016447     MOVE +0 TO WS-STOCK-KURANS5                                          
016448     MOVE +0 TO WS-STOCK-KURANS6                                          
016449     IF WS-KDERS > 20                                                     
016450       IF WS-SULEVANT-TOT = +0                                            
016451         MOVE +6 TO WS-KURANS                                             
016452         MOVE WS-STOCK-KURANS TO WS-STOCK-KURANS6                         
016453       ELSE                                                               
016454         MOVE +5 TO WS-KURANS                                             
016455         MOVE WS-STOCK-KURANS TO WS-STOCK-KURANS5                         
016456       END-IF                                                             
016457     ELSE                                                                 
016458       IF WS-TIFINLV > +0                                                 
016459         IF WS-TIFINLV > +50000                                           
016460           COMPUTE WS-TIFINLV2 = 1900000 + WS-TIFINLV                     
016461         ELSE                                                             
016462           COMPUTE WS-TIFINLV2 = 2000000 + WS-TIFINLV                     
016463         END-IF                                                           
016464         COMPUTE WS-CHECK = W-TIYYAAVVD - WS-TIFINLV2                     
016465*** NYA ARTIKLAR MINDRE ÄN 2 ÅR                                           
016466         IF WS-CHECK < 2001                                               
016467           MOVE +1 TO WS-KURANS                                           
016468           MOVE WS-STOCK-KURANS TO WS-STOCK-KURANS1                       
016469         ELSE                                                             
016470           IF  WS-KDERS = +0                                              
016471           AND WS-KVPB  = +0                                              
016472               MOVE +5 TO WS-KURANS                                       
016473               MOVE WS-STOCK-KURANS TO WS-STOCK-KURANS5                   
016474           ELSE                                                           
016475             IF WS-SULEVANT-TOT = +0                                      
016476               MOVE +5 TO WS-KURANS                                       
016477               MOVE WS-STOCK-KURANS TO WS-STOCK-KURANS5                   
016478             END-IF                                                       
016479           END-IF                                                         
016480         END-IF                                                           
016481       ELSE                                                               
016482         MOVE +1 TO WS-KURANS                                             
016483         MOVE WS-STOCK-KURANS TO WS-STOCK-KURANS1                         
016484       END-IF                                                             
016485     END-IF                                                               
016486     .                                                                    
016487     EJECT                                                                
016488                                                                          
016489 S05-GIVE-KURANS-SHELFLIFE SECTION.                                       
016490     IF WS-KURANS = +0                                                    
016491       IF WS-SULEVANT-TOT > WS-STOCK-KURANS                               
016492         MOVE WS-STOCK-KURANS TO WS-STOCK-KURANS1                         
016493       ELSE                                                               
016494         IF (WS-SULEVANT-TOT * 3) > WS-STOCK-KURANS                       
016495           MOVE WS-SULEVANT-TOT TO WS-STOCK-KURANS1                       
016496           COMPUTE WS-STOCK-KURANS2 = WS-STOCK-KURANS -                   
016497                                      WS-SULEVANT-TOT                     
016498         ELSE                                                             
016499           IF (WS-SULEVANT-TOT * 5) > WS-STOCK-KURANS                     
016500             MOVE WS-SULEVANT-TOT TO WS-STOCK-KURANS1                     
016501             COMPUTE WS-STOCK-KURANS2 = (WS-SULEVANT-TOT * 3) -           
016502                                         WS-SULEVANT-TOT                  
016503             COMPUTE WS-STOCK-KURANS3 = WS-STOCK-KURANS -                 
016504                                       (WS-SULEVANT-TOT * 3)              
016505           ELSE                                                           
016506             IF (WS-SULEVANT-TOT * 10) > WS-STOCK-KURANS                  
016507               MOVE WS-SULEVANT-TOT TO WS-STOCK-KURANS1                   
016508               COMPUTE WS-STOCK-KURANS2 = (WS-SULEVANT-TOT * 3) -         
016509                                           WS-SULEVANT-TOT                
016510               COMPUTE WS-STOCK-KURANS3 = (WS-SULEVANT-TOT * 5) -         
016511                                          (WS-SULEVANT-TOT * 3)           
016512               COMPUTE WS-STOCK-KURANS4 =  WS-STOCK-KURANS -              
016513                                          (WS-SULEVANT-TOT * 5)           
016514             ELSE                                                         
016515               MOVE WS-SULEVANT-TOT TO WS-STOCK-KURANS1                   
016516               COMPUTE WS-STOCK-KURANS2 = (WS-SULEVANT-TOT * 3) -         
016517                                           WS-SULEVANT-TOT                
016518               COMPUTE WS-STOCK-KURANS3 = (WS-SULEVANT-TOT * 5) -         
016519                                          (WS-SULEVANT-TOT * 3)           
016520               COMPUTE WS-STOCK-KURANS4 = (WS-SULEVANT-TOT * 10)          
016521                                          - (WS-SULEVANT-TOT * 5)         
016522               COMPUTE WS-STOCK-KURANS5 =  WS-STOCK-KURANS -              
016523                                          (WS-SULEVANT-TOT * 10)          
016524             END-IF                                                       
016525           END-IF                                                         
016526         END-IF                                                           
016527       END-IF                                                             
016528     END-IF                                                               
016529     .                                                                    
016530     EJECT                                                                
016531                                                                          
016532 S06-CREATE-FILE-W51284 SECTION.                                          
016533     MOVE IN-IDARTNR       TO UT-IDARTNR                                  
016534     MOVE IN-IDDC          TO UT-IDDC                                     
016535     MOVE IN-PRAVCOST      TO UT-PRAVCOST                                 
016536     MOVE WS-KDERS         TO UT-KDERS                                    
016537     MOVE WS-KDPSLLOC      TO UT-KDPSLLOC                                 
016538     MOVE WS-STOCK-KURANS  TO UT-KVLS-TOT                                 
016539     MOVE WS-SULEVANT-TOT  TO UT-SULEVANT-TOT                             
016542     IF WS-KURANS = +0                                                    
016543       MOVE 1              TO UT-KDKG                                     
016544     ELSE                                                                 
016545       MOVE WS-KURANS      TO UT-KDKG                                     
016546     END-IF                                                               
016547     MOVE WS-KVPB          TO UT-KVPB                                     
016548     MOVE WS-TIFINLV       TO UT-TIFINLV                                  
016549     MOVE WS-STOCK-KURANS1 TO UT-STOCK-KURANS1                            
016550     MOVE WS-STOCK-KURANS2 TO UT-STOCK-KURANS2                            
016551     MOVE WS-STOCK-KURANS3 TO UT-STOCK-KURANS3                            
016552     MOVE WS-STOCK-KURANS4 TO UT-STOCK-KURANS4                            
016553     MOVE WS-STOCK-KURANS5 TO UT-STOCK-KURANS5                            
016554     MOVE WS-STOCK-KURANS6 TO UT-STOCK-KURANS6                            
016555                                                                          
016556     WRITE UTPOST FROM UT-W51284                                          
016557     MOVE W51284-TRANSID TO POSTSUM-TRANSID                               
016558     CALL POSTSUM USING POSTSUM-PARM                                      
016559     .                                                                    
016560     EJECT                                                                
016561                                                                          
016562 S07-GIVE-KURANS-SHELFLIFE SECTION.                                       
016563     IF WS-KURANS = +0                                                    
016564       MOVE WS-STOCK-KURANS TO WS-STOCK-KURANS1                           
016565     END-IF                                                               
016566     .                                                                    
016567     EJECT                                                                
016570                                                                          
