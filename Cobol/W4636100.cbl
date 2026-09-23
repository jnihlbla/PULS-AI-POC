000100                                                                          
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W4636100.                                                
000500 AUTHOR.         BO SVENSSON.                                             
000600 DATE-WRITTEN.   00/11/13.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        KONTROLLERAR DIRECT BUSINESS RECORDS MED AVSEENDE PÅ             
001200*        SERIENUMMER I DE TIDIGARE MOTTAGNA FILERNA OCH DET               
001300*        FORMELLA INNEHÅLLET I POSTERNA.                                  
001400*        OM FEL MED SERIENUMMER ELLER ANNAT FATALT FEL PÅTRÄFFAS          
001500*        STOPPAS BEARBETNINGEN FÖR ÖVRIGA FEL SKICKS POSTER TILL          
001600*        FELFIL FÖR SENARE RETUR TILL AVSÄNDAREN.                         
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- KONKATINERAD OCH SORTAD DIRECT BUSINESS FIL                
003100     SELECT W46361                     ASSIGN TO W46361D1.                
003200     SKIP2                                                                
003300*          --- KONTROLLFIL MED SENAST MOTTAGNA SERIENUMMER                
003400     SELECT W46362I                    ASSIGN TO W46361D2.                
003500     SKIP2                                                                
003600                                                                          
003700*          --- UTFIL, KONTROLLFIL                                         
003800     SELECT W46362U                    ASSIGN TO W46361D3.                
003900     SKIP2                                                                
004000                                                                          
004100*          --- UTFIL, FELINDIKERADE FILER                                 
004200     SELECT W46364                     ASSIGN TO W46361D4.                
004300     SKIP2                                                                
004400                                                                          
004500*          --- UTFIL, FELINDIKERADE POSTER                                
004600     SELECT W46365                     ASSIGN TO W46361D5.                
004700     SKIP2                                                                
004800                                                                          
004900*          --- UTFIL, KORREKTA POSTER TILL SC                             
005000     SELECT W46366                     ASSIGN TO W46361D6.                
005100     EJECT                                                                
005200 DATA DIVISION.                                                           
005300     SKIP3                                                                
005400 FILE SECTION.                                                            
005500     SKIP3                                                                
005600 FD  W46361                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900                                                                          
006000 01  IN-POST         PIC X(190).                                          
006100                                                                          
006200     SKIP3                                                                
006300 FD  W46362I                                                              
006400     RECORDING       F                                                    
006500     BLOCK CONTAINS  0.                                                   
006600                                                                          
006700*01  POST -COPY W46362 -PRE  INSER-  -L.                                  
006800     EJECT                                                                
006900                                                                          
007000 FD  W46362U                                                              
007100     RECORDING       F                                                    
007200     BLOCK CONTAINS  0.                                                   
007300                                                                          
007400*01  POST -COPY W46362 -PRE  UTSER-  -L.                                  
007500     EJECT                                                                
007600                                                                          
007700 FD  W46364                                                               
007800     RECORDING       F                                                    
007900     BLOCK CONTAINS  0.                                                   
008000                                                                          
008100 01  UTFF-POST         PIC X(190).                                        
008200     EJECT                                                                
008300                                                                          
008400 FD  W46365                                                               
008500     RECORDING       F                                                    
008600     BLOCK CONTAINS  0.                                                   
008700                                                                          
008800 01  UTFP-POST         PIC X(190).                                        
008900     EJECT                                                                
009000                                                                          
009100 FD  W46366                                                               
009200     RECORDING       F                                                    
009300     BLOCK CONTAINS  0.                                                   
009400                                                                          
009500 01  UTOK-POST         PIC X(196).                                        
009600     EJECT                                                                
009700 WORKING-STORAGE SECTION.                                                 
009800                                                                          
009900*    -- CHECKED BY WY2000                                                 
010000 77  IDPGM                       PIC X(8)    VALUE 'W4636100'.            
010100 77  JA                          PIC X       VALUE 'J'.                   
010200 77  NEJ                         PIC X       VALUE 'N'.                   
010300     SKIP2                                                                
010400 01  FELTEXT.                                                             
010500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010700                                                                          
010800 77  W46361-EOF-SW               PIC X       VALUE 'N'.                   
010900     88  END-OF-W46361                       VALUE 'J'.                   
011000                                                                          
011100 77  W46364-OUT-SW               PIC X       VALUE 'N'.                   
011200     88  W46364-OUT-YES                      VALUE 'J'.                   
011300     EJECT                                                                
011400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011500                                                                          
011600     SKIP2                                                                
011700 01  DATUM-YYYYMMDD              PIC 9(8).                                
011800 01  DATUM-SSYYMMDD REDEFINES DATUM-YYYYMMDD.                             
011900     03  DATUM-SS                PIC 9(2).                                
012000     03  DATUM-YYMMDD            PIC 9(6).                                
012100     SKIP2                                                                
012200 01  W-W463PROD-TEST.                                                     
012300     03  W-PROD-PROG             PIC X(4).                                
012400     03  W-PROD-PRODTYPE         PIC X(1).                                
012500     SKIP2                                                                
012600 01  W-SUPL-SOK.                                                          
012700     03  W-SUPL-PROG             PIC X(4).                                
012800     03  W-SUPL-LAND             PIC X(3).                                
012900     03  W-SUPL-SUP              PIC X(2).                                
013000     EJECT                                                                
013100 01  WS-POSTTAB-START            PIC X(24)   VALUE                        
013200                                 'WS-POSTTAB-START  '.                    
013300                                                                          
013400 01  WS-POSTTAB-IX               PIC S9(3)   VALUE ZERO.                  
013500 01  WS-POSTTAB-ANT              PIC S9(3)   VALUE ZERO.                  
013600 01  WS-POSTTAB-MAX              PIC S9(3)   VALUE 10.                    
013700 01  WS-FELFLAGGA                PIC X(1)    VALUE 'N'.                   
013800 01  WS-POSTTAB.                                                          
013900     03  WS-POSTTAB-INGANG OCCURS 30.                                     
014000       05  WS-POST.                                                       
014100         07  WS-S-POST.                                                   
014200           09  WS-S-1-21           PIC X(21).                             
014300           09  WS-FELKOD           PIC 9(3).                              
014400           09  WS-S-25-30          PIC X(6).                              
014500         07  X0-POST               PIC X(160).                            
014600*        07  -COPY WINVBBB0       -RED X0-POST                            
014700*        07  -COPY WINVBBC0       -RED X0-POST                            
014800*        07  -COPY WINVBBD0       -RED X0-POST                            
014900*        07  -COPY WINVBBP0       -RED X0-POST                            
015000                                                                          
015100     EJECT                                                                
015200 01  DYNAMISKA-SUBPROGRAM.                                                
015300*                                                                         
015400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
015500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
015600     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
015700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
015800     SKIP2                                                                
015900*    --- PARAMETRAR TILL ABEND                                            
016000                                                                          
016100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
016200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
016300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
016400     EJECT                                                                
016500 01  FILLER                     PIC X(10)   VALUE 'WDATAREA'.             
016600*01 -COPY WDATAREA                                                        
016700     EJECT                                                                
016800 01  FILLER                     PIC X(10)   VALUE 'WDECAREA'.             
016900*01 -COPY WDECAREA                                                        
017000     EJECT                                                                
017100*    --- PARAMETRAR TILL POSTSUM                                          
017200*                                                                         
017300*01  -COPY W0005   -PRE  POSTSUM-                                         
017400     EJECT                                                                
017500 01  SPAR-AREA-START             PIC X(24)   VALUE                        
017600                                 'SPAR-AREA-START  '.                     
017700     SKIP2                                                                
017800 01  SPAR-AREA.                                                           
017900   03  W-SPAR-S-SERIALNO         PIC 9(7)  VALUE ZERO.                    
018000   03  W-SPAR-S-DATUM            PIC 9(6)  VALUE ZERO.                    
018100   03  W-SPAR-S-TID              PIC 9(8)  VALUE ZERO.                    
018200   03  W-SPAR-S-FELKOD           PIC 9(3)  VALUE ZERO.                    
018300   03  W-SPAR-ID-DELNOTE.                                                 
018400     05  W-SPAR-1-19             PIC X(19) VALUE SPACE.                   
018500     05  W-SPAR-RECTYPE          PIC X(1)  VALUE SPACE.                   
018600     05  W-SPAR-21-54            PIC X(34) VALUE SPACE.                   
018700   03  W-SPAR-LINETYPE           PIC X(1)  VALUE SPACE.                   
018800     EJECT                                                                
018900 01  IN-AREA-START               PIC X(24)   VALUE                        
019000                                 'IN-AREA-START  '.                       
019100     SKIP2                                                                
019200 01  IN-AREA.                                                             
019300     03  IN-S-DEL.                                                        
019400       05 IN-S-SERIALNO        PIC 9(7).                                  
019500       05 IN-S-DATUM           PIC 9(6).                                  
019600       05 IN-S-TID             PIC 9(8).                                  
019700       05 IN-S-FELKOD          PIC 9(3).                                  
019800       05 IN-S-NUMMER          PIC 9(6).                                  
019900     03  IN-POST               PIC X(160).                                
020000     03  IN-POST-JMF           REDEFINES IN-POST.                         
020100       05  IN-JMF-ID-DELNOTE.                                             
020200         07 IN-JMF-1-19        PIC X(19).                                 
020300         07 IN-JMF-RTYP        PIC X(1).                                  
020400         07 IN-JMF-21-54       PIC X(34).                                 
020500       05  IN-JMF-LINETYPE     PIC X(1).                                  
020600       05  FILLER              PIC X(105).                                
020700     EJECT                                                                
020800*    --- FÖR KONTROLL AV PRODUKTSLAG                                      
020900*                                                                         
021000*01  -COPY W463PROD                                                       
021100     EJECT                                                                
021200*    --- FÖR KONTROLL AV SUPPLIER                                         
021300*                                                                         
021400*01  -COPY W463SUP                                                        
021500     EJECT                                                                
021600*    --- TABELL FÖR SUPPLIERKOD                                           
021700*                                                                         
021800*01  -COPY W463SUPL                                                       
021900     EJECT                                                                
022000*    --- TABELL FÖR LANDKOD                                               
022100*                                                                         
022200*01  -COPY W463LAND                                                       
022300     EJECT                                                                
022400 01  UTFF-AREA-START             PIC X(24)   VALUE                        
022500                                 'UTFF-AREA-START  '.                     
022600     SKIP2                                                                
022700                                                                          
022800 01  UTFF-AREA                   PIC X(190).                              
022900                                                                          
023000     EJECT                                                                
023100 01  UTFP-AREA-START             PIC X(24)   VALUE                        
023200                                 'UTFP-AREA-START  '.                     
023300     SKIP2                                                                
023400                                                                          
023500 01  UTFP-AREA.                                                           
023600     03 UTFP-S-DEL.                                                       
023700       05  UTFP-1-21             PIC X(21).                               
023800       05  UTFP-FELKOD           PIC 9(3).                                
023900       05  UTFP-25-30            PIC X(6).                                
024000     03 UTFP-URSP-POST           PIC X(160).                              
024100                                                                          
024200     EJECT                                                                
024300 01  UTOK-AREA-START             PIC X(24)   VALUE                        
024400                                 'UTOK-AREA-START  '.                     
024500     SKIP2                                                                
024600                                                                          
024700 01  UTOK-AREA.                                                           
024800     03 UTOK-S-DEL               PIC X(30).                               
024900     03 UTOK-URSP-POST           PIC X(160).                              
025000     03 UTOK-PRODTYP             PIC X(1).                                
025100     03 UTOK-BASEDISC            PIC X(5).                                
025200     EJECT                                                                
025300 01  INUT-AREA-START             PIC X(24)   VALUE                        
025400                                 'INUT-AREA-START  '.                     
025500     SKIP2                                                                
025600                                                                          
025700*01  -COPY W46362  -PRE INUT-                                             
025800                                                                          
025900     EJECT                                                                
026000 PROCEDURE DIVISION.                                                      
026100 MAIN SECTION.                                                            
026200     SKIP2                                                                
026300                                                                          
026400     PERFORM A-INIT                                                       
026500     PERFORM S01-LAES-W46361                                              
026600     IF NOT END-OF-W46361                                                 
026700                                                                          
026800       PERFORM UNTIL END-OF-W46361                                        
026900                                                                          
027000*--------------------------------- NYTT SERIENR-DATTID                    
027100         MOVE IN-S-SERIALNO TO W-SPAR-S-SERIALNO                          
027200         MOVE IN-S-DATUM    TO W-SPAR-S-DATUM                             
027300         MOVE IN-S-TID      TO W-SPAR-S-TID                               
027400         MOVE IN-S-FELKOD   TO W-SPAR-S-FELKOD                            
027500         PERFORM B-FELFIL-UT                                              
027600         PERFORM UNTIL END-OF-W46361                                      
027700          OR IN-S-SERIALNO NOT = W-SPAR-S-SERIALNO                        
027800          OR IN-S-DATUM    NOT = W-SPAR-S-DATUM                           
027900          OR IN-S-TID      NOT = W-SPAR-S-TID                             
028000                                                                          
028100*--------------------------------- LÄS FÖRBI HEADER                       
028200           IF IN-JMF-RTYP = 'A'                                           
028300             PERFORM S01-LAES-W46361                                      
028400           END-IF                                                         
028500                                                                          
028600*--------------------------------- NYTT ID-DELNOTE                        
028700           MOVE IN-JMF-ID-DELNOTE TO W-SPAR-ID-DELNOTE                    
028800           PERFORM UNTIL END-OF-W46361                                    
028900            OR IN-S-SERIALNO     NOT = W-SPAR-S-SERIALNO                  
029000            OR IN-S-DATUM        NOT = W-SPAR-S-DATUM                     
029100            OR IN-S-TID          NOT = W-SPAR-S-TID                       
029200            OR IN-JMF-ID-DELNOTE NOT = W-SPAR-ID-DELNOTE                  
029300                                                                          
029400*--------------------------------- NY RADGRUPP                            
029500             MOVE LOW-VALUE       TO W-SPAR-LINETYPE                      
029600             PERFORM UNTIL END-OF-W46361                                  
029700              OR IN-S-SERIALNO     NOT = W-SPAR-S-SERIALNO                
029800              OR IN-S-DATUM        NOT = W-SPAR-S-DATUM                   
029900              OR IN-S-TID          NOT = W-SPAR-S-TID                     
030000              OR IN-JMF-ID-DELNOTE NOT = W-SPAR-ID-DELNOTE                
030100              OR IN-JMF-LINETYPE   <     W-SPAR-LINETYPE                  
030200              OR (IN-JMF-LINETYPE   = '2'                                 
030300              AND W-SPAR-LINETYPE   = '2')                                
030400               PERFORM C-SPARA-I-TABELL                                   
030500               MOVE IN-JMF-LINETYPE TO W-SPAR-LINETYPE                    
030600               PERFORM S01-LAES-W46361                                    
030700             END-PERFORM                                                  
030800             PERFORM D-KONTROLLERA-TABELL                                 
030900             PERFORM E-TOEM-TABELL                                        
031000             PERFORM S21-INIT-TABELL                                      
031100                                                                          
031200*--------------------------------- LÄS FÖRBI TRAILER                      
031300             IF IN-JMF-RTYP = 'Z'                                         
031400               PERFORM S01-LAES-W46361                                    
031500             END-IF                                                       
031600           END-PERFORM                                                    
031700         END-PERFORM                                                      
031800       END-PERFORM                                                        
031900     END-IF                                                               
032000                                                                          
032100     PERFORM S14-SKRIV-W46362                                             
032200     PERFORM Z-FINIT                                                      
032300                                                                          
032400     IF W46364-OUT-YES                                                    
032500       MOVE +8   TO RETURN-CODE                                           
032600     ELSE                                                                 
032700       MOVE ZERO TO RETURN-CODE                                           
032800     END-IF                                                               
032900                                                                          
033000     GOBACK                                                               
033100     .                                                                    
033200     EJECT                                                                
033300 A-INIT SECTION.                                                          
033400                                                                          
033500     OPEN INPUT  W46361                                                   
033600                 W46362I                                                  
033700                                                                          
033800     OPEN OUTPUT W46362U                                                  
033900                 W46364                                                   
034000                 W46365                                                   
034100                 W46366                                                   
034200     SKIP2                                                                
034300     PERFORM S02-LAES-W46362                                              
034400     PERFORM S21-INIT-TABELL                                              
034500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
034600     .                                                                    
034700     EJECT                                                                
034800 B-FELFIL-UT SECTION.                                                     
034900                                                                          
035000     IF  IN-JMF-1-19(1:4) = 'VCON'                                        
035100       IF W-SPAR-S-FELKOD > 0                                             
035200                                                                          
035300         PERFORM UNTIL END-OF-W46361                                      
035400          OR IN-S-SERIALNO NOT = W-SPAR-S-SERIALNO                        
035500          OR IN-S-DATUM  NOT = W-SPAR-S-DATUM                             
035600          OR IN-S-TID    NOT = W-SPAR-S-TID                               
035700           MOVE IN-AREA     TO UTFF-AREA                                  
035800           PERFORM S11-SKRIV-W46364                                       
035900           PERFORM S01-LAES-W46361                                        
036000         END-PERFORM                                                      
036100       ELSE                                                               
036200         IF IN-S-SERIALNO = INUT-VCON-SERIALNO + 1                        
036300           ADD 1            TO INUT-VCON-SERIALNO                         
036400         ELSE                                                             
036500           PERFORM UNTIL END-OF-W46361                                    
036600            OR IN-S-SERIALNO NOT = W-SPAR-S-SERIALNO                      
036700            OR IN-S-DATUM  NOT = W-SPAR-S-DATUM                           
036800            OR IN-S-TID    NOT = W-SPAR-S-TID                             
036900             MOVE 201         TO IN-S-FELKOD                              
037000             MOVE IN-AREA     TO UTFF-AREA                                
037100             PERFORM S11-SKRIV-W46364                                     
037200             PERFORM S01-LAES-W46361                                      
037300           END-PERFORM                                                    
037400         END-IF                                                           
037500       END-IF                                                             
037600                                                                          
037700     ELSE                                                                 
037800                                                                          
037900       IF IN-JMF-1-19(1:4) = 'VSER'                                       
038000         IF W-SPAR-S-FELKOD > 0                                           
038100                                                                          
038200           PERFORM UNTIL END-OF-W46361                                    
038300            OR IN-S-SERIALNO NOT = W-SPAR-S-SERIALNO                      
038400            OR IN-S-DATUM NOT = W-SPAR-S-DATUM                            
038500            OR IN-S-TID  NOT = W-SPAR-S-TID                               
038600             MOVE IN-AREA   TO UTFF-AREA                                  
038700             PERFORM S11-SKRIV-W46364                                     
038800             PERFORM S01-LAES-W46361                                      
038900           END-PERFORM                                                    
039000         ELSE                                                             
039100           IF IN-S-SERIALNO = INUT-VSER-SERIALNO + 1                      
039200             ADD 1          TO INUT-VSER-SERIALNO                         
039300           ELSE                                                           
039400             PERFORM UNTIL END-OF-W46361                                  
039500              OR IN-S-SERIALNO NOT = W-SPAR-S-SERIALNO                    
039600              OR IN-S-DATUM NOT = W-SPAR-S-DATUM                          
039700              OR IN-S-TID  NOT = W-SPAR-S-TID                             
039800               MOVE 201       TO IN-S-FELKOD                              
039900               MOVE IN-AREA   TO UTFF-AREA                                
040000               PERFORM S11-SKRIV-W46364                                   
040100               PERFORM S01-LAES-W46361                                    
040200             END-PERFORM                                                  
040300           END-IF                                                         
040400         END-IF                                                           
040500       ELSE                                                               
040600        IF IN-JMF-1-19(1:4) = 'VMER'                                      
040700          IF W-SPAR-S-FELKOD > 0                                          
040800                                                                          
040900            PERFORM UNTIL END-OF-W46361                                   
041000             OR IN-S-SERIALNO NOT = W-SPAR-S-SERIALNO                     
041100             OR IN-S-DATUM NOT = W-SPAR-S-DATUM                           
041200             OR IN-S-TID  NOT = W-SPAR-S-TID                              
041300              MOVE IN-AREA   TO UTFF-AREA                                 
041400              PERFORM S11-SKRIV-W46364                                    
041500              PERFORM S01-LAES-W46361                                     
041600            END-PERFORM                                                   
041700          ELSE                                                            
041800            IF IN-S-SERIALNO = INUT-VMER-SERIALNO + 1                     
041900              ADD 1          TO INUT-VMER-SERIALNO                        
042000            ELSE                                                          
042100              PERFORM UNTIL END-OF-W46361                                 
042200               OR IN-S-SERIALNO NOT = W-SPAR-S-SERIALNO                   
042300               OR IN-S-DATUM NOT = W-SPAR-S-DATUM                         
042400               OR IN-S-TID  NOT = W-SPAR-S-TID                            
042500                MOVE 201       TO IN-S-FELKOD                             
042600                MOVE IN-AREA   TO UTFF-AREA                               
042700                PERFORM S11-SKRIV-W46364                                  
042800                PERFORM S01-LAES-W46361                                   
042900              END-PERFORM                                                 
043000            END-IF                                                        
043100          END-IF                                                          
043200        ELSE                                                              
043300                                                                          
043400         IF W-SPAR-S-FELKOD > 0                                           
043500                                                                          
043600           PERFORM UNTIL END-OF-W46361                                    
043700            OR IN-S-SERIALNO NOT = W-SPAR-S-SERIALNO                      
043800            OR IN-S-DATUM NOT = W-SPAR-S-DATUM                            
043900            OR IN-S-TID  NOT = W-SPAR-S-TID                               
044000             MOVE IN-AREA   TO UTFF-AREA                                  
044100             PERFORM S11-SKRIV-W46364                                     
044200             PERFORM S01-LAES-W46361                                      
044300           END-PERFORM                                                    
044400         ELSE                                                             
044500           IF IN-S-SERIALNO = INUT-VTYR-SERIALNO + 1                      
044600             ADD 1          TO INUT-VTYR-SERIALNO                         
044700           ELSE                                                           
044800             PERFORM UNTIL END-OF-W46361                                  
044900              OR IN-S-SERIALNO NOT = W-SPAR-S-SERIALNO                    
045000              OR IN-S-DATUM NOT = W-SPAR-S-DATUM                          
045100              OR IN-S-TID  NOT = W-SPAR-S-TID                             
045200               MOVE 201       TO IN-S-FELKOD                              
045300               MOVE IN-AREA   TO UTFF-AREA                                
045400               PERFORM S11-SKRIV-W46364                                   
045500               PERFORM S01-LAES-W46361                                    
045600             END-PERFORM                                                  
045700           END-IF                                                         
045800         END-IF                                                           
045900        END-IF                                                            
046000       END-IF                                                             
046100     END-IF                                                               
046200     .                                                                    
046300 C-SPARA-I-TABELL SECTION.                                                
046400                                                                          
046500     ADD +1 TO WS-POSTTAB-ANT                                             
046600                                                                          
046700     IF WS-POSTTAB-ANT > WS-POSTTAB-MAX                                   
046800        MOVE 'FÖR MÅNGA POSTER' TO FELTEXT-STR                            
046900        DISPLAY FELTEXT                                                   
047000        PERFORM S99-ABEND                                                 
047100     END-IF                                                               
047200                                                                          
047300     MOVE IN-AREA TO WS-POST(WS-POSTTAB-ANT)                              
047400     .                                                                    
047500     EJECT                                                                
047600 D-KONTROLLERA-TABELL SECTION.                                            
047700                                                                          
047800     PERFORM DA-GENERELLA                                                 
047900                                                                          
048000     IF WS-FELFLAGGA = NEJ                                                
048100       MOVE +1  TO WS-POSTTAB-IX                                          
048200       PERFORM UNTIL WS-POSTTAB-IX > WS-POSTTAB-ANT                       
048300         PERFORM DB-LINETYPE                                              
048400         ADD +1  TO WS-POSTTAB-IX                                         
048500       END-PERFORM                                                        
048600     END-IF                                                               
048700     .                                                                    
048800     EJECT                                                                
048900 DA-GENERELLA SECTION.                                                    
049000                                                                          
049100     IF   B0-RECORDTYPE(1) NOT = 'B'                                      
049200     AND  B0-RECORDTYPE(1) NOT = 'C'                                      
049300     AND  B0-RECORDTYPE(1) NOT = 'D'                                      
049400     AND  B0-RECORDTYPE(1) NOT = 'P'                                      
049500       MOVE 406      TO WS-FELKOD(1)                                      
049600       MOVE JA       TO WS-FELFLAGGA                                      
049700     END-IF                                                               
049800                                                                          
049900     IF WS-FELFLAGGA = NEJ                                                
050000       SEARCH ALL LAND-X2-ING                                             
050100          AT END                                                          
050200             MOVE 407  TO WS-FELKOD(1)                                    
050300             MOVE JA   TO WS-FELFLAGGA                                    
050400          WHEN LAND-SOK(LAND-IX) = B0-BILL-LOC(1)                         
050500             CONTINUE                                                     
050600       END-SEARCH                                                         
050700     END-IF                                                               
050800                                                                          
050900     IF  WS-FELFLAGGA  = NEJ                                              
051000     AND (B0-RECORDTYPE(1) = 'B'                                          
051100      OR  B0-RECORDTYPE(1) = 'C'                                          
051200      OR  B0-RECORDTYPE(1) = 'P')                                         
051300     AND B0-DEL-NOTENO(1) = SPACE                                         
051400       MOVE 412    TO WS-FELKOD(1)                                        
051500       MOVE JA     TO WS-FELFLAGGA                                        
051600     END-IF                                                               
051700                                                                          
051800     MOVE B0-SENDLOC(1)  TO W463SUP-KOD                                   
051900     IF  WS-FELFLAGGA  = NEJ                                              
052000     AND B0-PROGRAM(1) = 'VTYR'                                           
052100     AND NOT W463SUP-VTYR-OK                                              
052200     OR  WS-FELFLAGGA  = NEJ                                              
052300     AND B0-PROGRAM(1) = 'VCON'                                           
052400     AND NOT W463SUP-VCON-OK                                              
052500     OR  WS-FELFLAGGA  = NEJ                                              
052600     AND B0-PROGRAM(1) = 'VSER'                                           
052700     AND NOT W463SUP-VSER-OK                                              
052800     OR  WS-FELFLAGGA  = NEJ                                              
052900     AND B0-PROGRAM(1) = 'VMER'                                           
053000     AND NOT W463SUP-VMER-OK                                              
053100       MOVE 403    TO WS-FELKOD(1)                                        
053200       MOVE JA     TO WS-FELFLAGGA                                        
053300     END-IF                                                               
053400                                                                          
053500     IF  WS-FELFLAGGA  = NEJ                                              
053600     AND WS-POSTTAB-ANT < 2                                               
053800       MOVE 900    TO WS-FELKOD(1)                                        
053900       MOVE JA     TO WS-FELFLAGGA                                        
054000     END-IF                                                               
054100                                                                          
054200     .                                                                    
054300     EJECT                                                                
054400 DB-LINETYPE SECTION.                                                     
054500                                                                          
054600     IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                                
054700     AND (B0-LINETYPE(WS-POSTTAB-IX)   NOT NUMERIC                        
054800      OR   B0-LINETYPE(WS-POSTTAB-IX)   NOT = 2                           
054900      AND  B0-LINETYPE(WS-POSTTAB-IX)   NOT = 3)                          
055000       MOVE 413      TO WS-FELKOD(WS-POSTTAB-IX)                          
055100       MOVE JA       TO WS-FELFLAGGA                                      
055200     END-IF                                                               
055300                                                                          
055400     IF  WS-FELKOD(WS-POSTTAB-IX)   = 0                                   
055500     AND WS-POSTTAB-IX              = 1                                   
055600     AND B0-LINETYPE(WS-POSTTAB-IX) NOT = 2                               
055700       MOVE 413      TO WS-FELKOD(WS-POSTTAB-IX)                          
055800       MOVE JA       TO WS-FELFLAGGA                                      
055900     END-IF                                                               
056000                                                                          
056100*---------------------------- KONTROLLER LINETYP 2                        
056200     IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                                
056300     AND  B0-LINETYPE(WS-POSTTAB-IX)   = 2                                
056400      IF B0-RECORDTYPE(WS-POSTTAB-IX) = 'B'                               
056500       IF B0-PARTNO(WS-POSTTAB-IX)     = SPACE                            
056600         MOVE 418  TO WS-FELKOD(WS-POSTTAB-IX)                            
056700         MOVE JA   TO WS-FELFLAGGA                                        
056800       END-IF                                                             
056900       IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                              
057000       AND  B0-UNIT-PRICE(WS-POSTTAB-IX) NOT NUMERIC                      
057100*15/12  OR  B0-UNIT-PRICE(WS-POSTTAB-IX) NOT > 0)                         
057200         MOVE 419  TO WS-FELKOD(WS-POSTTAB-IX)                            
057300         MOVE JA   TO WS-FELFLAGGA                                        
057400       END-IF                                                             
057500       IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                              
057600       AND (B0-QUANTITY(WS-POSTTAB-IX)   NOT NUMERIC                      
057700        OR  B0-QUANTITY(WS-POSTTAB-IX)   NOT > 0)                         
057800         MOVE 420  TO WS-FELKOD(WS-POSTTAB-IX)                            
057900         MOVE JA   TO WS-FELFLAGGA                                        
058000       END-IF                                                             
058100       IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                              
058200       AND B0-LINE-VALUE(WS-POSTTAB-IX) NOT NUMERIC                       
058300         MOVE 421  TO WS-FELKOD(WS-POSTTAB-IX)                            
058400         MOVE JA   TO WS-FELFLAGGA                                        
058500       END-IF                                                             
058600       IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                              
058700       AND (B0-NOCHARGE(WS-POSTTAB-IX)    NOT = 'Y'                       
058800       AND  B0-NOCHARGE(WS-POSTTAB-IX)    NOT = 'N')                      
058900         MOVE 423  TO WS-FELKOD(WS-POSTTAB-IX)                            
059000         MOVE JA   TO WS-FELFLAGGA                                        
059100       END-IF                                                             
059200       IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                              
059300       AND B0-NOCHARGE(WS-POSTTAB-IX)    = 'Y'                            
059400       AND B0-LINE-VALUE(WS-POSTTAB-IX)  NOT = 0                          
059500         MOVE 421  TO WS-FELKOD(WS-POSTTAB-IX)                            
059600         MOVE JA   TO WS-FELFLAGGA                                        
059700       END-IF                                                             
059800       IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                              
059900       AND B0-NOCHARGE(WS-POSTTAB-IX)    = 'N'                            
060000       AND B0-UNIT-PRICE(WS-POSTTAB-IX)  = 0                              
060100         MOVE 419  TO WS-FELKOD(WS-POSTTAB-IX)                            
060200         MOVE JA   TO WS-FELFLAGGA                                        
060300       END-IF                                                             
060400       IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                              
060500       AND B0-NOCHARGE(WS-POSTTAB-IX)    = 'N'                            
060600       AND B0-LINE-VALUE(WS-POSTTAB-IX)  = 0                              
060700         MOVE 421  TO WS-FELKOD(WS-POSTTAB-IX)                            
060800         MOVE JA   TO WS-FELFLAGGA                                        
060900       END-IF                                                             
061000       IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                              
061100        PERFORM DBA-KOLLA-DATUM                                           
061200       END-IF                                                             
061300      ELSE                                                                
061400       IF B0-RECORDTYPE(WS-POSTTAB-IX)  = 'C'                             
061500        IF B0-PARTNO(WS-POSTTAB-IX)     = SPACE                           
061600          MOVE 418 TO WS-FELKOD(WS-POSTTAB-IX)                            
061700          MOVE JA TO WS-FELFLAGGA                                         
061800        END-IF                                                            
061900        IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                             
062000        AND  B0-UNIT-PRICE(WS-POSTTAB-IX) NOT NUMERIC                     
062100*15/01BS OR  B0-UNIT-PRICE(WS-POSTTAB-IX) NOT > 0)                        
062200          MOVE 419 TO WS-FELKOD(WS-POSTTAB-IX)                            
062300          MOVE JA TO WS-FELFLAGGA                                         
062400        END-IF                                                            
062500        IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                             
062600        AND (B0-QUANTITY(WS-POSTTAB-IX)   NOT NUMERIC                     
062700         OR  B0-QUANTITY(WS-POSTTAB-IX)   NOT > 0)                        
062800          MOVE 420 TO WS-FELKOD(WS-POSTTAB-IX)                            
062900          MOVE JA TO WS-FELFLAGGA                                         
063000        END-IF                                                            
063100        IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                             
063200        AND B0-LINE-VALUE(WS-POSTTAB-IX) NOT NUMERIC                      
063300          MOVE 421 TO WS-FELKOD(WS-POSTTAB-IX)                            
063400          MOVE JA TO WS-FELFLAGGA                                         
063500        END-IF                                                            
063600        IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                             
063700        AND (B0-NOCHARGE(WS-POSTTAB-IX)    NOT = 'Y'                      
063800        AND  B0-NOCHARGE(WS-POSTTAB-IX)    NOT = 'N')                     
063900          MOVE 423  TO WS-FELKOD(WS-POSTTAB-IX)                           
064000          MOVE JA   TO WS-FELFLAGGA                                       
064100        END-IF                                                            
064200        IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                             
064300        AND B0-NOCHARGE(WS-POSTTAB-IX)    = 'Y'                           
064400        AND B0-LINE-VALUE(WS-POSTTAB-IX)  NOT = 0                         
064500          MOVE 421  TO WS-FELKOD(WS-POSTTAB-IX)                           
064600          MOVE JA   TO WS-FELFLAGGA                                       
064700        END-IF                                                            
064800        IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                             
064900        AND B0-NOCHARGE(WS-POSTTAB-IX)    = 'N'                           
065000        AND B0-UNIT-PRICE(WS-POSTTAB-IX)  = 0                             
065100          MOVE 419  TO WS-FELKOD(WS-POSTTAB-IX)                           
065200          MOVE JA   TO WS-FELFLAGGA                                       
065300        END-IF                                                            
065400        IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                             
065500        AND B0-NOCHARGE(WS-POSTTAB-IX)    = 'N'                           
065600        AND B0-LINE-VALUE(WS-POSTTAB-IX)  = 0                             
065700          MOVE 421  TO WS-FELKOD(WS-POSTTAB-IX)                           
065800          MOVE JA   TO WS-FELFLAGGA                                       
065900        END-IF                                                            
066000        IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                             
066100         PERFORM DBA-KOLLA-DATUM                                          
066200        END-IF                                                            
066300       ELSE                                                               
066400        IF B0-RECORDTYPE(WS-POSTTAB-IX)  = 'D'                            
066500         IF B0-UNIT-PRICE(WS-POSTTAB-IX) NOT NUMERIC                      
066600         OR B0-UNIT-PRICE(WS-POSTTAB-IX) NOT > 0                          
066700           MOVE 419 TO WS-FELKOD(WS-POSTTAB-IX)                           
066800           MOVE JA TO WS-FELFLAGGA                                        
066900         END-IF                                                           
067000         IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                            
067100         AND (B0-QUANTITY(WS-POSTTAB-IX)   NOT NUMERIC                    
067200          OR  B0-QUANTITY(WS-POSTTAB-IX)   NOT > 0)                       
067300           MOVE 420 TO WS-FELKOD(WS-POSTTAB-IX)                           
067400           MOVE JA TO WS-FELFLAGGA                                        
067500         END-IF                                                           
067600         IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                            
067700         AND (B0-LINE-VALUE(WS-POSTTAB-IX) NOT NUMERIC                    
067800          OR  B0-LINE-VALUE(WS-POSTTAB-IX) NOT > 0)                       
067900           MOVE 421 TO WS-FELKOD(WS-POSTTAB-IX)                           
068000           MOVE JA TO WS-FELFLAGGA                                        
068100         END-IF                                                           
068200         IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                            
068300         AND (D0-DEB-CRED(WS-POSTTAB-IX)    NOT = 'D'                     
068400         AND  D0-DEB-CRED(WS-POSTTAB-IX)    NOT = 'C')                    
068500           MOVE 423  TO WS-FELKOD(WS-POSTTAB-IX)                          
068600           MOVE JA   TO WS-FELFLAGGA                                      
068700         END-IF                                                           
068800         IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                            
068900          PERFORM DBA-KOLLA-DATUM                                         
069000         END-IF                                                           
069100        ELSE                                                              
069200         IF B0-RECORDTYPE(WS-POSTTAB-IX) = 'P'                            
069300          IF B0-PARTNO(WS-POSTTAB-IX)     = SPACE                         
069400            MOVE 418 TO WS-FELKOD(WS-POSTTAB-IX)                          
069500            MOVE JA TO WS-FELFLAGGA                                       
069600          END-IF                                                          
069700          IF B0-UNIT-PRICE(WS-POSTTAB-IX) NOT NUMERIC                     
069800          OR B0-UNIT-PRICE(WS-POSTTAB-IX) NOT > 0                         
069900            MOVE 419 TO WS-FELKOD(WS-POSTTAB-IX)                          
070000            MOVE JA TO WS-FELFLAGGA                                       
070100          END-IF                                                          
070200          IF WS-FELKOD(WS-POSTTAB-IX)      = 0                            
070300           PERFORM DBA-KOLLA-DATUM                                        
070400          END-IF                                                          
070500         END-IF                                                           
070600        END-IF                                                            
070700       END-IF                                                             
070800      END-IF                                                              
070900     END-IF                                                               
071000                                                                          
071100*---------------------------- KONTROLLER LINETYP 3                        
071200     IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                                
071300     AND  B0-LINETYPE(WS-POSTTAB-IX)   = 3                                
071400                                                                          
071500      IF WS-POSTTAB-IX NOT =                                              
071600         B0-TEXTCOUNT-2(WS-POSTTAB-IX) + 1                                
071700      AND WS-FELFLAGGA NOT = JA                                           
071800       MOVE 453        TO WS-FELKOD(WS-POSTTAB-IX)                        
071900       MOVE JA         TO WS-FELFLAGGA                                    
072000      ELSE                                                                
072100       IF WS-POSTTAB-IX = 2                                               
072200       AND WS-FELFLAGGA NOT = JA                                          
072300        MOVE B0-SUPPLIER(WS-POSTTAB-IX)                                   
072400                         TO W463SUP-KOD                                   
072500        IF  B0-PROGRAM(WS-POSTTAB-IX) = 'VTYR'                            
072600        AND NOT W463SUP-VTYR-OK                                           
072700        OR  B0-PROGRAM(WS-POSTTAB-IX) = 'VCON'                            
072800        AND NOT W463SUP-VCON-OK                                           
072900        OR  B0-PROGRAM(WS-POSTTAB-IX) = 'VSER'                            
073000        AND NOT W463SUP-VSER-OK                                           
073100        OR  B0-PROGRAM(WS-POSTTAB-IX) = 'VMER'                            
073200        AND NOT W463SUP-VMER-OK                                           
073300         MOVE 434        TO WS-FELKOD(WS-POSTTAB-IX)                      
073400         MOVE JA         TO WS-FELFLAGGA                                  
073500        ELSE                                                              
073600         MOVE B0-PROGRAM(WS-POSTTAB-IX)                                   
073700                        TO W-SUPL-PROG                                    
073800         MOVE B0-BILL-LOC(WS-POSTTAB-IX)                                  
073900                        TO W-SUPL-LAND                                    
074000         MOVE B0-SUPPLIER(WS-POSTTAB-IX)(1:2)                             
074100                        TO W-SUPL-SUP                                     
074200         SEARCH ALL SUPL-LEVNR-ING                                        
074300          AT END                                                          
074400             MOVE 434   TO WS-FELKOD(WS-POSTTAB-IX)                       
074500             MOVE JA    TO WS-FELFLAGGA                                   
074600          WHEN SUPL-SOK(SUPL-IX) = W-SUPL-SOK                             
074700             CONTINUE                                                     
074800         END-SEARCH                                                       
074900         IF B0-SUPPLIER(WS-POSTTAB-IX) NOT =                              
075000            B0-SENDLOC(WS-POSTTAB-IX)                                     
075100         AND WS-FELFLAGGA NOT = JA                                        
075200          MOVE 403       TO WS-FELKOD(WS-POSTTAB-IX)                      
075300          MOVE JA        TO WS-FELFLAGGA                                  
075400         END-IF                                                           
075500        END-IF                                                            
075600        MOVE B0-PROGRAM(WS-POSTTAB-IX)                                    
075700                         TO W-PROD-PROG                                   
075800        MOVE B0-PRODTYPE(WS-POSTTAB-IX)                                   
075900                         TO W-PROD-PRODTYPE                               
076000        MOVE W-W463PROD-TEST                                              
076100                         TO W463PROD-TEST                                 
076200        IF (B0-RECORDTYPE(WS-POSTTAB-IX) = 'B'                            
076300        OR  B0-RECORDTYPE(WS-POSTTAB-IX) = 'C')                           
076400        AND WS-FELFLAGGA = NEJ                                            
076500         IF NOT W463PROD-BC-OK                                            
076600          MOVE 451       TO WS-FELKOD(WS-POSTTAB-IX)                      
076700          MOVE JA        TO WS-FELFLAGGA                                  
076800         END-IF                                                           
076900        END-IF                                                            
077000        IF B0-RECORDTYPE(WS-POSTTAB-IX) = 'D'                             
077100        AND WS-FELFLAGGA = NEJ                                            
077200         IF NOT W463PROD-D-OK                                             
077300          MOVE 451       TO WS-FELKOD(WS-POSTTAB-IX)                      
077400          MOVE JA        TO WS-FELFLAGGA                                  
077500         END-IF                                                           
077600        END-IF                                                            
077700        IF B0-RECORDTYPE(WS-POSTTAB-IX) = 'P'                             
077800        AND WS-FELFLAGGA = NEJ                                            
077900         IF NOT W463PROD-P-OK                                             
078000          MOVE 451       TO WS-FELKOD(WS-POSTTAB-IX)                      
078100          MOVE JA        TO WS-FELFLAGGA                                  
078200         END-IF                                                           
078300        END-IF                                                            
078400        IF ((B0-RECORDTYPE(WS-POSTTAB-IX) = 'B'                           
078500        AND B0-NOCHARGE(WS-POSTTAB-IX - 1) = 'N')                         
078600        OR  B0-RECORDTYPE(WS-POSTTAB-IX) = 'P')                           
078700        AND WS-FELFLAGGA = NEJ                                            
078800         IF  W463PROD-KB                                                  
078900         AND (B0-LISTPRICE(WS-POSTTAB-IX) = '0,00      '                  
079000         OR   B0-LISTPRICE(WS-POSTTAB-IX) = '0000000,00')                 
079100          MOVE 448       TO WS-FELKOD(WS-POSTTAB-IX)                      
079200          MOVE JA        TO WS-FELFLAGGA                                  
079300         END-IF                                                           
079400        END-IF                                                            
079500        IF WS-FELFLAGGA = NEJ                                             
079600        AND B0-RECORDTYPE(WS-POSTTAB-IX) NOT = 'P'                        
079700        AND (B0-PRODTYPE(WS-POSTTAB-IX) = 'F'                             
079800        OR   B0-PRODTYPE(WS-POSTTAB-IX) = 'G'                             
079900        OR   B0-PRODTYPE(WS-POSTTAB-IX) = 'H')                            
080000        AND  B0-PRODQ-2(WS-POSTTAB-IX - 1) NOT NUMERIC                    
080100         MOVE 432        TO WS-FELKOD(WS-POSTTAB-IX)                      
080200         MOVE JA         TO WS-FELFLAGGA                                  
080300        END-IF                                                            
080400       END-IF                                                             
080500      END-IF                                                              
080600     END-IF                                                               
080700     .                                                                    
080800     EJECT                                                                
080900 DBA-KOLLA-DATUM SECTION.                                                 
081000                                                                          
081100*-------------------- KOLLA DATUM I DEBET TRANS                           
081200     IF B0-RECORDTYPE(WS-POSTTAB-IX)  = 'B'                               
081300      IF B0-DELNOTEDATE(WS-POSTTAB-IX) NOT NUMERIC                        
081400      OR B0-INVDUEDATE(WS-POSTTAB-IX) NOT NUMERIC                         
081500      OR B0-ORDER-DATE(WS-POSTTAB-IX) NOT NUMERIC                         
081600        IF B0-DELNOTEDATE(WS-POSTTAB-IX) NOT NUMERIC                      
081700           MOVE 425  TO WS-FELKOD(WS-POSTTAB-IX)                          
081800           MOVE JA   TO WS-FELFLAGGA                                      
081900        END-IF                                                            
082000        IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                             
082100        AND B0-INVDUEDATE(WS-POSTTAB-IX) NOT NUMERIC                      
082200           MOVE 429  TO WS-FELKOD(WS-POSTTAB-IX)                          
082300           MOVE JA   TO WS-FELFLAGGA                                      
082400        END-IF                                                            
082500        IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                             
082600        AND B0-ORDER-DATE(WS-POSTTAB-IX) NOT NUMERIC                      
082700           MOVE 430  TO WS-FELKOD(WS-POSTTAB-IX)                          
082800           MOVE JA   TO WS-FELFLAGGA                                      
082900        END-IF                                                            
083000      ELSE                                                                
083100       MOVE B0-DELNOTEDATE(WS-POSTTAB-IX)                                 
083200                          TO DATUM-YYYYMMDD                               
083300       MOVE DATUM-YYMMDD  TO DAT-I-TIDATUM                                
083400       MOVE "AAMMDD"      TO DAT-KDDATFORM                                
083500       CALL WDATKONV USING                                                
083600          DAT-KDDATFORM,                                                  
083700          DAT-I-TIDATUM,                                                  
083800          DAT-O-TIDATUM,                                                  
083900          DAT-KDSVAR                                                      
084000                                                                          
084100       IF  NOT DAT-KDSVAR-OK                                              
084200       OR  DATUM-SS NOT = 20                                              
084300        MOVE 425  TO WS-FELKOD(WS-POSTTAB-IX)                             
084400        MOVE JA   TO WS-FELFLAGGA                                         
084500       ELSE                                                               
084600        MOVE B0-ORDER-DATE(WS-POSTTAB-IX)                                 
084700                           TO DATUM-YYYYMMDD                              
084800        MOVE DATUM-YYMMDD  TO DAT-I-TIDATUM                               
084900        MOVE "AAMMDD"      TO DAT-KDDATFORM                               
085000        CALL WDATKONV USING                                               
085100           DAT-KDDATFORM,                                                 
085200           DAT-I-TIDATUM,                                                 
085300           DAT-O-TIDATUM,                                                 
085400           DAT-KDSVAR                                                     
085500                                                                          
085600        IF  NOT DAT-KDSVAR-OK                                             
085700        OR  DATUM-SS NOT = 20                                             
085800         MOVE 430  TO WS-FELKOD(WS-POSTTAB-IX)                            
085900         MOVE JA   TO WS-FELFLAGGA                                        
086000        ELSE                                                              
086100         MOVE B0-INVDUEDATE(WS-POSTTAB-IX)                                
086200                            TO DATUM-YYYYMMDD                             
086300         IF DATUM-YYYYMMDD > 0                                            
086400          MOVE DATUM-YYMMDD  TO DAT-I-TIDATUM                             
086500          MOVE "AAMMDD"      TO DAT-KDDATFORM                             
086600          CALL WDATKONV USING                                             
086700             DAT-KDDATFORM,                                               
086800             DAT-I-TIDATUM,                                               
086900             DAT-O-TIDATUM,                                               
087000             DAT-KDSVAR                                                   
087100                                                                          
087200          IF  NOT DAT-KDSVAR-OK                                           
087300          OR  DATUM-SS NOT = 20                                           
087400           MOVE 429  TO WS-FELKOD(WS-POSTTAB-IX)                          
087500           MOVE JA   TO WS-FELFLAGGA                                      
087600          END-IF                                                          
087700         END-IF                                                           
087800        END-IF                                                            
087900       END-IF                                                             
088000      END-IF                                                              
088100     END-IF                                                               
088200                                                                          
088300*-------------------- KOLLA DATUM I CREDIT TRANS                          
088400     IF C0-RECORDTYPE(WS-POSTTAB-IX)  = 'C'                               
088500      IF C0-DELNOTEDATE(WS-POSTTAB-IX) NOT NUMERIC                        
088600      OR C0-INVDUEDATE(WS-POSTTAB-IX) NOT NUMERIC                         
088700        IF C0-DELNOTEDATE(WS-POSTTAB-IX) NOT NUMERIC                      
088800           MOVE 425  TO WS-FELKOD(WS-POSTTAB-IX)                          
088900           MOVE JA   TO WS-FELFLAGGA                                      
089000        END-IF                                                            
089100        IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                             
089200        AND C0-INVDUEDATE(WS-POSTTAB-IX) NOT NUMERIC                      
089300           MOVE 429  TO WS-FELKOD(WS-POSTTAB-IX)                          
089400           MOVE JA   TO WS-FELFLAGGA                                      
089500        END-IF                                                            
089600      ELSE                                                                
089700       MOVE C0-DELNOTEDATE(WS-POSTTAB-IX)                                 
089800                          TO DATUM-YYYYMMDD                               
089900       MOVE DATUM-YYMMDD  TO DAT-I-TIDATUM                                
090000       MOVE "AAMMDD"      TO DAT-KDDATFORM                                
090100       CALL WDATKONV USING                                                
090200          DAT-KDDATFORM,                                                  
090300          DAT-I-TIDATUM,                                                  
090400          DAT-O-TIDATUM,                                                  
090500          DAT-KDSVAR                                                      
090600                                                                          
090700       IF  NOT DAT-KDSVAR-OK                                              
090800       OR (DATUM-SS NOT = 20                                              
090900       AND DATUM-SS NOT = 19)                                             
091000        MOVE 425  TO WS-FELKOD(WS-POSTTAB-IX)                             
091100        MOVE JA   TO WS-FELFLAGGA                                         
091200       ELSE                                                               
091300        MOVE C0-INVDUEDATE(WS-POSTTAB-IX)                                 
091400                           TO DATUM-YYYYMMDD                              
091500        IF DATUM-YYYYMMDD > 0                                             
091600         MOVE DATUM-YYMMDD  TO DAT-I-TIDATUM                              
091700         MOVE "AAMMDD"      TO DAT-KDDATFORM                              
091800         CALL WDATKONV USING                                              
091900            DAT-KDDATFORM,                                                
092000            DAT-I-TIDATUM,                                                
092100            DAT-O-TIDATUM,                                                
092200            DAT-KDSVAR                                                    
092300                                                                          
092400         IF  NOT DAT-KDSVAR-OK                                            
092500         OR  DATUM-SS NOT = 20                                            
092600          MOVE 429  TO WS-FELKOD(WS-POSTTAB-IX)                           
092700          MOVE JA   TO WS-FELFLAGGA                                       
092800         END-IF                                                           
092900        END-IF                                                            
093000       END-IF                                                             
093100      END-IF                                                              
093200     END-IF                                                               
093300                                                                          
093400*-------------------- KOLLA DATUM I ADJUST TRANS                          
093500     IF D0-RECORDTYPE(WS-POSTTAB-IX)  = 'D'                               
093600      IF D0-CREATEDATE(WS-POSTTAB-IX) NOT NUMERIC                         
093700        MOVE 425  TO WS-FELKOD(WS-POSTTAB-IX)                             
093800        MOVE JA   TO WS-FELFLAGGA                                         
093900      ELSE                                                                
094000       MOVE D0-CREATEDATE(WS-POSTTAB-IX)                                  
094100                          TO DATUM-YYYYMMDD                               
094200       MOVE DATUM-YYMMDD  TO DAT-I-TIDATUM                                
094300       MOVE "AAMMDD"      TO DAT-KDDATFORM                                
094400       CALL WDATKONV USING                                                
094500          DAT-KDDATFORM,                                                  
094600          DAT-I-TIDATUM,                                                  
094700          DAT-O-TIDATUM,                                                  
094800          DAT-KDSVAR                                                      
094900                                                                          
095000       IF  NOT DAT-KDSVAR-OK                                              
095100       OR  DATUM-SS NOT = 20                                              
095200        MOVE 425  TO WS-FELKOD(WS-POSTTAB-IX)                             
095300        MOVE JA   TO WS-FELFLAGGA                                         
095400       END-IF                                                             
095500      END-IF                                                              
095600     END-IF                                                               
095700                                                                          
095800*-------------------- KOLLA DATUM I PRICE  TRANS                          
095900     IF P0-RECORDTYPE(WS-POSTTAB-IX)  = 'P'                               
096000      IF P0-UPDATE-DATE(WS-POSTTAB-IX) NOT NUMERIC                        
096100      OR P0-VALIDITY-DATE(WS-POSTTAB-IX) NOT NUMERIC                      
096200        IF P0-UPDATE-DATE(WS-POSTTAB-IX) NOT NUMERIC                      
096300           MOVE 425  TO WS-FELKOD(WS-POSTTAB-IX)                          
096400           MOVE JA   TO WS-FELFLAGGA                                      
096500        END-IF                                                            
096600        IF  WS-FELKOD(WS-POSTTAB-IX)      = 0                             
096700        AND P0-VALIDITY-DATE(WS-POSTTAB-IX) NOT NUMERIC                   
096800           MOVE 429  TO WS-FELKOD(WS-POSTTAB-IX)                          
096900           MOVE JA   TO WS-FELFLAGGA                                      
097000        END-IF                                                            
097100      ELSE                                                                
097200       MOVE P0-UPDATE-DATE(WS-POSTTAB-IX)                                 
097300                          TO DATUM-YYYYMMDD                               
097400       MOVE DATUM-YYMMDD  TO DAT-I-TIDATUM                                
097500       MOVE "AAMMDD"      TO DAT-KDDATFORM                                
097600       CALL WDATKONV USING                                                
097700          DAT-KDDATFORM,                                                  
097800          DAT-I-TIDATUM,                                                  
097900          DAT-O-TIDATUM,                                                  
098000          DAT-KDSVAR                                                      
098100                                                                          
098200       IF  NOT DAT-KDSVAR-OK                                              
098300       OR  DATUM-SS NOT = 20                                              
098400        MOVE 425  TO WS-FELKOD(WS-POSTTAB-IX)                             
098500        MOVE JA   TO WS-FELFLAGGA                                         
098600       ELSE                                                               
098700        MOVE P0-VALIDITY-DATE(WS-POSTTAB-IX)                              
098800                           TO DATUM-YYYYMMDD                              
098900        IF DATUM-YYYYMMDD > 0                                             
099000         MOVE DATUM-YYMMDD  TO DAT-I-TIDATUM                              
099100         MOVE "AAMMDD"      TO DAT-KDDATFORM                              
099200         CALL WDATKONV USING                                              
099300            DAT-KDDATFORM,                                                
099400            DAT-I-TIDATUM,                                                
099500            DAT-O-TIDATUM,                                                
099600            DAT-KDSVAR                                                    
099700                                                                          
099800         IF  NOT DAT-KDSVAR-OK                                            
099900         OR  (DATUM-SS NOT = 20                                           
100000         AND  DATUM-SS NOT = 19)                                          
100100          MOVE 429  TO WS-FELKOD(WS-POSTTAB-IX)                           
100200          MOVE JA   TO WS-FELFLAGGA                                       
100300         END-IF                                                           
100400        END-IF                                                            
100500       END-IF                                                             
100600      END-IF                                                              
100700     END-IF                                                               
100800     .                                                                    
100900     EJECT                                                                
101000 E-TOEM-TABELL SECTION.                                                   
101100                                                                          
101200     IF WS-FELFLAGGA = JA                                                 
101300       MOVE +1              TO WS-POSTTAB-IX                              
101400       PERFORM UNTIL WS-POSTTAB-IX > WS-POSTTAB-ANT                       
101500         IF WS-FELKOD(WS-POSTTAB-IX) = 0                                  
101600           MOVE 900         TO WS-FELKOD(WS-POSTTAB-IX)                   
101700         END-IF                                                           
101800         MOVE WS-POST(WS-POSTTAB-IX) TO UTFP-AREA                         
101900         PERFORM S12-SKRIV-W46365                                         
102000         ADD +1             TO WS-POSTTAB-IX                              
102100       END-PERFORM                                                        
102200                                                                          
102300     ELSE                                                                 
102400       MOVE +1              TO WS-POSTTAB-IX                              
102500       PERFORM UNTIL WS-POSTTAB-IX > WS-POSTTAB-ANT                       
102600         MOVE WS-S-POST(WS-POSTTAB-IX)                                    
102700                            TO UTOK-S-DEL                                 
102800         MOVE X0-POST(WS-POSTTAB-IX)                                      
102900                            TO UTOK-URSP-POST                             
103000         IF B0-LINETYPE(WS-POSTTAB-IX) = 2                                
103100           MOVE B0-PRODTYPE(WS-POSTTAB-IX + 1)                            
103200                            TO UTOK-PRODTYP                               
103300           MOVE B0-DISC1   (WS-POSTTAB-IX + 1)                            
103400                            TO UTOK-BASEDISC                              
103500         ELSE                                                             
103600           MOVE SPACE       TO UTOK-PRODTYP                               
103700           MOVE SPACE       TO UTOK-BASEDISC                              
103800         END-IF                                                           
103900         PERFORM S13-SKRIV-W46366                                         
104000         ADD +1             TO WS-POSTTAB-IX                              
104100       END-PERFORM                                                        
104200     END-IF                                                               
104300     .                                                                    
104400     EJECT                                                                
104500 Z-FINIT SECTION.                                                         
104600                                                                          
104700     CLOSE W46361                                                         
104800           W46362I                                                        
104900           W46362U                                                        
105000           W46364                                                         
105100           W46365                                                         
105200           W46366                                                         
105300     SKIP2                                                                
105400     MOVE 'S' TO POSTSUM-OPKOD                                            
105500     CALL POSTSUM USING POSTSUM-PARM                                      
105600     .                                                                    
105700     EJECT                                                                
105800 S01-LAES-W46361  SECTION.                                                
105900                                                                          
106000     READ W46361 INTO IN-AREA                                             
106100     AT END                                                               
106200        MOVE HIGH-VALUE   TO IN-AREA                                      
106300        SET END-OF-W46361 TO TRUE                                         
106400                                                                          
106500     NOT AT END                                                           
106600        MOVE 'W46361'   TO POSTSUM-FDNAMN                                 
106700        MOVE 'W46361D1' TO POSTSUM-DDNAMN2                                
106800        MOVE SPACE      TO POSTSUM-TRANSTYP                               
106900        CALL POSTSUM USING POSTSUM-PARM                                   
107000     END-READ                                                             
107100     .                                                                    
107200     EJECT                                                                
107300 S02-LAES-W46362  SECTION.                                                
107400                                                                          
107500     READ W46362I INTO INUT-W46362                                        
107600     AT END                                                               
107700        MOVE 'SERIENUMMERPOST SAKNAS W46362' TO FELTEXT-STR               
107800        DISPLAY FELTEXT                                                   
107900        PERFORM S99-ABEND                                                 
108000                                                                          
108100     NOT AT END                                                           
108200        MOVE 'W46362I'   TO POSTSUM-FDNAMN                                
108300        MOVE 'W46361D2' TO POSTSUM-DDNAMN2                                
108400        MOVE SPACE      TO POSTSUM-TRANSTYP                               
108500        CALL POSTSUM USING POSTSUM-PARM                                   
108600     END-READ                                                             
108700     .                                                                    
108800     EJECT                                                                
108900 S11-SKRIV-W46364 SECTION.                                                
109000                                                                          
109100     WRITE UTFF-POST FROM UTFF-AREA                                       
109200                                                                          
109300     SET W46364-OUT-YES TO TRUE                                           
109400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
109500     MOVE 'W46364'   TO POSTSUM-FDNAMN                                    
109600     MOVE 'W46361D4' TO POSTSUM-DDNAMN2                                   
109700     CALL POSTSUM USING POSTSUM-PARM                                      
109800     .                                                                    
109900     EJECT                                                                
110000 S12-SKRIV-W46365 SECTION.                                                
110100                                                                          
110200     WRITE UTFP-POST FROM UTFP-AREA                                       
110300                                                                          
110400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
110500     MOVE 'W46365'   TO POSTSUM-FDNAMN                                    
110600     MOVE 'W46361D5' TO POSTSUM-DDNAMN2                                   
110700     CALL POSTSUM USING POSTSUM-PARM                                      
110800     .                                                                    
110900     EJECT                                                                
111000 S13-SKRIV-W46366 SECTION.                                                
111100                                                                          
111200     WRITE UTOK-POST FROM UTOK-AREA                                       
111300                                                                          
111400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
111500     MOVE 'W46366'   TO POSTSUM-FDNAMN                                    
111600     MOVE 'W46361D6' TO POSTSUM-DDNAMN2                                   
111700     CALL POSTSUM USING POSTSUM-PARM                                      
111800     .                                                                    
111900     EJECT                                                                
112000 S14-SKRIV-W46362 SECTION.                                                
112100                                                                          
112200     WRITE UTSER-POST FROM INUT-W46362                                    
112300                                                                          
112400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
112500     MOVE 'W46362U'   TO POSTSUM-FDNAMN                                   
112600     MOVE 'W46361D3' TO POSTSUM-DDNAMN2                                   
112700     CALL POSTSUM USING POSTSUM-PARM                                      
112800     .                                                                    
112900     EJECT                                                                
113000 S21-INIT-TABELL SECTION.                                                 
113100                                                                          
113200     MOVE +1                TO WS-POSTTAB-IX                              
113300     PERFORM UNTIL WS-POSTTAB-IX > WS-POSTTAB-MAX                         
113400       MOVE SPACE           TO WS-S-POST(WS-POSTTAB-IX)                   
113500       MOVE SPACE           TO WS-POST(WS-POSTTAB-IX)                     
113600       MOVE ZERO            TO WS-FELKOD(WS-POSTTAB-IX)                   
113700       ADD +1               TO WS-POSTTAB-IX                              
113800     END-PERFORM                                                          
113900                                                                          
114000     MOVE ZERO              TO WS-POSTTAB-IX                              
114100                               WS-POSTTAB-ANT                             
114200     MOVE NEJ               TO WS-FELFLAGGA                               
114300     .                                                                    
114400     EJECT                                                                
114500 S99-ABEND SECTION.                                                       
114600                                                                          
114700     SKIP2                                                                
114800     MOVE 'S' TO POSTSUM-OPKOD                                            
114900     CALL POSTSUM USING POSTSUM-PARM                                      
115000     CALL ABEND USING RKOD-ABEND-MED-DUMP                                 
115100     .                                                                    
