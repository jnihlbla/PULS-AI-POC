000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4612300.                                                 
000400*AUTHOR.        LENA BROMANDER                                            
000500*DATE-WRITTEN.  NOV  2018                                                 
000600                                                                          
000700*    REMARKS. KOPIERAT FRÅN W4616800                                      
000800*                                                                         
000900*    FUNKTION: *****************                                          
001000*                                                                         
001100*        IMPORTÖRSBEROENDE BEHANDLING (UNGERN)                            
001200*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001300*        SKAPAR START OCH SLUTKORT                                        
001400*        PT RIC SKALL EJ SKRIVAS                                          
001500*                                                                         
001600*        PT RIF SKALL ENDAST SISTA "RIF KORTET"                           
001700*        SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                       
001800*        (ENDAST ENGELSK TEXT).                                           
001900*    ABENDKODER:                                                          
002000*                                                                         
002010*                                                                         
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*- - - - - - - - - - - - INFIL:                                           
002900*                        - -  FIL TILL VIPS                               
003000     SELECT W46123                       ASSIGN TO UT-S-W46123D1.         
003100     SKIP2                                                                
003200*- - - - - - - - - - - - UTFIL:                                           
003300*                        - -  FIL TILL VIPS                               
003400     SELECT W46143                       ASSIGN TO UT-S-W46123D2.         
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W46123                                                               
004200     RECORDING      V                                                     
004300     BLOCK CONTAINS 0.                                                    
004400     SKIP2                                                                
004410 01  INPOST          PIC X(211).                                          
004420     SKIP2                                                                
011300 FD  W46143                                                               
011400     RECORDING      V                                                     
011500     BLOCK CONTAINS 0.                                                    
011600 01  UTPOST                       PIC X(80).                              
011610     SKIP2                                                                
011620 01  RID-POST  -COPY W461RIDN     -L.                                     
011630     SKIP2                                                                
011640 01  RIE-POST  -COPY W461RIEN     -L.                                     
011650     SKIP2                                                                
011660 01  RIH-POST  -COPY W461RIHN     -L.                                     
011670     SKIP2                                                                
011680 01  RIO-POST  -COPY W461RIO2     -L.                                     
011690     SKIP2                                                                
011691 01  RKB-POST  -COPY W461RKBN     -L.                                     
011692     SKIP2                                                                
011693 01  RKC-POST  -COPY W461RKCN     -L.                                     
011694     SKIP2                                                                
011695 01  RKD-POST  -COPY W461RKDN     -L.                                     
011700     SKIP2                                                                
012400 WORKING-STORAGE SECTION.                                                 
012500                                                                          
012600*    -- CHECKED BY WY2000                                                 
012700*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
012800 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4612300'.            
012900     SKIP2                                                                
013000*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
013100                                                                          
013200 77  JA                          PIC X(1)    VALUE 'J'.                   
013300 77  NEJ                         PIC X(1)    VALUE 'N'.                   
013400     SKIP2                                                                
013500*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
013600                                                                          
013700 77  W46123-EOF                  PIC X(1)    VALUE 'N'.                   
013800     SKIP2                                                                
013900*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
014000                                                                          
014100 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
014200 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +9000.          
014300 77  SPAR-IDDISTR                PIC S9(5)   COMP-3 VALUE +0.             
014400 77  SPAR-IDDISTR-RIB            PIC S9(5)   COMP-3 VALUE +0.             
014500 77  SPAR-IDDISTR-RIK            PIC S9(5)   COMP-3 VALUE +0.             
014600 77  SPAR-IDDISTR-RIX            PIC S9(5)   COMP-3 VALUE +0.             
014700                                                                          
014800*- - - - - - - - - - - - - -                                              
014900                                                                          
015000     EJECT                                                                
015010*      --- VALID IDDC CODES                                               
015020*                                                                         
015030*01    -COPY WWDCKONS                                                     
015040       EJECT                                                              
015050 01  TEST-IDDISTR                PIC 9(5) COMP-3.                         
015061       EJECT                                                              
015100 01  DAGENS-DATUM.                                                        
015200   03  DAGENS-DATUM-AR           PIC 9(2).                                
015300   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
015400   03  DAGENS-DATUM-DAG          PIC 9(2).                                
015500                                                                          
015600 01  DAGENS-TID.                                                          
015700   03  DAGENS-TID-TIM            PIC 9(2).                                
015800   03  DAGENS-TID-MIN            PIC 9(2).                                
015900   03  DAGENS-TID-SEK            PIC 9(2).                                
016000                                                                          
016100 01  DYNAMISKA-SUBPROGRAM.                                                
016200   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
016300   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
016400   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
016500     SKIP3                                                                
016600*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
016700                                                                          
016800 01  RETURKODER.                                                          
016900   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
017000   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
017100   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
017200     EJECT                                                                
017300*- - - - - - - - - - - - - -  ARBETSAREAOR                                
017400                                                                          
017500******************************************************************        
017600                                                                          
017700 01  W-ARBAREA.                                                           
017800 03  W-ARBAREA-X                 PIC X(211).                              
017900     SKIP2                                                                
017910*                                                                         
017920*03  FILLER  -COPY W461RIAN        -PRE W- -RED W-ARBAREA-X               
017930     EJECT                                                                
017940*03  FILLER  -COPY W461RIBN        -PRE W- -RED W-ARBAREA-X               
017950     EJECT                                                                
017960*03  FILLER  -COPY W461RICN        -PRE W- -RED W-ARBAREA-X               
017970     EJECT                                                                
017980*03  FILLER  -COPY W461RIDN        -PRE W- -RED W-ARBAREA-X               
017990     EJECT                                                                
017991*03  FILLER  -COPY W461RIEN        -PRE W- -RED W-ARBAREA-X               
017992     EJECT                                                                
017993*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
017994     EJECT                                                                
017995*03  FILLER  -COPY W461RIGN        -PRE W- -RED W-ARBAREA-X               
017996     EJECT                                                                
017997*03  FILLER  -COPY W461RIHN        -PRE W- -RED W-ARBAREA-X               
017998     EJECT                                                                
017999*03  FILLER  -COPY W461RIIN        -PRE W- -RED W-ARBAREA-X               
018000     EJECT                                                                
018010*03  FILLER  -COPY W461RIJN        -PRE W- -RED W-ARBAREA-X               
018020     EJECT                                                                
018030*03  FILLER  -COPY W461RIK1        -PRE W- -RED W-ARBAREA-X               
018040     EJECT                                                                
018050*03  FILLER  -COPY W461RILN        -PRE W- -RED W-ARBAREA-X               
018060     EJECT                                                                
018070*03  FILLER  -COPY W461RIMN        -PRE W- -RED W-ARBAREA-X               
018080     EJECT                                                                
018090*03  FILLER  -COPY W461RINN        -PRE W- -RED W-ARBAREA-X               
018091     EJECT                                                                
018092*03  FILLER  -COPY W461RIO2        -PRE W- -RED W-ARBAREA-X               
018093     EJECT                                                                
018094*03  FILLER  -COPY W461RIPN        -PRE W- -RED W-ARBAREA-X               
018095     EJECT                                                                
018096*03  FILLER  -COPY W461RIQN        -PRE W- -RED W-ARBAREA-X               
018097     EJECT                                                                
018098*03  FILLER  -COPY W461RIRN        -PRE W- -RED W-ARBAREA-X               
018099     EJECT                                                                
018100*03  FILLER  -COPY W461RISN        -PRE W- -RED W-ARBAREA-X               
018110     EJECT                                                                
018120*03  FILLER  -COPY W461RITN        -PRE W- -RED W-ARBAREA-X               
018130     EJECT                                                                
018140*03  FILLER  -COPY W461RIUN        -PRE W- -RED W-ARBAREA-X               
018150     EJECT                                                                
018160*03  FILLER  -COPY W461RIWN        -PRE W- -RED W-ARBAREA-X               
018170     EJECT                                                                
018180*03  FILLER  -COPY W461RIXN        -PRE W- -RED W-ARBAREA-X               
018190     EJECT                                                                
018191*03  FILLER  -COPY W461RIYN        -PRE W- -RED W-ARBAREA-X               
018192     EJECT                                                                
018193*03  FILLER  -COPY W461RIZN        -PRE W- -RED W-ARBAREA-X               
018194     EJECT                                                                
018195*03  FILLER  -COPY W461RKAN        -PRE W- -RED W-ARBAREA-X               
018196     EJECT                                                                
018197*03  FILLER  -COPY W461RKBN        -PRE W- -RED W-ARBAREA-X               
018198     EJECT                                                                
018199*03  FILLER  -COPY W461RKCN        -PRE W- -RED W-ARBAREA-X               
018200     EJECT                                                                
018210*03  FILLER  -COPY W461RKDN        -PRE W- -RED W-ARBAREA-X               
018220     EJECT                                                                
018230*03  FILLER  -COPY W461RKEN        -PRE W- -RED W-ARBAREA-X               
018240     EJECT                                                                
018250*03  FILLER  -COPY W461RKFN        -PRE W- -RED W-ARBAREA-X               
018260     EJECT                                                                
018270*03  FILLER  -COPY W461RKGN        -PRE W- -RED W-ARBAREA-X               
018280     EJECT                                                                
018290*03  FILLER  -COPY W461RKHN        -PRE W- -RED W-ARBAREA-X               
018291     EJECT                                                                
018292*03  FILLER  -COPY W461RKIN        -PRE W- -RED W-ARBAREA-X               
018293     EJECT                                                                
032400                                                                          
032500*01  -COPY W461RIFN        -PRE JFR-.                                     
032600     EJECT                                                                
032700*                             STARTKORT                                   
032800*01  -COPY W461RI0N                                                       
032900     EJECT                                                                
033000*                             SLUTKORT                                    
033100*01  -COPY W461RI9                                                        
033200     EJECT                                                                
033300*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
033400                                                                          
033500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
033600     SKIP2                                                                
033700*01  -COPY WDATKORT                                                       
033800     EJECT                                                                
033900*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
034000                                                                          
034100*01  -COPY W0005       -PRE  POSTSUM-.                                    
034200     EJECT                                                                
034300 PROCEDURE DIVISION.                                                      
034400     SKIP2                                                                
034500     PERFORM A-INIT                                                       
034600     PERFORM B-BEHANDLA                                                   
034700     PERFORM Z-FINIT                                                      
034800     MOVE ZERO TO RETURN-CODE                                             
034900     GOBACK                                                               
035000     .                                                                    
035100                                                                          
035200     SKIP3                                                                
035300 A-INIT SECTION.                                                          
035400     SKIP2                                                                
035500     OPEN INPUT W46123 OUTPUT W46143                                      
035600     SKIP2                                                                
035700     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
035800     SKIP2                                                                
035900*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
036000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
036100     MOVE D-AAR TO DAGENS-DATUM-AR                                        
036200     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
036300     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
036400     SKIP2                                                                
036500*- - - - - - - - - - - - - - - - TID                                      
036600*                                                                         
036700     ACCEPT   DAGENS-TID FROM TIME                                        
036800     .                                                                    
036900     EJECT                                                                
037000                                                                          
037100 B-BEHANDLA SECTION.                                                      
037200     SKIP2                                                                
037300     PERFORM BA-START-KORT                                                
037400     PERFORM S01-LAS-W46123                                               
037500     PERFORM UNTIL                                                        
037600      NOT ( W46123-EOF = NEJ )                                            
037700       IF W-RIF-IDPTYP = 'RKB' OR 'RKC' OR 'RKO' OR                       
037800                         'RKD' OR 'RKE' OR 'RKF'                          
040410                                                                          
040420         EVALUATE    W-RIF-IDPTYP                                         
040430                                                                          
040440           WHEN  'RKB'                                                    
040441                WRITE RKB-POST  FROM INPOST                               
040443                                                                          
040444           WHEN  'RKC'                                                    
040445                WRITE RKC-POST  FROM INPOST                               
040447                                                                          
040448           WHEN  'RKD'                                                    
040449                WRITE RKD-POST  FROM INPOST                               
040460                                                                          
040470           WHEN OTHER                                                     
040500                WRITE UTPOST   FROM INPOST                                
040601                                                                          
040602         END-EVALUATE                                                     
040610         ADD     +1           TO W-ANT-POSTER                             
040700         PERFORM S01-LAS-W46123                                           
040800       ELSE                                                               
040900         IF W-RIF-IDPTYP = 'RIF'                                          
041000           MOVE SPAR-IDDISTR-RIB TO TEST-IDDISTR                          
041100           MOVE W-ARBAREA TO JFR-RIF-W461RIFN-CTX                         
041300           PERFORM S01-LAS-W46123                                         
041400         ELSE                                                             
041500           IF JFR-RIF-IDPTYP = 'RIF'                                      
041510             MOVE SPAR-IDDISTR-RIB TO TEST-IDDISTR                        
041800             ADD +1           TO W-ANT-POSTER                             
041810             WRITE UTPOST  FROM JFR-RIF-W461RIFN-CTX                      
042100             MOVE SPACE TO JFR-RIF-W461RIFN-CTX                           
042300           END-IF                                                         
042400           IF W-RIF-IDPTYP = 'RIC'                                        
042500             PERFORM S01-LAS-W46123                                       
042600           ELSE                                                           
042700             EVALUATE  W-RIF-IDPTYP                                       
042800                                                                          
042900               WHEN 'RIA'                                                 
043100                    MOVE W-RIA-IDDISTR TO SPAR-IDDISTR                    
043200                                                                          
043300               WHEN 'RIB'                                                 
043500                    MOVE W-RIB-IDDISTR TO SPAR-IDDISTR-RIB                
043600                                                                          
043700               WHEN 'RIJ'                                                 
045700                    MOVE W-RIJ-IDDISTR TO SPAR-IDDISTR                    
045800                                                                          
045900               WHEN 'RIK'                                                 
046100                    MOVE W-RIK-IDDISTR TO SPAR-IDDISTR-RIK                
047700                                                                          
047800               WHEN 'RIQ'                                                 
048000                    MOVE W-RIQ-IDDISTR TO SPAR-IDDISTR                    
048100                                                                          
048200               WHEN 'RIR'                                                 
048400                    MOVE W-RIR-IDDISTR TO SPAR-IDDISTR                    
048500                                                                          
048600               WHEN 'RIT'                                                 
048800                    MOVE W-RIT-IDDISTR TO SPAR-IDDISTR                    
048900                                                                          
049300               WHEN 'RIW'                                                 
049500                    MOVE W-RIW-IDDISTR TO SPAR-IDDISTR                    
049600                                                                          
049700               WHEN 'RIX'                                                 
049900                    MOVE W-RIX-IDDISTR TO SPAR-IDDISTR-RIX                
050000                                                                          
050400               WHEN 'RKG'                                                 
050600                    MOVE W-RKG-IDDISTR TO SPAR-IDDISTR                    
050700                                                                          
051200             END-EVALUATE                                                 
051300                                                                          
051400             IF W-RIF-IDPTYP = 'RIB' OR 'RID' OR 'RIE' OR 'RIF' OR        
051500                               'RIG' OR 'RIH' OR 'RII'                    
051510                MOVE SPAR-IDDISTR-RIB TO TEST-IDDISTR                     
051700                ADD +1         TO W-ANT-POSTER                            
051701                                                                          
051710                EVALUATE W-RIF-IDPTYP                                     
051711                                                                          
051720                WHEN 'RID'                                                
051730                     WRITE RID-POST    FROM INPOST                        
051740                                                                          
051750                WHEN 'RIE'                                                
051760                     WRITE RIE-POST    FROM INPOST                        
051770                                                                          
051780                WHEN 'RIH'                                                
051790                     WRITE RIH-POST    FROM INPOST                        
051792                                                                          
051795                WHEN OTHER                                                
051796                     WRITE UTPOST    FROM INPOST                          
051798                                                                          
051799                END-EVALUATE                                              
052100             ELSE                                                         
052200               IF W-RIF-IDPTYP = 'RIK' OR 'RIL' OR 'RIM' OR               
052300                                 'RIN' OR 'RIO' OR 'RIP'                  
052410                 MOVE SPAR-IDDISTR-RIK TO TEST-IDDISTR                    
052500                 ADD +1        TO W-ANT-POSTER                            
052510                                                                          
052520                 EVALUATE W-RIF-IDPTYP                                    
052530                                                                          
052593                 WHEN 'RIO'                                               
052594                      WRITE RIO-POST    FROM INPOST                       
052596                                                                          
052597                 WHEN OTHER                                               
052598                      WRITE UTPOST    FROM INPOST                         
052600                                                                          
052601                 END-EVALUATE                                             
052900               ELSE                                                       
053000                 IF W-RIF-IDPTYP = 'RIX' OR 'RIY'                         
053010                   MOVE SPAR-IDDISTR-RIX TO TEST-IDDISTR                  
053200                   ADD +1    TO W-ANT-POSTER                              
053300                   WRITE UTPOST  FROM INPOST                              
053600                 ELSE                                                     
053610                   MOVE SPAR-IDDISTR TO TEST-IDDISTR                      
053800                   ADD +1    TO W-ANT-POSTER                              
053900                   WRITE UTPOST  FROM INPOST                              
054200                 END-IF                                                   
054300               END-IF                                                     
054400             END-IF                                                       
054500             PERFORM S01-LAS-W46123                                       
054600           END-IF                                                         
054700         END-IF                                                           
054800       END-IF                                                             
054900     END-PERFORM                                                          
055000     PERFORM BB-SLUT-KORT                                                 
055100     .                                                                    
055200     EJECT                                                                
055300                                                                          
100700 BA-START-KORT SECTION.                                                   
100800     SKIP2                                                                
100900     MOVE     'RI0'          TO START-IDPTYP                              
101000     MOVE     2364           TO START-IDDISTR                             
101100     MOVE     WC-CDC-SE      TO START-IDDC                                
101200     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
101300*                                                                         
101400     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
101500*                                                                         
101600     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
101700     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
101900     .                                                                    
102000     SKIP2                                                                
102100 BB-SLUT-KORT SECTION.                                                    
102200     SKIP2                                                                
102300     MOVE     'RI9'          TO SLUT-IDPTYP                               
102400     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
102500*                                                                         
102600     WRITE    UTPOST         FROM  SLUT-W461RI9                           
102800     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
102900     .                                                                    
103000     SKIP2                                                                
103100 S01-LAS-W46123 SECTION.                                                  
103200     SKIP2                                                                
103300     READ   W46123 INTO W-ARBAREA                                         
103400     AT END MOVE JA TO W46123-EOF                                         
103500     END-READ                                                             
103600                                                                          
103700     IF W46123-EOF = NEJ                                                  
103800                                                                          
103900       MOVE 'W46123'            TO POSTSUM-FDNAMN                         
104000       MOVE 'W46123D1'          TO POSTSUM-DDNAMN2                        
104100       MOVE SPACE               TO POSTSUM-TRANSTYP                       
104200       CALL POSTSUM             USING POSTSUM-PARM                        
104300                                                                          
104400     END-IF                                                               
104500     .                                                                    
104600     EJECT                                                                
104700 Z-FINIT SECTION.                                                         
104800     SKIP2                                                                
104900                                                                          
105000     CLOSE W46123 W46143                                                  
105100     SKIP2                                                                
105200*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
105300*                                    SKRIVNA POSTER                       
105400                                                                          
105500     MOVE 'S' TO POSTSUM-OPKOD                                            
105600     CALL POSTSUM USING POSTSUM-PARM                                      
105700     .                                                                    
