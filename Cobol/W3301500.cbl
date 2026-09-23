000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3301500.                                                
000400*AUTHOR.         RONNY STENHOLM.                                          
000500*DATE-WRITTEN.   91/06/17.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET SUMMERAR SEKUNDÄWREGISTRET FÖR P2-LISTAN.             
001100*        ANTALET MARKNADER MINSKAS ENLIGT FÖLJANDE:                       
001200*        FÖR PV:                                                          
001300*        1,7,8,10 (SVERIGE DANMARK FINLAND NORGE)                         
001400*        ÖVR NORDEN (<13 MEN EJ OVANSTÅENDE,EJ 4)                         
001500* -------TOTAL NORDEN                                                     
001600*        43,46                                                            
001700*        ÖVR N-AMERIKA (44,45)                                            
001800* -------TOTAL N-AMERIKA                                                  
001900*        15,17,21,23,27,28,30,31,32                                       
002000*        ÖVR V-EUROPA (13-33 MEN EJ OVANSTÅENDE)                          
002100* -------TOTAL V-EUROPA                                                   
002200*        81,94                                                            
002300*        ÖVR VCI (4,34-42,47-96)                                          
002400* -------TOTAL V-VCI                                                      
002500***********************************************                           
002510* RIKTIGA SUMMERINGS NIVÅER HAR EJ FASTSTÄLLTS PÅ LV ÄNNU                 
002520* DETTA INNEBÄR ATT DET ÄR I DAG MENINGSLÖST FÖR LV ATT                   
002530* GÖRA URVAL BASERADE PÅ NEDANSTÅENDE SUMMERING.                          
002540* STEFAN HEIMERSSON SKULLE ÅTERKOMMA MED RÄTT INDELNING./RS               
002600*        FÖR LV:                                                          
002700*        SVERIGE/FINLAND (1-5,8)                                          
002800*        DANMARK/NORGE   (7,9,10,11,12)                                   
002900*        BENELUX/FRANKR  (13-17,19-21)                                    
003000*        TYSKTALANDE/DIV (18,22,27,31,32,37,40)                           
003100*        SYD EUROPA      (23-26,28,)                                      
003200*        STORBRITANNIEN  (29,30)                                          
003300*        ÖVR VÄST EUROPA (33)                                             
003400* -------TOTAL V-EUROPA                                                   
003500*        ÖST EUROPA I    (38,39,41,42)                                    
003600*        ÖST EUROPA II   (34,36)                                          
003700*        ÖST EUROPA III  (35) (POLEN)                                     
003800*        NORD AMERIKA    (43-46)                                          
003900*        N.AFRII/M.ÖSTII (47,49,63,64,67,68,87)                           
004000*        AFRIKA          (48,50,51,54,55,57,58)                           
004100*        TUNISIEN        (56)                                             
004200*        N.AFR I/M.ÖST I (52,53,59-62,65,66,69)                           
004300*        SYD AMERIKA     (70-78)                                          
004400*        FJ.ÖST-JAP/TAIW (79,80,83-86,88,89,91-93)                        
004500*        AUS/JAPAN/TAIW  (81,82,90,94)                                    
004600*        ÖVRIGA VÄRLDEN  (95)                                             
004700* -------TOTAL TOT.OVERSEAS                                               
004800* OBSERVERA ALLA TOTAL "MARKNADER" SOM SKAPAS MÅSTE BENÄMNAS              
004900* OBSERVERA TOT.XXXXX ANNARS FUNGERAR INTE SUMMERINGEN RIKTIGT            
005000* OBSERVERA I E+ W3304600                                                 
005100*                                                                         
005200*                                                                         
005300*    ABENDKODER:                                                          
005400*        U0016 -  . . . .                                                 
005500*        U1000 -  . . . .                                                 
005600*                                                                         
005700                                                                          
005800     SKIP3                                                                
005900 ENVIRONMENT DIVISION.                                                    
006000     SKIP2                                                                
006100 INPUT-OUTPUT SECTION.                                                    
006200                                                                          
006300 FILE-CONTROL.                                                            
006400     SKIP2                                                                
006500*          --- SEKUNDÄWREGISTRET INFIL                                    
006600     SELECT W33015                     ASSIGN TO W33015D1.                
006700     SKIP2                                                                
006800*          --- INFOFIL INFIL                                              
006900     SELECT W33019                     ASSIGN TO W33015D2.                
007000     SKIP2                                                                
007100*          --- SEKUNDÄRREGISTER P2 UTFIL                                  
007200     SELECT W33022                     ASSIGN TO W33015D3.                
007300     EJECT                                                                
007400 DATA DIVISION.                                                           
007500     SKIP3                                                                
007600 FILE SECTION.                                                            
007700     SKIP3                                                                
007800 FD  W33015                                                               
007900     RECORDING       F                                                    
008000     BLOCK CONTAINS  0.                                                   
008100     SKIP2                                                                
008200*01  -COPY W33014      -L.                                                
008300     SKIP3                                                                
008400 FD  W33019                                                               
008500     RECORDING       F                                                    
008600     BLOCK CONTAINS  0.                                                   
008700     SKIP2                                                                
008800*01  -COPY W33019      -L.                                                
008900     SKIP3                                                                
009000 FD  W33022                                                               
009100     RECORDING       F                                                    
009200     BLOCK CONTAINS  0.                                                   
009300     SKIP2                                                                
009400*01  POST -COPY W33014 -PRE  UT-  -L.                                     
009500     EJECT                                                                
009600 WORKING-STORAGE SECTION.                                                 
009700     SKIP2                                                                
009701                                                                          
009710*    -- CHECKED BY WY2000                                                 
009800 77  IDPGM                       PIC X(8)    VALUE 'W3301500'.            
009900 77  JA                          PIC X       VALUE 'J'.                   
010000 77  NEJ                         PIC X       VALUE 'N'.                   
010200 77  IX                    PIC S9(2) VALUE ZERO COMP SYNC.                
010300 77  SPAR-IDARTNR          PIC S9(9) COMP-3  VALUE ZERO.                  
010400 77  TOT-RAKNARE-AF              PIC S9(9)  VALUE +0 COMP.                
010500                                                                          
010600 77  W33015-EOF-SW               PIC X       VALUE 'N'.                   
010700     88  END-OF-W33015                       VALUE 'J'.                   
010800 77  W33019-EOF-SW               PIC X       VALUE 'N'.                   
010900     88  END-OF-W33019                       VALUE 'J'.                   
011000     EJECT                                                                
011010                                                                          
011020 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
011030 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
011040 01  FILLER                  PIC X(16)   VALUE 'WS-FIL-SEKTION'.          
011050 01  WS-FIL-SEKTION              PIC X(30)   VALUE SPACE.                 
011060                                                                          
011100 01  DYNAMISKA-SUBPROGRAM.                                                
011200*                                                                         
011300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011500     SKIP2                                                                
011600*    --- TABELLAREA                                                       
011700*                                                                         
011800 01   FILLER                      PIC X(16)   VALUE 'TABELL'.             
011900 01   TAB-AREA.                                                           
012000     03  TABELL-RAD OCCURS 96 INDEXED BY TAB-INDEX.                       
012100        05  AREA -PRE P2- -COPY W33014.                                   
012200*    --- PARAMETRAR TILL ABEND                                            
012300                                                                          
012400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012600     SKIP2                                                                
012700 01  FELTEXT.                                                             
012800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013000     EJECT                                                                
013100*    --- PARAMETRAR TILL POSTSUM                                          
013200*                                                                         
013300*01  -COPY W0005   -PRE  POSTSUM-                                         
013400     EJECT                                                                
013500 01  NOLL-AREA-START             PIC X(24)   VALUE                        
013600                                 'NOLL-AREA-START'.                       
013700     SKIP2                                                                
013800                                                                          
013900*01  AREA -COPY W33014     -PRE NOLL-                                     
014000     EJECT                                                                
014100 01  IN-AREA-START               PIC X(24)   VALUE                        
014200                                 'IN-AREA-START  '.                       
014300     SKIP2                                                                
014400                                                                          
014500*01  AREA -COPY W33014     -PRE IN-                                       
014600     EJECT                                                                
014700                                                                          
014800*01  AREA -COPY W33019     -PRE I19-                                      
014900     EJECT                                                                
015000 01  UT-AREA-START               PIC X(24)   VALUE                        
015100                                 'UT-AREA-START  '.                       
015200     SKIP2                                                                
015300                                                                          
015400*01  AREA -COPY W33014     -PRE UT-                                       
015500     EJECT                                                                
015600 PROCEDURE DIVISION.                                                      
015700     SKIP2                                                                
015800 STYR SECTION.                                                            
015900     PERFORM A-INIT                                                       
016000     PERFORM S01-LAES-W33015                                              
016100     PERFORM S02-LAES-W33019                                              
016200     PERFORM UNTIL END-OF-W33015                                          
016600       MOVE IN-IDARTNR TO SPAR-IDARTNR                                    
016700       PERFORM B-KOLLA-PRODSLAG                                           
016900       PERFORM UNTIL SPAR-IDARTNR NOT = IN-IDARTNR OR                     
017000             END-OF-W33015                                                
017100         PERFORM C-GRUPPERA-MARKN-FOER-PVTAB                              
017200         PERFORM S01-LAES-W33015                                          
017300       END-PERFORM                                                        
018100       PERFORM E-SKRIV-UT-POSTERNA                                        
018500     END-PERFORM                                                          
018600                                                                          
018700     PERFORM Z-FINIT                                                      
018800     MOVE ZERO TO RETURN-CODE                                             
018900     GOBACK                                                               
019000     .                                                                    
019100     EJECT                                                                
019200 A-INIT SECTION.                                                          
019210     MOVE 'A-INIT '          TO WS-SEKTION                                
019300                                                                          
019400     OPEN INPUT  W33015                                                   
019500                 W33019                                                   
019600                                                                          
019700     OPEN OUTPUT W33022                                                   
019800     INITIALIZE TAB-AREA                                                  
019900     INITIALIZE NOLL-AREA                                                 
020000     MOVE ZERO TO UT-IDDISTR                                              
020100     MOVE ZERO TO UT-IDKONCNR                                             
020200     SKIP2                                                                
020300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020400     .                                                                    
020500     EJECT                                                                
020600 B-KOLLA-PRODSLAG SECTION.                                                
020610     MOVE 'B-KOLLA-PRODSLAG' TO WS-SEKTION                                
020700     SKIP2                                                                
020800     PERFORM UNTIL I19-IDARTNR = IN-IDARTNR                               
020900       PERFORM S02-LAES-W33019                                            
021000     END-PERFORM                                                          
021600     .                                                                    
021700     EJECT                                                                
021800 C-GRUPPERA-MARKN-FOER-PVTAB SECTION.                                     
021810     MOVE 'C-GRUPPERA-MARKN-FOER-PVTAB' TO WS-SEKTION                     
021900     SKIP2                                                                
022000     IF IN-KDMARK-BUDG  = +1 OR +7 OR +8 OR +10                           
022100       CONTINUE                                                           
022200     ELSE                                                                 
022300       IF IN-KDMARK-BUDG < +13                                            
022400         MOVE +12 TO IN-KDMARK-BUDG                                       
022500         MOVE 'ÖVR.NORDEN' TO IN-BEMARK-BUDG                              
022600       END-IF                                                             
022700     END-IF                                                               
022800     IF IN-KDMARK-BUDG = +44                                              
022900       MOVE +45 TO IN-KDMARK-BUDG                                         
023000       MOVE 'ÖVR.N-AMERIKA' TO IN-BEMARK-BUDG                             
023100     END-IF                                                               
023200     IF IN-KDMARK-BUDG = +13 OR +14 OR +16 OR +18 OR +19 OR               
023300                         +20 OR +22 OR +24 OR +25 OR +26 OR               
023400                         +29                                              
023500       MOVE +33 TO IN-KDMARK-BUDG                                         
023600       MOVE 'ÖVR.V-EUROPA' TO IN-BEMARK-BUDG                              
023700     END-IF                                                               
023800     IF IN-KDMARK-BUDG = +4 OR                                            
023900     ( IN-KDMARK-BUDG > +33 AND < +43 ) OR                                
024000     ( IN-KDMARK-BUDG > +46 )                                             
024100       MOVE +94 TO IN-KDMARK-BUDG                                         
024200       MOVE 'ÖVR.VCI' TO IN-BEMARK-BUDG                                   
024300     END-IF                                                               
024400     PERFORM CA-LAEGG-MARKN-I-PVTAB                                       
024500*----TOTALSUMMOR *                                                        
024600* OBS      *ALLA TOTAL "MARKNADER" SOM SKAPAS MÅSTE BENÄMNAS              
024700* OBS      *TOT.XXXXX ANNARS FUNGERAR INTE SUMMERINGEN RIKTIGT            
024800* OBS      *I E+ W3304600                                                 
024900     IF IN-KDMARK-BUDG = +1 OR +7 OR +8 OR +10 OR +12                     
025000       MOVE +13 TO IN-KDMARK-BUDG                                         
025100       MOVE 'TOT.NORDEN' TO IN-BEMARK-BUDG                                
025200       PERFORM CA-LAEGG-MARKN-I-PVTAB                                     
025300     ELSE                                                                 
025400       IF IN-KDMARK-BUDG = +43 OR +45 OR +46                              
025500         MOVE +47 TO IN-KDMARK-BUDG                                       
025600         MOVE 'TOT.N-AMERIKA' TO IN-BEMARK-BUDG                           
025700         PERFORM CA-LAEGG-MARKN-I-PVTAB                                   
025800       ELSE                                                               
025900         IF IN-KDMARK-BUDG = +15 OR +17 OR +21 OR +23 OR +27              
026000                         OR +28 OR +30 OR +31 OR +32 OR +33               
026100           MOVE +34 TO IN-KDMARK-BUDG                                     
026200           MOVE 'TOT.V-EUROPA ' TO IN-BEMARK-BUDG                         
026300           PERFORM CA-LAEGG-MARKN-I-PVTAB                                 
026400         ELSE                                                             
026500           IF IN-KDMARK-BUDG = +81 OR +94 OR +95                          
026600                                                                          
026700             MOVE +95 TO IN-KDMARK-BUDG                                   
026800             MOVE 'TOT.VCI      ' TO IN-BEMARK-BUDG                       
026900             PERFORM CA-LAEGG-MARKN-I-PVTAB                               
027000           END-IF                                                         
027100         END-IF                                                           
027200       END-IF                                                             
027300     END-IF                                                               
027400     MOVE +96 TO IN-KDMARK-BUDG                                           
027500     MOVE 'TOT.WW       ' TO IN-BEMARK-BUDG                               
027600     PERFORM CA-LAEGG-MARKN-I-PVTAB                                       
027700*----TOTALSUMMOR *                                                        
027800     .                                                                    
027900     EJECT                                                                
028000                                                                          
028100 CA-LAEGG-MARKN-I-PVTAB SECTION.                                          
028110     MOVE 'CA-LAEGG-MARKN-I-PVTAB' TO WS-SEKTION                          
028200     SKIP2                                                                
028300     MOVE IN-IDARTNR TO P2-IDARTNR(IN-KDMARK-BUDG)                        
028400     MOVE IN-KDMARK-BUDG TO P2-KDMARK-BUDG(IN-KDMARK-BUDG)                
028500     MOVE IN-BEMARK-BUDG TO P2-BEMARK-BUDG(IN-KDMARK-BUDG)                
028600     ADD IN-SUARTFSG-PER TO P2-SUARTFSG-PER(IN-KDMARK-BUDG)               
028700     ADD IN-SUARTFSG-AAR TO P2-SUARTFSG-AAR(IN-KDMARK-BUDG)               
028800     ADD IN-SUARTFSG-FAAR TO P2-SUARTFSG-FAAR(IN-KDMARK-BUDG)             
028900     ADD IN-SUARTFSG-RAAR TO P2-SUARTFSG-RAAR(IN-KDMARK-BUDG)             
029000     ADD IN-SUARTFSG-FRAAR TO                                             
029100          P2-SUARTFSG-FRAAR(IN-KDMARK-BUDG)                               
029210     ADD IN-SULEVANT-PER TO P2-SULEVANT-PER(IN-KDMARK-BUDG)               
029300     ADD IN-SULEVANT-AAR TO P2-SULEVANT-AAR(IN-KDMARK-BUDG)               
029400     ADD IN-SULEVANT-FAAR TO P2-SULEVANT-FAAR(IN-KDMARK-BUDG)             
029500     ADD IN-SULEVANT-RAAR TO P2-SULEVANT-RAAR(IN-KDMARK-BUDG)             
029600     ADD IN-SULEVANT-FRAAR TO                                             
029700          P2-SULEVANT-FRAAR(IN-KDMARK-BUDG)                               
029800     ADD IN-SUARTSJK-PER TO P2-SUARTSJK-PER(IN-KDMARK-BUDG)               
029900     ADD IN-SUARTSJK-AAR TO P2-SUARTSJK-AAR(IN-KDMARK-BUDG)               
030000     ADD IN-SUARTSJK-FAAR TO P2-SUARTSJK-FAAR(IN-KDMARK-BUDG)             
030100     ADD IN-SUARTSJK-RAAR TO P2-SUARTSJK-RAAR(IN-KDMARK-BUDG)             
030200     ADD IN-SUARTSJK-FRAAR TO                                             
030300          P2-SUARTSJK-FRAAR(IN-KDMARK-BUDG)                               
031000     .                                                                    
031100     EJECT                                                                
046800 E-SKRIV-UT-POSTERNA SECTION.                                             
046810     MOVE 'E-SKRIV-UT-POSTERNA' TO WS-SEKTION                             
046900*--------------------------------HÄR NOLLSTÄLLS OCKSÅ TABELLEN.           
047000     SKIP2                                                                
047100     MOVE +1 TO IX                                                        
047200     PERFORM UNTIL IX > 96                                                
047300       IF P2-IDARTNR(IX) > ZERO                                           
047400         MOVE P2-IDARTNR(IX)        TO UT-IDARTNR                         
047500         MOVE P2-KDMARK-BUDG(IX)    TO UT-KDMARK-BUDG                     
047600         MOVE P2-BEMARK-BUDG(IX)    TO UT-BEMARK-BUDG                     
047700         MOVE P2-SUARTFSG-PER(IX)   TO UT-SUARTFSG-PER                    
047800         MOVE P2-SUARTFSG-AAR(IX)   TO UT-SUARTFSG-AAR                    
047900         MOVE P2-SUARTFSG-FAAR(IX)  TO UT-SUARTFSG-FAAR                   
048000         MOVE P2-SUARTFSG-RAAR(IX)  TO UT-SUARTFSG-RAAR                   
048100         MOVE P2-SUARTFSG-FRAAR(IX) TO UT-SUARTFSG-FRAAR                  
048200         MOVE P2-SULEVANT-PER(IX)   TO UT-SULEVANT-PER                    
048300         MOVE P2-SULEVANT-AAR(IX)   TO UT-SULEVANT-AAR                    
048400         MOVE P2-SULEVANT-FAAR(IX)  TO UT-SULEVANT-FAAR                   
048500         MOVE P2-SULEVANT-RAAR(IX)  TO UT-SULEVANT-RAAR                   
048600         MOVE P2-SULEVANT-FRAAR(IX) TO UT-SULEVANT-FRAAR                  
048700         MOVE P2-SUARTSJK-PER(IX)   TO UT-SUARTSJK-PER                    
048800         MOVE P2-SUARTSJK-AAR(IX)   TO UT-SUARTSJK-AAR                    
048900         MOVE P2-SUARTSJK-FAAR(IX)  TO UT-SUARTSJK-FAAR                   
049000         MOVE P2-SUARTSJK-RAAR(IX)  TO UT-SUARTSJK-RAAR                   
049100         MOVE P2-SUARTSJK-FRAAR(IX) TO UT-SUARTSJK-FRAAR                  
049700*--------------------------------------NOLLSTÄLL                          
049800                                                                          
049900         MOVE SPACE TO                                                    
050000              P2-BEMARK-BUDG(IX)                                          
050100         MOVE ZERO TO                                                     
050200              P2-IDARTNR(IX)                                              
050300              P2-KDMARK-BUDG(IX)                                          
050400              P2-SUARTFSG-PER(IX)                                         
050500              P2-SUARTFSG-AAR(IX)                                         
050600              P2-SUARTFSG-FAAR(IX)                                        
050700              P2-SUARTFSG-RAAR(IX)                                        
050800              P2-SUARTFSG-FRAAR(IX)                                       
050900              P2-SULEVANT-PER(IX)                                         
051000              P2-SULEVANT-AAR(IX)                                         
051100              P2-SULEVANT-FAAR(IX)                                        
051200              P2-SULEVANT-RAAR(IX)                                        
051300              P2-SULEVANT-FRAAR(IX)                                       
051400              P2-SUARTSJK-PER(IX)                                         
051500              P2-SUARTSJK-AAR(IX)                                         
051600              P2-SUARTSJK-FAAR(IX)                                        
051700              P2-SUARTSJK-RAAR(IX)                                        
051800              P2-SUARTSJK-FRAAR(IX)                                       
052400         PERFORM S11-SKRIV-W33022                                         
052500       END-IF                                                             
052600     ADD  +1 TO IX                                                        
052700     END-PERFORM                                                          
052800     .                                                                    
052900     EJECT                                                                
053000 Z-FINIT SECTION.                                                         
053010     MOVE 'Z-FINIT' TO WS-SEKTION                                         
053100     SKIP2                                                                
053200     CLOSE W33015                                                         
053300           W33019                                                         
053400           W33022                                                         
053500     SKIP2                                                                
053600     MOVE 'T' TO POSTSUM-OPKOD                                            
053700     MOVE 'INFIL' TO POSTSUM-TRANSTYP                                     
053800     MOVE 'W33015' TO POSTSUM-FDNAMN                                      
053900     MOVE 'W33015D1' TO POSTSUM-DDNAMN2                                   
054000     MOVE TOT-RAKNARE-AF TO POSTSUM-TOTTRANS                              
054100     CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                        
054200     .                                                                    
054300     EJECT                                                                
054400 S01-LAES-W33015  SECTION.                                                
054410     MOVE 'S01-LAES-W33015' TO WS-FIL-SEKTION                             
054500     SKIP2                                                                
054600     READ W33015 INTO IN-AREA                                             
054700     AT END                                                               
054800     SET END-OF-W33015 TO TRUE                                            
054900                                                                          
055000     NOT AT END                                                           
055100        ADD +1 TO TOT-RAKNARE-AF                                          
055200     END-READ                                                             
055300     .                                                                    
055400     EJECT                                                                
055500 S02-LAES-W33019  SECTION.                                                
055510     MOVE 'S02-LAES-W33019' TO WS-FIL-SEKTION                             
055600     SKIP2                                                                
055700     READ W33019 INTO I19-AREA                                            
055800     AT END                                                               
055900     SET END-OF-W33019 TO TRUE                                            
056000                                                                          
056100     NOT AT END                                                           
056200        MOVE 'W33019' TO POSTSUM-FDNAMN                                   
056300        MOVE 'W33015D2' TO POSTSUM-DDNAMN2                                
056400        MOVE 'IN-19' TO POSTSUM-TRANSTYP                                  
056500        CALL POSTSUM USING POSTSUM-PARM                                   
056600     END-READ                                                             
056700     .                                                                    
056800     EJECT                                                                
056900 S11-SKRIV-W33022 SECTION.                                                
056910     MOVE 'S11-SKRIV-W33022' TO WS-FIL-SEKTION                            
057000     SKIP2                                                                
057100     WRITE UT-POST FROM UT-AREA                                           
057200                                                                          
057300     MOVE 'UTFIL' TO POSTSUM-TRANSTYP                                     
057400     MOVE 'W33022' TO POSTSUM-FDNAMN                                      
057500     MOVE 'W33015D3' TO POSTSUM-DDNAMN2                                   
057600     CALL POSTSUM USING POSTSUM-PARM                                      
057700     .                                                                    
057800     EJECT                                                                
