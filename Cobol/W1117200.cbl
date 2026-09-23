000003 ID DIVISION.                                                             
000004     SKIP2                                                                
000005 PROGRAM-ID.     W1117200.                                                
000006*AUTHOR.         BODIL LINDAHL.                                           
000007*DATE-WRITTEN.   JUNI 1992.                                               
000008*DATE-COMPILED.                                                           
000010*                                                                         
000020*    FUNKTION:                                                            
000030*        PROGRAMMET LÄSER FIL W11170 MED ERSÄTTNINGS-TRANSAR              
000040*        OCH SKAPAR FIL W11172 TILL VIPS.                                 
000050*                                                                         
000060*                                                                         
000070                                                                          
000080     SKIP3                                                                
000090 ENVIRONMENT DIVISION.                                                    
000100     SKIP2                                                                
000200 INPUT-OUTPUT SECTION.                                                    
000300                                                                          
000400 FILE-CONTROL.                                                            
000500     SKIP2                                                                
000600*          --- ERSÄTTNINGS-TRANSAR FRÅN W111P070                          
000700     SELECT W11170                     ASSIGN TO W11172D1.                
000800     SKIP2                                                                
000900*          --- ERSÄTTNINGS-INFO TILL VIPS                                 
001000     SELECT W11172                     ASSIGN TO W11172D2.                
001100     EJECT                                                                
001200 DATA DIVISION.                                                           
001300     SKIP3                                                                
001400 FILE SECTION.                                                            
001500     SKIP3                                                                
001600 FD  W11170                                                               
001700     RECORDING       V                                                    
001800     BLOCK CONTAINS  0.                                                   
001900     SKIP2                                                                
002000*01  -COPY W111701A      -L.                                              
002100     SKIP2                                                                
002200*01  -COPY W111702A      -L.                                              
002300     SKIP3                                                                
002400 FD  W11172                                                               
002500     RECORDING       V                                                    
002600     BLOCK CONTAINS  0.                                                   
002700     SKIP2                                                                
002800*01  POST -COPY W461S040 -PRE  RID-  -L.                                  
002900     SKIP2                                                                
003000*01  POST -COPY W461S041 -PRE  RIE-  -L.                                  
003100     SKIP2                                                                
003110*01  POST -COPY W461S042 -PRE  RIF-  -L.                                  
003111     SKIP2                                                                
003112*01  POST -COPY W461S043 -PRE  RKH-  -L.                                  
003120     EJECT                                                                
003130 WORKING-STORAGE SECTION.                                                 
003140     SKIP2                                                                
003141                                                                          
003142*    -- CHECKED BY WY2000                                                 
003150 77  IDPGM                       PIC X(8)   VALUE 'W1117200'.             
003160 77  JA                          PIC X      VALUE 'J'.                    
003170 77  NEJ                         PIC X      VALUE 'N'.                    
003180 77  W-IDLOPNR                   PIC S9(5)  VALUE  ZERO   COMP-3.         
003181 77  W-IDLOPNRE                  PIC S9(5)  VALUE  ZERO   COMP-3.         
003182                                                                          
003183 77  WS-IDARTNR-ERS              PIC S9(9)  VALUE  ZERO   COMP-3.         
003184 77  WS-REKSIFFR-ERS             PIC S9     VALUE  ZERO   COMP-3.         
003185 77  WS-DIERS-ERS                PIC S9(4)V9(3) VALUE ZERO                
003186                                                          COMP-3.         
003187 77  WS-DIERS-KVOT               PIC S9(4)V9(3) VALUE ZERO                
003188                                                          COMP-3.         
003189 77  WS-KDERS-NEW                PIC 9(3).                                
003190     88 ENTYDIG-ERSATTNING          VALUE 21 22 23 27.                    
003191     88 EJ-ENTYDIG-ERSATTNING       VALUE 24 25 26 28.                    
003192                                                                          
003200 77  W11170-EOF-SW               PIC X      VALUE 'N'.                    
003300     88  END-OF-W11170                      VALUE 'J'.                    
003400                                                                          
003500 77  BEHANDLA-702-SW             PIC X      VALUE 'N'.                    
003600     88  BEHANDLA-702                       VALUE 'J'.                    
003610*      --- VALID IDDC CODES                                               
003620*                                                                         
003630*01    -COPY WWDCKONS                                                     
003630*01    -COPY WWDC99                                                       
003640       EJECT                                                              
003700                                                                          
004200 01  DYNAMISKA-SUBPROGRAM.                                                
004300*                                                                         
004400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
004500     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
004600     EJECT                                                                
004700*    --- PARAMETRAR TILL POSTSUM                                          
004800*                                                                         
004900*01  -COPY W0005   -PRE  POSTSUM-                                         
005000     EJECT                                                                
005100*    --- PARAMETRAR TILL DATUMKORT                                        
005200*                                                                         
005210 01  PROGRAM-NAMN               PIC X(6)     VALUE 'W11172'.              
005220 01  DATUMKORT-ID               PIC X(6)     VALUE 'WDATUM'.              
005230                                                                          
005300*01  -COPY WDATKORT                                                       
005400     EJECT                                                                
005600 01  IN-AREA-START               PIC X(24)   VALUE                        
005700                                 'IN-AREA-START  '.                       
005800 01  IN-AREA.                                                             
005900     03  IN-IDPTYP               PIC X(3).                                
006000     03  FILLER                  PIC X(150).                              
006100*01  FILLER -COPY W111701A      -PRE IN701-   -RED  IN-AREA               
006200     EJECT                                                                
006300*01  FILLER -COPY W111702A      -PRE IN702-   -RED  IN-AREA               
006400     EJECT                                                                
006500 01  RID-AREA-START              PIC X(24)   VALUE                        
006600                                 'RID-AREA-START '.                       
006700                                                                          
006710*01  AREA   -COPY W461S040      -PRE RID-                                 
006720     EJECT                                                                
006730 01  RIE-AREA-START              PIC X(24)   VALUE                        
006731                                 'RIE-AREA-START '.                       
006732                                                                          
006733*01  AREA   -COPY W461S041      -PRE RIE-                                 
006734     EJECT                                                                
006735 01  RIF-AREA-START              PIC X(24)   VALUE                        
006736                                 'RIF-AREA-START '.                       
006737                                                                          
006738*01  AREA   -COPY W461S042      -PRE RIF-                                 
006739     EJECT                                                                
006740 01  RKH-AREA-START              PIC X(24)   VALUE                        
006741                                 'RKH-AREA-START '.                       
006742                                                                          
006743*01  AREA   -COPY W461S043      -PRE RKH-                                 
006744     EJECT                                                                
006745 PROCEDURE DIVISION.                                                      
006746                                                                          
006747                                                                          
006748     PERFORM A-INIT                                                       
006749                                                                          
006750     PERFORM S01-LAES-W11170                                              
006751     PERFORM UNTIL END-OF-W11170                                          
006752                                                                          
006753        EVALUATE TRUE                                                     
006754           WHEN IN-IDPTYP = '701'                                         
006756              PERFORM B-KOLLA-SKAPA-RKH-POST                              
006757              IF IN701-KDERS-NEW > 20                                     
006759                 MOVE IN701-IDARTNR-ERS                                   
006760                                  TO WS-IDARTNR-ERS                       
006761                 MOVE IN701-REKSIFFR-ERS                                  
006762                                  TO WS-REKSIFFR-ERS                      
006763                 MOVE IN701-KDERS-NEW                                     
006764                                  TO WS-KDERS-NEW                         
006765                 MOVE IN701-DIERS-ERS                                     
006766                                  TO WS-DIERS-ERS                         
006767                 MOVE JA          TO BEHANDLA-702-SW                      
006768                 ADD +1 TO W-IDLOPNRE                                     
006769                 IF IN701-KDERS-NEW = 29 OR 52                            
006770                    PERFORM D-SKAPA-RID-POST-EK29-52                      
006771                    MOVE NEJ      TO BEHANDLA-702-SW                      
006772                 END-IF                                                   
006774              END-IF                                                      
006775           WHEN IN-IDPTYP = '702'                                         
006776              IF BEHANDLA-702                                             
006777                 IF IN702-IDARTNR-ERS = WS-IDARTNR-ERS                    
006778                    PERFORM C-KOLLA-SKAPA-RID-RIE-RIF-POST                
006779                 END-IF                                                   
006780              END-IF                                                      
006781        END-EVALUATE                                                      
006790                                                                          
006800        PERFORM S01-LAES-W11170                                           
006900     END-PERFORM                                                          
007000                                                                          
007100     PERFORM Z-FINIT                                                      
007200     MOVE ZERO TO RETURN-CODE                                             
007300     GOBACK                                                               
007400     .                                                                    
007500     EJECT                                                                
007600 A-INIT SECTION.                                                          
007700                                                                          
007800     OPEN INPUT  W11170                                                   
007900     OPEN OUTPUT W11172                                                   
008000                                                                          
008100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
008200                                                                          
008800     MOVE D-IOCSDAT  TO W-IDLOPNR                                         
008810     MOVE ZERO       TO W-IDLOPNRE                                        
008900     .                                                                    
009000     EJECT                                                                
009100 B-KOLLA-SKAPA-RKH-POST SECTION.                                          
009200                                                                          
009300*****************************************************************         
009400*  RKH-POST = BACKNING                                          *         
009401*  - (NÄR GAMMAL EK > 20 OCH NY EK = 00)                        *         
009402*  - ÄT FEBR 93  RIVNING EK > 20 TILL < 10                      *         
009403*                BEHANDLAS SOM RIVNING TILL 00                  *         
009404*  - NÄR GAMMAL EK > 20 OCH NY EK > 20                          *         
009405*      ERSÄTTNINGEN RIVS DÅ OCH LÄGGS UPP PÅ NYTT               *         
009406*    - OM OLIKA EK = BYTE AV EK                                 *         
009407*    - OM SAMMA EK = UPPDATERING AV TILLKOMMANDE ARTIKLAR       *         
009410*****************************************************************         
009411                                                                          
009412     MOVE NEJ  TO BEHANDLA-702-SW                                         
009413     MOVE ZERO TO WS-IDARTNR-ERS                                          
009414                  WS-REKSIFFR-ERS                                         
009415                  WS-DIERS-ERS                                            
009416                  WS-KDERS-NEW                                            
009417                                                                          
009419                                                                          
009420     IF (IN701-KDERS-OLD > 20 AND IN701-KDERS-NEW < 10) OR                
009421        (IN701-KDERS-OLD > 20 AND IN701-KDERS-NEW > 20)                   
009422                                                                          
009423        MOVE ZERO                  TO RKH-BACK-SOR0-IDDISTR               
009424                                      RKH-BACK-SOR0-IDKUNDNR              
009425                                      RKH-BACK-SOR0-IDRONR                
009426                                      RKH-BACK-SOR0-TIRODAT               
009427        MOVE '040'                 TO RKH-BACK-SOR0-IDPTYP                
009428                                                                          
009429        MOVE 'RKH'                 TO RKH-BACK-IDPTYP                     
009430        MOVE IN701-IDARTNR-ERS     TO RKH-BACK-IDARTNR                    
009431        MOVE IN701-REKSIFFR-ERS    TO RKH-BACK-REKSIFFR                   
009432        MOVE IN701-KDERS-OLD       TO RKH-BACK-KDERS-OLD                  
009434        MOVE ZERO                  TO RKH-BACK-KDERS-NEW                  
009438                                                                          
009440        MOVE W-IDLOPNR             TO RKH-BACK-SOR0-IDLOPNR               
009450                                                                          
009460        PERFORM S14-SKRIV-RKH-POST                                        
009470     END-IF                                                               
009510     .                                                                    
009511     EJECT                                                                
009520 C-KOLLA-SKAPA-RID-RIE-RIF-POST SECTION.                                  
009530                                                                          
009540*****************************************************************         
009541*  POSTTYP RID RIE RIF INNEHÅLLER UPPGIFTER OM ERSATT OCH TILLK *         
009542*                      ARTIKEL RESP TEXT OCH SKAPAS:            *         
009545*  - NÄR ERSÄTTNINGEN GÅR UPPÅT TILL > 20                       *         
009546*  - NÄR GAMMAL EK > 20 OCH NY EK > 20                          *         
009550*  RID-POST = ENTYDIG ERSÄTTNING (SKAPAS OCKSÅ FÖR EK 29 O 52   *         
009551*             UTAN TILLKOMMANDE ARTIKLAR)                       *         
009552*  RIE-POST = EJ ENTYDIG ERSÄTTNING - TILLKOMMANDE ARTIKEL      *         
009553*  RIF-POST = EJ ENTYDIG ERSÄTTNING - TEXT TILL EJ ENT ERS      *         
009562*****************************************************************         
009563                                                                          
009564     EVALUATE TRUE                                                        
009565         WHEN ENTYDIG-ERSATTNING                                          
009567           PERFORM CA-SKAPA-RID-POST                                      
009568         WHEN EJ-ENTYDIG-ERSATTNING                                       
009569           IF IN702-FLTEXT = JA                                           
009570              PERFORM CB-SKAPA-RIF-POST                                   
009571           ELSE                                                           
009572              PERFORM CC-SKAPA-RIE-POST                                   
009573           END-IF                                                         
009574         WHEN OTHER                                                       
009575           CONTINUE                                                       
009576     END-EVALUATE                                                         
009577     .                                                                    
009578     EJECT                                                                
009579 CA-SKAPA-RID-POST SECTION.                                               
009580                                                                          
009581     MOVE ZERO                  TO RID-ERSD-SOR0-IDDISTR                  
009582                                   RID-ERSD-SOR0-IDKUNDNR                 
009583                                   RID-ERSD-SOR0-IDRONR                   
009584                                   RID-ERSD-SOR0-TIRODAT                  
009585     MOVE '040'                 TO RID-ERSD-SOR0-IDPTYP                   
009586                                                                          
009587     MOVE 'RID'                 TO RID-ERSD-IDPTYP                        
009588     MOVE WC-CDC-SE             TO RID-ERSD-IDDC                          
009589                                   RID-ERSD-KDERSUP                       
009590     MOVE +2                    TO RID-ERSD-KDDSP                         
009591     MOVE SPACE                 TO RID-ERSD-BERADREF                      
009592                                   RID-ERSD-BEVOLREF                      
009593     MOVE ZERO                  TO RID-ERSD-IDRONR                        
009594                                   RID-ERSD-KDRESTR                       
009595                                   RID-ERSD-KVBEART                       
009596                                   RID-ERSD-KVBEART-TILLK                 
009597     MOVE IN702-IDARTNR-ERS     TO RID-ERSD-IDARTNR                       
009598     MOVE IN702-IDKORTNR        TO RID-ERSD-IDKORTNR                      
009599     MOVE IN702-IDARTNR-TILLK   TO RID-ERSD-IDARTNR-TILLK                 
009600     MOVE IN702-REKSIFFR-TILLK  TO RID-ERSD-REKSIFFR-TILLK                
009601     MOVE WS-REKSIFFR-ERS       TO RID-ERSD-REKSIFFR                      
009602     MOVE WS-KDERS-NEW          TO RID-ERSD-KDERS                         
009603                                                                          
009604     COMPUTE WS-DIERS-KVOT =                                              
009605                     IN702-DIERS-TILLK / WS-DIERS-ERS                     
009606     IF WS-DIERS-KVOT < +1                                                
009607        MOVE +1                 TO RID-ERSD-DIERS-KVOT                    
009608     ELSE                                                                 
009609        MOVE WS-DIERS-KVOT      TO RID-ERSD-DIERS-KVOT                    
009610     END-IF                                                               
009611                                                                          
009617     MOVE W-IDLOPNR             TO RID-ERSD-SOR0-IDLOPNR                  
009618     MOVE W-IDLOPNRE            TO RID-ERSD-IDLOPNRE                      
009620                                                                          
009621     PERFORM S11-SKRIV-RID-POST                                           
009622     .                                                                    
009623     EJECT                                                                
009624 CB-SKAPA-RIF-POST SECTION.                                               
009625                                                                          
009626     MOVE ZERO                  TO RIF-ERSF-SOR0-IDDISTR                  
009627                                   RIF-ERSF-SOR0-IDKUNDNR                 
009628                                   RIF-ERSF-SOR0-IDRONR                   
009629                                   RIF-ERSF-SOR0-TIRODAT                  
009630     MOVE '040'                 TO RIF-ERSF-SOR0-IDPTYP                   
009631                                                                          
009632     MOVE 'RIF'                 TO RIF-ERSF-IDPTYP                        
009633     MOVE WC-CDC-SE             TO RIF-ERSF-IDDC                          
009634                                   RIF-ERSF-KDERSUP                       
009635     MOVE +2                    TO RIF-ERSF-KDDSP                         
009636     MOVE SPACE                 TO RIF-ERSF-BERADREF                      
009637                                   RIF-ERSF-BEVOLREF                      
009638     MOVE ZERO                  TO RIF-ERSF-IDRONR                        
009639                                   RIF-ERSF-KDRESTR                       
009640                                   RIF-ERSF-KVBEART                       
009641     MOVE IN702-IDARTNR-ERS     TO RIF-ERSF-IDARTNR                       
009642     MOVE IN702-IDKORTNR        TO RIF-ERSF-IDKORTNR                      
009643     MOVE IN702-BEERS           TO RIF-ERSF-BEERS                         
009645     MOVE WS-REKSIFFR-ERS       TO RIF-ERSF-REKSIFFR                      
009646     MOVE WS-KDERS-NEW          TO RIF-ERSF-KDERS                         
009647                                                                          
009658     MOVE W-IDLOPNR             TO RIF-ERSF-SOR0-IDLOPNR                  
009659     MOVE W-IDLOPNRE            TO RIF-ERSF-IDLOPNRE                      
009660                                                                          
009661     PERFORM S13-SKRIV-RIF-POST                                           
009662     .                                                                    
009663     EJECT                                                                
009664 CC-SKAPA-RIE-POST SECTION.                                               
009665                                                                          
009666     MOVE ZERO                  TO RIE-ERSE-SOR0-IDDISTR                  
009667                                   RIE-ERSE-SOR0-IDKUNDNR                 
009668                                   RIE-ERSE-SOR0-IDRONR                   
009669                                   RIE-ERSE-SOR0-TIRODAT                  
009670     MOVE '040'                 TO RIE-ERSE-SOR0-IDPTYP                   
009671                                                                          
009672     MOVE 'RIE'                 TO RIE-ERSE-IDPTYP                        
009673     MOVE WC-CDC-SE             TO RIE-ERSE-IDDC                          
009674                                   RIE-ERSE-KDERSUP                       
009675     MOVE +2                    TO RIE-ERSE-KDDSP                         
009676     MOVE SPACE                 TO RIE-ERSE-BERADREF                      
009677                                   RIE-ERSE-BEVOLREF                      
009678     MOVE ZERO                  TO RIE-ERSE-IDRONR                        
009679                                   RIE-ERSE-KDRESTR                       
009680                                   RIE-ERSE-KVBEART                       
009681                                   RIE-ERSE-KVBEART-TILLK                 
009682     MOVE IN702-IDARTNR-ERS     TO RIE-ERSE-IDARTNR                       
009683     MOVE IN702-IDKORTNR        TO RIE-ERSE-IDKORTNR                      
009684     MOVE IN702-IDARTNR-TILLK   TO RIE-ERSE-IDARTNR-TILLK                 
009685     MOVE IN702-REKSIFFR-TILLK  TO RIE-ERSE-REKSIFFR-TILLK                
009686     MOVE WS-REKSIFFR-ERS       TO RIE-ERSE-REKSIFFR                      
009687     MOVE WS-KDERS-NEW          TO RIE-ERSE-KDERS                         
009688                                                                          
009689     COMPUTE WS-DIERS-KVOT =                                              
009690                     IN702-DIERS-TILLK / WS-DIERS-ERS                     
009691     IF WS-DIERS-KVOT < +1                                                
009692        MOVE +1                 TO RIE-ERSE-DIERS-KVOT                    
009693     ELSE                                                                 
009694        MOVE WS-DIERS-KVOT      TO RIE-ERSE-DIERS-KVOT                    
009695     END-IF                                                               
009696                                                                          
009698     MOVE W-IDLOPNR             TO RIE-ERSE-SOR0-IDLOPNR                  
009699     MOVE W-IDLOPNRE            TO RIE-ERSE-IDLOPNRE                      
009700                                                                          
009701     PERFORM S12-SKRIV-RIE-POST                                           
009702     .                                                                    
009703     EJECT                                                                
009704 D-SKAPA-RID-POST-EK29-52 SECTION.                                        
009705                                                                          
009706*****************************************************************         
009707*  POSTTYP RID  SKAPAS FÖR ARTIKLAR MED EK 29 OCH 52 UTAN       *         
009708*               TILLKOMMANDE ARTIKLAR                           *         
009714*****************************************************************         
009715                                                                          
009716     MOVE ZERO                  TO RID-ERSD-SOR0-IDDISTR                  
009717                                   RID-ERSD-SOR0-IDKUNDNR                 
009718                                   RID-ERSD-SOR0-IDRONR                   
009719                                   RID-ERSD-SOR0-TIRODAT                  
009720     MOVE '040'                 TO RID-ERSD-SOR0-IDPTYP                   
009721                                                                          
009722     MOVE 'RID'                 TO RID-ERSD-IDPTYP                        
009723     MOVE WC-CDC-SE             TO RID-ERSD-IDDC                          
009724                                   RID-ERSD-KDERSUP                       
009725     MOVE +2                    TO RID-ERSD-KDDSP                         
009726     MOVE SPACE                 TO RID-ERSD-BERADREF                      
009727                                   RID-ERSD-BEVOLREF                      
009728     MOVE ZERO                  TO RID-ERSD-IDRONR                        
009729                                   RID-ERSD-KDRESTR                       
009730                                   RID-ERSD-KVBEART                       
009731                                   RID-ERSD-KVBEART-TILLK                 
009732     MOVE IN701-IDARTNR-ERS     TO RID-ERSD-IDARTNR                       
009733     MOVE +1                    TO RID-ERSD-IDKORTNR                      
009734     MOVE ZERO                  TO RID-ERSD-IDARTNR-TILLK                 
009735     MOVE ZERO                  TO RID-ERSD-REKSIFFR-TILLK                
009736     MOVE IN701-REKSIFFR-ERS    TO RID-ERSD-REKSIFFR                      
009737     MOVE IN701-KDERS-NEW       TO RID-ERSD-KDERS                         
009738     MOVE ZERO                  TO RID-ERSD-DIERS-KVOT                    
009739                                                                          
009741     MOVE W-IDLOPNR             TO RID-ERSD-SOR0-IDLOPNR                  
009742     MOVE W-IDLOPNRE            TO RID-ERSD-IDLOPNRE                      
009743                                                                          
009744     PERFORM S11-SKRIV-RID-POST                                           
009745     .                                                                    
009746     EJECT                                                                
009747 Z-FINIT SECTION.                                                         
009748                                                                          
009749     CLOSE W11170                                                         
009750           W11172                                                         
009751                                                                          
009752     MOVE 'S' TO POSTSUM-OPKOD                                            
009753     CALL POSTSUM USING POSTSUM-PARM                                      
009754     .                                                                    
009755     EJECT                                                                
009756 S01-LAES-W11170  SECTION.                                                
009757     SKIP2                                                                
009758     READ W11170 INTO IN-AREA                                             
009759     AT END                                                               
009760        SET END-OF-W11170 TO TRUE                                         
009761                                                                          
009770     NOT AT END                                                           
009800        MOVE 'W11170'   TO POSTSUM-FDNAMN                                 
009900        MOVE 'W11172D1' TO POSTSUM-DDNAMN2                                
009910        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
009911        CALL POSTSUM  USING POSTSUM-PARM                                  
009912     END-READ                                                             
009913     .                                                                    
009914     EJECT                                                                
009915 S11-SKRIV-RID-POST SECTION.                                              
009916                                                                          
009917     WRITE RID-POST FROM RID-AREA                                         
009918                                                                          
009920     MOVE 'W11172'   TO POSTSUM-FDNAMN                                    
009930     MOVE 'W11172D2' TO POSTSUM-DDNAMN2                                   
009931     MOVE 'RID'      TO POSTSUM-TRANSTYP                                  
009940     CALL POSTSUM USING POSTSUM-PARM                                      
009950     .                                                                    
009960     EJECT                                                                
009970 S12-SKRIV-RIE-POST SECTION.                                              
009980                                                                          
009990     WRITE RIE-POST FROM RIE-AREA                                         
010000                                                                          
010020     MOVE 'W11172'   TO POSTSUM-FDNAMN                                    
010030     MOVE 'W11172D2' TO POSTSUM-DDNAMN2                                   
010031     MOVE 'RIE'      TO POSTSUM-TRANSTYP                                  
010040     CALL POSTSUM USING POSTSUM-PARM                                      
010050     .                                                                    
010060     EJECT                                                                
010070 S13-SKRIV-RIF-POST SECTION.                                              
010080                                                                          
010090     WRITE RIF-POST FROM RIF-AREA                                         
010100                                                                          
010200     MOVE 'RIF'      TO POSTSUM-TRANSTYP                                  
010300     MOVE 'W11172'   TO POSTSUM-FDNAMN                                    
010400     MOVE 'W11172D2' TO POSTSUM-DDNAMN2                                   
010500     CALL POSTSUM USING POSTSUM-PARM                                      
010600     .                                                                    
010700     EJECT                                                                
010800 S14-SKRIV-RKH-POST SECTION.                                              
010900                                                                          
011000     WRITE RKH-POST FROM RKH-AREA                                         
011100                                                                          
011300     MOVE 'W11172'   TO POSTSUM-FDNAMN                                    
011400     MOVE 'W11172D2' TO POSTSUM-DDNAMN2                                   
011410     MOVE 'RKH'      TO POSTSUM-TRANSTYP                                  
011500     CALL POSTSUM USING POSTSUM-PARM                                      
011600     .                                                                    
011700     EJECT                                                                
