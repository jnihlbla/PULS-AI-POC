000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2714800.                                                
000400 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000500 DATE-WRITTEN.   02/03/21.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        MATCHAR FRAM SÄSONGSARTIKLAR SAMT ARTIKLAR MED                   
001100*        FÖRÄNDRAT PROGNOS                                                
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500     SELECT W27147NY                   ASSIGN TO W27148D1.                
002600     SKIP2                                                                
002700     SELECT W27147GAM                  ASSIGN TO W27148D2.                
002800*          --- SAMTLIGA DATAELEMENT FRÅN WDK711, IDDC = LDC               
002900     SELECT LDC-W27148                 ASSIGN TO W27148D3.                
003000                                                                          
003100*          --- SAMTLIGA DATAELEMENT FRÅN WDK711, IDDC = SDC               
003200     SELECT SDC-W27148                 ASSIGN TO W27148D4.                
003300                                                                          
003400*          --- SAMTLIGA DATAELEMENT FRÅN WDK711, IDDC = NDC               
003500     SELECT NDC-W27148                 ASSIGN TO W27148D5.                
003600                                                                          
003700*          --- INTERN REFILL NDC-CN                                       
003800     SELECT CHN-W27148                 ASSIGN TO W27148D6.                
003900                                                                          
011800     EJECT                                                                
011900 DATA DIVISION.                                                           
012000     SKIP3                                                                
012100 FILE SECTION.                                                            
012200     SKIP3                                                                
012300 FD  W27147NY                                                             
012400     RECORDING       F                                                    
012500     BLOCK CONTAINS  0.                                                   
012600                                                                          
012700*01  POST     -COPY W27147  -PRE W27147NY-    -L                          
012800     SKIP3                                                                
012900 FD  W27147GAM                                                            
013000     RECORDING       F                                                    
013100     BLOCK CONTAINS  0.                                                   
013200                                                                          
013300*01  POST     -COPY W27147  -PRE W27147GAM-    -L                         
013400     SKIP3                                                                
013500 FD  LDC-W27148                                                           
013600     RECORDING       F                                                    
013700     BLOCK CONTAINS  0.                                                   
013800                                                                          
013900*01  POST -COPY W27147 -PRE  LDC-W27148-   -L.                            
014000                                                                          
014100 FD  SDC-W27148                                                           
014200     RECORDING       F                                                    
014300     BLOCK CONTAINS  0.                                                   
014400                                                                          
014500*01  POST -COPY W27147 -PRE  SDC-W27148-   -L.                            
014600                                                                          
014700 FD  NDC-W27148                                                           
014800     RECORDING       F                                                    
014900     BLOCK CONTAINS  0.                                                   
015000                                                                          
015100*01  POST -COPY W27147 -PRE  NDC-W27148-   -L.                            
015200                                                                          
015300 FD  CHN-W27148                                                           
015400     RECORDING       F                                                    
015500     BLOCK CONTAINS  0.                                                   
015600                                                                          
015700*01  POST -COPY W27147 -PRE  CHN-W27148-   -L.                            
015800                                                                          
031400 WORKING-STORAGE SECTION.                                                 
031500     SKIP2                                                                
031600                                                                          
031700*    -- CHECKED BY WY2000                                                 
031800 77  IDPGM                       PIC X(8)    VALUE 'W2714800'.            
031900 77  JA                          PIC X       VALUE 'J'.                   
032000 77  NEJ                         PIC X       VALUE 'N'.                   
032100     SKIP2                                                                
032200                                                                          
032300 01 WS-IDNY.                                                              
032400  03 WS-IDNY-IDARTNR            PIC S9(9)  VALUE ZERO.                    
032500  03 WS-IDNY-IDDC               PIC X(2)   VALUE SPACE.                   
032600 01 WS-IDGAM.                                                             
032700  03 WS-IDGAM-IDARTNR           PIC S9(9)  VALUE ZERO.                    
032800  03 WS-IDGAM-IDDC              PIC X(2)   VALUE SPACE.                   
036100                                                                          
036200 01  FELTEXT.                                                             
036300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
036400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
036500                                                                          
036600 77  W27147NY-EOF-SW             PIC X       VALUE 'N'.                   
036700     88  END-OF-W27147NY                     VALUE 'J'.                   
036800                                                                          
036900 77  W27147GAM-EOF-SW            PIC X       VALUE 'N'.                   
037000     88  END-OF-W27147GAM                    VALUE 'J'.                   
037100     EJECT                                                                
037200*      --- VALID IDDC CODES                                               
037300*                                                                         
037400*01    -COPY WWDC99                                                       
037500                                                                          
037600     EJECT                                                                
037700 01  DYNAMISKA-SUBPROGRAM.                                                
037800*                                                                         
037900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
038000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
038100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
038200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
038300     EJECT                                                                
038400*    ---- PARAMETRAR TILL WDATKONV                                        
038500 01  FILLER               PIC X(16)   VALUE  'DATKONV-AREA'.              
038600*01  -COPY WDATAREA                                                       
038700*    --- PARAMETRAR TILL POSTSUM                                          
038800*                                                                         
038900*01  -COPY W0005   -PRE  POSTSUM-                                         
039000     EJECT                                                                
039100 01  W27147NY-AREA-START         PIC X(24)   VALUE                        
039200                                 'W27147NY-AREA-START  '.                 
039300     SKIP2                                                                
039400                                                                          
039500 01  W27147NY-AREA.                                                       
039600         05  -COPY W27147   -PRE W27147NY-                                
039700                                                                          
039800     EJECT                                                                
039900 01  W27147GAM-AREA-START        PIC X(24)   VALUE                        
040000                                 'W27147GAM-AREA-START  '.                
040100     SKIP2                                                                
040200                                                                          
040300 01  W27147GAM-AREA.                                                      
040400         05  -COPY W27147   -PRE W27147GAM-                               
040500                                                                          
040600     EJECT                                                                
040700                                                                          
040800     EJECT                                                                
040900 LINKAGE SECTION.                                                         
041000                                                                          
041100 PROCEDURE DIVISION.                                                      
041200                                                                          
041300     SKIP2                                                                
041400     PERFORM A-INIT                                                       
041500                                                                          
041600     PERFORM S01-LAES-W27147NY                                            
041700     PERFORM S02-LAES-W27147GAM                                           
041800                                                                          
041900     PERFORM UNTIL END-OF-W27147NY                                        
042000       PERFORM B-BEARBETA                                                 
042100     END-PERFORM                                                          
042200                                                                          
042300     PERFORM Z-FINIT                                                      
042400                                                                          
042500     MOVE ZERO TO RETURN-CODE                                             
042600     GOBACK                                                               
042700     .                                                                    
042800     EJECT                                                                
042900 A-INIT SECTION.                                                          
043100     OPEN INPUT  W27147NY                                                 
043200                 W27147GAM                                                
043300                                                                          
043400     OPEN OUTPUT LDC-W27148                                               
043500                 SDC-W27148                                               
043600                 NDC-W27148                                               
043700                 CHN-W27148                                               
046400     .                                                                    
046500     EJECT                                                                
046600 B-BEARBETA SECTION.                                                      
046800     IF END-OF-W27147GAM OR WS-IDGAM > WS-IDNY                            
047100       PERFORM C-SKRIV-UTPOST                                             
047200       PERFORM S01-LAES-W27147NY                                          
047400     ELSE                                                                 
047600       IF WS-IDNY > WS-IDGAM                                              
047800         PERFORM S02-LAES-W27147GAM                                       
048000       ELSE                                                               
048100         IF W27147NY-SLAG-KVPB-REF = W27147GAM-SLAG-KVPB-REF              
048300           AND W27147NY-SLAG-RESEASON (1)  = 1.00                         
048400           AND W27147NY-SLAG-RESEASON (2)  = 1.00                         
048500           AND W27147NY-SLAG-RESEASON (3)  = 1.00                         
048600           AND W27147NY-SLAG-RESEASON (4)  = 1.00                         
048700           AND W27147NY-SLAG-RESEASON (5)  = 1.00                         
048800           AND W27147NY-SLAG-RESEASON (6)  = 1.00                         
048900           AND W27147NY-SLAG-RESEASON (7)  = 1.00                         
049000           AND W27147NY-SLAG-RESEASON (8)  = 1.00                         
049100           AND W27147NY-SLAG-RESEASON (9)  = 1.00                         
049200           AND W27147NY-SLAG-RESEASON (10) = 1.00                         
049300           AND W27147NY-SLAG-RESEASON (11) = 1.00                         
049400           AND W27147NY-SLAG-RESEASON (12) = 1.00                         
049500            CONTINUE                                                      
049600         ELSE                                                             
049800*           SELEKTERA UT ALLA SÄSONGSARTIKLAR                             
049900*           SAMT DE DÄR PROGNOS FÖRÄNDRATS SEDAN GÅRDAGEN                 
051100            PERFORM C-SKRIV-UTPOST                                        
051200         END-IF                                                           
051300         PERFORM S01-LAES-W27147NY                                        
051400         PERFORM S02-LAES-W27147GAM                                       
051600       END-IF                                                             
051700     END-IF                                                               
051800     .                                                                    
051900     EJECT                                                                
052000 C-SKRIV-UTPOST SECTION.                                                  
052200     MOVE W27147NY-SLAG-IDDC TO WS-IDDC                                   
052300     EVALUATE TRUE                                                        
052400        WHEN LDC                                                          
052500           PERFORM S03-SKRIV-LDC-W27148                                   
052600        WHEN SDC                                                          
052700           PERFORM S04-SKRIV-SDC-W27148                                   
052800        WHEN NDC                                                          
052810           IF W27147NY-SLAG-IDDC-REF (1:1) = '7'                          
052900             PERFORM S06-SKRIV-CHN-W27148                                 
052910           ELSE                                                           
053000             PERFORM S05-SKRIV-NDC-W27148                                 
053100           END-IF                                                         
058000     END-EVALUATE                                                         
058100     .                                                                    
058200     EJECT                                                                
058300                                                                          
058400 Z-FINIT SECTION.                                                         
061600     CLOSE W27147NY                                                       
061700           W27147GAM                                                      
061800           LDC-W27148                                                     
061900           SDC-W27148                                                     
062000           NDC-W27148                                                     
062100           CHN-W27148                                                     
064700     .                                                                    
064800     EJECT                                                                
064900 S01-LAES-W27147NY SECTION.                                               
065100     READ W27147NY        INTO W27147NY-AREA                              
065200     AT END                                                               
065300        SET END-OF-W27147NY TO TRUE                                       
065400     NOT AT END                                                           
065600        MOVE W27147NY-SLAG-IDARTNR                                        
065700                          TO WS-IDNY-IDARTNR                              
065800        MOVE W27147NY-SLAG-IDDC                                           
065900                          TO WS-IDNY-IDDC                                 
066000     END-READ                                                             
066100     .                                                                    
066200     EJECT                                                                
066300 S02-LAES-W27147GAM SECTION.                                              
066500     READ W27147GAM       INTO W27147GAM-AREA                             
066600     AT END                                                               
066700        SET END-OF-W27147GAM TO TRUE                                      
066800     NOT AT END                                                           
067000        MOVE W27147GAM-SLAG-IDARTNR                                       
067100                          TO WS-IDGAM-IDARTNR                             
067200        MOVE W27147GAM-SLAG-IDDC                                          
067300                          TO WS-IDGAM-IDDC                                
067400                                                                          
067500     END-READ                                                             
067600     .                                                                    
067700     EJECT                                                                
067800 S03-SKRIV-LDC-W27148 SECTION.                                            
068000     WRITE LDC-W27148-POST FROM W27147NY-AREA                             
068200     .                                                                    
068300     EJECT                                                                
068500                                                                          
068600 S04-SKRIV-SDC-W27148 SECTION.                                            
068800     WRITE SDC-W27148-POST FROM W27147NY-AREA                             
069000     .                                                                    
069100     EJECT                                                                
069110                                                                          
069200 S05-SKRIV-NDC-W27148 SECTION.                                            
069210     WRITE NDC-W27148-POST FROM W27147NY-AREA                             
069220     .                                                                    
069230     EJECT                                                                
069300                                                                          
069400 S06-SKRIV-CHN-W27148 SECTION.                                            
069500     WRITE CHN-W27148-POST FROM W27147NY-AREA                             
069600     .                                                                    
069700     EJECT                                                                
069800                                                                          
