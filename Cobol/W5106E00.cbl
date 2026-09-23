000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5106E00.                                                
000300 AUTHOR.         ANDERS HENRIKSSON                                        
000400 DATE-WRITTEN.   2014-01-09                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        JUSTERAR PRISER MED VALUTAKURS                                   
001000*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300 INPUT-OUTPUT SECTION.                                                    
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- INFIL1-HÄNDELSEPOSTER                                      
002700     SELECT W5106V                     ASSIGN TO W5106ED1.                
002800     SKIP2                                                                
002900*          --- UTFIL1-KORREKTAPOSTER                                      
003000     SELECT W51063                     ASSIGN TO W5106ED2.                
003100     SKIP2                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W5106V                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY WDR901      -L.                                                
004100     SKIP3                                                                
004200 FD  W51063                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  POST -COPY WDR901 -PRE  RATT- -L.                                    
004700     SKIP3                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000*    -- CHECKED BY WY2000                                                 
006000 77  IDPGM                       PIC X(8)    VALUE 'W5106E00'.            
006100 77  JA                          PIC X       VALUE 'J'.                   
006200 77  NEJ                         PIC X       VALUE 'N'.                   
006300                                                                          
006400 77  W5106V-EOF-SW               PIC X       VALUE 'N'.                   
006500     88  END-OF-W5106V                       VALUE 'J'.                   
006600                                                                          
006700 77  WS-TOT-AMOUNT-DDI           PIC S9(9)V99  COMP-3 VALUE ZERO.         
006800 77  WS-LINE-AMOUNT-DDI          PIC S9(9)V99  COMP-3 VALUE ZERO.         
006900 77  WS-DIFF-AMOUNT-DDI          PIC S9(9)V99  COMP-3 VALUE ZERO.         
007000 77  WS-IDVERGL                  PIC X(10)   VALUE '          '.          
007100 77  WS-EKH-IDVERGL              PIC X(10)   VALUE SPACE.                 
007200     EJECT                                                                
007300                                                                          
007400 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
007500*01  FILLER  -COPY WWDIST19   -RED TEST-IDDISTR.                          
007600*01  FILLER  -COPY WWDIS134   -RED TEST-IDDISTR.                          
007700     EJECT                                                                
007800                                                                          
007900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008000 01  FILLER REDEFINES DAGENS-DATUM.                                       
008100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008400 01  W-AAAAMMDD                  PIC 9(8).                                
008500 01  WS-DAGENS-DATUM             PIC 9(8).                                
008600                                                                          
008700     EJECT                                                                
008800 01  DYNAMISKA-SUBPROGRAM.                                                
008900*                                                                         
009000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
010000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
020000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
020100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
020200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020300     SKIP2                                                                
020400 01  FELTEXT.                                                             
020500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
020600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
020700     EJECT                                                                
020800*    --- PARAMETRAR TILL DATKORT                                          
020900*                                                                         
021000 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W51063'.              
022000     SKIP2                                                                
022100 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
022200     SKIP2                                                                
022300*01  -COPY WDATKORT                                                       
022400     EJECT                                                                
022500*    --- PARAMETRAR TILL POSTSUM                                          
022600*                                                                         
022700*01  -COPY W0005   -PRE  POSTSUM-                                         
022800     EJECT                                                                
022900*01  -COPY WDATAREA                                                       
023000     EJECT                                                                
024000                                                                          
025000* ---IMS FUNKTIONSKODER----                                               
026000*01  -COPY W0003                                                          
027000     EJECT                                                                
028000                                                                          
029000 1  IN1-AREA-START              PIC X(24)   VALUE                         
030000                                 'IN1-AREA-START  '.                      
030100*01  AREA -COPY WDR901   -PRE IN1-                                        
030200*    05   -COPY W510EKHA -PRE IN1- -RED IN1-FIL-WDR901-DATA               
030300     EJECT                                                                
030400                                                                          
030500 01  RATT-AREA-START             PIC X(24)   VALUE                        
030600                                 'RATT-AREA-START  '.                     
030700*01  AREA -COPY WDR901   -PRE RATT-                                       
030800*    05   -COPY W510EKHA -PRE RATT- -RED RATT-FIL-WDR901-DATA             
030900     EJECT                                                                
031000                                                                          
032000 LINKAGE SECTION.                                                         
033000                                                                          
034000 PROCEDURE DIVISION .                                                     
034100                                                                          
034200 MAIN SECTION.                                                            
034300     ENTRY 'DLITCBL'.                                                     
034400                                                                          
034500     SKIP2                                                                
034600                                                                          
034700     PERFORM A-INIT                                                       
034800     PERFORM S01-LAES-W5106V                                              
034900     PERFORM UNTIL END-OF-W5106V                                          
035000       PERFORM BA-KONTROLLERA-POST                                        
035100       PERFORM S01-LAES-W5106V                                            
035200     END-PERFORM                                                          
035300                                                                          
035400     PERFORM Z-FINIT                                                      
035500                                                                          
035600     MOVE ZERO TO  RETURN-CODE                                            
035700     GOBACK                                                               
035800     .                                                                    
035900     EJECT                                                                
036000 A-INIT SECTION.                                                          
036100                                                                          
036200     OPEN INPUT  W5106V                                                   
036300     OPEN OUTPUT W51063                                                   
036400     SKIP2                                                                
036500     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
036600     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
036700     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
036800     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
036900     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
037000                                                                          
037100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
037200     .                                                                    
037300     EJECT                                                                
037400                                                                          
037500 BA-KONTROLLERA-POST SECTION.                                             
037600     MOVE IN1-EKH-IDDISTR TO TEST-IDDISTR                                 
037700     IF IN1-FIL-IDPGM = 'W4183000' OR 'W4263400' OR 'W4263500'            
037800        OR 'W5402000' OR 'W4183C00'                                       
037900        OR DIST19-SATS OR DIS134-BYTESRENOV                               
038000* SYSTEM W426-KRF, W54020, W41830 M. FL. SKALL GÅ DEN GAMLA VÄGEN         
039000       PERFORM BD-SKICKA-RATT-POST                                        
040000     ELSE                                                                 
050000**** NOLLSTÄLLNING AV BERÄKNING AV DDI POST MELLAN EVENT                  
060000       IF IN1-EKH-IDVERGL = WS-EKH-IDVERGL                                
061000         CONTINUE                                                         
062000       ELSE                                                               
063000           MOVE ZERO                 TO WS-TOT-AMOUNT-DDI                 
064000                                        WS-LINE-AMOUNT-DDI                
065000                                        WS-DIFF-AMOUNT-DDI                
066000         MOVE IN1-EKH-IDVERGL TO WS-EKH-IDVERGL                           
067000       END-IF                                                             
068000* HÄR GENERERAS KURSDIFF-POSTER FÖR DEALER-NET/DDI MARKNADER              
069000       IF IN1-EKH-KDEKNIVA = 'DET'                                        
069100         IF  (IN1-EKH-KDEKHHT = '102'                                     
069200         AND IN1-EKH-KDEKSHT = '145')                                     
069210         OR  (IN1-EKH-KDEKHHT = '102'                                     
069220         AND IN1-EKH-KDEKSHT = '135')                                     
069300           COMPUTE IN1-EKH-PRARTSTD ROUNDED = IN1-EKH-PRARTSTD *          
069400                                              IN1-EKH-PRKURS              
069500           COMPUTE WS-LINE-AMOUNT-DDI =  WS-LINE-AMOUNT-DDI +             
069600                                        (IN1-EKH-PRARTSTD *               
069700                                         IN1-EKH-KVANTAL)                 
069800         ELSE                                                             
069900           COMPUTE IN1-EKH-PRARTNTO ROUNDED = IN1-EKH-PRARTNTO *          
070000                                              IN1-EKH-PRKURS              
070100           END-COMPUTE                                                    
070200           COMPUTE WS-LINE-AMOUNT-DDI =  WS-LINE-AMOUNT-DDI +             
070300                                        (IN1-EKH-PRARTNTO *               
070400                                         IN1-EKH-KVANTAL)                 
070500           END-COMPUTE                                                    
070600         END-IF                                                           
070700         PERFORM BD-SKICKA-RATT-POST                                      
070800                                                                          
070900       ELSE                                                               
071000         IF IN1-EKH-KDEKNIVA = 'SUM'                                      
071100           IF  (IN1-EKH-KDEKHHT = '102'                                   
071200           AND IN1-EKH-KDEKSHT = '145')                                   
071210           OR  (IN1-EKH-KDEKHHT = '102'                                   
071220           AND IN1-EKH-KDEKSHT = '135')                                   
071300             CONTINUE                                                     
071400           ELSE                                                           
071500             COMPUTE IN1-EKH-SUVAT ROUNDED = IN1-EKH-SUVAT  *             
071600                                             IN1-EKH-PRKURS               
071700             END-COMPUTE                                                  
071800             COMPUTE IN1-EKH-SUBEL ROUNDED = IN1-EKH-SUBEL  *             
071900                                             IN1-EKH-PRKURS               
072000             END-COMPUTE                                                  
072100           END-IF                                                         
072200           PERFORM BD-SKICKA-RATT-POST                                    
072300                                                                          
072400           COMPUTE WS-TOT-AMOUNT-DDI =  IN1-EKH-SUBEL                     
072500           END-COMPUTE                                                    
072600           COMPUTE WS-DIFF-AMOUNT-DDI =  WS-TOT-AMOUNT-DDI -              
072700                                         WS-LINE-AMOUNT-DDI               
072800           END-COMPUTE                                                    
072900           IF WS-DIFF-AMOUNT-DDI NOT = ZERO                               
073000             DISPLAY '    '                                               
073100             DISPLAY 'VER=' IN1-EKH-IDVERGL                               
073200             DISPLAY 'TOT=' WS-TOT-AMOUNT-DDI                             
073300             DISPLAY 'LIN=' WS-LINE-AMOUNT-DDI                            
073400             DISPLAY 'DIF=' WS-DIFF-AMOUNT-DDI                            
073500                                                                          
073600             MOVE 'DDI'                TO IN1-EKH-KDEKNIVA                
073700             MOVE WS-DIFF-AMOUNT-DDI   TO IN1-EKH-SUBEL                   
073800             MOVE ZERO                 TO IN1-EKH-SUVAT                   
073900                                                                          
074000               PERFORM BD-SKICKA-RATT-POST                                
074100           END-IF                                                         
074200           MOVE ZERO                 TO WS-TOT-AMOUNT-DDI                 
074300                                        WS-LINE-AMOUNT-DDI                
074400                                        WS-DIFF-AMOUNT-DDI                
074500         ELSE                                                             
074600           IF  (IN1-EKH-KDEKHHT = '102'                                   
074700           AND IN1-EKH-KDEKSHT = '145')                                   
074710           OR  (IN1-EKH-KDEKHHT = '102'                                   
074720           AND IN1-EKH-KDEKSHT = '135')                                   
074800             CONTINUE                                                     
074900           ELSE                                                           
075000             COMPUTE IN1-EKH-SUBEL ROUNDED = IN1-EKH-SUBEL  *             
075100                                             IN1-EKH-PRKURS               
075200             END-COMPUTE                                                  
075300           END-IF                                                         
075400           COMPUTE WS-LINE-AMOUNT-DDI =  WS-LINE-AMOUNT-DDI +             
075500                                         IN1-EKH-SUBEL                    
075600           END-COMPUTE                                                    
075700           PERFORM BD-SKICKA-RATT-POST                                    
075800         END-IF                                                           
075900       END-IF                                                             
076000     END-IF                                                               
076100     .                                                                    
076200     EJECT                                                                
076300                                                                          
076400 BD-SKICKA-RATT-POST SECTION.                                             
076500     MOVE IN1-AREA TO RATT-AREA                                           
076600     PERFORM S11-SKRIV-RATT-POST                                          
076700     .                                                                    
076800     EJECT                                                                
076900                                                                          
077000 Z-FINIT SECTION.                                                         
077100     CLOSE W5106V                                                         
077200           W51063                                                         
077300     SKIP2                                                                
077400     MOVE 'S' TO POSTSUM-OPKOD                                            
077500     CALL POSTSUM USING POSTSUM-PARM                                      
077600     .                                                                    
077700     EJECT                                                                
077800                                                                          
077900 S01-LAES-W5106V  SECTION.                                                
078000     READ W5106V INTO IN1-AREA                                            
078100     AT END                                                               
078200        MOVE HIGH-VALUE TO IN1-AREA                                       
078300        SET END-OF-W5106V TO TRUE                                         
078400                                                                          
078500     NOT AT END                                                           
078600        MOVE 'W5106V' TO POSTSUM-FDNAMN                                   
078700        MOVE 'W5106ED1' TO POSTSUM-DDNAMN2                                
078800        MOVE 'INPOST'   TO POSTSUM-TRANSTYP                               
078900        CALL POSTSUM USING POSTSUM-PARM                                   
079000     END-READ                                                             
079100     .                                                                    
079200     EJECT                                                                
079300                                                                          
079400 S11-SKRIV-RATT-POST SECTION.                                             
079500     WRITE RATT-POST FROM RATT-AREA                                       
079600                                                                          
079700     MOVE 'GODK-POST' TO POSTSUM-TRANSTYP                                 
079800     MOVE 'W51063' TO POSTSUM-FDNAMN                                      
079900     MOVE 'W51063D2' TO POSTSUM-DDNAMN2                                   
080000     CALL POSTSUM USING POSTSUM-PARM                                      
081000     .                                                                    
082000     EJECT                                                                
