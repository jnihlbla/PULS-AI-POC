000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5156E00.                                                
000300 AUTHOR.         ANDERS HENRIKSSON                                        
000400 DATE-WRITTEN.   2017-11-24                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        RÄKNAR UT VALUTADIFF                                             
001000*                                                                         
001100                                                                          
001200     SKIP3                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400 INPUT-OUTPUT SECTION.                                                    
001500 FILE-CONTROL.                                                            
001600     SKIP2                                                                
001700*          --- INFIL1-HÄNDELSEPOSTER                                      
001800     SELECT W5156V                     ASSIGN TO W5156ED1.                
001900     SKIP2                                                                
002000*          --- UTFIL1-KORREKTAPOSTER                                      
002100     SELECT W51564                     ASSIGN TO W5156ED2.                
002200     SKIP2                                                                
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700 FD  W5156V                                                               
002800     RECORDING       F                                                    
002900     BLOCK CONTAINS  0.                                                   
003000                                                                          
003100*01  -COPY WDR801      -L.                                                
003200     SKIP3                                                                
003300 FD  W51564                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  POST -COPY WDR801 -PRE  RATT- -L.                                    
003800     SKIP3                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W5156E00'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004600 77  W5156V-EOF-SW               PIC X       VALUE 'N'.                   
004700     88  END-OF-W5156V                       VALUE 'J'.                   
004800                                                                          
004900 77  WS-TOT-AMOUNT-DDI           PIC S9(9)V99  COMP-3 VALUE ZERO.         
005000 77  WS-LINE-AMOUNT-DDI          PIC S9(9)V99  COMP-3 VALUE ZERO.         
005100 77  WS-DIFF-AMOUNT-DDI          PIC S9(9)V99  COMP-3 VALUE ZERO.         
005110 77  WS-SUBEL                    PIC S9(11)V99  COMP-3 VALUE ZERO.        
005120 77  WS-PRARTNTO                 PIC S9(11)V99  COMP-3 VALUE ZERO.        
005200 77  WS-IDVERGL                  PIC X(10)   VALUE '          '.          
005300 77  WS-EKH-IDVERGL              PIC X(10)   VALUE SPACE.                 
005301 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
005302 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
005303                                                                          
005340 01  WS-PRKURS-IN                 PIC S9(6)V9(5) COMP-3.                  
005360 01  WS-PRKURS-IN3                PIC S9(6)V9(5) COMP-3.                  
005370 77  WS-TIAA                      PIC S9(2)   VALUE ZERO.                 
005380 77  WS-TIMM                      PIC S9(2)   VALUE ZERO.                 
005390 77  WS-TIAA-CR                   PIC S9(2)   VALUE ZERO.                 
005391 77  WS-TIMM-CR                   PIC S9(2)   VALUE ZERO.                 
005400     EJECT                                                                
006000                                                                          
006100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006200 01  FILLER REDEFINES DAGENS-DATUM.                                       
006300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006600 01  W-AAAAMMDD                  PIC 9(8).                                
006700 01  WS-DAGENS-DATUM             PIC 9(8).                                
006800                                                                          
006900     EJECT                                                                
007000 01  DYNAMISKA-SUBPROGRAM.                                                
007100*                                                                         
007200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007610     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
007700     SKIP2                                                                
007800 01  FELTEXT.                                                             
007900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL DATKORT                                          
008300*                                                                         
008400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W5156E'.              
008500     SKIP2                                                                
008600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
008700     SKIP2                                                                
008800*01  -COPY WDATKORT                                                       
008900     EJECT                                                                
008901                                                                          
008910*01  -COPY W510CURR                                                       
008920     EJECT                                                                
009000*    --- PARAMETRAR TILL POSTSUM                                          
009100*                                                                         
009200*01  -COPY W0005   -PRE  POSTSUM-                                         
009300     EJECT                                                                
009400*01  -COPY WDATAREA                                                       
009500     EJECT                                                                
009600                                                                          
009601 01 IN1-AREA-START              PIC X(24)   VALUE                         
009602                                 'IN1-AREA-START  '.                      
009603*01  AREA -COPY WDR801   -PRE IN1-                                        
009604*    05   -COPY W510EKHA -PRE IN1- -RED IN1-FIL-WDR801-DATA               
009605     EJECT                                                                
009606                                                                          
009607 01  RATT-AREA-START             PIC X(24)   VALUE                        
009608                                 'RATT-AREA-START  '.                     
009609*01  AREA -COPY WDR801   -PRE RATT-                                       
009610*    05   -COPY W510EKHA -PRE RATT- -RED RATT-FIL-WDR801-DATA             
009611     EJECT                                                                
009612                                                                          
011200                                                                          
011300 LINKAGE SECTION.                                                         
011310*01  -COPY W0008  -PRE WDG2-                                              
011320     05  FILLER                  PIC X.                                   
011330                                                                          
011400                                                                          
011500 PROCEDURE DIVISION  USING WDG2-PCB.                                      
011600                                                                          
011700 MAIN SECTION.                                                            
011800     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
011900                                                                          
012200     PERFORM A-INIT                                                       
012300     PERFORM S01-LAES-W5156V                                              
012400     PERFORM UNTIL END-OF-W5156V                                          
012410       PERFORM S02-GET-CURRENCY                                           
012500       PERFORM BA-KONTROLLERA-POST                                        
012600       PERFORM S01-LAES-W5156V                                            
012700     END-PERFORM                                                          
012800                                                                          
012900     PERFORM Z-FINIT                                                      
013000                                                                          
013100     MOVE ZERO TO  RETURN-CODE                                            
013200     GOBACK                                                               
013300     .                                                                    
013400     EJECT                                                                
013410                                                                          
013500 A-INIT SECTION.                                                          
013700     OPEN INPUT  W5156V                                                   
013800     OPEN OUTPUT W51564                                                   
013900     SKIP2                                                                
014000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
014100     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
014110                        W-DATE-AAMM(1:2)                                  
014120                        WS-TIAA                                           
014200     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
014210                        W-DATE-AAMM(3:2)                                  
014220                        WS-TIMM                                           
014300     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
014400     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
014500                                                                          
014600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014601                                                                          
014740     .                                                                    
014800     EJECT                                                                
014900                                                                          
015000 BA-KONTROLLERA-POST SECTION.                                             
015700     IF  (IN1-EKH-KDEKHHT = '102'                                         
015710     AND IN1-EKH-KDEKSHT = '120')                                         
015720     OR  (IN1-EKH-KDEKHHT = '102'                                         
015730     AND IN1-EKH-KDEKSHT = '124')                                         
015731     OR  (IN1-EKH-KDEKHHT = '102'                                         
015732     AND IN1-EKH-KDEKSHT = '130')                                         
015733     OR  (IN1-EKH-KDEKHHT = '102'                                         
015734     AND IN1-EKH-KDEKSHT = '134')                                         
015735     OR  (IN1-EKH-KDEKHHT = '102'                                         
015736     AND IN1-EKH-KDEKSHT = '125')                                         
015740     OR  (IN1-EKH-KDEKHHT = '303'                                         
015750     AND IN1-EKH-KDEKSHT = '3XX')                                         
015760     OR  (IN1-EKH-KDEKHHT = '303'                                         
015770     AND IN1-EKH-KDEKSHT = '301')                                         
015780     OR  (IN1-EKH-KDEKHHT = '303'                                         
015790     AND IN1-EKH-KDEKSHT = '307')                                         
015800**** NOLLSTÄLLNING AV BERÄKNING AV DDI POST MELLAN EVENT                  
015900       IF IN1-EKH-IDVERGL = WS-EKH-IDVERGL                                
016000         CONTINUE                                                         
016100       ELSE                                                               
016200         MOVE ZERO            TO WS-TOT-AMOUNT-DDI                        
016300                                 WS-LINE-AMOUNT-DDI                       
016400                                 WS-DIFF-AMOUNT-DDI                       
016500         MOVE IN1-EKH-IDVERGL TO WS-EKH-IDVERGL                           
016600       END-IF                                                             
016700* HÄR GENERERAS KURSDIFF-POSTER FÖR DEALER-NET/DDI MARKNADER              
016800       IF IN1-EKH-KDEKNIVA = 'DET'                                        
016900         COMPUTE IN1-EKH-PRARTNTO ROUNDED = (IN1-EKH-PRARTNTO *           
017000                  IN1-EKH-KVANTAL) / WS-PRKURS-IN3                        
017200         COMPUTE WS-LINE-AMOUNT-DDI =  WS-LINE-AMOUNT-DDI +               
017300                                        IN1-EKH-PRARTNTO                  
017600         PERFORM BD-SKICKA-RATT-POST                                      
017700                                                                          
017800       ELSE                                                               
017900         IF IN1-EKH-KDEKNIVA = 'SUM'                                      
018300           COMPUTE IN1-EKH-SUBEL ROUNDED = IN1-EKH-SUBEL  /               
018400                                           WS-PRKURS-IN3                  
018600           PERFORM BD-SKICKA-RATT-POST                                    
018700                                                                          
018800           COMPUTE WS-TOT-AMOUNT-DDI =  IN1-EKH-SUBEL                     
019000           COMPUTE WS-DIFF-AMOUNT-DDI =  WS-TOT-AMOUNT-DDI -              
019100                                         WS-LINE-AMOUNT-DDI               
019300           IF WS-DIFF-AMOUNT-DDI NOT = ZERO                               
019400             DISPLAY '    '                                               
019500             DISPLAY 'VER=' IN1-EKH-IDVERGL                               
019600             DISPLAY 'TOT=' WS-TOT-AMOUNT-DDI                             
019700             DISPLAY 'LIN=' WS-LINE-AMOUNT-DDI                            
019800             DISPLAY 'DIF=' WS-DIFF-AMOUNT-DDI                            
019900                                                                          
020000             MOVE 'DDI'                TO IN1-EKH-KDEKNIVA                
020100             MOVE WS-DIFF-AMOUNT-DDI   TO IN1-EKH-SUBEL                   
020200             MOVE ZERO                 TO IN1-EKH-SUVAT                   
020300                                                                          
020400               PERFORM BD-SKICKA-RATT-POST                                
020500           END-IF                                                         
020600           MOVE ZERO                 TO WS-TOT-AMOUNT-DDI                 
020700                                        WS-LINE-AMOUNT-DDI                
020800                                        WS-DIFF-AMOUNT-DDI                
020900         ELSE                                                             
021000           COMPUTE IN1-EKH-SUBEL ROUNDED = IN1-EKH-SUBEL  /               
021100                                           WS-PRKURS-IN3                  
021200           END-COMPUTE                                                    
021300           COMPUTE WS-LINE-AMOUNT-DDI =  WS-LINE-AMOUNT-DDI +             
021400                                         IN1-EKH-SUBEL                    
021500           END-COMPUTE                                                    
021600           PERFORM BD-SKICKA-RATT-POST                                    
021700         END-IF                                                           
021800       END-IF                                                             
021810     ELSE                                                                 
021811       IF  (IN1-EKH-KDEKHHT = '103'                                       
021812       AND IN1-EKH-KDEKSHT = '102')                                       
021817         IF IN1-EKH-IDVERGL = WS-EKH-IDVERGL                              
021818           CONTINUE                                                       
021819         ELSE                                                             
021820           MOVE ZERO            TO WS-TOT-AMOUNT-DDI                      
021821                                   WS-LINE-AMOUNT-DDI                     
021822                                   WS-DIFF-AMOUNT-DDI                     
021823                                   WS-SUBEL                               
021824                                   WS-PRARTNTO                            
021825           MOVE IN1-EKH-IDVERGL TO WS-EKH-IDVERGL                         
021826         END-IF                                                           
021827         IF IN1-EKH-KDEKNIVA = 'DET'                                      
021828           COMPUTE WS-PRARTNTO ROUNDED =                                  
021829                   IN1-EKH-KVANTAL * IN1-EKH-PRARTSTD                     
021830           COMPUTE WS-LINE-AMOUNT-DDI =  WS-LINE-AMOUNT-DDI +             
021831                                      WS-PRARTNTO                         
021832           PERFORM BD-SKICKA-RATT-POST                                    
021833         ELSE                                                             
021834* HÄR GENERERAS KURSDIFF-POSTER FÖR DEALER-NET/DDI MARKNADER              
021835           IF IN1-EKH-KDEKNIVA = 'SUM'                                    
021836             COMPUTE WS-SUBEL ROUNDED = IN1-EKH-SUBEL *                   
021837                                        IN1-EKH-PRKURS                    
021838             PERFORM BD-SKICKA-RATT-POST                                  
021839                                                                          
021840             COMPUTE WS-TOT-AMOUNT-DDI =  WS-SUBEL                        
021841             COMPUTE WS-DIFF-AMOUNT-DDI =  WS-TOT-AMOUNT-DDI -            
021842                                           WS-LINE-AMOUNT-DDI             
021843             IF WS-DIFF-AMOUNT-DDI NOT = ZERO                             
021844               MOVE 'DDI'                TO IN1-EKH-KDEKNIVA              
021845               MOVE WS-DIFF-AMOUNT-DDI   TO IN1-EKH-SUBEL                 
021846               MOVE ZERO                 TO IN1-EKH-SUVAT                 
021847                                                                          
021848               PERFORM BD-SKICKA-RATT-POST                                
021849             END-IF                                                       
021850             MOVE ZERO                 TO WS-TOT-AMOUNT-DDI               
021851                                          WS-LINE-AMOUNT-DDI              
021852                                          WS-DIFF-AMOUNT-DDI              
021853                                          WS-SUBEL                        
021854                                          WS-PRARTNTO                     
021855           ELSE                                                           
021856             PERFORM BD-SKICKA-RATT-POST                                  
021857           END-IF                                                         
021858         END-IF                                                           
021859       ELSE                                                               
021860           PERFORM BD-SKICKA-RATT-POST                                    
021861       END-IF                                                             
021900     END-IF                                                               
022000     .                                                                    
022100     EJECT                                                                
022200                                                                          
022300 BD-SKICKA-RATT-POST SECTION.                                             
022400     MOVE IN1-AREA TO RATT-AREA                                           
022500     PERFORM S11-SKRIV-RATT-POST                                          
022600     .                                                                    
022700     EJECT                                                                
022800                                                                          
022900 Z-FINIT SECTION.                                                         
023000     CLOSE W5156V                                                         
023100           W51564                                                         
023200     SKIP2                                                                
023300     MOVE 'S' TO POSTSUM-OPKOD                                            
023400     CALL POSTSUM USING POSTSUM-PARM                                      
023500     .                                                                    
023600     EJECT                                                                
023700                                                                          
023800 S01-LAES-W5156V  SECTION.                                                
023900     READ W5156V INTO IN1-AREA                                            
024000     AT END                                                               
024100        MOVE HIGH-VALUE TO IN1-AREA                                       
024200        SET END-OF-W5156V TO TRUE                                         
024300                                                                          
024400     NOT AT END                                                           
024500        MOVE 'W5156V' TO POSTSUM-FDNAMN                                   
024600        MOVE 'W5156ED1' TO POSTSUM-DDNAMN2                                
024700        MOVE 'INPOST'   TO POSTSUM-TRANSTYP                               
024800        CALL POSTSUM USING POSTSUM-PARM                                   
024900     END-READ                                                             
025000     .                                                                    
025100     EJECT                                                                
025200                                                                          
025210 S02-GET-CURRENCY SECTION.                                                
025220     IF IN1-FIL-IDPGM = 'W4183300'                                        
025230       IF IN1-EKH-DAAVIDAT > ZERO                                         
025240         MOVE IN1-EKH-DAAVIDAT(5:2) TO WS-TIAA-CR                         
025250         MOVE IN1-EKH-DAAVIDAT(7:2) TO WS-TIMM-CR                         
025260       ELSE                                                               
025270         MOVE WS-TIAA              TO WS-TIAA-CR                          
025280         MOVE WS-TIMM              TO WS-TIMM-CR                          
025290       END-IF                                                             
025291     ELSE                                                                 
025292       MOVE WS-TIAA                TO WS-TIAA-CR                          
025293       MOVE WS-TIMM                TO WS-TIMM-CR                          
025294     END-IF                                                               
025295     MOVE WS-TIAA-CR               TO W-DATE-AAMM(1:2)                    
025296     MOVE WS-TIMM-CR               TO W-DATE-AAMM(3:2)                    
025303     MOVE 'INR'                 TO CURR-KDVALISO-ROW                      
025304     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
025305     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
025306     MOVE 'M'                   TO CURR-KDVALTYP                          
025307     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
025308     IF CURR-KDSVAR = ' '                                                 
025309       MOVE CURR-PRKURS-NEW     TO WS-PRKURS-IN                           
025310     ELSE                                                                 
025311       MOVE 1                   TO WS-PRKURS-IN                           
025312     END-IF                                                               
025313     MOVE WS-PRKURS-IN          TO WS-PRKURS-IN3                          
025315     .                                                                    
025316     EJECT                                                                
025320 S11-SKRIV-RATT-POST SECTION.                                             
025400     WRITE RATT-POST FROM RATT-AREA                                       
025500                                                                          
025600     MOVE 'GODK-POST' TO POSTSUM-TRANSTYP                                 
025700     MOVE 'W51564' TO POSTSUM-FDNAMN                                      
025800     MOVE 'W5156ED2' TO POSTSUM-DDNAMN2                                   
025900     CALL POSTSUM USING POSTSUM-PARM                                      
026000     .                                                                    
026100     EJECT                                                                
026101                                                                          
