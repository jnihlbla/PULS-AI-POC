000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5122900.                                                
000301 AUTHOR.         BARSHARANI BISHOYE.                                      
000401 DATE-WRITTEN.   DECENBER 2019.                                           
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNKTION:                                                            
000801*                                                                         
000901*       -PROGRAMMET LÄSER      WDG2                                       
001001*                                                                         
001101*    ABENDKODER:                                                          
001201*        U0016 -  . . . .                                                 
001301*        U1000 -  . . . .                                                 
001401*                                                                         
001501                                                                          
001601 ENVIRONMENT DIVISION.                                                    
001701                                                                          
001801 INPUT-OUTPUT SECTION.                                                    
001901                                                                          
002001 FILE-CONTROL.                                                            
002101*          --- INPUT FROM W5126B                                          
002201     SELECT W5126B                     ASSIGN TO W51229D1.                
002301                                                                          
002401*          --- OUTPUT WITH INR CNY KRW CURRENCY                           
002501     SELECT W51229                     ASSIGN TO W51229D2.                
002601                                                                          
002701     EJECT                                                                
002801                                                                          
002901 DATA DIVISION.                                                           
003001                                                                          
003101 FILE SECTION.                                                            
003201 FD  W5126B                                                               
003301     RECORDING       F                                                    
003401     BLOCK CONTAINS  0.                                                   
003501*01  -COPY WDR801    -PRE  IN-  -L.                                       
003601                                                                          
003701 FD  W51229                                                               
003801     RECORDING       F                                                    
003901     BLOCK CONTAINS  0.                                                   
004001*01  POST   -COPY W51229 -PRE  UT-  -L.                                   
004101                                                                          
004201 WORKING-STORAGE SECTION.                                                 
004301 77  IDPGM                        PIC X(8)    VALUE 'W5122900'.           
004401 77  JA                           PIC X       VALUE 'J'.                  
004501 77  NEJ                          PIC X       VALUE 'N'.                  
004601 77  FELTEXT                      PIC X(80).                              
004701 77  W-PRKURS                     PIC S9(6)V9(5) VALUE +0                 
004801                                                 COMP-3.                  
004802 77  WS-PRARTNTO                  PIC S9(11)V9(2) VALUE +0                
004803                                                  COMP-3.                 
004901 77  WS-SAVE-KDVALISO             PIC X(3)    VALUE SPACE.                
005000 77  WS-ACTUAL-DATE               PIC S9(16)  COMP-3 VALUE ZERO.          
005100 77  WS-ACTUAL-DATE-X             PIC X(16)   VALUE ZERO.                 
005200 77  W5126B-EOF-SW                PIC X       VALUE 'N'.                  
005300     88  END-OF-W5126B                        VALUE 'J'.                  
005400 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
005401 77  WS-SAVE-MONTH                PIC 9(4)    VALUE ZERO.                 
005402 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
005403                                                                          
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
006300     03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.           
006400     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
006500     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
006510     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
006600                                                                          
007801*    --- PARAMETRAR TILL ABEND                                            
007901 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
008001 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
008101 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
008201     EJECT                                                                
008301                                                                          
008401*    --- PARAMETRAR TILL POSTSUM                                          
008501*01  -COPY W0005   -PRE  POSTSUM-                                         
008601     EJECT                                                                
008602*01  -COPY W510CURR                                                       
008603     EJECT                                                                
008701                                                                          
008801 01  IN-AREA-START               PIC X(24)   VALUE                        
008901                                 'IN-AREA-START  '.                       
009001                                                                          
009101*01  AREA -COPY WDR801     -PRE IN-                                       
009201*    05   -COPY W510EKHA   -PRE IN- -RED IN-FIL-WDR801-DATA               
009301     EJECT                                                                
009401                                                                          
009501 01  UT-AREA-START               PIC X(24)   VALUE                        
009601                                 'UT-AREA-START  '.                       
009701                                                                          
009801*01  AREA -COPY W51229     -PRE UT-                                       
009901     EJECT                                                                
010001                                                                          
012801 LINKAGE SECTION.                                                         
012901*01  -COPY W0008  -PRE 9305-                                              
013001     05  FILLER                  PIC X.                                   
013101                                                                          
013201     EJECT                                                                
013301                                                                          
013401 PROCEDURE DIVISION  USING 9305-PCB.                                      
013501                                                                          
013601 MAIN SECTION.                                                            
013701     ENTRY 'DLITCBL' USING 9305-PCB.                                      
013801                                                                          
013901     PERFORM A-INIT                                                       
014001                                                                          
014101     PERFORM S01-READ-W5126B                                              
014201     PERFORM UNTIL END-OF-W5126B                                          
014301       PERFORM B-CONVERT-CURRENCY                                         
014401       PERFORM S01-READ-W5126B                                            
014501     END-PERFORM                                                          
014601                                                                          
014701     PERFORM Z-FINIT                                                      
014801     MOVE ZERO TO RETURN-CODE                                             
014901     GOBACK                                                               
015001     .                                                                    
015101     EJECT                                                                
015201                                                                          
015301 A-INIT SECTION.                                                          
015401     OPEN INPUT  W5126B                                                   
015501     OPEN OUTPUT W51229                                                   
015601     .                                                                    
015701     EJECT                                                                
015801                                                                          
015901 B-CONVERT-CURRENCY SECTION.                                              
016001      PERFORM BA-GET-EXCHRATE                                             
016101                                                                          
016201      COMPUTE WS-PRARTNTO  ROUNDED =                                      
016301              IN-EKH-SUBEL / W-PRKURS                                     
016401      MOVE IN-EKH-IDDC-REC         TO UT-IDDC-REC                         
016501      MOVE IN-EKH-IDDC-SEND        TO UT-IDDC-SEND                        
016601      MOVE IN-EKH-IDVERGL          TO UT-IDVERGL                          
016701      MOVE IN-EKH-KDEKHHT          TO UT-KDEKHHT                          
016801      MOVE IN-EKH-KDEKSHT          TO UT-KDEKSHT                          
016901      MOVE IN-EKH-KDEKNIVA         TO UT-KDEKNIVA                         
017001      MOVE IN-EKH-DAVERDAT         TO UT-DAINLEV                          
017101      MOVE IN-EKH-KDVALISO         TO UT-KDVALISO                         
017201      MOVE WS-PRARTNTO             TO UT-SUNTO-TOT                        
017301      MOVE IN-EKH-KDTRADP          TO UT-KDTRADP                          
017401      PERFORM S02-WRITE-W51229                                            
017501     .                                                                    
017601     EJECT                                                                
017701                                                                          
017801 BA-GET-EXCHRATE SECTION.                                                 
017901                                                                          
018001     IF  IN-EKH-KDVALISO = WS-SAVE-KDVALISO                               
018101     AND IN-EKH-DAVERDAT(3:4) = W-DATE-AAMM                               
018201       CONTINUE                                                           
018301     ELSE                                                                 
018401       MOVE IN-EKH-KDVALISO          TO WS-SAVE-KDVALISO                  
018501                                                                          
018601       MOVE IN-EKH-DAVERDAT(3:4)     TO W-DATE-AAMM                       
018602       MOVE W-DATE-AAMM              TO CURR-TIAAMM                       
019702       MOVE WS-KDVALISO-HUV          TO CURR-KDVALISO-HUV                 
019703       MOVE IN-EKH-KDVALISO          TO CURR-KDVALISO-ROW                 
019704       MOVE 'M'                      TO CURR-KDVALTYP                     
019705       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
019706       IF CURR-KDSVAR = ' '                                               
019707         MOVE CURR-PRKURS-NEW     TO W-PRKURS                             
019709       ELSE                                                               
019710         MOVE 1                   TO W-PRKURS                             
019730       END-IF                                                             
019801     END-IF                                                               
019901     .                                                                    
020001     EJECT                                                                
020101 Z-FINIT SECTION.                                                         
020201     CLOSE W5126B                                                         
020301     CLOSE W51229                                                         
020401     MOVE 'S' TO POSTSUM-OPKOD                                            
020501     CALL POSTSUM USING POSTSUM-PARM                                      
020601     .                                                                    
020701     EJECT                                                                
020801                                                                          
021001                                                                          
021101 S01-READ-W5126B  SECTION.                                                
021201     READ W5126B INTO IN-AREA                                             
021301     AT END                                                               
021401        SET END-OF-W5126B TO TRUE                                         
021501                                                                          
021601     NOT AT END                                                           
021701        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
021801        MOVE 'W5126B'     TO POSTSUM-FDNAMN                               
021901        MOVE 'W51229D1'   TO POSTSUM-DDNAMN2                              
022001        CALL POSTSUM USING POSTSUM-PARM                                   
022101     END-READ                                                             
022201     .                                                                    
022301                                                                          
022401 S02-WRITE-W51229 SECTION.                                                
022501     WRITE UT-POST              FROM UT-AREA                              
022601                                                                          
022701     MOVE 'UT'                  TO POSTSUM-TRANSTYP                       
022801     MOVE 'W51229'              TO POSTSUM-FDNAMN                         
022901     MOVE 'W51229D2'            TO POSTSUM-DDNAMN2                        
023001     CALL POSTSUM USING POSTSUM-PARM                                      
023101     .                                                                    
023201     SKIP3                                                                
023301                                                                          
