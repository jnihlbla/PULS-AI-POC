000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5128000.                                                
000301 AUTHOR.         ARCHANA BHAT.                                            
000401 DATE-WRITTEN.   OCT 2019.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNKTION:                                                            
000801*                                                                         
000901*       -PROGRAM READS         WDG2                                       
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
002101*          --- INPUT FROM W51262                                          
002201     SELECT W51280                     ASSIGN TO W51280D1.                
002301                                                                          
002401*          --- OUTPUT WITH CONVERTED CURRENCY                             
002501     SELECT W51281                     ASSIGN TO W51280D2.                
002601                                                                          
002701     EJECT                                                                
002801                                                                          
002901 DATA DIVISION.                                                           
003001                                                                          
003101 FILE SECTION.                                                            
003201 FD  W51280                                                               
003301     RECORDING       F                                                    
003401     BLOCK CONTAINS  0.                                                   
003501 01  IN-POST.                                                             
003601*    03  -COPY W51280    -PRE  IN-  -L.                                   
003701                                                                          
003801 FD  W51281                                                               
003901     RECORDING       F                                                    
004001     BLOCK CONTAINS  0.                                                   
004101*01  POST   -COPY W51281 -PRE  UT-  -L.                                   
004201                                                                          
004301 WORKING-STORAGE SECTION.                                                 
004401 77  IDPGM                        PIC X(8)    VALUE 'W5128000'.           
004501 77  JA                           PIC X       VALUE 'J'.                  
004601 77  NEJ                          PIC X       VALUE 'N'.                  
004701 77  FELTEXT                      PIC X(80).                              
004801 77  W51280-EOF-SW                PIC X       VALUE 'N'.                  
004901     88  END-OF-W51280                        VALUE 'J'.                  
005001 77  WS-PRARTNTO-TOT              PIC S9(11)V9(2) VALUE +0                
005101                                                   COMP-3.                
005201 77  WS-PRARTNTO                  PIC S9(11)V9(2) VALUE +0                
005301                                                   COMP-3.                
005400 77  W-PRKURS                     PIC S9(6)V9(5) VALUE +0                 
005500                                                   COMP-3.                
005601 77  W-REVALUTA                   PIC S9(5) COMP-3 VALUE ZERO.            
005701 77  WS-SAVE-KDVALISO             PIC X(3)         VALUE SPACE.           
005800 77  WS-ACTUAL-DATE               PIC S9(16) COMP-3 VALUE ZERO.           
005900 77  WS-ACTUAL-DATE-X             PIC X(16)   VALUE ZERO.                 
006000 77  WS-DATE-DISPLAY              PIC 9(16)   VALUE ZERO.                 
006101 77  WS-SAVE-MONTH                PIC 9(4)    VALUE ZERO.                 
006102 77  W-DATE-AAMM                  PIC 9(4)    VALUE ZERO.                 
006103 77  WS-KDVALISO-HUV              PIC X(3)    VALUE 'SEK'.                
006600                                                                          
006700 01  DYNAMISKA-SUBPROGRAM.                                                
006800     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
006900     03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.           
007000     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
007100     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
007110     03  W510CURR                 PIC X(8)    VALUE 'W510CURR'.           
007200                                                                          
008401*    --- PARAMETRAR TILL ABEND                                            
008501 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
008601 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
008701 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
008801     EJECT                                                                
008901                                                                          
009001*    --- PARAMETRAR TILL POSTSUM                                          
009101*01  -COPY W0005   -PRE  POSTSUM-                                         
009201     EJECT                                                                
009202*01  -COPY W510CURR                                                       
009203     EJECT                                                                
009301                                                                          
009401 01  IN-AREA-START               PIC X(24)   VALUE                        
009501                                 'IN-AREA-START  '.                       
009601                                                                          
009701*01  AREA -COPY W51280     -PRE IN-                                       
009801     EJECT                                                                
009901                                                                          
010001 01  UT-AREA-START               PIC X(24)   VALUE                        
010101                                 'UT-AREA-START  '.                       
010201                                                                          
010301*01  AREA -COPY W51281     -PRE UT-                                       
010401     EJECT                                                                
010501                                                                          
013301 LINKAGE SECTION.                                                         
013401*01  -COPY W0008  -PRE 9305-                                              
013501     05  FILLER                  PIC X.                                   
013601                                                                          
013701     EJECT                                                                
013801                                                                          
013901 PROCEDURE DIVISION  USING 9305-PCB.                                      
014001                                                                          
014101 MAIN SECTION.                                                            
014201     ENTRY 'DLITCBL' USING 9305-PCB.                                      
014301                                                                          
014401     PERFORM A-INIT                                                       
014501                                                                          
014601     PERFORM S01-READ-W51280                                              
014701     PERFORM UNTIL END-OF-W51280                                          
014801       PERFORM B-CONVERT-CURRENCY                                         
014901       PERFORM S01-READ-W51280                                            
015001     END-PERFORM                                                          
015101                                                                          
015201     PERFORM Z-FINIT                                                      
015301     MOVE ZERO TO RETURN-CODE                                             
015401     GOBACK                                                               
015501     .                                                                    
015601     EJECT                                                                
015701                                                                          
015801 A-INIT SECTION.                                                          
015901     OPEN INPUT  W51280                                                   
016001     OPEN OUTPUT W51281                                                   
016002                                                                          
016003     MOVE  ZERO  TO  UT-IDLOPNRM                                          
016101     .                                                                    
016201     EJECT                                                                
016301                                                                          
016401 B-CONVERT-CURRENCY SECTION.                                              
016501                                                                          
016601     PERFORM BA-GET-EXCHRATE                                              
016701                                                                          
016801     COMPUTE WS-PRARTNTO   ROUNDED =                                      
016901             IN-PRARTNTO * IN-KVAVIS                                      
017001     COMPUTE WS-PRARTNTO-TOT  ROUNDED =                                   
017101             WS-PRARTNTO / (W-PRKURS / W-REVALUTA)                        
017201                                                                          
017301     MOVE IN-IDPTYP               TO UT-IDPTYP                            
017401     MOVE IN-IDARTNR              TO UT-IDARTNR                           
017501     MOVE IN-IDDC                 TO UT-IDDC                              
017601     MOVE IN-KVAVIS               TO UT-KVAVIS                            
017701     MOVE IN-PRARTNTO             TO UT-PRARTNTO                          
017801     MOVE WS-PRARTNTO             TO UT-PRARTNTO-SEK                      
017901     MOVE W-PRKURS                TO UT-PRKURS                            
018001     MOVE WS-PRARTNTO-TOT         TO UT-SUNTO-TOT                         
018101     MOVE IN-KDVALISO             TO UT-KDVALISO                          
018201     MOVE IN-KDTRADP              TO UT-KDTRADP                           
018202     MOVE IN-IDLEVNR              TO UT-IDLEVNR                           
018203     MOVE IN-IDFAKT               TO UT-IDFAKT                            
018301     COMPUTE WS-DATE-DISPLAY   = 9999999999999999                         
018401                                  - IN-DAINLEV                            
018501     MOVE WS-DATE-DISPLAY(1:8)     TO UT-DAREGDAT                         
018601     PERFORM S02-WRITE-W51281                                             
018701     .                                                                    
018801     EJECT                                                                
018901                                                                          
019001 BA-GET-EXCHRATE SECTION.                                                 
019101                                                                          
019201     COMPUTE WS-ACTUAL-DATE    = 9999999999999999                         
019301                                  - IN-DAINLEV                            
019401     MOVE WS-ACTUAL-DATE           TO WS-ACTUAL-DATE-X                    
019501     MOVE WS-ACTUAL-DATE-X(3:4)    TO W-DATE-AAMM                         
019601                                                                          
019701     IF IN-KDVALISO = WS-SAVE-KDVALISO                                    
019801     AND W-DATE-AAMM = WS-SAVE-MONTH                                      
019901       CONTINUE                                                           
020001     ELSE                                                                 
020101       MOVE IN-KDVALISO           TO WS-SAVE-KDVALISO                     
020102       MOVE W-DATE-AAMM           TO CURR-TIAAMM                          
020103                                     WS-SAVE-MONTH                        
020104       MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                    
020105       MOVE IN-KDVALISO           TO CURR-KDVALISO-ROW                    
020106       MOVE 'M'                   TO CURR-KDVALTYP                        
020107       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
020108       IF CURR-KDSVAR = ' '                                               
020109         MOVE CURR-PRKURS-NEW     TO W-PRKURS                             
020110         MOVE CURR-REVALUTA-TO    TO W-REVALUTA                           
020120       ELSE                                                               
020130         MOVE 1                   TO W-PRKURS                             
020140         MOVE 1                   TO W-REVALUTA                           
020150       END-IF                                                             
020201                                                                          
021701     END-IF                                                               
021801     .                                                                    
021901     EJECT                                                                
022001 Z-FINIT SECTION.                                                         
022101     CLOSE W51280                                                         
022201           W51281                                                         
022301     MOVE 'S' TO POSTSUM-OPKOD                                            
022401     CALL POSTSUM USING POSTSUM-PARM                                      
022501     .                                                                    
022601     EJECT                                                                
022801                                                                          
022901 S01-READ-W51280  SECTION.                                                
023001     READ W51280 INTO IN-AREA                                             
023101     AT END                                                               
023201        SET END-OF-W51280 TO TRUE                                         
023301                                                                          
023401     NOT AT END                                                           
023501        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
023601        MOVE 'W51280'     TO POSTSUM-FDNAMN                               
023701        MOVE 'W51280D1'   TO POSTSUM-DDNAMN2                              
023801        CALL POSTSUM USING POSTSUM-PARM                                   
023901     END-READ                                                             
024001     .                                                                    
024101                                                                          
024201 S02-WRITE-W51281 SECTION.                                                
024301     WRITE UT-POST              FROM UT-AREA                              
024401                                                                          
024501     MOVE 'UT'                  TO POSTSUM-TRANSTYP                       
024601     MOVE 'W51281'              TO POSTSUM-FDNAMN                         
024701     MOVE 'W51280D2'            TO POSTSUM-DDNAMN2                        
024801     CALL POSTSUM USING POSTSUM-PARM                                      
024901     .                                                                    
025001                                                                          
