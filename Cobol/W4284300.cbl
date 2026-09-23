020000 ID DIVISION.                                                             
030000 PROGRAM-ID.     W4284300.                                                
040000 AUTHOR.         SUSANNE OLSSON.                                          
050000 DATE-WRITTEN.   02/05/17.                                                
060000 DATE-COMPILED.                                                           
070000                                                                          
080000*                                                                         
090000*    FUNKTION:                                                            
100000*        PGM RÄKNAR OM PRARTNTO-LOC TILL SEK FÖR ATT KUNNA                
110000*        TA MED DDI-KUNDER MED LOKALT PRIS I FAKTURAN I VIOS-             
120000*        UPPFÖLJNINGEN.                                                   
130014*        DETTA GÄLLER ÄVEN FÖR DEALER-NET OCH CENTRAL-PRICE KUNDER        
130114*                                                                         
131008*        PROGRAMMET LÄSER    WDG2 VALUTAKURSER H-TYP 9305                 
140008*                            WDGX9308 VIA W510CURR PGM                    
141008*                                                                         
142008*                                                                         
150000*    ABENDKODER:                                                          
160000*        U0016 -  . . . .                                                 
170000*        U1000 -  . . . .                                                 
180000*                                                                         
190000                                                                          
200000     SKIP3                                                                
210000 ENVIRONMENT DIVISION.                                                    
220000     SKIP2                                                                
230000 INPUT-OUTPUT SECTION.                                                    
240000                                                                          
250000 FILE-CONTROL.                                                            
260100     SKIP2                                                                
260200*          --- FAKTURERADE RADER UNDER PERIODEN                           
260300     SELECT W42844                     ASSIGN TO W42843D1.                
260400     SKIP2                                                                
260500*          --- FAKTURERADE RADER UNDER PERIODEN I SEK                     
261000     SELECT W42843                     ASSIGN TO W42843D2.                
280000     EJECT                                                                
290000 DATA DIVISION.                                                           
300000     SKIP3                                                                
310000 FILE SECTION.                                                            
320100     SKIP3                                                                
320200 FD  W42844                                                               
320300     RECORDING       F                                                    
320400     BLOCK CONTAINS  0.                                                   
320500                                                                          
320600*01  -COPY W47905F      -L.                                               
320700     SKIP3                                                                
320800 FD  W42843                                                               
320900     RECORDING       F                                                    
321000     BLOCK CONTAINS  0.                                                   
321100                                                                          
322000*01  POST -COPY W47905F -PRE  UT-  -L.                                    
330000     EJECT                                                                
340000 WORKING-STORAGE SECTION.                                                 
350000                                                                          
360000 77  IDPGM                       PIC X(8)    VALUE 'W4284300'.            
370000 77  JA                          PIC X       VALUE 'J'.                   
380000 77  NEJ                         PIC X       VALUE 'N'.                   
380100 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
380200 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
400100                                                                          
400200 77  W42844-EOF-SW               PIC X       VALUE 'N'.                   
401000     88  END-OF-W42844                       VALUE 'J'.                   
410000     EJECT                                                                
420000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
430000 01  FILLER REDEFINES DAGENS-DATUM.                                       
440000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
450000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
460000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
470000     EJECT                                                                
472000 01  FILLER              PIC X(16) VALUE 'TEST-IDDISTRIKT '.              
473000                                                                          
474000 01  TEST-IDDISTR        PIC 9(5) COMP-3.                                 
475000*01  FILLER   -COPY WWDIST79   -RED TEST-IDDISTR.                         
476000     EJECT                                                                
480000 01  DYNAMISKA-SUBPROGRAM.                                                
490000*                                                                         
500000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
511000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
512000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
513000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
513100     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
520000     SKIP2                                                                
520100*    --- PARAMETRAR TILL W510CURR                                         
520200*01  -COPY W510CURR                                                       
530000*    --- PARAMETRAR TILL ABEND                                            
540000                                                                          
550000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
560000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
570000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
580000     SKIP2                                                                
590000 01  FELTEXT.                                                             
600000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
610000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
620100     EJECT                                                                
620200*    --- PARAMETRAR TILL POSTSUM                                          
620300*                                                                         
621000*01  -COPY W0005   -PRE  POSTSUM-                                         
640100     EJECT                                                                
640200                                                                          
643700 01  IN-AREA-START               PIC X(24)   VALUE                        
643800                                 'IN-AREA-START  '.                       
643900     SKIP2                                                                
644000                                                                          
644100*01  AREA -COPY W47905F     -PRE IN-                                      
644200     EJECT                                                                
644300 01  UT-AREA-START               PIC X(24)   VALUE                        
644400                                 'UT-AREA-START  '.                       
644500     SKIP2                                                                
644600                                                                          
645000*01  AREA -COPY W47905F     -PRE UT-                                      
650000     EJECT                                                                
651000 LINKAGE SECTION.                                                         
652000     SKIP3                                                                
656000*01  -COPY W0008  -PRE 9305-                                              
657000     05  FILLER                  PIC X.                                   
658000     EJECT                                                                
660000 PROCEDURE DIVISION  USING 9305-PCB.                                      
660101 MAIN SECTION.                                                            
661000     ENTRY 'DLITCBL' USING 9305-PCB.                                      
690000     SKIP2                                                                
700000                                                                          
710000     PERFORM A-INIT                                                       
721000     PERFORM S01-LAES-W42844                                              
730001     PERFORM UNTIL END-OF-W42844                                          
740000                                                                          
740103       MOVE IN-AREA  TO UT-AREA                                           
740203                                                                          
740305       MOVE IN-IDDISTR   TO TEST-IDDISTR                                  
740606                                                                          
741014       IF DIST79-DEALER-PRICE OR                                          
741016          DIST79-ECOM-PRICE                                               
741101         IF IN-PRARTNTO-LOC > +0                                          
741211*          MOVE IN-KDVALISO         TO W-KDVALISO                         
741311*JOURFIX                                                                  
741411           IF IN-KDVALISO = SPACE                                         
741511              IF IN-IDDISTR = 1778                                        
741612                 MOVE 'EUR' TO CURR-KDVALISO-ROW                          
741711              END-IF                                                      
741811           ELSE                                                           
741911             MOVE IN-KDVALISO         TO CURR-KDVALISO-ROW                
742011           END-IF                                                         
743001**** LÄS PRKURS HTYP 9305               *****                             
744002           MOVE W-DATE-AAMM           TO CURR-TIAAMM                      
744003           MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                
744004           MOVE 'M'                   TO CURR-KDVALTYP                    
744005                                                                          
744006           CALL W510CURR USING CURR-W510CURR 9305-PCB                     
744007           IF CURR-KDSVAR = ' '                                           
744008             CONTINUE                                                     
744009           ELSE                                                           
744010             MOVE 1                   TO CURR-PRKURS-NEW                  
744020           END-IF                                                         
745001                                                                          
746001           COMPUTE UT-PRARTNTO  ROUNDED =                                 
747013                      IN-PRARTNTO-LOC  * CURR-PRKURS-NEW                  
749001           END-COMPUTE                                                    
750001         END-IF                                                           
760001       END-IF                                                             
770000                                                                          
780003       PERFORM S11-SKRIV-W42843                                           
790003                                                                          
801000       PERFORM S01-LAES-W42844                                            
810000     END-PERFORM                                                          
820000                                                                          
830000                                                                          
840000     PERFORM Z-FINIT                                                      
850000                                                                          
860000     MOVE ZERO TO RETURN-CODE                                             
870000     GOBACK                                                               
880000     .                                                                    
890000     EJECT                                                                
900000 A-INIT SECTION.                                                          
910100                                                                          
911000     OPEN INPUT  W42844                                                   
920100                                                                          
921000     OPEN OUTPUT W42843                                                   
930000     SKIP2                                                                
940000     ACCEPT DAGENS-DATUM  FROM DATE                                       
950016     MOVE DAGENS-DATUM-AAR    TO W-DATE-AAMM(1:2)                         
950017     MOVE DAGENS-DATUM-MAANAD TO W-DATE-AAMM(3:2)                         
951000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
960000     .                                                                    
970000     EJECT                                                                
980000 Z-FINIT SECTION.                                                         
990100     CLOSE W42844                                                         
991000           W42843                                                         
000100     SKIP2                                                                
000200     MOVE 'S' TO POSTSUM-OPKOD                                            
001000     CALL POSTSUM USING POSTSUM-PARM                                      
010000     .                                                                    
020100     EJECT                                                                
020200 S01-LAES-W42844  SECTION.                                                
020300     READ W42844 INTO IN-AREA                                             
020400     AT END                                                               
020500        MOVE HIGH-VALUE TO IN-AREA                                        
020600        SET END-OF-W42844 TO TRUE                                         
020700                                                                          
020800     NOT AT END                                                           
020901        MOVE 'W42844'    TO POSTSUM-FDNAMN                                
021001        MOVE 'W42843D1'  TO POSTSUM-DDNAMN2                               
021301        MOVE SPACE       TO POSTSUM-TRANSTYP                              
021400        CALL POSTSUM USING POSTSUM-PARM                                   
021500     END-READ                                                             
022000     .                                                                    
030100     EJECT                                                                
030200 S11-SKRIV-W42843 SECTION.                                                
030300                                                                          
030400     WRITE UT-POST FROM UT-AREA                                           
030500                                                                          
030601     MOVE SPACE    TO POSTSUM-TRANSTYP                                    
030700     MOVE 'W42843' TO POSTSUM-FDNAMN                                      
030800     MOVE 'W42843D2' TO POSTSUM-DDNAMN2                                   
030900     CALL POSTSUM USING POSTSUM-PARM                                      
031000     .                                                                    
050000     EJECT                                                                
