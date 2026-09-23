020000 ID DIVISION.                                                             
030000 PROGRAM-ID.     W4405B00.                                                
040000 AUTHOR.         BO SVENSSON.                                             
050000 DATE-WRITTEN.   03/03/27.                                                
060000 DATE-COMPILED.                                                           
070000                                                                          
080000*                                                                         
090000*    FUNKTION:                                                            
100000*        LÄSER TRANSAR FRÅN VORKÖ NY.                                     
110000*        KOLLAR HUR LÄNGE EN DEN RAD INNOM EN ORDER SOM BLEV KLAR         
120000*        SIST LEGAT.                                                      
130000*        SKICKAR EN RAD PER ORDER OCH OVAN DAGAR TILL RENSNINGPGM.        
140000*                                                                         
150000*                                                                         
160000*    ABENDKODER:                                                          
170000*        U0016 -  . . . .                                                 
180000*        U1000 -  . . . .                                                 
190000*                                                                         
200000                                                                          
210000     SKIP3                                                                
220000 ENVIRONMENT DIVISION.                                                    
230000     SKIP2                                                                
240000 INPUT-OUTPUT SECTION.                                                    
250000                                                                          
260000 FILE-CONTROL.                                                            
270100     SKIP2                                                                
270200*          --- RADER FRÅN VORKÖ NY                                        
270300     SELECT W4405B                     ASSIGN TO W4405BD1.                
270400     SKIP2                                                                
270500*          --- POSTER FRÅN VORKÖ NY TILL RENSNING                         
271000     SELECT W4405C                     ASSIGN TO W4405BD2.                
290000     EJECT                                                                
300000 DATA DIVISION.                                                           
310000     SKIP3                                                                
320000 FILE SECTION.                                                            
330100     SKIP3                                                                
330200 FD  W4405B                                                               
330300     RECORDING       F                                                    
330400     BLOCK CONTAINS  0.                                                   
330500                                                                          
330600*01  -COPY W4405B      -L.                                                
330700     SKIP3                                                                
330800 FD  W4405C                                                               
330900     RECORDING       F                                                    
331000     BLOCK CONTAINS  0.                                                   
331100                                                                          
332000*01  POST -COPY W4405B -PRE  UT-  -L.                                     
340000     EJECT                                                                
350000 WORKING-STORAGE SECTION.                                                 
360000                                                                          
370000 77  IDPGM                       PIC X(8)    VALUE 'W4405B00'.            
380000 77  JA                          PIC X       VALUE 'J'.                   
390000 77  NEJ                         PIC X       VALUE 'N'.                   
400000 77  IN-EJ-TOM-SW                PIC X       VALUE 'N'.                   
410000   88  IN-EJ-TOM                 VALUE 'J'.                               
410100                                                                          
410200 77  W4405B-EOF-SW               PIC X       VALUE 'N'.                   
411000     88  END-OF-W4405B                       VALUE 'J'.                   
420000     EJECT                                                                
430000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
440000 01  FILLER REDEFINES DAGENS-DATUM.                                       
450000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
460000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
470000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
480000     EJECT                                                                
490000 01  DYNAMISKA-SUBPROGRAM.                                                
500000*                                                                         
510000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
521000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
530000     SKIP2                                                                
540000*    --- PARAMETRAR TILL ABEND                                            
550000                                                                          
560000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
570000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
580000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
590000     SKIP2                                                                
600000 01  FELTEXT.                                                             
610000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
620000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
630100     EJECT                                                                
630200*    --- PARAMETRAR TILL POSTSUM                                          
630300*                                                                         
631000*01  -COPY W0005   -PRE  POSTSUM-                                         
650100     EJECT                                                                
650200 01  IN-AREA-START               PIC X(24)   VALUE                        
650300                                 'IN-AREA-START  '.                       
650400     SKIP2                                                                
650500                                                                          
650600*01  AREA -COPY W4405B     -PRE IN-                                       
650700     EJECT                                                                
650800 01  UT-AREA-START               PIC X(24)   VALUE                        
650900                                 'UT-AREA-START  '.                       
651000     SKIP2                                                                
651100                                                                          
652000*01  AREA -COPY W4405B     -PRE UT-                                       
653000     EJECT                                                                
654000 01  JMF-AREA-START              PIC X(24)   VALUE                        
655000                                 'JMF-AREA-START '.                       
656000     SKIP2                                                                
657000                                                                          
658000*01  AREA -COPY W4405B     -PRE JMF-                                      
660000     EJECT                                                                
670000 PROCEDURE DIVISION.                                                      
680000 MAIN SECTION.                                                            
700000     SKIP2                                                                
710000                                                                          
720000     PERFORM A-INIT                                                       
731000     PERFORM S01-LAES-W4405B                                              
731100                                                                          
732000     IF NOT END-OF-W4405B                                                 
732100       MOVE IN-IDDISTR           TO JMF-IDDISTR                           
732200       MOVE IN-IDKUNDNR          TO JMF-IDKUNDNR                          
732300       MOVE IN-IDKUNDRF          TO JMF-IDKUNDRF                          
732400       MOVE IN-TIREGDAT-URSP TO JMF-TIREGDAT-URSP                         
732500       MOVE ZERO                 TO JMF-IDARTNR                           
732600                                    JMF-TIREGTID-URSP                     
732700                                    JMF-TIREGDAT-AVV                      
732800                                    JMF-TIREGTID-AVV                      
733003       MOVE IN-KVWORKD           TO JMF-KVWORKD                           
733004       IF IN-TIKLAR = 010101                                              
733005*         VI SÄTTER TIKLAR I NOLL-POSTEN FÖR GAMLA RADER                  
733006*         FÖR ATT DE SEDAN SKALL DELETEAS I W4405C00                      
733007                                                                          
733008          MOVE IN-TIKLAR         TO JMF-TIKLAR                            
733009       ELSE                                                               
733010          MOVE ZERO              TO JMF-TIKLAR                            
733020       END-IF                                                             
733103                                                                          
733203       MOVE JA                   TO IN-EJ-TOM-SW                          
733303     END-IF                                                               
734000                                                                          
740000     PERFORM UNTIL END-OF-W4405B                                          
741000       IF  IN-IDDISTR       = JMF-IDDISTR                                 
742000       AND IN-IDKUNDNR      = JMF-IDKUNDNR                                
743000       AND IN-IDKUNDRF      = JMF-IDKUNDRF                                
744000       AND IN-TIREGDAT-URSP = JMF-TIREGDAT-URSP                           
750001         IF  IN-KVWORKD < JMF-KVWORKD                                     
760001           MOVE IN-KVWORKD TO JMF-KVWORKD                                 
761000         END-IF                                                           
761010                                                                          
761100         IF IN-TIKLAR = 010101                                            
761200            MOVE IN-TIKLAR       TO JMF-TIKLAR                            
761500         END-IF                                                           
762000                                                                          
770000         MOVE IN-AREA    TO UT-AREA                                       
771000         PERFORM S11-SKRIV-W4405C                                         
780000       ELSE                                                               
791000         MOVE JMF-AREA TO UT-AREA                                         
792000         PERFORM S11-SKRIV-W4405C                                         
792200                                                                          
793000         MOVE IN-IDDISTR         TO JMF-IDDISTR                           
794000         MOVE IN-IDKUNDNR        TO JMF-IDKUNDNR                          
795000         MOVE IN-IDKUNDRF        TO JMF-IDKUNDRF                          
796000         MOVE IN-TIREGDAT-URSP   TO JMF-TIREGDAT-URSP                     
797000         MOVE ZERO               TO JMF-IDARTNR                           
798000                                    JMF-TIREGTID-URSP                     
800000                                    JMF-TIREGDAT-AVV                      
810000                                    JMF-TIREGTID-AVV                      
810201         MOVE IN-KVWORKD         TO JMF-KVWORKD                           
810202         IF IN-TIKLAR = 010101                                            
810206            MOVE IN-TIKLAR       TO JMF-TIKLAR                            
810207         ELSE                                                             
810208            MOVE ZERO            TO JMF-TIKLAR                            
810209         END-IF                                                           
810300                                                                          
810400         MOVE IN-AREA            TO UT-AREA                               
810500         PERFORM S11-SKRIV-W4405C                                         
810600       END-IF                                                             
810700                                                                          
811000       PERFORM S01-LAES-W4405B                                            
820000     END-PERFORM                                                          
830000                                                                          
831000     IF IN-EJ-TOM                                                         
831100       MOVE JMF-AREA TO UT-AREA                                           
831200       PERFORM S11-SKRIV-W4405C                                           
832000     END-IF                                                               
840000                                                                          
850000     PERFORM Z-FINIT                                                      
860000                                                                          
870000     MOVE ZERO TO RETURN-CODE                                             
880000     GOBACK                                                               
890000     .                                                                    
900000     EJECT                                                                
910000 A-INIT SECTION.                                                          
920100                                                                          
921000     OPEN INPUT  W4405B                                                   
930100                                                                          
931000     OPEN OUTPUT W4405C                                                   
940000     SKIP2                                                                
950000     ACCEPT DAGENS-DATUM  FROM DATE                                       
961000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
970000     .                                                                    
980000     EJECT                                                                
990000 Z-FINIT SECTION.                                                         
000100     CLOSE W4405B                                                         
001000           W4405C                                                         
010100     SKIP2                                                                
010200     MOVE 'S' TO POSTSUM-OPKOD                                            
011000     CALL POSTSUM USING POSTSUM-PARM                                      
020000     .                                                                    
030100     EJECT                                                                
030200 S01-LAES-W4405B  SECTION.                                                
030300     READ W4405B INTO IN-AREA                                             
030400     AT END                                                               
030500        MOVE HIGH-VALUE TO IN-AREA                                        
030600        SET END-OF-W4405B TO TRUE                                         
030700                                                                          
030800     NOT AT END                                                           
030900        MOVE 'W4405B'   TO POSTSUM-FDNAMN                                 
031000        MOVE 'W4405BD1' TO POSTSUM-DDNAMN2                                
031300        MOVE SPACE      TO POSTSUM-TRANSTYP                               
031400        CALL POSTSUM USING POSTSUM-PARM                                   
031500     END-READ                                                             
032000     .                                                                    
040100     EJECT                                                                
040200 S11-SKRIV-W4405C SECTION.                                                
040300                                                                          
040400     WRITE UT-POST FROM UT-AREA                                           
040500                                                                          
040602     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
040700     MOVE 'W4405C'   TO POSTSUM-FDNAMN                                    
040800     MOVE 'W4405BD2' TO POSTSUM-DDNAMN2                                   
040900     CALL POSTSUM USING POSTSUM-PARM                                      
041000     .                                                                    
060000     EJECT                                                                
070000 S99-ABEND SECTION.                                                       
080000                                                                          
090100     SKIP2                                                                
090200     MOVE 'S' TO POSTSUM-OPKOD                                            
091000     CALL POSTSUM USING POSTSUM-PARM                                      
100000     CALL ABEND USING RKOD-ABEND                                          
110000     .                                                                    
