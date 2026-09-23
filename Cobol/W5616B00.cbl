000100 ID DIVISION.                                                             
000201 PROGRAM-ID.    W5616B00.                                                 
000301                                                                          
000401*    AUTHOR.        ARCHANA BHAT.                                         
000501*    DATE-WRITTEN   NOVEMBER 2017.                                        
000601*                                                                         
000701*    FUNKTION:                                                            
000801*    MATCHA TVÅ FILER OCH SELECTERA UT HÄNDELSE 102-121                   
000802*    MATCHA TVÅ FILER OCH SELECTERA UT HÄNDELSE 102-131                   
000901     EJECT                                                                
001001 ENVIRONMENT DIVISION.                                                    
001101                                                                          
001201 INPUT-OUTPUT SECTION.                                                    
001301 FILE-CONTROL.                                                            
001401                                                                          
001501     SELECT W5104A  ASSIGN       TO W5616BD1.                             
001601                                                                          
001701     SELECT W56161A ASSIGN       TO W5616BD2.                             
001801                                                                          
001901     SELECT W56161  ASSIGN       TO W5616BD3.                             
002001                                                                          
002101     SELECT W5616A  ASSIGN       TO W5616BD4.                             
002201                                                                          
002301     EJECT                                                                
002401 DATA DIVISION.                                                           
002501 FILE SECTION.                                                            
002601                                                                          
002701 FD  W5104A                                                               
002801     RECORDING F                                                          
002901     BLOCK CONTAINS 0.                                                    
003001                                                                          
003101 01  INPOST1.                                                             
003201*    03   -COPY W56160    -L                                              
003301     SKIP2                                                                
003401                                                                          
003501 FD  W56161A                                                              
003601     RECORDING F                                                          
003701     BLOCK CONTAINS 0.                                                    
003801                                                                          
003901 01  INPOST2.                                                             
004001*    03   -COPY W56160    -L                                              
004101     SKIP2                                                                
004201                                                                          
004301 FD  W56161                                                               
004401     RECORDING F                                                          
004501     BLOCK CONTAINS 0.                                                    
004601                                                                          
004701*01  UTPOST1     -COPY W56160    -L                                       
004801     SKIP2                                                                
004901                                                                          
005001 FD  W5616A                                                               
005101     RECORDING F                                                          
005201     BLOCK CONTAINS 0.                                                    
005301                                                                          
005401*01  UTPOST2     -COPY W56160    -L                                       
005501     SKIP2                                                                
005601                                                                          
005701 WORKING-STORAGE SECTION.                                                 
005801     SKIP3                                                                
005901 77  IDPGM                   PIC X(8)      VALUE 'W5616B00'.              
006001 77  JA                      PIC X         VALUE 'J'.                     
006101 77  NEJ                     PIC X         VALUE 'N'.                     
006201 77  FELTEXT                 PIC X(80).                                   
006301 77  EOF-W5104A              PIC X         VALUE 'N'.                     
006401 77  EOF-W56161A             PIC X         VALUE 'N'.                     
006501 77  WS-IX                   PIC S9(3)   COMP-3 VALUE ZERO.               
006601                                                                          
006701 01  SUBPROGRAM.                                                          
006801     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
006901     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
007001     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
008001     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
009001                                                                          
010101*    --- PARAMETRAR TILL ABEND                                            
010201 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
010301 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
010401 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
010501     EJECT                                                                
010601                                                                          
010701 01  W5104A-TRANSID.                                                      
010801     03  FILLER              PIC X(6) VALUE 'W5104A'.                     
010901     03  FILLER              PIC X(8) VALUE 'W5616BD1'.                   
011001     03  FILLER              PIC X(4) VALUE ' IN1'.                       
011101                                                                          
011201 01  W56161A-TRANSID.                                                     
011301     03  FILLER              PIC X(6) VALUE 'W56161'.                     
011401     03  FILLER              PIC X(8) VALUE 'W5616BD2'.                   
011501     03  FILLER              PIC X(4) VALUE ' IN2'.                       
011601                                                                          
011701 01  W56161-TRANSID.                                                      
011801     03  FILLER              PIC X(6) VALUE 'W56161'.                     
011901     03  FILLER              PIC X(8) VALUE 'W5616BD3'.                   
012001     03  FILLER              PIC X(4) VALUE ' UT1'.                       
012101                                                                          
012201 01  W5616A-TRANSID.                                                      
012301     03  FILLER              PIC X(6) VALUE 'W5616A'.                     
012401     03  FILLER              PIC X(8) VALUE 'W5616BD4'.                   
012501     03  FILLER              PIC X(4) VALUE ' UT2'.                       
012601                                                                          
012701     EJECT                                                                
012801*   -COPY W0005  -PRE POSTSUM-                                            
012901     EJECT                                                                
013001                                                                          
013101 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
013201 01  INAREA1.                                                             
013301*    03  -COPY W56160 -PRE IN1-                                           
013401     EJECT                                                                
013501                                                                          
013601 01  FILLER                  PIC X(16)   VALUE 'IN-AREA2   '.             
013701 01  INAREA2.                                                             
013801*    03  -COPY W56160 -PRE IN2-                                           
013901     EJECT                                                                
014001                                                                          
014101 01  FILLER                  PIC X(16)   VALUE 'UT-AREA1    '.            
014201 01  UTAREA1.                                                             
014301*    03  -COPY W56160 -PRE UT1-                                           
014401     EJECT                                                                
014501                                                                          
014601 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
014701 01  UTAREA2.                                                             
014801*    03  -COPY W56160 -PRE UT2-                                           
014901     EJECT                                                                
015001                                                                          
017401 LINKAGE SECTION.                                                         
017701                                                                          
017801 PROCEDURE DIVISION.                                                      
017901 MAIN SECTION.                                                            
018101                                                                          
018201     PERFORM A-INIT                                                       
018301                                                                          
018401     PERFORM S01-READ-W5104A-POST                                         
018501     PERFORM S02-READ-W56161A-POST                                        
018601                                                                          
018701     PERFORM UNTIL  EOF-W5104A = JA AND EOF-W56161A = JA                  
018801       PERFORM B-CHECK-MATCH                                              
018901     END-PERFORM                                                          
019001                                                                          
019101     PERFORM Z-END                                                        
019201                                                                          
019301     MOVE ZERO TO RETURN-CODE                                             
019401     GOBACK                                                               
019501     .                                                                    
019601     EJECT                                                                
019701                                                                          
019801 A-INIT SECTION.                                                          
019901     OPEN INPUT  W5104A                                                   
020001                 W56161A                                                  
020101          OUTPUT W56161                                                   
020201                 W5616A                                                   
020301                                                                          
020401     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
020501     .                                                                    
020601     EJECT                                                                
020701                                                                          
020801 B-CHECK-MATCH SECTION.                                                   
020901     PERFORM UNTIL  EOF-W56161A = JA                                      
021001       IF IN1-EKHT-IDVERGL < IN2-EKHT-IDVERGL                             
021101       AND EOF-W5104A = NEJ                                               
021201         PERFORM S04-CREATE-FILE-W5616A                                   
021301         PERFORM S01-READ-W5104A-POST                                     
021401       ELSE                                                               
021501                                                                          
021601         IF IN1-EKHT-IDVERGL = IN2-EKHT-IDVERGL                           
021701         AND EOF-W5104A = NEJ                                             
021801           IF IN2-EKHT-KDEKHHT = '102'                                    
021901           AND (IN2-EKHT-KDEKSHT = '121'                                  
021902           OR   IN2-EKHT-KDEKSHT = '131')                                 
022001             PERFORM UNTIL (IN1-EKHT-IDVERGL NOT =                        
022101                           IN2-EKHT-IDVERGL)                              
022201                     OR EOF-W5104A = JA                                   
022501               PERFORM S03-CREATE-FILE-W56161-102                         
022601               PERFORM S01-READ-W5104A-POST                               
022701             END-PERFORM                                                  
022801           ELSE                                                           
022901             PERFORM S04-CREATE-FILE-W5616A                               
023001             PERFORM S01-READ-W5104A-POST                                 
023101           END-IF                                                         
023201         ELSE                                                             
023301                                                                          
023401           IF IN1-EKHT-IDVERGL > IN2-EKHT-IDVERGL                         
023501           AND EOF-W5104A = NEJ                                           
023601             PERFORM S03-CREATE-FILE-W56161                               
023701             PERFORM S02-READ-W56161A-POST                                
023801           ELSE                                                           
023901                                                                          
024001             IF EOF-W5104A = JA                                           
024101             AND EOF-W56161A = NEJ                                        
024201               PERFORM S03-CREATE-FILE-W56161                             
024301               PERFORM S02-READ-W56161A-POST                              
024401             END-IF                                                       
024501           END-IF                                                         
024601         END-IF                                                           
024701       END-IF                                                             
024801     END-PERFORM                                                          
024901                                                                          
025001     IF EOF-W56161A = JA                                                  
025101     AND EOF-W5104A = NEJ                                                 
025201       PERFORM UNTIL  EOF-W5104A = JA                                     
025301         PERFORM S04-CREATE-FILE-W5616A                                   
025401         PERFORM S01-READ-W5104A-POST                                     
025501       END-PERFORM                                                        
025601     END-IF                                                               
025701     .                                                                    
025801     EJECT                                                                
025901                                                                          
026001 Z-END SECTION.                                                           
026101     CLOSE W5104A                                                         
026201           W56161A                                                        
026301           W56161                                                         
026401           W5616A                                                         
026501                                                                          
026601     MOVE 'S'        TO POSTSUM-OPKOD                                     
026701     CALL POSTSUM USING POSTSUM-PARM                                      
026801     .                                                                    
026901     EJECT                                                                
027001                                                                          
027101 S01-READ-W5104A-POST SECTION.                                            
027201     READ W5104A INTO INAREA1                                             
027301     AT END                                                               
027401       MOVE JA TO EOF-W5104A                                              
027501     NOT AT END                                                           
027601       MOVE W5104A-TRANSID TO POSTSUM-TRANSID                             
027701       CALL POSTSUM USING POSTSUM-PARM                                    
027801     END-READ                                                             
027901     .                                                                    
028001     SKIP2                                                                
028101                                                                          
028201 S02-READ-W56161A-POST SECTION.                                           
028301     READ W56161A INTO INAREA2                                            
028401     AT END                                                               
028501       MOVE JA TO EOF-W56161A                                             
028601     NOT AT END                                                           
028701       MOVE W56161-TRANSID TO POSTSUM-TRANSID                             
028801       CALL POSTSUM USING POSTSUM-PARM                                    
028901     END-READ                                                             
029001     .                                                                    
029101     SKIP2                                                                
029201                                                                          
029301 S03-CREATE-FILE-W56161 SECTION.                                          
029401     MOVE INAREA2 TO UTAREA1                                              
029501     WRITE UTPOST1 FROM UTAREA1                                           
029601     MOVE W56161-TRANSID TO POSTSUM-TRANSID                               
029701     CALL POSTSUM USING POSTSUM-PARM                                      
029801     .                                                                    
029901     EJECT                                                                
030001                                                                          
030101 S03-CREATE-FILE-W56161-102 SECTION.                                      
030201     MOVE INAREA1 TO UTAREA1                                              
030301     MOVE IN2-EKHT-IDPGM         TO UT1-EKHT-IDPGM                        
030401     MOVE IN2-EKHT-TIREGDAT      TO UT1-EKHT-TIREGDAT                     
030501     MOVE IN2-EKHT-TIKLOCK       TO UT1-EKHT-TIKLOCK                      
030601     MOVE IN2-EKHT-IDCPYTXT      TO UT1-EKHT-IDCPYTXT                     
030701     MOVE IN2-EKHT-DAVERDAT      TO UT1-EKHT-DAVERDAT                     
030801     MOVE IN2-EKHT-KDEKSHT       TO UT1-EKHT-KDEKSHT                      
030901     WRITE UTPOST1 FROM UTAREA1                                           
031001     MOVE W56161-TRANSID TO POSTSUM-TRANSID                               
031101     CALL POSTSUM USING POSTSUM-PARM                                      
031201     .                                                                    
031301     EJECT                                                                
031401                                                                          
031501 S04-CREATE-FILE-W5616A SECTION.                                          
031601     MOVE INAREA1 TO UTAREA2                                              
031701     WRITE UTPOST2 FROM UTAREA2                                           
031801     MOVE W56161-TRANSID TO POSTSUM-TRANSID                               
031901     CALL POSTSUM USING POSTSUM-PARM                                      
032001     .                                                                    
032101     EJECT                                                                
