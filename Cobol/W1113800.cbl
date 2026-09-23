000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1113800.                                                
000300 AUTHOR.         KIHLBERG STEFAN.                                         
000400 DATE-WRITTEN.   13/12/05.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        PROGRAM TO UNPACK CSV FILE FROM LOGENT                           
001000*                                                                         
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- CUSTOM INFO FROM LOGENT                                    
002500     SELECT W11137                     ASSIGN TO W11138D1.                
240200     SKIP2                                                                
240300*          --- CUSTOM DATA FROM LOGENT, UNPACKED                          
240400     SELECT W11138                     ASSIGN TO W11138D2.                
240500     EJECT                                                                
240600 DATA DIVISION.                                                           
240700     SKIP3                                                                
240800 FILE SECTION.                                                            
240900     SKIP3                                                                
241000 FD  W11137                                                               
242000     RECORDING       V                                                    
243000     BLOCK CONTAINS  0.                                                   
244000     SKIP3                                                                
245000 01  LOGENT-RECORD         PIC X(251).                                    
246000 FD  W11138                                                               
247000     RECORDING       F                                                    
248000     BLOCK CONTAINS  0.                                                   
249000                                                                          
250000*01  RECORD -COPY ARTSTAUR -PRE  UT-  -L.                                 
260000     EJECT                                                                
270000 WORKING-STORAGE SECTION.                                                 
280000                                                                          
290000 77  IDPGM                       PIC X(8)    VALUE 'W1113800'.            
300000 77  YES                         PIC X       VALUE 'J'.                   
310000 77  NOO                         PIC X       VALUE 'N'.                   
320000                                                                          
330000 77  W11137-EOF-SW               PIC X       VALUE 'N'.                   
340000     88  END-OF-W11137                       VALUE 'J'.                   
350000                                                                          
360000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
370000     88  INDATA-OK                           VALUE 'J'.                   
380000     88  INDATA-FEL                          VALUE 'N'.                   
381000                                                                          
382000 77  FLSKRIV                     PIC X       VALUE 'N'.                   
382100                                                                          
382200 01  WORK-FIELDS.                                                         
382300     03 WS-NO-OF-POSTINGS        PIC 9(6)    VALUE ZERO.                  
382400     EJECT                                                                
382500                                                                          
382600 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
382700 01  FILLER REDEFINES TODAYS-DATE.                                        
382800     03  TODAYS-DATE-YEAR        PIC 9(2).                                
382900     03  TODAYS-DATE-MONTH       PIC 9(2).                                
383000     03  TODAYS-DATE-DAY         PIC 9(2).                                
384000 01  TODAYS-TIME                 PIC 9(6)    VALUE ZERO.                  
385000 01  FILLER REDEFINES TODAYS-TIME.                                        
386000     03  TODAYS-TIME-HH          PIC 9(2).                                
387000     03  TODAYS-TIME-MM          PIC 9(2).                                
388000     EJECT                                                                
389000 01  GENERAL-SUBPROGRAMS.                                                 
390000*                                                                         
400000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
410000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
420000     SKIP2                                                                
430000*    --- PARAMETERS TO ABEND                                              
440000                                                                          
450000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
460000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
470000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
480000     SKIP2                                                                
490000 01  ERROR-TEXT.                                                          
500000     03  FILLER            PIC X(10)   VALUE 'ERROR-TEXT'.                
510000     03  ERROR-TEXT-STR    PIC X(72)   VALUE SPACE.                       
520000                                                                          
530000*    --- PARAMETRAR TILL POSTSUM                                          
540000*                                                                         
550000*01  -COPY W0005   -PRE  POSTSUM-                                         
560000     EJECT                                                                
570000 01  IN-AREA-START               PIC X(24)   VALUE                        
580000                                 'IN-AREA-START  '.                       
590000     SKIP2                                                                
600000 01  IN-AREA                     PIC X(251).                              
610000                                                                          
620000 01  WS-LOGENT-AREA.                                                      
621000     03 WS-LOGENT-IDARTNR          PIC X(15).                             
621100     03 WS-LOGENT-ATGARD           PIC X(15).                             
621200     03 WS-LOGENT-IDLEVNR          PIC X(15).                             
621300     03 WS-LOGENT-BEART-SWE        PIC X(15).                             
621400     03 WS-LOGENT-BEART-GB         PIC X(15).                             
621500     03 WS-LOGENT-KDSORT           PIC X(15).                             
621600     03 WS-LOGENT-IDSTATNR-PULS    PIC X(15).                             
621700     03 WS-LOGENT-IDSTATNR-LOGENT  PIC X(15).                             
621800     03 WS-LOGENT-KDARTURS-PULS    PIC X(15).                             
621900     03 WS-LOGENT-KDARTURS-LOGENT  PIC X(15).                             
622000                                                                          
622100 01  UT-AREA-START               PIC X(24)   VALUE                        
622200                                 'UT-AREA-START  '.                       
622300     SKIP2                                                                
622400                                                                          
622500*01  AREA -COPY ARTSTAUR     -PRE UT-                                     
622600     EJECT                                                                
622700 PROCEDURE DIVISION.                                                      
622800 MAIN SECTION.                                                            
622900     SKIP2                                                                
623000                                                                          
624000     PERFORM A-INIT                                                       
625000     PERFORM S01-READ-W11137                                              
626000     PERFORM UNTIL END-OF-W11137                                          
627000                                                                          
628000       PERFORM B-SKAPA-RAD                                                
629000       IF FLSKRIV = YES                                                   
630000         PERFORM S11-WRITE-W11138                                         
640000       END-IF                                                             
650000       PERFORM S01-READ-W11137                                            
660000     END-PERFORM                                                          
670000                                                                          
680000                                                                          
690000     PERFORM Z-FINIT                                                      
700000                                                                          
710000     MOVE ZERO TO RETURN-CODE                                             
720000     GOBACK                                                               
730000     .                                                                    
740000     EJECT                                                                
750000 A-INIT SECTION.                                                          
760000                                                                          
770000     OPEN INPUT  W11137                                                   
780000                                                                          
790000     OPEN OUTPUT W11138                                                   
800000     SKIP2                                                                
810000     ACCEPT TODAYS-DATE  FROM DATE                                        
820000     ACCEPT TODAYS-TIME (1:4) FROM TIME                                   
830000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
840000     .                                                                    
850000     EJECT                                                                
860000 B-SKAPA-RAD SECTION.                                                     
870000                                                                          
880000     MOVE NOO TO FLSKRIV                                                  
890000     MOVE SPACE TO UT-AREA                                                
900000     UNSTRING IN-AREA DELIMITED BY ';'                                    
910000     INTO                                                                 
920000          WS-LOGENT-IDARTNR                                               
930000          WS-LOGENT-ATGARD                                                
940000          WS-LOGENT-IDLEVNR                                               
950000          WS-LOGENT-BEART-SWE                                             
951000          WS-LOGENT-BEART-GB                                              
952000          WS-LOGENT-KDSORT                                                
953000          WS-LOGENT-IDSTATNR-PULS                                         
954000          WS-LOGENT-IDSTATNR-LOGENT                                       
954100          WS-LOGENT-KDARTURS-PULS                                         
954200          WS-LOGENT-KDARTURS-LOGENT                                       
954300                                                                          
954400     MOVE ZERO TO TALLY                                                   
954500     INSPECT WS-LOGENT-IDARTNR                                            
954600       TALLYING TALLY FOR CHARACTERS BEFORE INITIAL SPACE                 
954610     IF TALLY > 0                                                         
954700     AND WS-LOGENT-IDARTNR(1:TALLY) NUMERIC                               
954800       MOVE '22'                      TO UT-POSTTYP                       
954900       MOVE WS-LOGENT-IDARTNR         TO UT-ARTIKELNR                     
955000       MOVE WS-LOGENT-BEART-SWE       TO UT-BENAMNING-SE                  
956000       MOVE SPACE                     TO UT-ATGARDSKOD                    
957000       MOVE WS-LOGENT-IDLEVNR         TO UT-LEVID                         
957100                                                                          
957200       MOVE ZERO TO TALLY                                                 
957300       INSPECT WS-LOGENT-IDSTATNR-LOGENT                                  
957400         TALLYING TALLY FOR CHARACTERS BEFORE INITIAL SPACE               
957410       IF TALLY > 0                                                       
957500       AND WS-LOGENT-IDSTATNR-LOGENT(1:TALLY) NUMERIC                     
958000         MOVE WS-LOGENT-IDSTATNR-LOGENT TO UT-STATNR                      
958100       END-IF                                                             
958200                                                                          
959000       MOVE WS-LOGENT-KDARTURS-LOGENT TO UT-ARTIKELURSPRUNG               
959100       MOVE YES TO FLSKRIV                                                
959200     END-IF                                                               
959300     .                                                                    
959400     EJECT                                                                
959500 Z-FINIT SECTION.                                                         
959600     CLOSE W11137                                                         
959700           W11138                                                         
959800     SKIP2                                                                
959900     MOVE 'S' TO POSTSUM-OPKOD                                            
960000     CALL POSTSUM USING POSTSUM-PARM                                      
970000     .                                                                    
000100     EJECT                                                                
000200 S01-READ-W11137  SECTION.                                                
000300     READ W11137 INTO IN-AREA                                             
000400     AT END                                                               
000500        MOVE HIGH-VALUE TO IN-AREA                                        
000600        SET END-OF-W11137 TO TRUE                                         
000700                                                                          
000800     NOT AT END                                                           
000900        MOVE 'W11137' TO POSTSUM-FDNAMN                                   
001000        MOVE 'W11138D1' TO POSTSUM-DDNAMN2                                
001200        MOVE SPACE     TO POSTSUM-TRANSTYP                                
001300        CALL POSTSUM USING POSTSUM-PARM                                   
001400     END-READ                                                             
002000     .                                                                    
010100     EJECT                                                                
010200 S11-WRITE-W11138 SECTION.                                                
010300                                                                          
010400     WRITE UT-RECORD FROM UT-AREA                                         
010500                                                                          
010600     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
010700     MOVE 'W11138' TO POSTSUM-FDNAMN                                      
010800     MOVE 'W11138D2' TO POSTSUM-DDNAMN2                                   
010900     CALL POSTSUM USING POSTSUM-PARM                                      
011000     .                                                                    
