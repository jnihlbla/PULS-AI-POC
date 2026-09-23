010000 ID DIVISION.                                                             
020000 PROGRAM-ID.     W3718400.                                                
030000 AUTHOR.         BO HAMMARIN.                                             
040000 DATE-WRITTEN.   00/05/11.                                                
050000 DATE-COMPILED.                                                           
060000                                                                          
070000*                                                                         
080000*    FUNKTION:                                                            
090000*        PGM LÄSER KOMPLETT RAPPORTFIL-BYTES OCH                          
100000*        SKAPAR LADD-FIL FÖR LEAKAGE/SURPLUS                              
110000*                                                                         
120000*                                                                         
130000*    ABENDKODER:                                                          
140000*        U0016 -  . . . .                                                 
150000*        U1000 -  . . . .                                                 
160000*                                                                         
170000                                                                          
190000 ENVIRONMENT DIVISION.                                                    
200000                                                                          
210000 INPUT-OUTPUT SECTION.                                                    
220000                                                                          
230000 FILE-CONTROL.                                                            
240200*          --- RAPPORTFIL - BYTES                                         
240300     SELECT W37141                     ASSIGN TO W37184D1.                
240400                                                                          
240500*          --- LADD-FIL LEAKAGE/SURPLUS                                   
241008     SELECT W37175                     ASSIGN TO W37184D2.                
260000     EJECT                                                                
261000                                                                          
270000 DATA DIVISION.                                                           
280000                                                                          
290000 FILE SECTION.                                                            
300200 FD  W37141                                                               
300300     RECORDING       F                                                    
300400     BLOCK CONTAINS  0.                                                   
300600*01  -COPY W37109      -L.                                                
300700                                                                          
300808 FD  W37175                                                               
300900     RECORDING       F                                                    
301000     BLOCK CONTAINS  0.                                                   
302008*01  POST -COPY W37175 -PRE  UT-  -L.                                     
310000     EJECT                                                                
311000                                                                          
320000 WORKING-STORAGE SECTION.                                                 
340000 77  IDPGM                       PIC X(8)    VALUE 'W3718400'.            
350000 77  JA                          PIC X       VALUE 'J'.                   
360000 77  NEJ                         PIC X       VALUE 'N'.                   
380100                                                                          
380200 77  W37141-EOF-SW               PIC X       VALUE 'N'.                   
381000     88  END-OF-W37141                       VALUE 'J'.                   
390000     EJECT                                                                
391000                                                                          
400000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
410000 01  FILLER REDEFINES DAGENS-DATUM.                                       
420000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
430000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
440000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
450000     EJECT                                                                
451000                                                                          
452002 01  TEST-IDARTNR                PIC  9(9)   COMP-3 VALUE ZERO.           
453002*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
454002     EJECT                                                                
454102                                                                          
455002*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
456002     EJECT                                                                
457002                                                                          
460000 01  DYNAMISKA-SUBPROGRAM.                                                
470000*                                                                         
480000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
491000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
500000                                                                          
510000*    --- PARAMETRAR TILL ABEND                                            
530000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
540000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
550000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
560000                                                                          
570000 01  FELTEXT.                                                             
580000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
590000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
600100     EJECT                                                                
600200                                                                          
600300*    --- PARAMETRAR TILL POSTSUM                                          
600400*                                                                         
601000*01  -COPY W0005   -PRE  POSTSUM-                                         
620100     EJECT                                                                
620200                                                                          
620300 01  IN-AREA-START               PIC X(24)   VALUE                        
620400                                 'IN-AREA-START  '.                       
620600                                                                          
620700*01  AREA -COPY W37109     -PRE IN-                                       
620800     EJECT                                                                
620900                                                                          
621000 01  UT-AREA-START               PIC X(24)   VALUE                        
621100                                 'UT-AREA-START  '.                       
621300                                                                          
622008*01  AREA -COPY W37175     -PRE UT-                                       
630000     EJECT                                                                
631000                                                                          
640000 PROCEDURE DIVISION.                                                      
680000                                                                          
681000 MAIN SECTION.                                                            
690000     PERFORM A-INIT                                                       
701004     PERFORM S01-READ-W37141                                              
710000     PERFORM UNTIL END-OF-W37141                                          
720002       IF IN-IDPTYP = 'FAK' OR 'KRE' OR 'RET' OR 'ADJ'                    
730004         PERFORM B-BUILD-LOADREC                                          
731008         PERFORM S11-WRITE-W37175                                         
740004       END-IF                                                             
781004       PERFORM S01-READ-W37141                                            
790000     END-PERFORM                                                          
810000                                                                          
820000     PERFORM Z-FINIT                                                      
830000                                                                          
840000     MOVE ZERO TO RETURN-CODE                                             
850000     GOBACK                                                               
860000     .                                                                    
870000     EJECT                                                                
871000                                                                          
880000 A-INIT SECTION.                                                          
891000     OPEN INPUT  W37141                                                   
901008     OPEN OUTPUT W37175                                                   
910000                                                                          
920000     ACCEPT DAGENS-DATUM  FROM DATE                                       
931000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
940000     .                                                                    
950000     EJECT                                                                
951000                                                                          
952004 B-BUILD-LOADREC SECTION.                                                 
952102     MOVE IN-IDPTYP     TO UT-IDPTYP                                      
952202     MOVE IN-DAAAVV     TO UT-DAAAVV                                      
952302                                                                          
953002     MOVE IN-IDARTNR    TO TEST-IDARTNR                                   
954002                           UT-IDARTNR                                     
955002     IF BYT02-RENOV                                                       
956002       IF BYT16-BYTES                                                     
957002         ADD 6000       TO UT-IDARTNR                                     
957102       ELSE                                                               
957202         ADD 1000       TO UT-IDARTNR                                     
957302       END-IF                                                             
957402     END-IF                                                               
957502                                                                          
957602     MOVE IN-BEART-ENG  TO UT-BEART-ENG                                   
957703     MOVE IN-IDFKNGRP   TO UT-IDFKNGRP                                    
958102     MOVE IN-IDDISTR    TO UT-IDDISTR                                     
958202     MOVE IN-IDKUNDNR   TO UT-IDKUNDNR                                    
958302     MOVE IN-IDBYTRAP   TO UT-IDBYTRAP                                    
958402     MOVE IN-IDBYTRAD   TO UT-IDBYTRAD                                    
958502     MOVE IN-KDBYTREF   TO UT-KDBYTREF                                    
958503     IF IN-IDPTYP = 'RET'                                                 
958504        MOVE IN-KVRETUR-GODK TO UT-KVANTAL                                
958505     ELSE                                                                 
958802        MOVE IN-KVANTAL      TO UT-KVANTAL                                
958803     END-IF                                                               
958902     MOVE IN-KVPOINT    TO UT-KVPOINT                                     
959102     .                                                                    
959202     EJECT                                                                
959302                                                                          
960000 Z-FINIT SECTION.                                                         
970100     CLOSE W37141                                                         
971008           W37175                                                         
980100                                                                          
980200     MOVE 'S' TO POSTSUM-OPKOD                                            
981000     CALL POSTSUM USING POSTSUM-PARM                                      
990000     .                                                                    
000100     EJECT                                                                
000200                                                                          
000304 S01-READ-W37141  SECTION.                                                
000400     READ W37141          INTO IN-AREA                                    
000500     AT END                                                               
000600        MOVE HIGH-VALUE   TO IN-AREA                                      
000700        SET END-OF-W37141 TO TRUE                                         
000800                                                                          
000900     NOT AT END                                                           
001000        MOVE 'W37141'   TO POSTSUM-FDNAMN                                 
001106        MOVE 'W37184D1' TO POSTSUM-DDNAMN2                                
001300        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
001400        CALL POSTSUM USING POSTSUM-PARM                                   
001500     END-READ                                                             
002000     .                                                                    
010200                                                                          
010308 S11-WRITE-W37175 SECTION.                                                
010500     WRITE UT-POST      FROM UT-AREA                                      
010600                                                                          
010700     MOVE 'UT'          TO POSTSUM-TRANSTYP                               
010808     MOVE 'W37175'      TO POSTSUM-FDNAMN                                 
010900     MOVE 'W37184D2'    TO POSTSUM-DDNAMN2                                
011000     CALL POSTSUM USING POSTSUM-PARM                                      
012000     .                                                                    
