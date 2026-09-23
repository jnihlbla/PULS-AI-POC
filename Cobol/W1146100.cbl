000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1146100.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   05/12/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER KVITTERINGAR FRÅN SI PLUS                                  
001000*                                                                         
001100*                                                                         
001200*    ABENDKODER:                                                          
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
002400*          --- KVITTERINGAR FRÅN SI+                                      
002500     SELECT W11460                     ASSIGN TO W11461D1.                
240400     SKIP2                                                                
240500*          --- RK = 00                                                    
240604     SELECT W1146A                     ASSIGN TO W11461D2.                
240700     SKIP2                                                                
240806*          --- RK = 02, 03                                                
240904     SELECT W1146B                     ASSIGN TO W11461D3.                
241000     SKIP2                                                                
241100*          --- RK = 04                                                    
241204     SELECT W1146C                     ASSIGN TO W11461D4.                
241300     SKIP2                                                                
241400*          --- RK = 08                                                    
241504     SELECT W1146D                     ASSIGN TO W11461D5.                
241600     SKIP2                                                                
241700*          --- RK = 10                                                    
241804     SELECT W1146E                     ASSIGN TO W11461D6.                
241900     SKIP2                                                                
242000*          --- RK = 12                                                    
242104     SELECT W1146F                     ASSIGN TO W11461D7.                
242200     SKIP2                                                                
242300*          --- RK = 20                                                    
243004     SELECT W1146G                     ASSIGN TO W11461D8.                
260000     EJECT                                                                
270000 DATA DIVISION.                                                           
280000     SKIP3                                                                
290000 FILE SECTION.                                                            
300100     SKIP3                                                                
300200 FD  W11460                                                               
300300     RECORDING       F                                                    
300400     BLOCK CONTAINS  0.                                                   
300500                                                                          
300600*01  -COPY T335R309      -L.                                              
300700     SKIP3                                                                
300800 FD  W1146A                                                               
300900     RECORDING       F                                                    
301000     BLOCK CONTAINS  0.                                                   
301100                                                                          
301200*01  POST -COPY T335R309 -PRE  UTA-  -L.                                  
301300     SKIP3                                                                
301400 FD  W1146B                                                               
301500     RECORDING       F                                                    
301600     BLOCK CONTAINS  0.                                                   
301700                                                                          
301800*01  POST -COPY T335R309 -PRE  UTB-  -L.                                  
301900     SKIP3                                                                
302003 FD  W1146C                                                               
302100     RECORDING       F                                                    
302200     BLOCK CONTAINS  0.                                                   
302300                                                                          
302400*01  POST -COPY T335R309 -PRE  UTC-  -L.                                  
302500     SKIP3                                                                
302600 FD  W1146D                                                               
302700     RECORDING       F                                                    
302800     BLOCK CONTAINS  0.                                                   
302900                                                                          
303000*01  POST -COPY T335R309 -PRE  UTD-  -L.                                  
303100     SKIP3                                                                
303200 FD  W1146E                                                               
303300     RECORDING       F                                                    
303400     BLOCK CONTAINS  0.                                                   
303500                                                                          
303600*01  POST -COPY T335R309 -PRE  UTE-  -L.                                  
303700     SKIP3                                                                
303800 FD  W1146F                                                               
303900     RECORDING       F                                                    
304000     BLOCK CONTAINS  0.                                                   
304100                                                                          
304200*01  POST -COPY T335R309 -PRE  UTF-  -L.                                  
304300     SKIP3                                                                
304400 FD  W1146G                                                               
304500     RECORDING       F                                                    
304600     BLOCK CONTAINS  0.                                                   
304700                                                                          
305000*01  POST -COPY T335R309 -PRE  UTG-  -L.                                  
310000     EJECT                                                                
320000 WORKING-STORAGE SECTION.                                                 
330000                                                                          
340004 77  IDPGM                       PIC X(8)    VALUE 'W1146100'.            
350000 77  JA                          PIC X       VALUE 'J'.                   
360000 77  NEJ                         PIC X       VALUE 'N'.                   
380100                                                                          
380200 77  W11460-EOF-SW               PIC X       VALUE 'N'.                   
381000     88  END-OF-W11460                       VALUE 'J'.                   
390000     EJECT                                                                
400000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
410000 01  FILLER REDEFINES DAGENS-DATUM.                                       
420000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
430000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
440000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
450000     EJECT                                                                
460000 01  DYNAMISKA-SUBPROGRAM.                                                
470000*                                                                         
480000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
491000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
500000     SKIP2                                                                
510000*    --- PARAMETRAR TILL ABEND                                            
520000                                                                          
530000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
540000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
550000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
560000     SKIP2                                                                
570000 01  FELTEXT.                                                             
580000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
590000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
600100     EJECT                                                                
600200*    --- PARAMETRAR TILL POSTSUM                                          
600300*                                                                         
601000*01  -COPY W0005   -PRE  POSTSUM-                                         
620100     EJECT                                                                
620200 01  IN-AREA-START               PIC X(24)   VALUE                        
620300                                 'IN-AREA-START  '.                       
620400     SKIP2                                                                
620500                                                                          
620600*01  AREA -COPY T335R309     -PRE IN-                                     
620700     EJECT                                                                
620800 01  UTA-AREA-START              PIC X(24)   VALUE                        
620900                                 'UTA-AREA-START  '.                      
621000     SKIP2                                                                
621100                                                                          
621200*01  AREA -COPY T335R309     -PRE UTA-                                    
621300     EJECT                                                                
621400 01  UTB-AREA-START              PIC X(24)   VALUE                        
621500                                 'UTB-AREA-START  '.                      
621600     SKIP2                                                                
621700                                                                          
621800*01  AREA -COPY T335R309     -PRE UTB-                                    
621900     EJECT                                                                
622000 01  UTC-AREA-START              PIC X(24)   VALUE                        
622100                                 'UTC-AREA-START  '.                      
622200     SKIP2                                                                
622300                                                                          
622400*01  AREA -COPY T335R309     -PRE UTC-                                    
622500     EJECT                                                                
622600 01  UTD-AREA-START              PIC X(24)   VALUE                        
622700                                 'UTD-AREA-START  '.                      
622800     SKIP2                                                                
622900                                                                          
623000*01  AREA -COPY T335R309     -PRE UTD-                                    
623100     EJECT                                                                
623200 01  UTE-AREA-START              PIC X(24)   VALUE                        
623300                                 'UTE-AREA-START  '.                      
623400     SKIP2                                                                
623500                                                                          
623600*01  AREA -COPY T335R309     -PRE UTE-                                    
623700     EJECT                                                                
623800 01  UTF-AREA-START              PIC X(24)   VALUE                        
623900                                 'UTF-AREA-START  '.                      
624000     SKIP2                                                                
624100                                                                          
624200*01  AREA -COPY T335R309     -PRE UTF-                                    
624300     EJECT                                                                
624400 01  UTG-AREA-START              PIC X(24)   VALUE                        
624500                                 'UTG-AREA-START  '.                      
624600     SKIP2                                                                
624700                                                                          
625000*01  AREA -COPY T335R309     -PRE UTG-                                    
630000     EJECT                                                                
640000 PROCEDURE DIVISION.                                                      
650000 MAIN SECTION.                                                            
670000     SKIP2                                                                
680000                                                                          
690000     PERFORM A-INIT                                                       
701000     PERFORM S01-LAES-W11460                                              
710001     PERFORM UNTIL END-OF-W11460                                          
720001       IF IN-RETURN-CODE = '  ' OR '00'                                   
730001          MOVE IN-AREA TO UTA-AREA                                        
740001          PERFORM S11-SKRIV-W1146A                                        
750001       ELSE                                                               
751005          IF IN-RETURN-CODE = '02' OR '03'                                
752001             MOVE IN-AREA TO UTB-AREA                                     
753001             PERFORM S12-SKRIV-W1146B                                     
754001          ELSE                                                            
755001             IF IN-RETURN-CODE = '04'                                     
756001                MOVE IN-AREA TO UTC-AREA                                  
757001                PERFORM S13-SKRIV-W1146C                                  
758001             ELSE                                                         
759001                IF IN-RETURN-CODE = '08'                                  
759101                   MOVE IN-AREA TO UTD-AREA                               
759201                   PERFORM S14-SKRIV-W1146D                               
759301                ELSE                                                      
759401                   IF IN-RETURN-CODE = '10'                               
759501                      MOVE IN-AREA TO UTE-AREA                            
759601                      PERFORM S15-SKRIV-W1146E                            
759701                   ELSE                                                   
759801                      IF IN-RETURN-CODE = '05' OR '12'                    
759901                         MOVE IN-AREA TO UTF-AREA                         
760001                         PERFORM S16-SKRIV-W1146F                         
760101                      ELSE                                                
760201                         IF IN-RETURN-CODE = '20'                         
760301                            MOVE IN-AREA TO UTG-AREA                      
760401                            PERFORM S17-SKRIV-W1146G                      
760501                         END-IF                                           
760601                      END-IF                                              
761001                   END-IF                                                 
762001                END-IF                                                    
763001             END-IF                                                       
764001          END-IF                                                          
765001       END-IF                                                             
770000                                                                          
781000       PERFORM S01-LAES-W11460                                            
790000     END-PERFORM                                                          
800000                                                                          
810000                                                                          
820000     PERFORM Z-FINIT                                                      
830000                                                                          
840000     MOVE ZERO TO RETURN-CODE                                             
850000     GOBACK                                                               
860000     .                                                                    
870000     EJECT                                                                
880000 A-INIT SECTION.                                                          
890100                                                                          
891000     OPEN INPUT  W11460                                                   
900100                                                                          
900200     OPEN OUTPUT W1146A                                                   
900300                 W1146B                                                   
900403                 W1146C                                                   
900500                 W1146D                                                   
900600                 W1146E                                                   
900700                 W1146F                                                   
901000                 W1146G                                                   
910000     SKIP2                                                                
920000     ACCEPT DAGENS-DATUM  FROM DATE                                       
931000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
940000     .                                                                    
950000     EJECT                                                                
960000 Z-FINIT SECTION.                                                         
970100     CLOSE W11460                                                         
970200           W1146A                                                         
970300           W1146B                                                         
970403           W1146C                                                         
970500           W1146D                                                         
970600           W1146E                                                         
970700           W1146F                                                         
971000           W1146G                                                         
980100     SKIP2                                                                
980200     MOVE 'S' TO POSTSUM-OPKOD                                            
981000     CALL POSTSUM USING POSTSUM-PARM                                      
990000     .                                                                    
000100     EJECT                                                                
000200 S01-LAES-W11460  SECTION.                                                
000300     READ W11460 INTO IN-AREA                                             
000400     AT END                                                               
000500        MOVE HIGH-VALUE TO IN-AREA                                        
000600        SET END-OF-W11460 TO TRUE                                         
000700                                                                          
000800     NOT AT END                                                           
000901        MOVE 'W11460'   TO POSTSUM-FDNAMN                                 
001004        MOVE 'W11461D1' TO POSTSUM-DDNAMN2                                
001301        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
001400        CALL POSTSUM USING POSTSUM-PARM                                   
001500     END-READ                                                             
002000     .                                                                    
010100     EJECT                                                                
010200 S11-SKRIV-W1146A SECTION.                                                
010300                                                                          
010400     WRITE UTA-POST FROM UTA-AREA                                         
010500                                                                          
010601     MOVE 'UTA'      TO POSTSUM-TRANSTYP                                  
010701     MOVE 'W1146A'   TO POSTSUM-FDNAMN                                    
010804     MOVE 'W11461D2' TO POSTSUM-DDNAMN2                                   
010900     CALL POSTSUM USING POSTSUM-PARM                                      
011000     .                                                                    
011100     EJECT                                                                
011200 S12-SKRIV-W1146B SECTION.                                                
011300                                                                          
011400     WRITE UTB-POST FROM UTB-AREA                                         
011500                                                                          
011601     MOVE 'UTB'      TO POSTSUM-TRANSTYP                                  
011701     MOVE 'W1146B'   TO POSTSUM-FDNAMN                                    
011804     MOVE 'W11461D3' TO POSTSUM-DDNAMN2                                   
011900     CALL POSTSUM USING POSTSUM-PARM                                      
012000     .                                                                    
012100     EJECT                                                                
012202 S13-SKRIV-W1146C SECTION.                                                
012300                                                                          
012400     WRITE UTC-POST FROM UTC-AREA                                         
012500                                                                          
012601     MOVE 'UTC'      TO POSTSUM-TRANSTYP                                  
012703     MOVE 'W1146C'   TO POSTSUM-FDNAMN                                    
012804     MOVE 'W11461D4' TO POSTSUM-DDNAMN2                                   
012900     CALL POSTSUM USING POSTSUM-PARM                                      
013000     .                                                                    
013100     EJECT                                                                
013200 S14-SKRIV-W1146D SECTION.                                                
013300                                                                          
013400     WRITE UTD-POST FROM UTD-AREA                                         
013500                                                                          
013601     MOVE 'UTD'      TO POSTSUM-TRANSTYP                                  
013701     MOVE 'W1146D'   TO POSTSUM-FDNAMN                                    
013804     MOVE 'W11461D5' TO POSTSUM-DDNAMN2                                   
013900     CALL POSTSUM USING POSTSUM-PARM                                      
014000     .                                                                    
014100     EJECT                                                                
014200 S15-SKRIV-W1146E SECTION.                                                
014300                                                                          
014400     WRITE UTE-POST FROM UTE-AREA                                         
014500                                                                          
014601     MOVE 'UTE'      TO POSTSUM-TRANSTYP                                  
014701     MOVE 'W1146E'   TO POSTSUM-FDNAMN                                    
014804     MOVE 'W11461D6' TO POSTSUM-DDNAMN2                                   
014900     CALL POSTSUM USING POSTSUM-PARM                                      
015000     .                                                                    
015100     EJECT                                                                
015200 S16-SKRIV-W1146F SECTION.                                                
015300                                                                          
015400     WRITE UTF-POST FROM UTF-AREA                                         
015500                                                                          
015601     MOVE 'UTF'      TO POSTSUM-TRANSTYP                                  
015701     MOVE 'W1146F'   TO POSTSUM-FDNAMN                                    
015804     MOVE 'W11461D7' TO POSTSUM-DDNAMN2                                   
015900     CALL POSTSUM USING POSTSUM-PARM                                      
016000     .                                                                    
016100     EJECT                                                                
016200 S17-SKRIV-W1146G SECTION.                                                
016300                                                                          
016400     WRITE UTG-POST FROM UTG-AREA                                         
016500                                                                          
016601     MOVE 'UTG'      TO POSTSUM-TRANSTYP                                  
016701     MOVE 'W1146G'   TO POSTSUM-FDNAMN                                    
016804     MOVE 'W11461D8' TO POSTSUM-DDNAMN2                                   
016900     CALL POSTSUM USING POSTSUM-PARM                                      
017000     .                                                                    
030000     EJECT                                                                
040000 S99-ABEND SECTION.                                                       
050000                                                                          
060100     SKIP2                                                                
060200     MOVE 'S' TO POSTSUM-OPKOD                                            
061000     CALL POSTSUM USING POSTSUM-PARM                                      
070000     CALL ABEND USING RKOD-ABEND                                          
080000     .                                                                    
