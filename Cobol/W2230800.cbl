000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2230800.                                                
000400 AUTHOR.         ARVIDSSON LENA.                                          
000500 DATE-WRITTEN.   04/08/18.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER INFIL W11135. OM DET ÄR ETT BYTESNR SÅ SKAPAS ETT          
001100*        MOTSVARANDE OBJEKTNR MED SAMMA KDARTURS OCH LEVNR SOM            
001200*        BYTESNUMRET. DESSA POSTER LÄGGS SEDAN PÅ UTFIL W22309 SOM        
001300*        GÅR VIDARE TILL  PGM W2231000, I VILKET KDARTURS                 
001310*        UPPDATERAS.                                                      
001400*                                                                         
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002701     SKIP2                                                                
002702*          --- INFIL MED ARTIKELURSPRUNG                                  
002703     SELECT W11135                     ASSIGN TO W22308D1.                
002704     SKIP2                                                                
002705*          --- UTFIL MED ARTIKELURSPRUNG INKL OBJEKTSNR                   
002710     SELECT W22309                     ASSIGN TO W22308D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003301     SKIP3                                                                
003302 FD  W11135                                                               
003303     RECORDING       F                                                    
003304     BLOCK CONTAINS  0.                                                   
003305                                                                          
003306*01  -COPY W11135      -PRE  IN-  -L.                                     
003307     SKIP3                                                                
003308 FD  W22309                                                               
003309     RECORDING       F                                                    
003310     BLOCK CONTAINS  0.                                                   
003311                                                                          
003320*01  POST -COPY W11135 -PRE  UT-  -L.                                     
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'W2230800'.            
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004101                                                                          
004102 77  W11135-EOF-SW               PIC X       VALUE 'N'.                   
004110     88  END-OF-W11135                       VALUE 'J'.                   
004200     EJECT                                                                
004300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004400 01  FILLER REDEFINES DAGENS-DATUM.                                       
004500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004800     EJECT                                                                
004900 01  DYNAMISKA-SUBPROGRAM.                                                
005000*                                                                         
005100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005210     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005300     SKIP2                                                                
005400*    --- PARAMETRAR TILL ABEND                                            
005500                                                                          
005600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005900     SKIP2                                                                
006000 01  FELTEXT.                                                             
006100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006301     EJECT                                                                
006302*    --- PARAMETRAR TILL POSTSUM                                          
006303*                                                                         
006310*01  -COPY W0005   -PRE  POSTSUM-                                         
006501     EJECT                                                                
006502 01  IN-AREA-START               PIC X(24)   VALUE                        
006503                                 'IN-AREA-START  '.                       
006504     SKIP2                                                                
006505                                                                          
006506*01  AREA -COPY W11135     -PRE IN-                                       
006507     EJECT                                                                
006508 01  UT-AREA-START               PIC X(24)   VALUE                        
006509                                 'UT-AREA-START  '.                       
006510     SKIP2                                                                
006511                                                                          
006520*01  AREA -COPY W11135     -PRE UT-                                       
006600     EJECT                                                                
006610*                                                                         
006611*    --- FÖR TEST AV BYTESNR                                              
006633*                                                                         
006634 01  FILLER                      PIC  X(16)  VALUE 'BYTES-TEST'.          
006635 01  TEST-IDARTNR                PIC  9(9)   COMP-3.                      
006640*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
006650                                                                          
006660*01  FILLER  -COPY WWBYT03     -RED TEST-IDARTNR.                         
006670                                                                          
006680*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
006690                                                                          
006700 PROCEDURE DIVISION.                                                      
006800 MAIN SECTION.                                                            
007000     SKIP2                                                                
007100                                                                          
007200     PERFORM A-INIT                                                       
007310     PERFORM S01-LAES-W11135                                              
007400     PERFORM UNTIL END-OF-W11135                                          
007500                                                                          
007510******************************************************************        
007520****   OM DET ÄR ETT BYTESNR SÖKER MAN UPP DESS MOTSVARANDE   ****        
007530****   OBJEKTNR. OBJEKTSNUMRET TILLDELAS DÅ SAMMA             ****        
007540****   URSPRUNGSKOD SOM BYTESNUMRET OCH SKRIVS UT PÅ          ****        
007541****   UTFILEN W22309,                                        ****        
007550******************************************************************        
007560       MOVE IN-IDARTNR  TO TEST-IDARTNR                                   
007561                           UT-IDARTNR                                     
007562       MOVE IN-IDLEVNR  TO UT-IDLEVNR                                     
007564       MOVE IN-KDARTURS TO UT-KDARTURS                                    
007566       PERFORM S11-SKRIV-W22309                                           
007570       IF NOT BYT02-RENOV                                                 
007580          CONTINUE                                                        
007590       ELSE                                                               
007592          IF BYT16-BYTES                                                  
007593             COMPUTE TEST-IDARTNR = TEST-IDARTNR +                        
007594                                    6000                                  
007595             END-COMPUTE                                                  
007596          ELSE                                                            
007597             COMPUTE TEST-IDARTNR = TEST-IDARTNR +                        
007598                                    1000                                  
007599             END-COMPUTE                                                  
007600          END-IF                                                          
007602          MOVE TEST-IDARTNR   TO UT-IDARTNR                               
007603          PERFORM S11-SKRIV-W22309                                        
007620       END-IF                                                             
008110       PERFORM S01-LAES-W11135                                            
008200     END-PERFORM                                                          
008300                                                                          
008400                                                                          
008500     PERFORM Z-FINIT                                                      
008600                                                                          
008700     MOVE ZERO TO RETURN-CODE                                             
008800     GOBACK                                                               
008900     .                                                                    
009000     EJECT                                                                
009100 A-INIT SECTION.                                                          
009201                                                                          
009210     OPEN INPUT  W11135                                                   
009301                                                                          
009310     OPEN OUTPUT W22309                                                   
009400     SKIP2                                                                
009500     ACCEPT DAGENS-DATUM  FROM DATE                                       
009610     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009700     .                                                                    
009800     EJECT                                                                
009900 Z-FINIT SECTION.                                                         
010001     CLOSE W11135                                                         
010010           W22309                                                         
010101     SKIP2                                                                
010102     MOVE 'S' TO POSTSUM-OPKOD                                            
010110     CALL POSTSUM USING POSTSUM-PARM                                      
010200     .                                                                    
010301     EJECT                                                                
010302 S01-LAES-W11135  SECTION.                                                
010303     READ W11135 INTO IN-AREA                                             
010304     AT END                                                               
010305        MOVE HIGH-VALUE TO IN-AREA                                        
010306        SET END-OF-W11135 TO TRUE                                         
010307                                                                          
010308     NOT AT END                                                           
010309        MOVE 'W11135' TO POSTSUM-FDNAMN                                   
010310        MOVE 'W22308D1' TO POSTSUM-DDNAMN2                                
010311*       -- ÄNDRA TILL MOVE SPACE OM FILEN SAKNAR POSTTYP                  
010312*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
010313        MOVE SPACE TO POSTSUM-TRANSTYP                                    
010314        CALL POSTSUM USING POSTSUM-PARM                                   
010315     END-READ                                                             
010320     .                                                                    
010401     EJECT                                                                
010402 S11-SKRIV-W22309 SECTION.                                                
010403                                                                          
010404     WRITE UT-POST FROM UT-AREA                                           
010405                                                                          
010406*    MOVE SPACE TO POSTSUM-TRANSTYP                                       
010407     MOVE 'W22309' TO POSTSUM-FDNAMN                                      
010408     MOVE 'W22308D2' TO POSTSUM-DDNAMN2                                   
010409     CALL POSTSUM USING POSTSUM-PARM                                      
010410     .                                                                    
010600     EJECT                                                                
010700*S99-ABEND SECTION.                                                       
010800*                                                                         
010901*    SKIP2                                                                
010902*    MOVE 'S' TO POSTSUM-OPKOD                                            
010910*    CALL POSTSUM USING POSTSUM-PARM                                      
011000*    CALL ABEND USING RKOD-ABEND                                          
011100*    .                                                                    
