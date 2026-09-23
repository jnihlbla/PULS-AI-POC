000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3719900.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   00/09/26.                                                
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        FIXPGM FÖR ATT KOMPLETTERA LADDFIL - SURPLUS/LEAKAGE             
001000*        MED RÄTT FUNKTIONSGRUPP                                          
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400*          --- LADD-FIL LEAKAGE/SURPLUS                                   
002403     SELECT W37175                     ASSIGN TO W37199D1.                
002404                                                                          
002405*          --- KOMPLETTERAD LADD-FIL LEAKAGE/SURPLUS                      
002410     SELECT W37178                     ASSIGN TO W37199D2.                
002600     EJECT                                                                
002610                                                                          
002620*          --- LAGERBAND                                                  
002630     SELECT W01160                     ASSIGN TO W37199D3.                
002640     EJECT                                                                
002650                                                                          
002700 DATA DIVISION.                                                           
002800                                                                          
002900 FILE SECTION.                                                            
003002 FD  W37175                                                               
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003006*01  -COPY W37175      -L.                                                
003007                                                                          
003008 FD  W37178                                                               
003009     RECORDING       F                                                    
003010     BLOCK CONTAINS  0.                                                   
003020*01  POST -COPY W37175 -PRE  UT-  -L.                                     
003100     EJECT                                                                
003110                                                                          
003120 FD  W01160                                                               
003130     RECORDING       F                                                    
003140     BLOCK CONTAINS  0.                                                   
003150*01  POST -COPY W01160 -PRE  W01160-  -L.                                 
003160     EJECT                                                                
003170                                                                          
003200 WORKING-STORAGE SECTION.                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'W3719900'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003801                                                                          
003802 77  W37175-EOF-SW               PIC X       VALUE 'N'.                   
003810     88  END-OF-W37175                       VALUE 'J'.                   
003820 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
003830     88  END-OF-W01160                       VALUE 'J'.                   
003900     EJECT                                                                
003910                                                                          
004000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES DAGENS-DATUM.                                       
004200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004500     EJECT                                                                
004510                                                                          
004520 01  TEST-IDARTNR                PIC  9(9)   COMP-3 VALUE ZERO.           
004530*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
004540     EJECT                                                                
004541                                                                          
004550*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
004560     EJECT                                                                
004570                                                                          
004600 01  DYNAMISKA-SUBPROGRAM.                                                
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005000                                                                          
005100*    --- PARAMETRAR TILL ABEND                                            
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005600                                                                          
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006001     EJECT                                                                
006002                                                                          
006003*    --- PARAMETRAR TILL POSTSUM                                          
006004*                                                                         
006010*01  -COPY W0005   -PRE  POSTSUM-                                         
006201     EJECT                                                                
006202                                                                          
006203 01  IN-AREA-START               PIC X(24)   VALUE                        
006204                                 'IN-AREA-START  '.                       
006206                                                                          
006207*01  AREA -COPY W37175     -PRE IN-                                       
006208     EJECT                                                                
006209                                                                          
006210 01  W01160-AREA-START           PIC X(24)   VALUE                        
006211                                 'W01160-START  '.                        
006212                                                                          
006213*01  AREA -COPY W01160     -PRE W01160-                                   
006214     EJECT                                                                
006215                                                                          
006216 01  UT-AREA-START               PIC X(24)   VALUE                        
006217                                 'UT-AREA-START  '.                       
006218                                                                          
006220*01  AREA -COPY W37175     -PRE UT-                                       
006300     EJECT                                                                
006310                                                                          
006400 PROCEDURE DIVISION.                                                      
006800                                                                          
006810 MAIN SECTION.                                                            
006900     PERFORM A-INIT                                                       
007020     PERFORM S01-READ-W37175                                              
007021     PERFORM S02-READ-W01160                                              
007030                                                                          
007100     PERFORM UNTIL END-OF-W37175                                          
007200       IF IN-IDARTNR <= W01160-CLAG-IDARTNR                               
007210         IF IN-IDPTYP = 'FAK' OR 'KRE' OR 'RET' OR 'ADJ'                  
007300           PERFORM B-BUILD-LOADREC                                        
007310           PERFORM S11-WRITE-W37178                                       
007400         END-IF                                                           
007810         PERFORM S01-READ-W37175                                          
007820       ELSE                                                               
007830         PERFORM S02-READ-W01160                                          
007840       END-IF                                                             
007900     END-PERFORM                                                          
008100                                                                          
008200     PERFORM Z-FINIT                                                      
008300                                                                          
008400     MOVE ZERO TO RETURN-CODE                                             
008500     GOBACK                                                               
008600     .                                                                    
008700     EJECT                                                                
008710                                                                          
008800 A-INIT SECTION.                                                          
008910     OPEN INPUT  W37175 W01160                                            
009010     OPEN OUTPUT W37178                                                   
009100                                                                          
009200     ACCEPT DAGENS-DATUM  FROM DATE                                       
009310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009400     .                                                                    
009500     EJECT                                                                
009510                                                                          
009520 B-BUILD-LOADREC SECTION.                                                 
009521     MOVE W01160-CLAG-IDFKNGRP TO UT-IDFKNGRP                             
009522     MOVE IN-IDPTYP            TO UT-IDPTYP                               
009523     MOVE IN-DAAAVV            TO UT-DAAAVV                               
009524                                                                          
009530     MOVE IN-IDARTNR           TO UT-IDARTNR                              
009575                                                                          
009576     MOVE IN-BEART-ENG  TO UT-BEART-ENG                                   
009581     MOVE IN-IDDISTR    TO UT-IDDISTR                                     
009582     MOVE IN-IDKUNDNR   TO UT-IDKUNDNR                                    
009583     MOVE IN-IDBYTRAP   TO UT-IDBYTRAP                                    
009584     MOVE IN-IDBYTRAD   TO UT-IDBYTRAD                                    
009585     MOVE IN-KDBYTREF   TO UT-KDBYTREF                                    
009588     MOVE IN-KVANTAL    TO UT-KVANTAL                                     
009589     MOVE IN-KVPOINT    TO UT-KVPOINT                                     
009591     .                                                                    
009592     EJECT                                                                
009593                                                                          
009600 Z-FINIT SECTION.                                                         
009701     CLOSE W37175 W01160                                                  
009710           W37178                                                         
009801                                                                          
009802     MOVE 'S' TO POSTSUM-OPKOD                                            
009810     CALL POSTSUM USING POSTSUM-PARM                                      
009900     .                                                                    
010001     EJECT                                                                
010002                                                                          
010003 S01-READ-W37175  SECTION.                                                
010004     READ W37175          INTO IN-AREA                                    
010005     AT END                                                               
010006        MOVE HIGH-VALUE   TO IN-AREA                                      
010007        SET END-OF-W37175 TO TRUE                                         
010008                                                                          
010009     NOT AT END                                                           
010010        MOVE 'W37175'   TO POSTSUM-FDNAMN                                 
010011        MOVE 'W37199D1' TO POSTSUM-DDNAMN2                                
010013        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
010014        CALL POSTSUM USING POSTSUM-PARM                                   
010015     END-READ                                                             
010020     .                                                                    
010102                                                                          
010103 S02-READ-W01160  SECTION.                                                
010104     READ W01160          INTO W01160-AREA                                
010105     AT END                                                               
010106        MOVE HIGH-VALUE   TO W01160-AREA                                  
010107        SET END-OF-W01160 TO TRUE                                         
010108                                                                          
010109     NOT AT END                                                           
010110        MOVE 'W01160'   TO POSTSUM-FDNAMN                                 
010111        MOVE 'W37199D3' TO POSTSUM-DDNAMN2                                
010112        MOVE 'LB'       TO POSTSUM-TRANSTYP                               
010113        CALL POSTSUM USING POSTSUM-PARM                                   
010114     END-READ                                                             
010115     .                                                                    
010116                                                                          
010117 S11-WRITE-W37178 SECTION.                                                
010118     WRITE UT-POST      FROM UT-AREA                                      
010119                                                                          
010120     MOVE 'UT'          TO POSTSUM-TRANSTYP                               
010121     MOVE 'W37178'      TO POSTSUM-FDNAMN                                 
010122     MOVE 'W37199D2'    TO POSTSUM-DDNAMN2                                
010123     CALL POSTSUM USING POSTSUM-PARM                                      
010130     .                                                                    
