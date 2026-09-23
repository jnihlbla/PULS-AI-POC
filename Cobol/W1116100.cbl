000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W1116100.                                                
000400 AUTHOR.         GÖRAN KJELLSON.                                          
000500 DATE-WRITTEN.   19/01/15.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        JUSTERA ERS.KOD FÖR POSTER TILL VR                               
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002401                                                                          
002402*          --- ERSÄTTNINGSFIL (CENTRALT ARTIKEL REGISTER)                 
002403     SELECT W91045                     ASSIGN TO W11161D1.                
002404                                                                          
002405*          --- NDC ARTIKELINFO SORTERAD ART/DC                            
002406     SELECT W01184                     ASSIGN TO W11161D2.                
002407                                                                          
002408*          --- ERSÄTTNINGSFIL VR                                          
002410     SELECT W11161                     ASSIGN TO W11161D3.                
002600                                                                          
002610                                                                          
002700 DATA DIVISION.                                                           
002800                                                                          
002900 FILE SECTION.                                                            
003001                                                                          
003002 FD  W91045                                                               
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003006*01  -COPY W91045      -L.                                                
003007                                                                          
003008                                                                          
003009 FD  W01184                                                               
003010     RECORDING       F                                                    
003011     BLOCK CONTAINS  0.                                                   
003012                                                                          
003013*01  -COPY W01184      -L.                                                
003014                                                                          
003015 FD  W11161                                                               
003016     RECORDING       F                                                    
003017     BLOCK CONTAINS  0.                                                   
003018                                                                          
003020*01  POST -COPY W11161 -PRE  UT-  -L.                                     
003100                                                                          
003110                                                                          
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W1116100'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003801                                                                          
003802 77  W91045-EOF-SW               PIC X       VALUE 'N'.                   
003803     88  END-OF-W91045                       VALUE 'J'.                   
003804                                                                          
003805 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
003810     88  END-OF-W01184                       VALUE 'J'.                   
003900                                                                          
004500                                                                          
004600 01  W-IDARTNR                   PIC S9(9)   VALUE ZERO COMP-3.           
004601 01  SW-CHANGE-KDERS             PIC X(1)    VALUE 'J'.                   
004610                                                                          
004620 01  DYNAMISKA-SUBPROGRAM.                                                
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005000                                                                          
005100*    --- PARAMETRAR TILL ABEND                                            
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005600                                                                          
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006001                                                                          
006002*    --- PARAMETRAR TILL POSTSUM                                          
006003*                                                                         
006010*01  -COPY W0005   -PRE  POSTSUM-                                         
006201                                                                          
006202                                                                          
006203 01  IN-AREA-START               PIC X(24)   VALUE                        
006204                                 'IN-AREA-START  '.                       
006206                                                                          
006207*01  AREA -COPY W91045     -PRE IN-                                       
006208                                                                          
006209 01  LB-AREA-START               PIC X(24)   VALUE                        
006210                                 'LB-AREA-START  '.                       
006211                                                                          
006212                                                                          
006213*01  AREA -COPY W01184     -PRE LB-                                       
006214                                                                          
006215 01  UT-AREA-START               PIC X(24)   VALUE                        
006216                                 'UT-AREA-START  '.                       
006217                                                                          
006218                                                                          
006220*01  AREA -COPY W11161     -PRE UT-                                       
006300                                                                          
006310                                                                          
006400 PROCEDURE DIVISION.                                                      
006500 MAIN SECTION.                                                            
006800                                                                          
006900     PERFORM A-INIT                                                       
007001     PERFORM S01-LAES-W91045                                              
007010     PERFORM S02-LAES-W01184                                              
007100     PERFORM UNTIL END-OF-W91045                                          
007300                                                                          
007310        MOVE IN-IDARTNR  TO UT-IDARTNR                                    
007320        MOVE IN-KDERS    TO UT-KDERS                                      
007330        IF IN-TIERSDAT > 16010 AND IN-TIERSDAT < 60010                    
007400          IF IN-KDERS = 21 OR 22 OR 24 OR 27 OR 28 OR 29                  
007500                                                                          
007502             PERFORM UNTIL END-OF-W01184                                  
007503                        OR LB-SLAG-IDARTNR NOT < IN-IDARTNR               
007504                PERFORM S02-LAES-W01184                                   
007505             END-PERFORM                                                  
007506                                                                          
007507             IF END-OF-W01184                                             
007508             OR LB-SLAG-IDARTNR NOT = IN-IDARTNR                          
007509                MOVE NEJ TO SW-CHANGE-KDERS                               
007510             ELSE                                                         
007511                MOVE NEJ TO SW-CHANGE-KDERS                               
007512                PERFORM UNTIL END-OF-W01184                               
007513                           OR LB-SLAG-IDARTNR NOT = IN-IDARTNR            
007514                           OR SW-CHANGE-KDERS = JA                        
007515                   IF LB-SLAG-TIERSDAT-VIPS = ZERO                        
007517                      MOVE JA TO SW-CHANGE-KDERS                          
007518                   END-IF                                                 
007519                   PERFORM S02-LAES-W01184                                
007520                END-PERFORM                                               
007521             END-IF                                                       
007522                                                                          
007523             IF SW-CHANGE-KDERS = JA                                      
007525                IF IN-KDERS = 21 OR 24 OR 29                              
007526                   SUBTRACT 10 FROM UT-KDERS                              
007527                ELSE                                                      
007528                   SUBTRACT 20 FROM UT-KDERS                              
007529                END-IF                                                    
007530             END-IF                                                       
007700          END-IF                                                          
007701        END-IF                                                            
007702                                                                          
007706        PERFORM S11-SKRIV-W11161                                          
007710        PERFORM S01-LAES-W91045                                           
007720                                                                          
007800     END-PERFORM                                                          
008100                                                                          
008200     PERFORM Z-FINIT                                                      
008300                                                                          
008400     MOVE ZERO TO RETURN-CODE                                             
008500     GOBACK                                                               
008600     .                                                                    
008700                                                                          
008710                                                                          
008800 A-INIT SECTION.                                                          
008901                                                                          
008902     OPEN INPUT  W91045                                                   
008910                 W01184                                                   
009001                                                                          
009010     OPEN OUTPUT W11161                                                   
009100                                                                          
009310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009400     .                                                                    
009500                                                                          
009510                                                                          
009600 Z-FINIT SECTION.                                                         
009701     CLOSE W91045                                                         
009702           W01184                                                         
009710           W11161                                                         
009801                                                                          
009802     MOVE 'S' TO POSTSUM-OPKOD                                            
009810     CALL POSTSUM USING POSTSUM-PARM                                      
009900     .                                                                    
010001                                                                          
010002                                                                          
010003 S01-LAES-W91045  SECTION.                                                
010004     READ W91045 INTO IN-AREA                                             
010005     AT END                                                               
010006        MOVE HIGH-VALUE   TO IN-AREA                                      
010007        SET END-OF-W91045 TO TRUE                                         
010008                                                                          
010009     NOT AT END                                                           
010010        MOVE 'W91045'     TO POSTSUM-FDNAMN                               
010011        MOVE 'W11161D1'   TO POSTSUM-DDNAMN2                              
010013        MOVE SPACE        TO POSTSUM-TRANSTYP                             
010014        CALL POSTSUM USING POSTSUM-PARM                                   
010015     END-READ                                                             
010016     .                                                                    
010017                                                                          
010018                                                                          
010019 S02-LAES-W01184  SECTION.                                                
010020     READ W01184 INTO LB-AREA                                             
010021     AT END                                                               
010022        MOVE HIGH-VALUE TO LB-AREA                                        
010023        SET END-OF-W01184 TO TRUE                                         
010024                                                                          
010025     NOT AT END                                                           
010026        MOVE 'W01184'     TO POSTSUM-FDNAMN                               
010027        MOVE 'W11161D2'   TO POSTSUM-DDNAMN2                              
010029        MOVE SPACE        TO POSTSUM-TRANSTYP                             
010030        CALL POSTSUM USING POSTSUM-PARM                                   
010031     END-READ                                                             
010040     .                                                                    
010101                                                                          
010102                                                                          
010103 S11-SKRIV-W11161 SECTION.                                                
010104                                                                          
010105     WRITE UT-POST FROM UT-AREA                                           
010106                                                                          
010107     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
010108     MOVE 'W11161'   TO POSTSUM-FDNAMN                                    
010109     MOVE 'W11161D3' TO POSTSUM-DDNAMN2                                   
010110     CALL POSTSUM USING POSTSUM-PARM                                      
010120     .                                                                    
010300                                                                          
010310                                                                          
010400 S99-ABEND SECTION.                                                       
010500                                                                          
010602     MOVE 'S' TO POSTSUM-OPKOD                                            
010610     CALL POSTSUM USING POSTSUM-PARM                                      
010700     CALL ABEND USING RKOD-ABEND                                          
010800     .                                                                    
