000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4284700.                                                
000400 AUTHOR.         LENA BROMANDER.                                          
000500 DATE-WRITTEN.   16/10/12.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SÅLLAR UT AKTUELLA FAKTURAPOSTER INFÖR SKAPANDE VIOS-LIST        
001100*        A W8XS11                                                         
001200*        UTPOST SAKNAR KDFAKTYP FÖR ATT PASSA INFIL I SKELS W8XS11        
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002501     SKIP2                                                                
002502*          --- FAKTURAFIL                                                 
002503     SELECT W42846                     ASSIGN TO W42847D1.                
002504     SKIP2                                                                
002505*          --- SÅLLAD FAKTURAFIL                                          
002510     SELECT W42847                     ASSIGN TO W42847D2.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003101     SKIP3                                                                
003102 FD  W42846                                                               
003103     RECORDING       F                                                    
003104     BLOCK CONTAINS  0.                                                   
003105                                                                          
003106*01  -COPY W42845      -L.                                                
003107     SKIP3                                                                
003108 FD  W42847                                                               
003109     RECORDING       F                                                    
003110     BLOCK CONTAINS  0.                                                   
003111                                                                          
003120*01  POST -COPY W42847 -PRE  UT-  -L.                                     
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500 77  IDPGM                       PIC X(8)    VALUE 'W4284700'.            
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003901                                                                          
003902 77  W42846-EOF-SW               PIC X       VALUE 'N'.                   
003910     88  END-OF-W42846                       VALUE 'J'.                   
004000     EJECT                                                                
004100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004200 01  FILLER REDEFINES DAGENS-DATUM.                                       
004300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004600     EJECT                                                                
004700 01  DYNAMISKA-SUBPROGRAM.                                                
004800*                                                                         
004900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005100     SKIP2                                                                
005200*    --- PARAMETRAR TILL ABEND                                            
005300                                                                          
005400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005700     SKIP2                                                                
005800 01  FELTEXT.                                                             
005900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006101     EJECT                                                                
006102*    --- PARAMETRAR TILL POSTSUM                                          
006103*                                                                         
006110*01  -COPY W0005   -PRE  POSTSUM-                                         
006301     EJECT                                                                
006302 01  IN-AREA-START               PIC X(24)   VALUE                        
006303                                 'IN-AREA-START  '.                       
006304     SKIP2                                                                
006305                                                                          
006306*01  AREA -COPY W42845     -PRE IN-                                       
006307     EJECT                                                                
006308 01  UT-AREA-START               PIC X(24)   VALUE                        
006309                                 'UT-AREA-START  '.                       
006310     SKIP2                                                                
006311                                                                          
006320*01  AREA -COPY W42847     -PRE UT-                                       
006400     EJECT                                                                
006500 PROCEDURE DIVISION.                                                      
006600 MAIN SECTION.                                                            
006800     SKIP2                                                                
006900                                                                          
007000     PERFORM A-INIT                                                       
007110     PERFORM S01-LAES-W42846                                              
007200     PERFORM UNTIL END-OF-W42846                                          
007201                                                                          
007300       IF IN-KDFAKTYP = 'R'                                               
007520         MOVE IN-IDDISTR        TO UT-IDDISTR                             
007550         MOVE IN-IDKUNDNR       TO UT-IDKUNDNR                            
007580         MOVE IN-IDDC           TO UT-IDDC                                
007592         MOVE IN-IDDC-RET       TO UT-IDDC-RET                            
007595         MOVE IN-IDFTG          TO UT-IDFTG                               
007598         MOVE IN-KDORDKL        TO UT-KDORDKL                             
007601         MOVE IN-KVRADER        TO UT-KVRADER                             
007605         MOVE IN-PRARTNTO       TO UT-PRARTNTO                            
007608         MOVE IN-TIFAKT-SAAPP   TO UT-TIFAKT-SAAPP                        
007610         PERFORM S11-SKRIV-W42847                                         
007700       END-IF                                                             
007800                                                                          
007910       PERFORM S01-LAES-W42846                                            
008000     END-PERFORM                                                          
008100                                                                          
008200                                                                          
008300     PERFORM Z-FINIT                                                      
008400                                                                          
008500     MOVE ZERO TO RETURN-CODE                                             
008600     GOBACK                                                               
008700     .                                                                    
008800     EJECT                                                                
008900 A-INIT SECTION.                                                          
009001                                                                          
009010     OPEN INPUT  W42846                                                   
009101                                                                          
009110     OPEN OUTPUT W42847                                                   
009200     SKIP2                                                                
009300     ACCEPT DAGENS-DATUM  FROM DATE                                       
009410     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009500     .                                                                    
009600     EJECT                                                                
009700 Z-FINIT SECTION.                                                         
009801     CLOSE W42846                                                         
009810           W42847                                                         
009901     SKIP2                                                                
009902     MOVE 'S' TO POSTSUM-OPKOD                                            
009910     CALL POSTSUM USING POSTSUM-PARM                                      
010000     .                                                                    
010101     EJECT                                                                
010102 S01-LAES-W42846  SECTION.                                                
010103     READ W42846 INTO IN-AREA                                             
010104     AT END                                                               
010105        MOVE HIGH-VALUE TO IN-AREA                                        
010106        SET END-OF-W42846 TO TRUE                                         
010107                                                                          
010108     NOT AT END                                                           
010109        MOVE 'W42846' TO POSTSUM-FDNAMN                                   
010110        MOVE 'W42847D1' TO POSTSUM-DDNAMN2                                
010111                                                                          
010112        MOVE SPACE     TO POSTSUM-TRANSTYP                                
010113        CALL POSTSUM USING POSTSUM-PARM                                   
010114     END-READ                                                             
010120     .                                                                    
010201     EJECT                                                                
010202 S11-SKRIV-W42847 SECTION.                                                
010203                                                                          
010204     WRITE UT-POST FROM UT-AREA                                           
010205                                                                          
010206     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
010207     MOVE 'W42847' TO POSTSUM-FDNAMN                                      
010208     MOVE 'W42847D2' TO POSTSUM-DDNAMN2                                   
010209     CALL POSTSUM USING POSTSUM-PARM                                      
010210     .                                                                    
010400     EJECT                                                                
010500 S99-ABEND SECTION.                                                       
010600                                                                          
010701     SKIP2                                                                
010702     MOVE 'S' TO POSTSUM-OPKOD                                            
010710     CALL POSTSUM USING POSTSUM-PARM                                      
010800     CALL ABEND USING RKOD-ABEND                                          
010900     .                                                                    
