000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4302000.                                                
000400*AUTHOR.         MICHAEL KOWAL.                                           
000500*DATE-WRITTEN.   MARS 1982.                                               
000600*                                                                         
000700*    REMARKS.                                                             
000710*        EFTERSOM PROGRAMMET "FÖRSVUNNET" HAR DET SKRIVITS                
000720*        IN AV ANN WESTBERG 92/12/09 IGEN.                                
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET RENSAR PLOCKREGISTRET W43023 FRÅN ARTIKLAR            
001100*        SOM EJ LÄNGRE FINNS PÅ LAGERBANDET.                              
001200*                                                                         
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
002503     SELECT IN-LB                      ASSIGN TO UT-S-W43020D1.           
002506     SELECT IN-REG                     ASSIGN TO UT-S-W43020D2.           
002510     SELECT UT-REG                     ASSIGN TO UT-S-W43020D3.           
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900                                                                          
003000 FILE SECTION.                                                            
003101     SKIP3                                                                
003102 FD  IN-LB                                                                
003103     LABEL RECORD STANDARD                                                
003104     RECORDING       F                                                    
003105     BLOCK CONTAINS  0.                                                   
003106     SKIP2                                                                
003107*01  -COPY W011100      -L.                                               
003108     SKIP3                                                                
003109 FD  IN-REG                                                               
003110     LABEL RECORD STANDARD                                                
003111     RECORDING       V                                                    
003112     BLOCK CONTAINS  0.                                                   
003113     SKIP2                                                                
003114*01  -COPY W430436      -L.                                               
003115     SKIP3                                                                
003116 FD  UT-REG                                                               
003117     LABEL RECORD STANDARD                                                
003118     RECORDING       V                                                    
003119     BLOCK CONTAINS  0.                                                   
003120     SKIP2                                                                
003121 01  UTREGPOST.                                                           
003122*  03  FILLER  -COPY W430436    -L.                                       
003123     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400     SKIP2                                                                
003401                                                                          
003410*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(8)    VALUE 'W4302000'.            
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003710                                                                          
003800 01  FILLER                 PIC X(20)   VALUE 'SWITCHAR START'.           
003901                                                                          
003902 01  LBPOST-FINNS           PIC X.                                        
003903     88   LBPOST-FINNS-JA         VALUE 'J'.                              
003904     88   LBPOST-FINNS-NEJ        VALUE 'N'.                              
003905 01  REGPOST-FINNS          PIC X.                                        
003906     88  REGPOST-FINNS-JA         VALUE 'J'.                              
003907     88  REGPOST-FINNS-NEJ        VALUE 'N'.                              
003908                                                                          
003909 01  FILLER                 PIC X(20)   VALUE 'SPAR-ELEMENT'.             
003910     SKIP2                                                                
003911 01  LB-ID.                                                               
003912     03  ID-LB-IDARTNR           PIC S9(9)   COMP-3.                      
003914 01  REG-ID.                                                              
003915     03  ID-REG-IDARTNR          PIC S9(9)   COMP-3.                      
003917                                                                          
003918 01   DYNAMISKA-SUBPROGRAM.                                               
003919  03   POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
003920                                                                          
003926 01   FILLER                      PIC X(12)   VALUE 'LB-AREA'.            
003927*01  AREA -COPY W011100   -PRE LB-                                        
003928                                                                          
003929 01   FILLER                      PIC X(12)   VALUE 'REG-AREA'.           
003930*01  AREA -COPY W430436   -PRE REG-                                       
003931                                                                          
003932*    --- PARAMETRAR TILL POSTSUM                                          
003933                                                                          
003934*01  -COPY W0005   -PRE  POSTSUM-                                         
003935     EJECT                                                                
003936                                                                          
003937*    --- PARAMETRAR TILL DATKORT                                          
003938*                                                                         
003939 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W43020'.              
003940     SKIP2                                                                
003941 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
003942     SKIP2                                                                
003943*01  -COPY WDATKORT                                                       
004000     EJECT                                                                
007300 PROCEDURE DIVISION.                                                      
007500                                                                          
007700     PERFORM B-INITIERA                                                   
007800     MOVE JA TO LBPOST-FINNS REGPOST-FINNS                                
007801     PERFORM S01-LAS-LB                                                   
007810     PERFORM S02-LAS-REG                                                  
007900     PERFORM UNTIL LBPOST-FINNS-NEJ OR REGPOST-FINNS-NEJ                  
008000         IF LB-ID = REG-ID                                                
008100             PERFORM A-SKRIV-REG-POST                                     
008200             PERFORM S01-LAS-LB                                           
008300             PERFORM S02-LAS-REG                                          
008400         ELSE                                                             
008410             IF LB-ID >= REG-ID                                           
008420                 PERFORM S02-LAS-REG                                      
008430             ELSE                                                         
008440                 PERFORM S01-LAS-LB                                       
008450             END-IF                                                       
008460         END-IF                                                           
008470     END-PERFORM                                                          
008480     PERFORM C-AVSLUTA                                                    
008491     MOVE ZERO TO RETURN-CODE                                             
008492     GOBACK                                                               
009400     .                                                                    
009500     EJECT                                                                
009600 A-SKRIV-REG-POST SECTION.                                                
009701                                                                          
010000     WRITE UTREGPOST FROM REG-AREA                                        
010100     MOVE 'UT-REG' TO POSTSUM-FDNAMN                                      
010110     MOVE 'W43020D3' TO POSTSUM-DDNAMN2                                   
010200     CALL POSTSUM USING POSTSUM-PARM                                      
010500     .                                                                    
010600     EJECT                                                                
010610 B-INITIERA SECTION.                                                      
010620                                                                          
010630     OPEN INPUT  IN-REG IN-LB                                             
010650                                                                          
010660     OPEN OUTPUT UT-REG                                                   
010670                                                                          
010671     MOVE SPACE TO POSTSUM-TRANSTYP                                       
010672     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
010694     .                                                                    
010695     EJECT                                                                
010700 C-AVSLUTA SECTION.                                                       
010800                                                                          
010801     CLOSE IN-REG                                                         
010802           IN-LB                                                          
010810           UT-REG                                                         
010901                                                                          
010902     MOVE 'S' TO POSTSUM-OPKOD                                            
010910     CALL POSTSUM USING POSTSUM-PARM                                      
011000     .                                                                    
011101     EJECT                                                                
011102 S01-LAS-LB SECTION.                                                      
011103                                                                          
011104     READ IN-LB INTO LB-AREA                                              
011105     AT END                                                               
011106        MOVE HIGH-VALUE TO LB-ID                                          
011107        MOVE NEJ TO LBPOST-FINNS                                          
011108                                                                          
011109     NOT AT END                                                           
011110        MOVE LB-IDARTNR  TO ID-LB-IDARTNR                                 
011112                                                                          
011113        MOVE 'LB' TO POSTSUM-FDNAMN                                       
011114        MOVE 'W43020D1' TO POSTSUM-DDNAMN2                                
011115        CALL POSTSUM USING POSTSUM-PARM                                   
011116                                                                          
011119     END-READ                                                             
011120     .                                                                    
011121     EJECT                                                                
011122 S02-LAS-REG SECTION.                                                     
011123                                                                          
011124     READ IN-REG         INTO REG-AREA                                    
011125     AT END                                                               
011127        MOVE NEJ         TO REGPOST-FINNS                                 
011128        MOVE HIGH-VALUE  TO REG-ID                                        
011129                                                                          
011130     NOT AT END                                                           
011131        MOVE REG-IDARTNR TO ID-REG-IDARTNR                                
011133                                                                          
011134        MOVE 'IN-REG'    TO POSTSUM-FDNAMN                                
011135        MOVE 'W43020D2'  TO POSTSUM-DDNAMN2                               
011136        CALL POSTSUM     USING POSTSUM-PARM                               
011137                                                                          
011140     END-READ                                                             
011141     .                                                                    
011142     EJECT                                                                
