000100*COMPOPT VRTEREUS=YES                                                     
000200**COMPOPT INCLMOD=VRTEUOPT                                                
000300                                                                          
000400 ID  DIVISION.                                                            
000500 PROGRAM-ID.    W400ARTU.                                                 
000600 AUTHOR.        MARGARETA GABRIELSSON.                                    
000700 DATE-WRITTEN.  SEPTEMBER 1997.                                           
000800                                                                          
000900*                                                                         
001000      REMARKS.                                                            
001100*                                                                         
001200*        PROGRAMMET ÄR EN SUBRUTIN SOM ANVÄNDS I DISTRIBUTIONS-           
001300*        SYSTEMET FÖR ATT ÖVERSÄTTA ISO-KDARTURS TILL NUMERISKT           
001400*        KDARTURS ELLER TILL LANDET I KLARTEXT PÅ ENGELSKA ELLER          
001500*        SVENSKA                                                          
001600*        ÖVERSÄTTER NUMERISKT TILL ISO-KDARTURS  OCKSÅ                    
001700                                                                          
001800*        LÄNKCOPYTEXT: W400ARTU                                           
001900                                                                          
002000*        WORKCOPYTEXT: W400URSP    ÖVERSÄTTNINGAR                         
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002500                                                                          
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                   PIC X(8)    VALUE 'W400ARTU'.                
003000 77  JA                      PIC X       VALUE 'J'.                       
003100 77  NEJ                     PIC X       VALUE 'N'.                       
003200     EJECT                                                                
003300*01  -COPY W400URSP                                                       
003400     EJECT                                                                
003500 LINKAGE SECTION.                                                         
003600                                                                          
003700*01  -COPY W400ARTU                                                       
003800     EJECT                                                                
003900 PROCEDURE DIVISION USING ARTU-W400ARTU.                                  
004000 MAIN SECTION.                                                            
004100                                                                          
004200     IF ARTU-KDARTURS NOT = SPACE                                         
004300       SET URSP-IX                       TO +1                            
004400       SEARCH BEARTURS-GRP AT END                                         
004500         MOVE ZERO                       TO ARTU-KDARTURS-NUM             
004600         MOVE SPACES                     TO ARTU-BEARTURS-ENG             
004700                                          ARTU-BEARTURS-SVE               
004800         WHEN ARTU-KDARTURS     =  KDARTURS (URSP-IX)                     
004900           MOVE KDARTURS-NUM(URSP-IX)    TO ARTU-KDARTURS-NUM             
005000           MOVE BEARTURS-ENG(URSP-IX)    TO ARTU-BEARTURS-ENG             
005100           MOVE BEARTURS-SVE(URSP-IX)    TO ARTU-BEARTURS-SVE             
005200       END-SEARCH                                                         
005300                                                                          
005400     ELSE                                                                 
005500      IF   ARTU-KDARTURS-NUM NUMERIC                                      
005600      AND  ARTU-KDARTURS-NUM NOT = ZERO                                   
005700       SET URSP-IX                       TO +1                            
005800       SEARCH BEARTURS-GRP AT END                                         
005900         MOVE SPACE                      TO ARTU-KDARTURS                 
006000         MOVE SPACES                     TO ARTU-BEARTURS-ENG             
006100                                          ARTU-BEARTURS-SVE               
006200         WHEN ARTU-KDARTURS-NUM  =   KDARTURS-NUM (URSP-IX)               
006300           MOVE KDARTURS(URSP-IX)        TO ARTU-KDARTURS                 
006400           MOVE BEARTURS-ENG(URSP-IX)    TO ARTU-BEARTURS-ENG             
006500           MOVE BEARTURS-SVE(URSP-IX)    TO ARTU-BEARTURS-SVE             
006600         END-SEARCH                                                       
006700                                                                          
006800       ELSE                                                               
006900         MOVE ZERO                       TO ARTU-KDARTURS-NUM             
007000         MOVE SPACES                     TO ARTU-BEARTURS-ENG             
007100                                            ARTU-BEARTURS-SVE             
007200       END-IF                                                             
007300     END-IF                                                               
007400                                                                          
007500     MOVE ZERO TO RETURN-CODE                                             
007600     GOBACK                                                               
007700     .                                                                    
