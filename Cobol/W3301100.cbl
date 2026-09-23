000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W3301100.                                        
000400 AUTHOR.                 GAVIN SMITH                                      
000500 DATE-WRITTEN.       MAY 1998.                                            
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
000901*            :  YEAR 2000 ADAPTATIONS                                     
000910*        BECAUSE THE PRECEDING PROGRAMS STILL ONLY USE                    
000920*        2 POSITIONS FOR YEAR,(W33002,6,8,AND 10)                         
000930*        THE SORT IN THESE PROGRAMS WILL RESULT IN                        
000940*        INCORRECT SEQUENCING WHEN THE YEAR 2000 IS REACHED.              
000950*                                                                         
000960*        TO CORRECT THIS , THIS PROGRAM DOES THE FOLLOWING:               
000970*                                                                         
000980*                                                                         
000990*        FILE W33018, INPUT INTO THE SALES AND TARGET SYSTEM.             
000991*        THIS FILE INCLUDES YYYY, BUT NEEDS TO BE SORTED.                 
000992*                                                                         
001000*        FILE W33011, THIS IS THE WEEKLY INPUT FILE USED TO               
001100*        UPDATE THE PRIMARY SALES REGISTER (W33013)                       
001200*        THE DATE IN THIS FILE IS IN YY FORMAT, THIS MUST                 
001300*        BE CONVERTED TO YYYY FORMAT. AFTER THIS THE FILE                 
001400*        IS SORTED TO ENABLE A MATCH WITH THE W33013 FILE.                
001500*                                                                         
001600******************************************************************        
004300*                                                                         
004400*    ABENDKODER:                                                          
004500*--------------------------                                               
004600     EJECT                                                                
004700 ENVIRONMENT DIVISION.                                                    
004800                                                                          
004900 INPUT-OUTPUT SECTION.                                                    
005000******************************************************************        
005100 FILE-CONTROL.                                                            
005200     SKIP2                                                                
005300*    ---- INFILER:                                                        
005400*                                                                         
005500     SELECT  W33011        ASSIGN  W33011D1.                              
005510     SELECT  W33018        ASSIGN  W33011D2.                              
005520*                                                                         
005600     SKIP2                                                                
005700*    ---- SORTFIL:                                                        
005701     SELECT  SORT11       ASSIGN  W33011DS.                               
005702     SELECT  SORT18       ASSIGN  W33011DS.                               
005703*                                                                         
005710*    ---- UTFILER:                                                        
005800*                            - UTPOSTER                                   
005810*                            - TO WEEKUPDAT PRIMARY REG                   
005900     SELECT  W33011U       ASSIGN  W33011D3.                              
006000*                            - TO WEEKUPDAT SALES AND TARG                
006100     SELECT  W33018U       ASSIGN  W33011D4.                              
006104******************************************************************        
006105     EJECT                                                                
006106 DATA DIVISION.                                                           
006107                                                                          
006108 FILE SECTION.                                                            
006109     SKIP2                                                                
006110 FD  W33011                                                               
006120     LABEL RECORD STANDARD                                                
006130     RECORDING  V                                                         
006140     BLOCK CONTAINS 0.                                                    
006170*01  INSUMMA-POST   -COPY W330110     -L.                                 
006171*01  INJUST-POST   -COPY W330200     -L.                                  
006180     EJECT                                                                
006190 FD  W33018                                                               
006200     LABEL RECORD STANDARD                                                
006300     RECORDING  F                                                         
006400     BLOCK CONTAINS 0.                                                    
006600*01  POST -COPY W33018  -PRE IN-  -L                                      
006800     EJECT                                                                
007000 FD  W33011U                                                              
007100     LABEL RECORD STANDARD                                                
007200     RECORDING  V                                                         
007300     BLOCK CONTAINS 0.                                                    
007400*01  POST   -COPY W330110C  -PRE UTSUMMA-   -L.                           
007500*01  POST   -COPY W330200C  -PRE UTJUST-   -L.                            
007600     EJECT                                                                
007700                                                                          
007800 FD  W33018U                                                              
007900     LABEL RECORD STANDARD                                                
008000     RECORDING  F                                                         
008100     BLOCK CONTAINS 0.                                                    
008200*01  POST   -COPY W33018   -PRE UT-   -L.                                 
008300     EJECT                                                                
008310 SD  SORT11.                                                              
008311*    RECORDING V.                                                         
008320*01  S11-POST -COPY W330110C                                              
008330*01  S20-POST -COPY W330200C                                              
008360 SD  SORT18.                                                              
008361*    RECORDING F.                                                         
008370*01  POST -COPY W33018         -PRE S18-                                  
008400 WORKING-STORAGE SECTION.                                                 
008500     SKIP2                                                                
008510                                                                          
008600*    -- CHECKED BY WY2000                                                 
008800 77  PROGRAM-NAMN            PIC X(8) VALUE 'W3301100'.                   
008900     SKIP2                                                                
009000*    ---- KONSTANTER                                                      
009100                                                                          
009200 77  JA                      PIC X       VALUE 'J'.                       
009300 77  NEJ                     PIC X       VALUE 'N'.                       
012300*    ---- END-OF-FILE SWITCHAR                                            
012400                                                                          
012500 77  W33011-SW               PIC X       VALUE 'N'.                       
012600   88  EOF-W33011                        VALUE 'J'.                       
012610 77  W33018-SW               PIC X       VALUE 'N'.                       
012620   88  EOF-W33018                        VALUE 'J'.                       
012700     EJECT                                                                
012800*    ----  SWITCHAR ÖVRIGA                                                
012900                                                                          
013000 77  INDATA-FEL              PIC X       VALUE 'N'.                       
013100                                                                          
016400*    ----  AREA FÖR INPOSTER                                              
016500     EJECT                                                                
016610*01  I11-AREA    -COPY W330110                                            
016800*01  I20-AREA    -COPY W330200  -RED I11-AREA                             
016810*01  AREA    -COPY W33018   -PRE I18-                                     
016820*    ----  SORT WORK AREAS                                                
016900*01  S11WS-AREA              -COPY W330110C                               
017000*01  S20WS-AREA              -COPY W330200C     -RED S11WS-AREA           
017010*01  AREA -PRE S18WS-  -COPY W33018                                       
017020*    ----  AREA FÖR UTPOSTER                                              
017030*01  AREA    -COPY W330110C  -PRE U11-                                    
017040*01  AREA    -COPY W330200C  -PRE U20-                                    
017050*01  AREA    -COPY W33018   -PRE U18-                                     
017100                                                                          
017200                                                                          
017300*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
017400     SKIP2                                                                
017500 01  DYNAMISKA-SUBPROGRAM.                                                
017600   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
017700   03  POSTSUM               PIC X(8)    VALUE 'POSTSUM '.                
017800     SKIP2                                                                
017900*    ---- PARAMETRAR TILL ABEND                                           
018100                                                                          
018200 01   RKOD-ABEND-UTAN-DUMP     PIC S9(4)   VALUE +16  COMP SYNC.          
018210 01   RKOD-ABEND-SORT-W33011   PIC S9(4)   VALUE +17  COMP SYNC.          
018220 01   RKOD-ABEND-SORT-W33018   PIC S9(4)   VALUE +18  COMP SYNC.          
018300     SKIP2                                                                
018400     EJECT                                                                
018500*    ----  PARAMETRAR TILL POSTSUM                                        
018600                                                                          
018700*01  -COPY W0005  -PRE POSTSUM-.                                          
018800     EJECT                                                                
025800 PROCEDURE DIVISION.                                                      
025860                                                                          
025870 STYR SECTION.                                                            
025880                                                                          
025890     PERFORM A-INIT                                                       
025900     SORT SORT18                                                          
025910             ASCENDING KEY   S18-IDARTNR                                  
025920                             S18-TIFSGVV                                  
025930                             S18-IDDISTR                                  
025950             USING         W33018                                         
025960             GIVING        W33018U                                        
025961                                                                          
025962     IF SORT-RETURN NOT = 0                                               
025963     DISPLAY 'ERROR IN SORT FOR FILE W33018'                              
025964     CALL ABEND USING RKOD-ABEND-SORT-W33018                              
025965     END-IF                                                               
025966                                                                          
025967     SORT SORT11                                                          
025968             ASCENDING KEY       IDARTNR  IN S11-POST                     
025969                                 DAFSGVV  IN S11-POST                     
025970                                 IDDISTR  IN S11-POST                     
025971                                 IDPTYP   IN S11-POST                     
025972             INPUT PROCEDURE B-SORT                                       
025973             GIVING        W33011U                                        
025974                                                                          
025975     IF SORT-RETURN NOT = 0                                               
025976     DISPLAY 'ERROR IN SORT FOR FILE W33011'                              
025977     CALL ABEND USING RKOD-ABEND-SORT-W33011                              
025978     END-IF                                                               
025982                                                                          
025983     PERFORM Z-FINIT                                                      
025984     MOVE ZERO TO RETURN-CODE                                             
025985     GOBACK                                                               
025990     .                                                                    
025991                                                                          
026000 A-INIT SECTION.                                                          
026001                                                                          
026010     OPEN INPUT    W33011                                                 
026020*                  W33018                                                 
026030*    OPEN OUTPUT   W33011U                                                
026040*                  W33018U                                                
026050                                                                          
026100     CONTINUE                                                             
026200     .                                                                    
026201                                                                          
026210 B-SORT SECTION.                                                          
026211     PERFORM  S01-LAS-INFIL-11                                            
026212                                                                          
026220     PERFORM UNTIL EOF-W33011                                             
026221                                                                          
026230       PERFORM BA-CHANGE-DATE                                             
026240       PERFORM BB-MOVE-RELEASE                                            
026250       PERFORM S01-LAS-INFIL-11                                           
026260     END-PERFORM                                                          
026270     CONTINUE                                                             
026280     .                                                                    
026281                                                                          
026298 BA-CHANGE-DATE SECTION.                                                  
026299                                                                          
026300     IF TIFSGVV  IN I11-AREA < 7000                                       
026301        COMPUTE                                                           
026302        DAFSGVV IN S11WS-AREA = TIFSGVV  IN I11-AREA + 200000             
026303        END-COMPUTE                                                       
026307     ELSE                                                                 
026308        COMPUTE                                                           
026309        DAFSGVV IN S11WS-AREA = TIFSGVV  IN I11-AREA + 190000             
026310        END-COMPUTE                                                       
026314     END-IF                                                               
026315     CONTINUE                                                             
026316     .                                                                    
026317                                                                          
026318 BB-MOVE-RELEASE SECTION.                                                 
026319                                                                          
026320     IF                                                                   
026321        IDPTYP IN I11-AREA =  '200'                                       
026322     THEN                                                                 
026323        MOVE  CORR W330200 IN I20-AREA TO W330200 IN S20WS-AREA           
026330        RELEASE  S20-POST FROM S20WS-AREA                                 
026337     ELSE                                                                 
026338        MOVE  CORR W330110 IN I11-AREA TO  W330110 IN S11WS-AREA          
026339        RELEASE S11-POST FROM S11WS-AREA                                  
026346     END-IF                                                               
026347     CONTINUE                                                             
026348     .                                                                    
026349 Z-FINIT SECTION.                                                         
026350     CLOSE         W33011                                                 
026351*                  W33018                                                 
026360*                  W33011U                                                
026370*                  W33018U                                                
026371                                                                          
026393                                                                          
026400     CONTINUE                                                             
026500     .                                                                    
026510                                                                          
026600 S01-LAS-INFIL-11 SECTION.                                                
026700                                                                          
026800     READ W33011 INTO I11-AREA                                            
026900     AT END                                                               
027000         MOVE 'J' TO W33011-SW                                            
027100     END-READ                                                             
027200     CONTINUE                                                             
027300     .                                                                    
027400                                                                          
