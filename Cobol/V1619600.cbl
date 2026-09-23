000100***************************************************************           
000200 ID  DIVISION.                                                            
000300***************************************************************           
000400 PROGRAM-ID.    V1619600.                                                 
000500 AUTHOR.        CARINA HOLMQVIST                                          
000600 DATE-WRITTEN.  DECEMBER  1991                                            
000700***************************************************************           
000800*                                                                         
000900*REMARKS:                                                                 
001000*                                                                         
001100*  * RETURNS                                                              
001200*       EXPANDED DATASET NAME                                             
001210*       CREATION DATE                                                     
001211*       LAST VALID DATE                                                   
001220*                                                                         
001300*  * PARMS:                                                               
001400*                                                                         
001500*       DSNAME          X(44)                                             
001600*       NO-VALID-DAYS   9(3)                                              
001610*       CREATE-DATE     X(6) (YYMMDD)                                     
001620*       LAST-VALID-DATE X(6) (YYMMDD)                                     
001700*                                                                         
001800*  * RETURNCODES:                                                         
001900*                                                                         
002000*         ALWAYS ZERO                                                     
002300*                                                                         
002400***************************************************************           
002500     EJECT                                                                
002600***************************************************************           
002700 ENVIRONMENT DIVISION.                                                    
002800***************************************************************           
002900     SKIP2                                                                
003000*--------------------------------------------------------------           
003100 CONFIGURATION SECTION.                                                   
003200*--------------------------------------------------------------           
003300 SOURCE-COMPUTER. IBM-370.                                                
003400*SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
003500     SKIP2                                                                
003600***************************************************************           
003700 DATA DIVISION.                                                           
003800***************************************************************           
003900     SKIP2                                                                
004000*--------------------------------------------------------------           
004100 WORKING-STORAGE SECTION.                                                 
004200*--------------------------------------------------------------           
004300 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1619600'.             
004400 01  ABEND-SECTION                PIC X(25) VALUE SPACE.                  
004500 01  RETURN-CODES.                                                        
004600     03  RCODE                    PIC S9(4) COMP SYNC VALUE ZERO.         
004700     03  RCODE-4                  PIC S9(4) COMP SYNC VALUE 4.            
004800     03  RCODE-8                  PIC S9(4) COMP SYNC VALUE 8.            
004900     03  RCODE-12                 PIC S9(4) COMP SYNC VALUE 12.           
004910     03  RCODE-16                 PIC S9(4) COMP SYNC VALUE 16.           
005000     03  RCODE-20                 PIC S9(4) COMP SYNC VALUE 20.           
005100     03  RCODE-DISPL              PIC Z(4)-.                              
005200     SKIP2                                                                
005300 01  GENERAL-KONSTANTER.                                                  
005400     03 YES                       PIC X(1)  VALUE 'Y'.                    
005500     03 NOO                       PIC X(1)  VALUE 'N'.                    
005600     SKIP2                                                                
005610 01  W-VARIABLES.                                                         
005620     03 W-USERID                  PIC X(7)  VALUE SPACE.                  
005630     03 W-DSNAME                  PIC X(37) VALUE SPACE.                  
005700 01 SWITCHES.                                                             
005800     03 SW-ERROR                  PIC X(1)  VALUE 'N'.                    
005810     03 SW-MIGRAT                 PIC X(1)  VALUE 'N'.                    
005900     SKIP2                                                                
006000*--------------------------------------------------------------           
006100*DYNAMIC SUBPROGRAMS                                                      
006200*--------------------------------------------------------------           
006300 01  DYNAMIC-SUBPROGRAMS.                                                 
006310     03 V16195                    PIC X(8)  VALUE 'V16195  '.             
006400     03 GETCDATE                  PIC X(8)  VALUE 'GETCDATE'.             
006500     03 AYDATRUT                  PIC X(8)  VALUE 'AYDATRUT'.             
006700     SKIP2                                                                
006800 01  GETCDATE-PARMS.                                                      
006900     03 GETCDATE-DSNAME           PIC X(44) VALUE SPACE.                  
007000     03 GETCDATE-YEAR             PIC S9(4) COMP.                         
007100     03 GETCDATE-DAYS             PIC S9(4) COMP.                         
007200     SKIP2                                                                
007210 01  GETCDATE-YEAR-CHAR           PIC X(4).                               
007220 01  GETCDATE-DAYS-CHAR           PIC X(4).                               
007230 01  VGETLID-PARMS.                                                       
007240     03 VGETLID-LENGTH            PIC S9(4) COMP VALUE ZERO.              
007250     03 VGETLID-LOGONID           PIC X(8)  VALUE SPACE.                  
007300 01  AYDATRUT-PARMS.                                                      
007400     03 AYDATRUT-FUNCTION              PIC X(2).                          
007410     03 AYDATRUT-IN-DATE.                                                 
007420        05 AYDATRUT-IN-YEAR            PIC X(2).                          
007430        05 AYDATRUT-IN-DAYS            PIC X(3).                          
007440        05 FILLER                      PIC X(3).                          
007491     03 FILLER REDEFINES AYDATRUT-IN-DATE.                                
007492        05 AYDATRUT-IN-DATE-NUM        PIC 9(5).                          
007493        05 FILLER                      PIC XXX.                           
007496     03 AYDATRUT-OUT-DATE.                                                
007497        05 AYDATRUT-OUT-CENTURY        PIC X(2).                          
007498        05 AYDATRUT-OUT-YEAR           PIC X(2).                          
007499        05 AYDATRUT-OUT-MONTH          PIC X(2).                          
007500        05 AYDATRUT-OUT-DAY            PIC X(2).                          
007501     03 FILLER REDEFINES AYDATRUT-OUT-DATE.                               
007502        05 AYDATRUT-OUT-DATE-NUM       PIC 9(5).                          
007503        05 FILLER                      PIC XXX.                           
007505     03 AYDATRUT-VERSION               PIC X(4).                          
007700     SKIP2                                                                
008400*--------------------------------------------------------------           
008500 LINKAGE SECTION.                                                         
008600*--------------------------------------------------------------           
008700 01 PARM.                                                                 
008800     03 PARM-DSNAME            PIC X(44).                                 
008900     03 PARM-NO-VALID-DAYS     PIC 9(3).                                  
008901     03 PARM-CREATE-YEAR       PIC X(2).                                  
008902     03 PARM-CREATE-MONTH      PIC X(2).                                  
008903     03 PARM-CREATE-DAY        PIC X(2).                                  
008904     03 PARM-LAST-VALID-YEAR   PIC X(2).                                  
008905     03 PARM-LAST-VALID-MONTH  PIC X(2).                                  
008906     03 PARM-LAST-VALID-DAY    PIC X(2).                                  
009000*************************************************************             
009210 PROCEDURE DIVISION USING PARM.                                           
009300*************************************************************             
009400     PERFORM A-INIT                                                       
009401     IF SW-ERROR = NOO THEN                                               
009402       PERFORM B-INIT-DSNAME                                              
009410       PERFORM C-CREATION-DATE                                            
009420       IF SW-ERROR = NOO THEN                                             
009421         IF SW-MIGRAT = YES THEN                                          
009422           MOVE 'MI'  TO PARM-CREATE-YEAR                                 
009423           MOVE 'GR'  TO PARM-CREATE-MONTH                                
009424           MOVE 'AT'  TO PARM-CREATE-DAY                                  
009425           MOVE '- '  TO PARM-LAST-VALID-YEAR                             
009426           MOVE SPACE TO PARM-LAST-VALID-MONTH                            
009427           MOVE SPACE TO PARM-LAST-VALID-DAY                              
009428         ELSE                                                             
009429           PERFORM D-LAST-VALID-DATE                                      
009430           IF SW-ERROR = YES THEN                                         
009431             MOVE '- '  TO PARM-LAST-VALID-YEAR                           
009432             MOVE SPACE TO PARM-LAST-VALID-MONTH                          
009433             MOVE SPACE TO PARM-LAST-VALID-DAY                            
009434           END-IF                                                         
009435         END-IF                                                           
009437       ELSE                                                               
009438          MOVE '- '  TO PARM-CREATE-YEAR                                  
009439          MOVE SPACE TO PARM-CREATE-MONTH                                 
009440          MOVE SPACE TO PARM-CREATE-DAY                                   
009441          MOVE '- '  TO PARM-LAST-VALID-YEAR                              
009442          MOVE SPACE TO PARM-LAST-VALID-MONTH                             
009443          MOVE SPACE TO PARM-LAST-VALID-DAY                               
009444       END-IF                                                             
009445     ELSE                                                                 
009449       MOVE '- '  TO PARM-CREATE-YEAR                                     
009450       MOVE SPACE TO PARM-CREATE-MONTH                                    
009451       MOVE SPACE TO PARM-CREATE-DAY                                      
009452       MOVE '- '  TO PARM-LAST-VALID-YEAR                                 
009453       MOVE SPACE TO PARM-LAST-VALID-MONTH                                
009454       MOVE SPACE TO PARM-LAST-VALID-DAY                                  
009455     END-IF                                                               
009460     PERFORM Z-FINIT                                                      
010900     GOBACK                                                               
011000     CONTINUE.                                                            
011100                                                                          
011200*--------------------------------------------------------------           
011300 A-INIT SECTION.                                                          
011400*--------------------------------------------------------------           
011500     MOVE 'A-INIT                  ' TO ABEND-SECTION                     
011600D    DISPLAY ABEND-SECTION                                                
011700     SKIP2                                                                
011800     MOVE NOO TO SW-ERROR                                                 
011801     MOVE NOO TO SW-MIGRAT                                                
011901     IF PARM-DSNAME = SPACE                                               
011902        MOVE YES TO SW-ERROR                                              
011903     END-IF                                                               
011910     CALL V16195 USING VGETLID-LENGTH                                     
011920                       VGETLID-LOGONID                                    
011930     MOVE VGETLID-LOGONID TO W-USERID                                     
012000     CONTINUE.                                                            
012100     EJECT                                                                
013310*--------------------------------------------------------------           
013320 B-INIT-DSNAME SECTION.                                                   
013330*--------------------------------------------------------------           
013340     MOVE 'B-INIT-DSNAME       ' TO ABEND-SECTION                         
013350D    DISPLAY ABEND-SECTION                                                
013360     SKIP2                                                                
013370     IF PARM-DSNAME(1:2) = '*.' THEN                                      
013372        MOVE PARM-DSNAME(2:37) TO W-DSNAME                                
013375        MOVE W-USERID          TO PARM-DSNAME(1:7)                        
013377        MOVE W-DSNAME          TO PARM-DSNAME(8:37)                       
013390     END-IF                                                               
013391     CONTINUE.                                                            
013392     EJECT                                                                
013400*--------------------------------------------------------------           
013500 C-CREATION-DATE SECTION.                                                 
013600*--------------------------------------------------------------           
013700     MOVE 'C-CREATION-DATE           ' TO ABEND-SECTION                   
013800D    DISPLAY ABEND-SECTION                                                
013900     SKIP2                                                                
014000     MOVE PARM-DSNAME TO GETCDATE-DSNAME                                  
014010*                                            -- CREATION DATE 電DD        
014020     CALL GETCDATE USING GETCDATE-DSNAME                                  
014030                         GETCDATE-YEAR                                    
014040                         GETCDATE-DAYS                                    
014042     IF RETURN-CODE = 0 THEN                                              
014043       MOVE GETCDATE-DSNAME TO PARM-DSNAME                                
014050       MOVE GETCDATE-YEAR   TO GETCDATE-YEAR-CHAR                         
014060       MOVE GETCDATE-DAYS   TO GETCDATE-DAYS-CHAR                         
014070       MOVE GETCDATE-YEAR-CHAR(3:2) TO AYDATRUT-IN-YEAR                   
014080       MOVE GETCDATE-DAYS-CHAR(2:3) TO AYDATRUT-IN-DAYS                   
014090*                                            -- 電DDD -> 電電MMDD         
014100       MOVE '08'          TO AYDATRUT-FUNCTION                            
014110       MOVE '1920'        TO AYDATRUT-VERSION                             
014120       CALL AYDATRUT USING AYDATRUT-FUNCTION                              
014130                           AYDATRUT-IN-DATE                               
014140                           AYDATRUT-OUT-DATE                              
014150                           AYDATRUT-VERSION                               
014151       IF RETURN-CODE = 0 THEN                                            
014152          MOVE AYDATRUT-OUT-YEAR TO PARM-CREATE-YEAR                      
014153          MOVE AYDATRUT-OUT-MONTH TO PARM-CREATE-MONTH                    
014154          MOVE AYDATRUT-OUT-DAY  TO PARM-CREATE-DAY                       
014156       ELSE                                                               
014157          MOVE YES TO SW-ERROR                                            
014161       END-IF                                                             
014162     ELSE                                                                 
014163       IF RETURN-CODE = 6 THEN                                            
014164          MOVE GETCDATE-DSNAME TO PARM-DSNAME                             
014165          MOVE YES TO SW-MIGRAT                                           
014166       ELSE                                                               
014167          MOVE YES TO SW-ERROR                                            
014168       END-IF                                                             
014169     END-IF                                                               
014200     CONTINUE.                                                            
014300     EJECT                                                                
014400*--------------------------------------------------------------           
014500 D-LAST-VALID-DATE SECTION.                                               
014600*--------------------------------------------------------------           
014700     MOVE 'D-LAST-VALID-DATE       ' TO ABEND-SECTION                     
014800D    DISPLAY ABEND-SECTION                                                
014900     SKIP2                                                                
015000     MOVE GETCDATE-YEAR-CHAR(3:2) TO AYDATRUT-IN-YEAR                     
015100     MOVE GETCDATE-DAYS-CHAR(2:3) TO AYDATRUT-IN-DAYS                     
015200*                                            -- 電DDD -> RRRRR            
015300     MOVE '13'          TO AYDATRUT-FUNCTION                              
015400     MOVE '1920'        TO AYDATRUT-VERSION                               
015500     CALL AYDATRUT USING AYDATRUT-FUNCTION                                
015600                         AYDATRUT-IN-DATE                                 
015700                         AYDATRUT-OUT-DATE                                
015800                         AYDATRUT-VERSION                                 
016000     MOVE AYDATRUT-OUT-DATE TO AYDATRUT-IN-DATE                           
016100     COMPUTE AYDATRUT-IN-DATE-NUM = AYDATRUT-IN-DATE-NUM +                
016110                                     PARM-NO-VALID-DAYS                   
016200*                                            -- RRRRR -> 電電MMDD         
016300     MOVE '06'          TO AYDATRUT-FUNCTION                              
016400     MOVE '1920'        TO AYDATRUT-VERSION                               
016500     CALL AYDATRUT USING AYDATRUT-FUNCTION                                
016600                         AYDATRUT-IN-DATE                                 
016700                         AYDATRUT-OUT-DATE                                
016800                         AYDATRUT-VERSION                                 
016810     IF RETURN-CODE = 0 THEN                                              
016820        MOVE AYDATRUT-OUT-YEAR TO PARM-LAST-VALID-YEAR                    
016830        MOVE AYDATRUT-OUT-MONTH TO PARM-LAST-VALID-MONTH                  
016840        MOVE AYDATRUT-OUT-DAY  TO PARM-LAST-VALID-DAY                     
016850     ELSE                                                                 
016860        MOVE YES TO SW-ERROR                                              
016870     END-IF                                                               
017600     CONTINUE.                                                            
017700     EJECT                                                                
017800*--------------------------------------------------------------           
017900 Z-FINIT SECTION.                                                         
018000*--------------------------------------------------------------           
018100     MOVE 'Z-FINIT                 ' TO ABEND-SECTION                     
018200D    DISPLAY ABEND-SECTION                                                
018300     SKIP2                                                                
018400     MOVE RCODE TO RETURN-CODE                                            
018700     CONTINUE.                                                            
018800     EJECT                                                                
