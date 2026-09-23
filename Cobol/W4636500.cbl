000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4636500.                                                 
000400 AUTHOR.        BO SVENSSON.                                              
000500 DATE-WRITTEN.  DEC  2000.                                                
000600                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001200*        PROGRAMMET SAMLAR IHOP RESPONSETRANSAKTIONER                     
001210*        FÖR DIRECT BUSINESS TILL EN FIL SOM SKALL                        
001600*        SKICKAS TILL IMS ADVANSYS.                                       
001702*                                                                         
001900*                                                                         
002000*    INDATA.                                                              
002100*        FIL             W46365                                           
002101*                        W46370                                           
002102*                        W46363 SERIENUMMER                               
002103*                                                                         
002110*    UTDATA.                                                              
002120*        FIL             W46369                                           
002130*                        W46363 SERIENUMMER                               
002200                                                                          
002300     EJECT                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700     SKIP2                                                                
002800 FILE-CONTROL.                                                            
002900                                                                          
003112*          --- FELFIL FRÅN KONTROLLPGM.                                   
003113     SELECT W46365                     ASSIGN TO W46365D1.                
003114     SKIP2                                                                
003115*          --- FEEDBACK POSTER FRÅN VIPS.                                 
003116     SELECT W46370                     ASSIGN TO W46365D2.                
003117     SKIP2                                                                
003118*          --- KONTROLLFIL MED SENAST MOTTAGNA SERIENUMMER                
003119     SELECT W46363I                    ASSIGN TO W46365D3.                
003120     SKIP2                                                                
003121*          --- UTFIL, KONTROLLFIL                                         
003122     SELECT W46363U                    ASSIGN TO W46365D4.                
003123     SKIP2                                                                
003124*          --- FEEDBACK FIL TILL IMS ADVANSYS                             
003125     SELECT W46369                     ASSIGN TO W46365D5.                
003130                                                                          
003200 DATA DIVISION.                                                           
003300 FILE SECTION.                                                            
003400                                                                          
003600     SKIP3                                                                
003700 FD  W46365                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100 01  INFP-POST       PIC X(190).                                          
004110                                                                          
004111     SKIP3                                                                
004112 FD  W46370                                                               
004113     RECORDING       F                                                    
004114     BLOCK CONTAINS  0.                                                   
004115                                                                          
004116 01  INV0-POST       PIC X(204).                                          
004117                                                                          
004120     SKIP3                                                                
004130 FD  W46363I                                                              
004140     RECORDING       F                                                    
004150     BLOCK CONTAINS  0.                                                   
004160                                                                          
004170*01  POST -COPY W46363 -PRE  INSER-  -L.                                  
004180                                                                          
004200     SKIP3                                                                
004210 FD  W46369                                                               
004220     LABEL RECORD    STANDARD                                             
004230     RECORDING       F                                                    
004240     BLOCK CONTAINS  0.                                                   
004250                                                                          
004260*01  UT-POST  -COPY WINVBFV0  -L                                          
004300                                                                          
004301     SKIP3                                                                
004302 FD  W46363U                                                              
004303     RECORDING       F                                                    
004304     BLOCK CONTAINS  0.                                                   
004305                                                                          
004306*01  POST -COPY W46363 -PRE  UTSER-  -L.                                  
004380                                                                          
004390     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500*                                                                         
004600 77  PROGRAM-NAMN                PIC X(08) VALUE 'W4636500'.              
004700                                                                          
004800 77  JA                          PIC X(1)    VALUE 'J'.                   
004900 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004910 77  W-IX                        PIC 9(3)    VALUE ZERO.                  
005000     SKIP3                                                                
005001                                                                          
005002 77  W46365-EOF-SW               PIC X       VALUE 'N'.                   
005003     88  END-OF-W46365                       VALUE 'J'.                   
005004                                                                          
005005 77  W46370-EOF-SW               PIC X       VALUE 'N'.                   
005006     88  END-OF-W46370                       VALUE 'J'.                   
005007                                                                          
005008 01  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
005009     SKIP3                                                                
005010 01  DAGENS-YYYYMMDD-X.                                                   
005011     03  DAGENS-SS               PIC 9(2)    VALUE ZERO.                  
005012     03  DAGENS-YYMMDD           PIC 9(6)    VALUE ZERO.                  
005013 01  DAGENS-YYYYMMDD REDEFINES DAGENS-YYYYMMDD-X                          
005014                                 PIC 9(8).                                
005015     SKIP2                                                                
005020*                                                                         
005032*    --- TABELL FÖR RECORDTYP SUMMOR                                      
005033*                                                                         
005034*01  -COPY W463RSUM                                                       
005040     SKIP2                                                                
005200 01  FILLER                      PIC X(8)    VALUE 'UT-AREA '.            
005210*                                                                         
005400*01  AREA    -COPY WINVBFV0  -PRE  UT                                     
005401*01  HEAD    -COPY WINVBHU0  -PRE  UT -RED UTAREA                         
005402*01  TRAI    -COPY WINVBTU0  -PRE  UT -RED UTAREA                         
005403                                                                          
005404     SKIP2                                                                
005405 01  INUT-AREA-START             PIC X(24)   VALUE                        
005406                                 'INUT-AREA-START  '.                     
005407*                                                                         
005408*01  -COPY W46363  -PRE INUT-                                             
005409                                                                          
005410     EJECT                                                                
005411 01  INFP-AREA-START             PIC X(24)   VALUE                        
005412                                 'INFP-AREA-START  '.                     
005413     SKIP2                                                                
005414                                                                          
005415 01  INFP-AREA.                                                           
005416     03 INFP-S-DEL.                                                       
005417       05  INFP-1-21             PIC X(21).                               
005418       05  INFP-FELKOD           PIC 9(3).                                
005419       05  INFP-25-30            PIC X(6).                                
005420     03 INFP-URSP-POST           PIC X(160).                              
005431                                                                          
005432     EJECT                                                                
005433 01  INV0-AREA-START             PIC X(24)   VALUE                        
005434                                 'INV0-AREA-START  '.                     
005435     SKIP2                                                                
005436                                                                          
005437 01  INV0-AREA.                                                           
005438     03 INV0-URSP-POST           PIC X(155).                              
005439     03 INV0-VALID-IND           PIC X(1).                                
005440     03 INV0-VALID-FEEDB         PIC X(48).                               
005441                                                                          
005450     EJECT                                                                
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800*                                                                         
005810     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006200                                                                          
006215     EJECT                                                                
006216*    --- PARAMETRAR TILL ABEND                                            
006217                                                                          
006218 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006219 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006220     SKIP2                                                                
006221 01  FELTEXT.                                                             
006222     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006223     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007900     EJECT                                                                
008000*01  -COPY W0005   -PRE POSTSUM-.                                         
008200     EJECT                                                                
010400 PROCEDURE DIVISION.                                                      
010500 MAIN SECTION.                                                            
010600                                                                          
010700     PERFORM A-INIT                                                       
010710     PERFORM S02-LAES-W46363                                              
010711     PERFORM S04-LAES-W46365                                              
010712     PERFORM S05-LAES-W46370                                              
010713                                                                          
010714     IF NOT END-OF-W46365                                                 
010715     OR NOT END-OF-W46370                                                 
010720       ADD 1 TO INUT-VTYR-SERIALNO                                        
010730       PERFORM C-HEADER                                                   
010900       PERFORM UNTIL END-OF-W46365                                        
011143         PERFORM B-SKAPA-POST                                             
012400         PERFORM S04-LAES-W46365                                          
012500       END-PERFORM                                                        
012600                                                                          
012602       PERFORM UNTIL END-OF-W46370                                        
012608         PERFORM E-SKAPA-POST                                             
012611         PERFORM S05-LAES-W46370                                          
012612       END-PERFORM                                                        
012613                                                                          
012614       PERFORM D-TRAILER                                                  
012615     END-IF                                                               
012616                                                                          
012617     PERFORM S03-SKRIV-W46363                                             
012620                                                                          
012700     PERFORM Z-FINIT                                                      
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 A-INIT SECTION.                                                          
013300                                                                          
013410     OPEN INPUT  W46365                                                   
013411                 W46370                                                   
013412                 W46363I                                                  
013413                                                                          
013420     OPEN OUTPUT W46369                                                   
013430                 W46363U                                                  
013500                                                                          
013510     ACCEPT DAGENS-YYMMDD FROM DATE                                       
013511     ACCEPT DAGENS-TID    FROM TIME                                       
013520     MOVE 20              TO DAGENS-SS                                    
013521     SET POSTTYP-IX       TO 21                                           
013530                                                                          
013600     MOVE PROGRAM-NAMN    TO POSTSUM-PROGNAMN                             
013700     .                                                                    
013800     EJECT                                                                
013850 B-SKAPA-POST SECTION.                                                    
013900                                                                          
014000     MOVE SPACE                 TO UTAREA                                 
014010     MOVE 'VTYR'                TO UTV0-PROGRAM                           
014020     MOVE '04'                  TO UTV0-FILEDEFNO                         
014030     MOVE 'VMD'                 TO UTV0-SENDLOC                           
014040     MOVE 'IMS'                 TO UTV0-RECLOC                            
014050     MOVE INUT-VTYR-SERIALNO    TO UTV0-SERIALNO                          
014060     MOVE 'V'                   TO UTV0-RECORDTYPE                        
014061     MOVE INFP-URSP-POST(1:155) TO UTV0-ORIG-BILL-DATA                    
014070     MOVE 'N'                   TO UTV0-VALIDITY-IND                      
014071                                                                          
014080     MOVE 1                     TO W-IX                                   
014090     PERFORM UNTIL W-IX > 12                                              
014091       MOVE ZERO                TO UTV0-ERROR-CODE(W-IX)                  
014092       ADD 1                    TO W-IX                                   
014093     END-PERFORM                                                          
014094     MOVE INFP-FELKOD           TO UTV0-ERROR-CODE(1)                     
014100                                                                          
014200                                                                          
014300     ADD 1 TO POSTTYP-RAKNARE(POSTTYP-IX)                                 
014400     PERFORM S01-SKRIV-W46369                                             
014500                                                                          
016900     .                                                                    
016910     EJECT                                                                
016920 E-SKAPA-POST SECTION.                                                    
016930                                                                          
016940     MOVE SPACE                 TO UTAREA                                 
016950     MOVE 'VTYR'                TO UTV0-PROGRAM                           
016951     MOVE '04'                  TO UTV0-FILEDEFNO                         
016952     MOVE 'VMD'                 TO UTV0-SENDLOC                           
016953     MOVE 'IMS'                 TO UTV0-RECLOC                            
016954     MOVE INUT-VTYR-SERIALNO    TO UTV0-SERIALNO                          
016955     MOVE 'V'                   TO UTV0-RECORDTYPE                        
016956     MOVE INV0-URSP-POST        TO UTV0-ORIG-BILL-DATA                    
016957     MOVE INV0-VALID-IND        TO UTV0-VALIDITY-IND                      
016959     MOVE INV0-VALID-FEEDB      TO UTV0-FEEDB-DATA                        
016966                                                                          
016967     ADD 1 TO POSTTYP-RAKNARE(POSTTYP-IX)                                 
016968     PERFORM S01-SKRIV-W46369                                             
016969                                                                          
016970     .                                                                    
016971     EJECT                                                                
016980 C-HEADER SECTION.                                                        
017000                                                                          
017100     MOVE SPACE                 TO UTAREA                                 
017200     MOVE 'VTYR'                TO UTH0-PROGRAM                           
017300     MOVE '04'                  TO UTH0-FILEDEFNO                         
017400     MOVE 'VMD'                 TO UTH0-SENDLOC                           
017500     MOVE 'IMS'                 TO UTH0-RECLOC                            
017510     MOVE INUT-VTYR-SERIALNO    TO UTH0-SERIALNO                          
017520     MOVE 'A'                   TO UTH0-RECORDTYPE                        
017530     MOVE DAGENS-YYYYMMDD       TO UTH0-CREDATE                           
017540     MOVE DAGENS-TID            TO UTH0-CRETIME                           
017550     MOVE 'N.N.'                TO UTH0-CONTACT-NAME                      
017560     MOVE '+46 31???????'       TO UTH0-CONTACT-PHONE                     
017570     MOVE '?????@???????'       TO UTH0-CONTACT-EMAIL                     
017571                                                                          
017580     PERFORM S01-SKRIV-W46369                                             
017600     .                                                                    
017601     EJECT                                                                
017602 D-TRAILER SECTION.                                                       
017603                                                                          
017604     MOVE SPACE                 TO UTAREA                                 
017605     MOVE 'VTYR'                TO UTT0-PROGRAM                           
017606     MOVE '04'                  TO UTT0-FILEDEFNO                         
017607     MOVE 'VMD'                 TO UTT0-SENDLOC                           
017608     MOVE 'IMS'                 TO UTT0-RECLOC                            
017609     MOVE INUT-VTYR-SERIALNO    TO UTT0-SERIALNO                          
017610     MOVE 'Z'                   TO UTT0-RECORDTYPE                        
017611                                                                          
017612     MOVE 1 TO W-IX                                                       
017613     PERFORM UNTIL W-IX > 24                                              
017614       MOVE ZERO TO UTT0-RECORD-COUNTS(W-IX)                              
017615       ADD 1 TO W-IX                                                      
017616     END-PERFORM                                                          
017617                                                                          
017618     SET POSTTYP-IX TO 21                                                 
017619     MOVE 21        TO W-IX                                               
017620     MOVE POSTTYP-RAKNARE(POSTTYP-IX)                                     
017621                    TO UTT0-RECORD-COUNTS(W-IX)                           
017622                                                                          
017623     PERFORM S01-SKRIV-W46369                                             
017624     .                                                                    
017625     EJECT                                                                
017626 Z-FINIT  SECTION.                                                        
017627                                                                          
017628     CLOSE W46365                                                         
017629           W46370                                                         
017630           W46363I                                                        
017631           W46369                                                         
017632           W46363U                                                        
017633                                                                          
017634     MOVE 'S'          TO POSTSUM-OPKOD                                   
017635                                                                          
017636     CALL POSTSUM USING POSTSUM-PARM                                      
017637     .                                                                    
017638     EJECT                                                                
017639 S01-SKRIV-W46369 SECTION.                                                
017640                                                                          
017641     WRITE UT-POST     FROM UTAREA                                        
017642                                                                          
017643     MOVE 'W46369'     TO POSTSUM-FDNAMN                                  
017644     MOVE 'W46365D4'   TO POSTSUM-DDNAMN2                                 
017645     MOVE SPACE        TO POSTSUM-TRANSTYP                                
017646                                                                          
017647     CALL POSTSUM USING POSTSUM-PARM                                      
017648     .                                                                    
017649     EJECT                                                                
017650 S02-LAES-W46363  SECTION.                                                
017651                                                                          
017652     READ W46363I INTO INUT-W46363                                        
017653     AT END                                                               
017654        MOVE 'SERIENUMMERPOST SAKNAS W46363' TO FELTEXT-STR               
017655        DISPLAY FELTEXT                                                   
017656        PERFORM S99-ABEND                                                 
017657                                                                          
017658     NOT AT END                                                           
017659        MOVE 'W46363I'   TO POSTSUM-FDNAMN                                
017660        MOVE 'W46365D3' TO POSTSUM-DDNAMN2                                
017661        MOVE SPACE      TO POSTSUM-TRANSTYP                               
017662        CALL POSTSUM USING POSTSUM-PARM                                   
017663     END-READ                                                             
017664     .                                                                    
017665     EJECT                                                                
017666 S03-SKRIV-W46363 SECTION.                                                
017667                                                                          
017668     WRITE UTSER-POST FROM INUT-W46363                                    
017669                                                                          
017670     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
017671     MOVE 'W46363U'   TO POSTSUM-FDNAMN                                   
017672     MOVE 'W46365D5' TO POSTSUM-DDNAMN2                                   
017673     CALL POSTSUM USING POSTSUM-PARM                                      
017674     .                                                                    
017675     EJECT                                                                
017676 S04-LAES-W46365  SECTION.                                                
017677                                                                          
017678     READ W46365 INTO INFP-AREA                                           
017679     AT END                                                               
017680        MOVE HIGH-VALUE   TO INFP-AREA                                    
017681        SET END-OF-W46365 TO TRUE                                         
017800                                                                          
017900     NOT AT END                                                           
018000        MOVE 'W46365'   TO POSTSUM-FDNAMN                                 
018100        MOVE 'W46365D1' TO POSTSUM-DDNAMN2                                
018200        MOVE SPACE      TO POSTSUM-TRANSTYP                               
018300        CALL POSTSUM USING POSTSUM-PARM                                   
018400     END-READ                                                             
018500     .                                                                    
018600     EJECT                                                                
018700 S05-LAES-W46370  SECTION.                                                
018800                                                                          
018900     READ W46370 INTO INV0-AREA                                           
018910     AT END                                                               
018920        MOVE HIGH-VALUE   TO INV0-AREA                                    
018930        SET END-OF-W46370 TO TRUE                                         
019400                                                                          
019500     NOT AT END                                                           
019600        MOVE 'W46370'   TO POSTSUM-FDNAMN                                 
019700        MOVE 'W46365D2' TO POSTSUM-DDNAMN2                                
019800        MOVE SPACE      TO POSTSUM-TRANSTYP                               
019900        CALL POSTSUM USING POSTSUM-PARM                                   
020000     END-READ                                                             
020100     .                                                                    
020200     EJECT                                                                
020300 S99-ABEND SECTION.                                                       
020400                                                                          
020500     SKIP2                                                                
020600     MOVE 'S' TO POSTSUM-OPKOD                                            
020700     CALL POSTSUM USING POSTSUM-PARM                                      
020800     CALL ABEND USING RKOD-ABEND-MED-DUMP                                 
020900     .                                                                    
