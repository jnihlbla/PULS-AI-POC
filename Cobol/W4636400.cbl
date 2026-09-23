000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4636400.                                                 
000400 AUTHOR.        BO SVENSSON.                                              
000500 DATE-WRITTEN.  NOV  2000.                                                
000600                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*        PROGRAMMET ÄR ETT SB-PGM.                                        
001101*                                                                         
001200*        PGM. LÄSER WDB7, VIPS KUNDUPPGIFTER.                             
001210*        SKAPAR FIL SOM SKALL SKICKAS TILL                                
001600*        IMS ADVANSYS FÖR VIDARE SPRIDNING                                
001700*        TILL DIRECT BUSINESS LEVERANTÖRER                                
001701*        I EUROPA.                                                        
001702*                                                                         
001710*    DB.                                                                  
001800*        DB              WDB7   (SB)                                      
001900*                                                                         
002000*    UTDATA.                                                              
002100*        FIL             W46368                                           
002100*                                                                         
002100*        CCID 10277114 -> NEW MAIL DESTINATION                            
002200                                                                          
002300     EJECT                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700     SKIP2                                                                
002800 FILE-CONTROL.                                                            
002900                                                                          
003112*          --- KONTROLLFIL MED SENAST MOTTAGNA SERIENUMMER                
003113     SELECT W46363I                    ASSIGN TO W46364D1.                
003114     SKIP2                                                                
003115*          --- UTFIL, KONTROLLFIL                                         
003116     SELECT W46363U                    ASSIGN TO W46364D2.                
003117     SKIP2                                                                
003118*          --- UTFIL TILL IMS-ADVANSYS                                    
003119     SELECT W46368                     ASSIGN TO W46364D3.                
003130                                                                          
003200 DATA DIVISION.                                                           
003300 FILE SECTION.                                                            
003400                                                                          
003600     SKIP3                                                                
003700 FD  W46363I                                                              
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  POST -COPY W46363 -PRE  INSER-  -L.                                  
004110                                                                          
004200     SKIP3                                                                
004210 FD  W46368                                                               
004220     LABEL RECORD    STANDARD                                             
004230     RECORDING       F                                                    
004240     BLOCK CONTAINS  0.                                                   
004250                                                                          
004260*01  UT-POST  -COPY WINVBCS0  -L                                          
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
004600 77  PROGRAM-NAMN                PIC X(08) VALUE 'W4636400'.              
004700                                                                          
004800 77  JA                          PIC X(1)    VALUE 'J'.                   
004900 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004910 77  W-IX                        PIC 9(3)    VALUE ZERO.                  
005000     SKIP3                                                                
005001 01  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
005002     SKIP3                                                                
005003 01  DAGENS-YYYYMMDD-X.                                                   
005004     03  DAGENS-SS               PIC 9(2)    VALUE ZERO.                  
005005     03  DAGENS-YYMMDD           PIC 9(6)    VALUE ZERO.                  
005006 01  DAGENS-YYYYMMDD REDEFINES DAGENS-YYYYMMDD-X                          
005007                                 PIC 9(8).                                
005008     SKIP2                                                                
005010*    --- TABELL FÖR LANDKOD                                               
005020*                                                                         
005030*01  -COPY W463LAND                                                       
005031     SKIP2                                                                
005032*    --- TABELL FÖR RECORDTYP SUMMOR                                      
005033*                                                                         
005034*01  -COPY W463RSUM                                                       
005040     SKIP2                                                                
005200 01  FILLER                      PIC X(8)    VALUE 'UT-AREA '.            
005210*                                                                         
005400*01  AREA    -COPY WINVBCS0  -PRE  UT                                     
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
006230     SKIP2                                                                
006231*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
006232*                                                                         
006233     SKIP2                                                                
006234 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006235     SKIP3                                                                
006236                                                                          
006600 01  IMS-WS.                                                              
006700                                                                          
006800     03  STATUS-WS               PIC X(2).                                
006900        88  SEGMENT-FINNS                    VALUE '  '.                  
007100        88  END-OF-DATA                      VALUE 'GB'.                  
007200                                                                          
007300     03  GODK-STATUSKODER.                                                
007400         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
007410     SKIP3                                                                
007420 01  SSA1                        PIC X(64).                               
007500                                                                          
007600     EJECT                                                                
007700*01  -COPY W0003                                                          
007900     EJECT                                                                
008000*01  -COPY W0005   -PRE POSTSUM-.                                         
008200     EJECT                                                                
008210 01  FILLER                      PIC X(16)   VALUE                        
008220                                             'WDB701-AREA'.               
008300 01  DLI-IO-AREA.                                                         
008400     03  IO-AREA                 PIC X(600).                              
008500     SKIP3                                                                
008600*    03  WDB701    -COPY WDB701  -RED IO-AREA                             
009700     EJECT                                                                
009800 LINKAGE SECTION.                                                         
009900     SKIP3                                                                
010000*01  -COPY W0008   -PRE WDB7-                                             
010200     05  FILLER                  PIC X.                                   
010330     EJECT                                                                
010400 PROCEDURE DIVISION  USING WDB7-PCB.                                      
010500     ENTRY 'DLITCBL' USING WDB7-PCB.                                      
010600                                                                          
010700     PERFORM A-INIT                                                       
010710     PERFORM S02-LAES-W46363                                              
010720     ADD 1 TO INUT-VTYR-SERIALNO                                          
010730     PERFORM C-HEADER                                                     
010800     PERFORM IMS-GN-WDB7                                                  
010900     PERFORM UNTIL END-OF-DATA                                            
011000       EVALUATE WDB7-SEG-NAME-FB                                          
011100         WHEN 'WDB701'                                                    
011110           SEARCH ALL X2-LAND-ING                                         
011120              AT END                                                      
011130                 CONTINUE                                                 
011140              WHEN X2-SOK(X2-IX) = GMTD-IDLANDX2                          
011143                   PERFORM B-SKAPA-POST                                   
011170           END-SEARCH                                                     
012300       END-EVALUATE                                                       
012400       PERFORM IMS-GN-WDB7                                                
012500     END-PERFORM                                                          
012600                                                                          
012602     PERFORM D-TRAILER                                                    
012610     PERFORM S03-SKRIV-W46363                                             
012620                                                                          
012700     PERFORM Z-FINIT                                                      
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 A-INIT SECTION.                                                          
013300                                                                          
013410     OPEN INPUT  W46363I                                                  
013411                                                                          
013420     OPEN OUTPUT W46368                                                   
013430                 W46363U                                                  
013500                                                                          
013510     ACCEPT DAGENS-YYMMDD FROM DATE                                       
013511     ACCEPT DAGENS-TID    FROM TIME                                       
013520     MOVE 20              TO DAGENS-SS                                    
013521     SET POSTTYP-IX       TO 18                                           
013530                                                                          
013600     MOVE PROGRAM-NAMN    TO POSTSUM-PROGNAMN                             
013700     .                                                                    
013850 B-SKAPA-POST SECTION.                                                    
013900                                                                          
013910     IF GMTD-FLDIRAFF = 'Y'                                               
014000       MOVE SPACE                 TO UTAREA                               
014010       MOVE 'VTYR'                TO UTS0-PROGRAM                         
014020       MOVE '03'                  TO UTS0-FILEDEFNO                       
014030       MOVE 'VMD'                 TO UTS0-SENDLOC                         
014040       MOVE 'IMS'                 TO UTS0-RECLOC                          
014050       MOVE INUT-VTYR-SERIALNO    TO UTS0-SERIALNO                        
014060       MOVE 'S'                   TO UTS0-RECORDTYPE                      
014070       MOVE X2-LAND(X2-IX)        TO UTS0-BILL-LOC                        
014080       MOVE GMTD-IDDEALER-VIPS    TO UTS0-CUST-ORDERING                   
014090       MOVE 'A'                   TO UTS0-SUBRECORD-TYPE                  
014092       MOVE GMTD-BEDEALER-VIPSINV TO UTS0-BILL-ADDR-1                     
014093       MOVE GMTD-ADDEALER-INVRAD1 TO UTS0-BILL-ADDR-2                     
014094       MOVE GMTD-ADDEALER-INVRAD2 TO UTS0-BILL-ADDR-3                     
014095       MOVE GMTD-ADPOSTNR-INV     TO UTS0-BILL-ADDR-4                     
014096       MOVE GMTD-ADCITY-INV       TO UTS0-BILL-ADDR-5                     
014100                                                                          
014110       PERFORM S01-SKRIV-W46368                                           
014111       ADD 1 TO POSTTYP-RAKNARE(POSTTYP-IX)                               
014112                                                                          
014113       MOVE SPACE                 TO UTAREA                               
014114       MOVE 'VTYR'                TO UTS0-PROGRAM                         
014115       MOVE '03'                  TO UTS0-FILEDEFNO                       
014116       MOVE 'VMD'                 TO UTS0-SENDLOC                         
014117       MOVE 'IMS'                 TO UTS0-RECLOC                          
014118       MOVE INUT-VTYR-SERIALNO    TO UTS0-SERIALNO                        
014120       MOVE 'S'                   TO UTS0-RECORDTYPE                      
014121       MOVE X2-LAND(X2-IX)        TO UTS0-BILL-LOC                        
014122       MOVE GMTD-IDDEALER-VIPS    TO UTS0-CUST-ORDERING                   
014123       MOVE 'B'                   TO UTS0-SUBRECORD-TYPE                  
014124       MOVE GMTD-BEDEALER-VIPSGMT TO UTS0-SHIP-ADDR-1                     
014125       MOVE GMTD-ADDEALER-GMTRAD1 TO UTS0-SHIP-ADDR-2                     
014126       MOVE GMTD-ADDEALER-GMTRAD2 TO UTS0-SHIP-ADDR-3                     
014127       MOVE GMTD-ADPOSTNR-GMT     TO UTS0-SHIP-ADDR-4                     
014128       MOVE GMTD-ADCITY-GMT       TO UTS0-SHIP-ADDR-5                     
014137                                                                          
014138       PERFORM S01-SKRIV-W46368                                           
014139       ADD 1 TO POSTTYP-RAKNARE(POSTTYP-IX)                               
014140                                                                          
014141       MOVE SPACE                 TO UTAREA                               
014142       MOVE 'VTYR'                TO UTS0-PROGRAM                         
014143       MOVE '03'                  TO UTS0-FILEDEFNO                       
014144       MOVE 'VMD'                 TO UTS0-SENDLOC                         
014145       MOVE 'IMS'                 TO UTS0-RECLOC                          
014146       MOVE INUT-VTYR-SERIALNO    TO UTS0-SERIALNO                        
014148       MOVE 'S'                   TO UTS0-RECORDTYPE                      
014149       MOVE X2-LAND(X2-IX)        TO UTS0-BILL-LOC                        
014150       MOVE GMTD-IDDEALER-VIPS    TO UTS0-CUST-ORDERING                   
014151       MOVE 'C'                   TO UTS0-SUBRECORD-TYPE                  
014152       MOVE GMTD-IDDEALER-VIPSINV TO UTS0-CUST-ACC-CODE                   
014153                                                                          
014154       IF GMTD-KDKNDSTA = '2'                                             
014155       OR GMTD-KDKNDSTA = '3'                                             
014156       OR GMTD-KDCREDIT = '2'                                             
014158         MOVE 'H'                 TO UTS0-CREDIT-RATE                     
014159       ELSE                                                               
014160         MOVE 'A'                 TO UTS0-CREDIT-RATE                     
014161       END-IF                                                             
014162       MOVE 0                     TO UTS0-SALES-REG                       
014164                                                                          
014165       IF (GMTD-KDKNDSTA = '2'                                            
014166       OR  GMTD-KDKNDSTA = '3')                                           
014172       AND GMTD-DAREGDAT > 0                                              
014173       AND GMTD-DAREGDAT < DAGENS-YYYYMMDD - 20000                        
014174         MOVE GMTD-DAREGDAT       TO UTS0-EXPIRE-DATE                     
014175         ADD  20000               TO UTS0-EXPIRE-DATE                     
014176       ELSE                                                               
014177         MOVE ZERO                TO UTS0-EXPIRE-DATE                     
014178       END-IF                                                             
014180                                                                          
014190       PERFORM S01-SKRIV-W46368                                           
014191       ADD 1 TO POSTTYP-RAKNARE(POSTTYP-IX)                               
014192     END-IF                                                               
014193                                                                          
016900     .                                                                    
016951     EJECT                                                                
016960 C-HEADER SECTION.                                                        
017000                                                                          
017100     MOVE SPACE                 TO UTAREA                                 
017200     MOVE 'VTYR'                TO UTH0-PROGRAM                           
017300     MOVE '03'                  TO UTH0-FILEDEFNO                         
017400     MOVE 'VMD'                 TO UTH0-SENDLOC                           
017500     MOVE 'IMS'                 TO UTH0-RECLOC                            
017510     MOVE INUT-VTYR-SERIALNO    TO UTH0-SERIALNO                          
017520     MOVE 'A'                   TO UTH0-RECORDTYPE                        
017530     MOVE DAGENS-YYYYMMDD       TO UTH0-CREDATE                           
017540     MOVE DAGENS-TID            TO UTH0-CRETIME                           
017550     MOVE 'N.N.'                TO UTH0-CONTACT-NAME                      
017560     MOVE 'N/A'                 TO UTH0-CONTACT-PHONE                     
017570     MOVE 'WSYST@VOLVOCARS.COM'                                           
017571                                TO UTH0-CONTACT-EMAIL                     
017572                                                                          
017580     PERFORM S01-SKRIV-W46368                                             
017600     .                                                                    
017601     EJECT                                                                
017602 D-TRAILER SECTION.                                                       
017603                                                                          
017604     MOVE SPACE                 TO UTAREA                                 
017605     MOVE 'VTYR'                TO UTT0-PROGRAM                           
017606     MOVE '03'                  TO UTT0-FILEDEFNO                         
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
017618     SET POSTTYP-IX TO 18                                                 
017619     MOVE 18        TO W-IX                                               
017620     MOVE POSTTYP-RAKNARE(POSTTYP-IX)                                     
017621                    TO UTT0-RECORD-COUNTS(W-IX)                           
017622                                                                          
017623     PERFORM S01-SKRIV-W46368                                             
017624     .                                                                    
017625     EJECT                                                                
017626 Z-FINIT  SECTION.                                                        
017627                                                                          
017628     CLOSE W46363I                                                        
017629           W46368                                                         
017630           W46363U                                                        
017631                                                                          
017632     MOVE 'S'          TO POSTSUM-OPKOD                                   
017633                                                                          
017634     CALL POSTSUM USING POSTSUM-PARM                                      
017635     .                                                                    
017636     EJECT                                                                
017637 S01-SKRIV-W46368 SECTION.                                                
017638                                                                          
017639     WRITE UT-POST     FROM UTAREA                                        
017640                                                                          
017641     MOVE 'W46368'     TO POSTSUM-FDNAMN                                  
017642     MOVE 'W46364D2'   TO POSTSUM-DDNAMN2                                 
017643     MOVE SPACE        TO POSTSUM-TRANSTYP                                
017644                                                                          
017645     CALL POSTSUM USING POSTSUM-PARM                                      
017646     .                                                                    
017647     EJECT                                                                
017648 S02-LAES-W46363  SECTION.                                                
017649                                                                          
017650     READ W46363I INTO INUT-W46363                                        
017651     AT END                                                               
017652        MOVE 'SERIENUMMERPOST SAKNAS W46363' TO FELTEXT-STR               
017653        DISPLAY FELTEXT                                                   
017654        CALL FELLOG                                                       
017655                                                                          
017656     NOT AT END                                                           
017657        MOVE 'W46363I'   TO POSTSUM-FDNAMN                                
017658        MOVE 'W46364D1' TO POSTSUM-DDNAMN2                                
017659        MOVE SPACE      TO POSTSUM-TRANSTYP                               
017660        CALL POSTSUM USING POSTSUM-PARM                                   
017661     END-READ                                                             
017662     .                                                                    
017663     EJECT                                                                
017664 S03-SKRIV-W46363 SECTION.                                                
017665                                                                          
017666     WRITE UTSER-POST FROM INUT-W46363                                    
017667                                                                          
017668     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
017669     MOVE 'W46363U'   TO POSTSUM-FDNAMN                                   
017670     MOVE 'W46364D3' TO POSTSUM-DDNAMN2                                   
017671     CALL POSTSUM USING POSTSUM-PARM                                      
017672     .                                                                    
017680     EJECT                                                                
017700 IMS-GN-WDB7 SECTION.                                                     
017800                                                                          
017900     MOVE '  GAGKGB'       TO GODK-STATUSKODER                            
018000     CALL CBLTDLI USING GN WDB7-PCB DLI-IO-AREA                           
018100     MOVE WDB7-STATUS-CODE TO STATUS-WS                                   
018200     PERFORM IMS-STATUSKONTROLL                                           
018300     .                                                                    
018400 IMS-STATUSKONTROLL SECTION.                                              
018500                                                                          
018600     SET STATUS-IX TO 1                                                   
018700     SEARCH GODK-STATUS                                                   
018800       AT END CALL FELLOG                                                 
018900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
019000     END-SEARCH                                                           
019100     .                                                                    
