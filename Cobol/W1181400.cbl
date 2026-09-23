000124 ID DIVISION.                                                             
000224 PROGRAM-ID.     W1181400.                                                
000324 AUTHOR.         SATHISH T.                                               
000424 DATE-WRITTEN.   Dec 2024.                                                
000524 DATE-COMPILED.                                                           
000624                                                                          
000724*                                                                         
000824*    FUNCTION:                                                            
000924*        FORMATS THE CAPTURED ERROR RECORDS FROM PRINS-TCPLM.             
001024*        THE OUTPUT FILE WILL BE SENT THRU D&P AS MAIL ATTACHMENT         
001124*                                                                         
001224*                                                                         
001324*    ABENDCODES:                                                          
001424*        U0016 -  . . . .                                                 
001524*        U1000 -  . . . .                                                 
001624*                                                                         
001724                                                                          
001824     SKIP3                                                                
001924 ENVIRONMENT DIVISION.                                                    
002024     SKIP2                                                                
002124 INPUT-OUTPUT SECTION.                                                    
002224                                                                          
002324 FILE-CONTROL.                                                            
002424     SKIP2                                                                
002524*          --- ERROR FILE FROM  W11811                                    
002624     SELECT INFILE               ASSIGN TO W11814D1.                      
002724     SKIP2                                                                
002824*          --- OUTPUT .CSV FILE TO D&P                                    
002924     SELECT OUTFILE              ASSIGN TO W11814D2.                      
003024     SKIP2                                                                
003124                                                                          
003224     EJECT                                                                
003324 DATA DIVISION.                                                           
003424     SKIP3                                                                
003524 FILE SECTION.                                                            
003624     SKIP3                                                                
003724 FD  INFILE                                                               
003824     RECORDING       F                                                    
003924     BLOCK CONTAINS  0.                                                   
004024                                                                          
005124*01  -COPY W1181102  -L.                                                  
006024     SKIP3                                                                
006124                                                                          
006200 FD  OUTFILE                                                              
006300     RECORDING       V                                                    
006424     BLOCK CONTAINS  0.                                                   
006524                                                                          
006624 01  UT-RECORD                   PIC X(400).                              
006700     EJECT                                                                
006824                                                                          
006900 WORKING-STORAGE SECTION.                                                 
007000                                                                          
007100 77  IDPGM                       PIC X(8)    VALUE 'W1181400'.            
007600                                                                          
007700 77  INFILE-EOF-SW               PIC X       VALUE 'N'.                   
007800     88  END-OF-INFILE                       VALUE 'J'.                   
007900                                                                          
008000 77  WS-INDX                     PIC 9(2)    VALUE ZERO.                  
008100 77  MAX-INDX                    PIC 9(2)    VALUE 10.                    
008124                                                                          
008224 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
009024 01  FILLER REDEFINES TODAYS-DATE.                                        
010024     03  TODAYS-DATE-YEAR        PIC 9(2).                                
010124     03  TODAYS-DATE-MONTH       PIC 9(2).                                
010224     03  TODAYS-DATE-DAY         PIC 9(2).                                
010324     EJECT                                                                
010424                                                                          
010425 01  WS-DAP-LINE1.                                                        
010426     03  FILLER                  PIC X(336)  VALUE                        
010427                                 '¤DAPW11814-001'.                        
010428 01  WS-DAP-LINE2.                                                        
010429     03  FILLER                  PIC X(4)    VALUE '¤DAP'.                
010430     03  WS-DAP-TYPE             PIC X(2)    VALUE SPACE.                 
010440     03  FILLER                  PIC X(328)  VALUE SPACE.                 
010460                                                                          
010500 01  GENERAL-SUBPROGRAMS.                                                 
010600*                                                                         
010700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010900     SKIP2                                                                
011000*    --- PARAMETERS TO ABEND                                              
011100                                                                          
011200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
011500     SKIP2                                                                
011600 01  ERROR-TEXT.                                                          
011610     03  FILLER                  PIC X(11)   VALUE 'ERROR-TEXT'.          
011620     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
011630     EJECT                                                                
011700*    --- PARAMETRAR TILL POSTSUM                                          
011800*                                                                         
011900*01  -COPY W0005   -PRE  POSTSUM-                                         
012000     EJECT                                                                
012100 01  IN-AREA-START               PIC X(24)   VALUE                        
012200                                 'IN-AREA-START  '.                       
012300     SKIP2                                                                
012400                                                                          
012500*01  AREA -COPY W1181102   -PRE IN-                                       
012600     EJECT                                                                
012700 01  UT-AREA-START               PIC X(24)   VALUE                        
012800                                 'UT-AREA-START  '.                       
012900     SKIP2                                                                
013024 01  UT-AREA                     PIC X(400)  VALUE SPACE.                 
013124     EJECT                                                                
014000                                                                          
015024 01  UT-HEADER.                                                           
015124     03  UT-H-IDARTNR            PIC X(11)   VALUE 'PART NUMBER'.         
015224     03  FILLER                  PIC X(01)   VALUE ';'.                   
015225     03  UT-H-IDBERED            PIC X(15)   VALUE                        
015226                                               'PART PLANNER ID'.         
015227     03  FILLER                  PIC X(01)   VALUE ';'.                   
015228     03  UT-H-IDCDS              PIC X(05)   VALUE 'CDSID'.               
015229     03  FILLER                  PIC X(01)   VALUE ';'.                   
015230     03  UT-H-SPAREPARTUNIT      PIC X(14)   VALUE                        
015231                                              'SPAREPART UNIT'.           
015240     03  FILLER                  PIC X(01)   VALUE ';'.                   
015324     03  UT-H-ERRMSG1            PIC X(08)   VALUE 'ERR MSG1'.            
015424     03  FILLER                  PIC X(01)   VALUE ';'.                   
015524     03  UT-H-ERRMSG2            PIC X(08)   VALUE 'ERR MSG2'.            
015624     03  FILLER                  PIC X(01)   VALUE ';'.                   
015700     03  UT-H-ERRMSG3            PIC X(08)   VALUE 'ERR MSG3'.            
015800     03  FILLER                  PIC X(01)   VALUE ';'.                   
015900     03  UT-H-ERRMSG4            PIC X(08)   VALUE 'ERR MSG4'.            
016000     03  FILLER                  PIC X(01)   VALUE ';'.                   
016100     03  UT-H-ERRMSG5            PIC X(08)   VALUE 'ERR MSG5'.            
016200     03  FILLER                  PIC X(01)   VALUE ';'.                   
016300     03  UT-H-ERRMSG6            PIC X(08)   VALUE 'ERR MSG6'.            
016400     03  FILLER                  PIC X(01)   VALUE ';'.                   
016500     03  UT-H-ERRMSG7            PIC X(08)   VALUE 'ERR MSG7'.            
016600     03  FILLER                  PIC X(01)   VALUE ';'.                   
016700     03  UT-H-ERRMSG8            PIC X(08)   VALUE 'ERR MSG8'.            
016800     03  FILLER                  PIC X(01)   VALUE ';'.                   
016900     03  UT-H-ERRMSG9            PIC X(08)   VALUE 'ERR MSG9'.            
017000     03  FILLER                  PIC X(01)   VALUE ';'.                   
017100     03  UT-H-ERRMSG10           PIC X(09)   VALUE 'ERR MSG10'.           
017200     03  FILLER                  PIC X(01)   VALUE ';'.                   
017300                                                                          
017400 01  UT-RAD.                                                              
017500     03  UT-IDARTNR              PIC X(09)   VALUE ZERO.                  
017600     03  FILLER                  PIC X(01)   VALUE ';'.                   
017700     03  UT-IDBERED              PIC X(02)   VALUE SPACE.                 
017710     03  FILLER                  PIC X(01)   VALUE ';'.                   
017720     03  UT-IDCDS                PIC X(08)   VALUE ZERO.                  
017721     03  FILLER                  PIC X(01)   VALUE ';'.                   
017722     03  UT-SPAREPARTUNIT        PIC X(08)   VALUE ZERO.                  
017723     03  FILLER                  PIC X(01)   VALUE ';'.                   
017724     03  UT-GRP-FELTEXT OCCURS 10.                                        
017800         05  UT-FELTEXT          PIC X(30)   VALUE ZERO.                  
017900         05  FILLER              PIC X(01)   VALUE ';'.                   
018000     EJECT                                                                
018100                                                                          
018200 PROCEDURE DIVISION.                                                      
018300 MAIN SECTION.                                                            
018400     SKIP2                                                                
018500                                                                          
018600     PERFORM A-INIT                                                       
018724                                                                          
018824     PERFORM S01-READ-INFILE                                              
018924                                                                          
018926     PERFORM S10-WRITE-HEADER                                             
019010                                                                          
019224                                                                          
019300     PERFORM UNTIL END-OF-INFILE                                          
019400                                                                          
019500       IF IN-SPAREPARTUNIT = WS-DAP-TYPE                                  
019700          CONTINUE                                                        
019800       ELSE                                                               
019900          PERFORM S10-WRITE-HEADER                                        
020024       END-IF                                                             
020025                                                                          
020026       PERFORM B-WRITE-DATA                                               
021024       PERFORM S01-READ-INFILE                                            
021025                                                                          
022024     END-PERFORM                                                          
022124                                                                          
022224     PERFORM Z-FINIT                                                      
022324                                                                          
022424     MOVE ZERO                   TO    RETURN-CODE                        
022524     GOBACK                                                               
022624     .                                                                    
022724     EJECT                                                                
022824 A-INIT SECTION.                                                          
022924                                                                          
023024     OPEN INPUT  INFILE                                                   
023124     OPEN OUTPUT OUTFILE                                                  
023224                                                                          
023324     ACCEPT TODAYS-DATE          FROM DATE                                
023424     MOVE IDPGM                  TO        POSTSUM-PROGNAMN               
023524     INITIALIZE UT-RAD                                                    
023624     .                                                                    
023724     EJECT                                                                
023824                                                                          
025224 B-WRITE-DATA SECTION.                                                    
025324                                                                          
025424     MOVE IN-IDARTNR         TO  UT-IDARTNR                               
025425     MOVE IN-IDBERED         TO  UT-IDBERED                               
025426     MOVE IN-IDCDS           TO  UT-IDCDS                                 
025427     MOVE IN-SPAREPARTUNIT   TO  UT-SPAREPARTUNIT                         
025428                                                                          
025524     MOVE +1                 TO  WS-INDX                                  
025624     PERFORM UNTIL WS-INDX    > MAX-INDX                                  
025627       IF IN-FELTEXT (WS-INDX) > SPACES                                   
025628          MOVE IN-FELTEXT(WS-INDX)                                        
025629                             TO  UT-FELTEXT(WS-INDX)                      
025630       ELSE                                                               
025631          MOVE MAX-INDX      TO WS-INDX                                   
025632       END-IF                                                             
025640       ADD 1                 TO WS-INDX                                   
026124     END-PERFORM                                                          
026224     MOVE UT-RAD             TO  UT-AREA                                  
026324                                                                          
026424     PERFORM S11-WRITE-OUTFILE                                            
026524     INITIALIZE UT-RAD                                                    
026624     .                                                                    
026724     EJECT                                                                
026824                                                                          
026924 Z-FINIT SECTION.                                                         
027024                                                                          
027124     CLOSE INFILE                                                         
027224           OUTFILE                                                        
027324     SKIP2                                                                
027424     MOVE 'S'                    TO    POSTSUM-OPKOD                      
027524     CALL POSTSUM             USING    POSTSUM-PARM                       
027624     .                                                                    
027724     EJECT                                                                
027824                                                                          
027924 S01-READ-INFILE  SECTION.                                                
028024     READ INFILE                 INTO  IN-AREA                            
028124     AT END                                                               
028224        MOVE HIGH-VALUE          TO    IN-AREA                            
028324        SET END-OF-INFILE        TO TRUE                                  
028424                                                                          
028524     NOT AT END                                                           
028624        MOVE 'INFILE'            TO    POSTSUM-FDNAMN                     
028724        MOVE 'W11814D1'          TO    POSTSUM-DDNAMN2                    
028824        MOVE SPACES              TO    POSTSUM-TRANSTYP                   
028924        CALL POSTSUM          USING    POSTSUM-PARM                       
029024     END-READ                                                             
029124     .                                                                    
029224     EJECT                                                                
029324                                                                          
029424                                                                          
029425 S10-WRITE-HEADER SECTION.                                                
029426                                                                          
029427     MOVE WS-DAP-LINE1           TO UT-AREA                               
029428     PERFORM S11-WRITE-OUTFILE                                            
029430     MOVE IN-SPAREPARTUNIT       TO WS-DAP-TYPE                           
029436     MOVE WS-DAP-LINE2           TO UT-AREA                               
029437     PERFORM S11-WRITE-OUTFILE                                            
029438     MOVE UT-HEADER              TO UT-AREA                               
029439     PERFORM S11-WRITE-OUTFILE                                            
029521     .                                                                    
029524 S11-WRITE-OUTFILE SECTION.                                               
029624                                                                          
029724     WRITE UT-RECORD             FROM  UT-AREA                            
029824     MOVE SPACES                 TO    UT-AREA                            
029924                                                                          
030024     MOVE SPACES                 TO    POSTSUM-TRANSTYP                   
030124     MOVE 'OUTFILE'              TO    POSTSUM-FDNAMN                     
030224     MOVE 'W11814D2'             TO    POSTSUM-DDNAMN2                    
030324     CALL POSTSUM             USING    POSTSUM-PARM                       
030424     .                                                                    
030524     EJECT                                                                
